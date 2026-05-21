use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    grapher::item::ItemID,
    literal_parsing::Literal,
    parser::BuiltinType,
    ref_count::{NoDealloc, Rc},
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
};

pub struct UniqueNodes {
    pub types: Vec<TypeID>,
    _arena: Bump,
}

pub type DataID = Rc<DataNode, NoDealloc>;
pub type CtrlID = Rc<CtrlKind, NoDealloc>;
pub type BranchID = Rc<Branch, NoDealloc>;
pub type MergeID = Rc<Merge, NoDealloc>;
pub type PhiID = Rc<Phi, NoDealloc>;
pub type TypeID = Rc<TypeKind, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DataNode {
    pub ty: TypeID,
    pub kind: DataKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch {
    pub ctrl: CtrlID,
    pub condition: DataID,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Merge {
    pub branches: Vec<CtrlID>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Phi {
    pub merge: MergeID, // merge always needs to have the same number of branches as the phi variants
    pub variants: Vec<DataID>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum CtrlKind {
    Start,
    Merge { merge: MergeID },
    TrueBranch { branch: BranchID },
    FalseBranch { branch: BranchID },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeKind {
    Type,
    BuiltinType(BuiltinType),

    DataType { data: DataID },

    Error,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeKey {
    Type,
    BuiltinType(BuiltinType),

    DataType { data: usize },

    Error,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum DataKind {
    Literal {
        literal: Literal,
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,

    Unary {
        op: UnaryOp,
        value: DataID,
    },
    Binary {
        op: BinaryOp,
        lhs: DataID,
        rhs: DataID,
    },
    Load {
        ctrl: CtrlID,
        addr: DataID,
    },

    Phi {
        phi: PhiID,
    },

    Type {
        ty: TypeID,
    },

    Placeholder,
    Error,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum DataKey {
    Literal {
        literal: Literal,
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,
    Unary {
        op: UnaryOp,
        input: usize,
    },
    Binary {
        op: BinaryOp,
        lhs: usize,
        rhs: usize,
    },
    Load {
        mem: usize,
        addr: usize,
    },
    Phi {
        merge: usize,
        variants: Vec<usize>,
    },

    Type {
        ty: usize,
    },

    Item {
        item: ItemID,
    },

    Error,
}

#[derive(Debug)]
pub struct Graph {
    arena: Bump,
    type_cache: HashMap<TypeKey, TypeID>,
    // these are certain always needed things that are therefore not stored in the HashMap
    start: CtrlID,      // == CtrlKind::Start
    unit: DataID,       // .kind == DataKind::Unit
    error_type: TypeID, // == TypeKind::Error
    error: DataID,      // .kind == DataKind::Error
}

impl Graph {
    pub fn new(arena: Bump) -> Self {
        let mut type_cache = HashMap::new();

        let start = Rc::<CtrlKind, NoDealloc>::new_in_bump(CtrlKind::Start, &arena);
        let unit_type = Rc::<TypeKind, NoDealloc>::new_in_bump(
            TypeKind::BuiltinType(BuiltinType::Unit),
            &arena,
        );
        let key = unit_type.key();
        type_cache.insert(key, unit_type.clone());

        let unit = Rc::<DataNode, NoDealloc>::new_in_bump(
            DataNode {
                ty: unit_type.clone(),
                kind: DataKind::Unit,
            },
            &arena,
        );
        let error_type = Rc::<TypeKind, NoDealloc>::new_in_bump(TypeKind::Error, &arena);
        let error = Rc::<DataNode, NoDealloc>::new_in_bump(
            DataNode {
                ty: error_type.clone(),
                kind: DataKind::Error,
            },
            &arena,
        );
        Self {
            type_cache,
            arena,
            start,
            unit,
            error_type,
            error,
        }
    }

    pub fn destruct(mut self) -> UniqueNodes {
        UniqueNodes {
            types: self.type_cache.drain().map(|(_, v)| v).collect(),
            _arena: self.arena,
        }
    }

    fn push_type(&mut self, ty: TypeKind) -> TypeID {
        let key = ty.key();
        if let Some(existing) = self.type_cache.get(&key) {
            return existing.clone();
        }

        let id = Rc::<TypeKind, NoDealloc>::new_in_bump(ty, &self.arena);
        self.type_cache.insert(key, id.clone());
        id
    }

    fn push_data(&mut self, kind: DataKind, ty: TypeID) -> DataID {
        Rc::<DataNode, NoDealloc>::new_in_bump(DataNode { kind, ty }, &self.arena)
    }

    fn push_ctrl_node(&mut self, node: CtrlKind) -> CtrlID {
        Rc::<CtrlKind, NoDealloc>::new_in_bump(node, &self.arena)
    }

    fn push_branch(&mut self, branch: Branch) -> BranchID {
        Rc::<Branch, NoDealloc>::new_in_bump(branch, &self.arena)
    }

    pub fn type_as_data(&mut self, ty: TypeID) -> DataID {
        let types_type = self.push_type(TypeKind::Type);
        self.push_data(DataKind::Type { ty }, types_type)
    }

    pub fn add_merge(&mut self, branches: Vec<CtrlID>) -> MergeID {
        Rc::<Merge, NoDealloc>::new_in_bump(Merge { branches }, &self.arena)
    }

    pub fn add_ctrl_merge(&mut self, merge: MergeID) -> CtrlID {
        self.push_ctrl_node(CtrlKind::Merge { merge })
    }

    pub fn add_branch(&mut self, ctrl: CtrlID, condition: DataID) -> (CtrlID, CtrlID) {
        let branch = self.push_branch(Branch { ctrl, condition });
        (
            self.push_ctrl_node(CtrlKind::FalseBranch {
                branch: branch.clone(),
            }),
            self.push_ctrl_node(CtrlKind::TrueBranch { branch }),
        )
    }

    pub fn add_load(&mut self, ctrl: CtrlID, addr: DataID, ty: TypeID) -> DataID {
        self.push_data(DataKind::Load { ctrl, addr }, ty)
    }

    pub fn add_literal(&mut self, literal: Literal) -> DataID {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Complit));
        self.push_data(DataKind::Literal { literal }, ty)
    }

    pub fn add_unary(&mut self, op: UnaryOp, value: DataID, ty: TypeID) -> DataID {
        self.push_data(DataKind::Unary { op, value }, ty)
    }

    pub fn add_binary(&mut self, op: BinaryOp, lhs: DataID, rhs: DataID, ty: TypeID) -> DataID {
        self.push_data(DataKind::Binary { op, lhs, rhs }, ty)
    }

    pub fn add_phi(&mut self, merge: MergeID, variants: Vec<DataID>) -> PhiID {
        Rc::<Phi, NoDealloc>::new_in_bump(Phi { merge, variants }, &self.arena)
    }

    pub fn add_data_phi(&mut self, phi: PhiID, ty: TypeID) -> DataID {
        self.push_data(DataKind::Phi { phi }, ty)
    }

    pub fn add_boolean(&mut self, boolean: bool) -> DataID {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Bool));
        self.push_data(DataKind::Boolean(boolean), ty)
    }

    pub fn add_builtin_type(&mut self, ty: BuiltinType) -> TypeID {
        self.push_type(TypeKind::BuiltinType(ty))
    }

    pub fn add_placeholder(&mut self) -> DataID {
        self.push_data(DataKind::Placeholder, self.error_type())
    }

    pub fn start(&self) -> CtrlID {
        self.start.clone()
    }
    pub fn unit(&self) -> DataID {
        self.unit.clone()
    }
    pub fn error(&self) -> DataID {
        self.error.clone()
    }
    pub fn error_type(&self) -> TypeID {
        self.error_type.clone()
    }
}

impl TypeKind {
    fn key(&self) -> TypeKey {
        match self {
            TypeKind::Type => TypeKey::Type,
            TypeKind::BuiltinType(builtin_type) => TypeKey::BuiltinType(*builtin_type),
            TypeKind::DataType { data } => TypeKey::DataType { data: data.addr() },
            TypeKind::Error => TypeKey::Error,
        }
    }
}

impl DataKind {
    fn key(&self) -> DataKey {
        match self {
            DataKind::Literal { literal } => DataKey::Literal {
                literal: literal.clone(),
            },
            DataKind::Quote { quote } => DataKey::Quote {
                quote: quote.clone(),
            },
            DataKind::Boolean(boolean) => DataKey::Boolean(*boolean),
            DataKind::Unit => DataKey::Unit,
            DataKind::Unary { op, value: input } => DataKey::Unary {
                op: *op,
                input: input.addr(),
            },
            DataKind::Binary { op, lhs, rhs } => DataKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            },
            DataKind::Load { ctrl: mem, addr } => DataKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            },
            DataKind::Phi { phi } => DataKey::Phi {
                merge: phi.merge.addr(),
                variants: phi.variants.iter().map(|variant| variant.addr()).collect(),
            },
            DataKind::Type { ty } => DataKey::Type { ty: ty.addr() },
            DataKind::Error => DataKey::Error,
            Self::Placeholder => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::{CtrlKind, DataKind, Graph, TypeKind};
    use crate::{literal_parsing::Literal, parser::BuiltinType};

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn unit_uses_the_canonical_builtin_unit_type() {
        let mut graph = graph();

        let unit = graph.unit();
        let unit_type = graph.add_builtin_type(BuiltinType::Unit);

        assert_eq!(unit.ty.addr(), unit_type.addr());
    }

    #[test]
    fn deduplicates_stable_type_nodes() {
        let mut graph = graph();

        let ty_a = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let ty_b = graph.add_builtin_type(BuiltinType::Signed { size: 32 });

        assert_eq!(ty_a.addr(), ty_b.addr());
    }

    #[test]
    fn branches_remember_input_control_and_condition() {
        let mut graph = graph();
        let ctrl = graph.start();
        let condition = graph.add_boolean(true);

        let (false_ctrl, true_ctrl) = graph.add_branch(ctrl.clone(), condition.clone());

        let CtrlKind::FalseBranch {
            branch: false_branch,
        } = &*false_ctrl
        else {
            panic!("expected false branch");
        };
        let CtrlKind::TrueBranch {
            branch: true_branch,
        } = &*true_ctrl
        else {
            panic!("expected true branch");
        };

        assert_eq!(false_branch.addr(), true_branch.addr());
        assert_eq!(false_branch.ctrl.addr(), ctrl.addr());
        assert_eq!(false_branch.condition.addr(), condition.addr());
    }

    #[test]
    fn error_node_is_not_accidentally_deduped_through_the_cache() {
        let graph = graph();
        let error = graph.error();

        assert!(matches!(&error.kind, DataKind::Error));
        assert!(matches!(*error.ty, TypeKind::Error));
    }
}
