use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    grapher::{BuildError, BuildResult, item::ItemID},
    literal_parsing::Literal,
    parser::BuiltinType,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    utilities::{NoDealloc, Rc},
};

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
    Never,

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
    Never,
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
    data_cache: HashMap<DataKey, DataID>,
    type_cache: HashMap<TypeKey, TypeID>,
    // these are certain always needed things that are therefore not stored in the HashMap
    start: CtrlID,      // == CtrlKind::Start
    unit_type: TypeID,  // == TypeKind::Unit
    unit: DataID,       // .kind == DataKind::Unit
    error_type: TypeID, // == TypeKind::Error
    error: DataID,      // .kind == DataKind::Error
}
impl Graph {
    pub fn new(arena: Bump) -> Self {
        let start = Rc::<CtrlKind, NoDealloc>::new_in_bump(CtrlKind::Start, &arena);
        let unit_type = Rc::<TypeKind, NoDealloc>::new_in_bump(
            TypeKind::BuiltinType(BuiltinType::Unit),
            &arena,
        );
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
            data_cache: HashMap::new(),
            type_cache: HashMap::new(),
            arena,
            start,
            unit,
            unit_type,
            error_type,
            error,
        }
    }

    fn push_data(&mut self, kind: DataKind, ty: TypeID) -> DataID {
        let key = kind.key();
        if let Some(ref key) = key
            && let Some(existing) = self.data_cache.get(key)
        {
            return existing.clone();
        }

        let node = DataNode { kind, ty };
        let id = Rc::<DataNode, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.data_cache.insert(key, id.clone());
        }
        id
    }

    fn push_type(&mut self, ty: TypeKind) -> TypeID {
        let key = ty.key();
        if let Some(ref key) = key
            && let Some(existing) = self.type_cache.get(key)
        {
            return existing.clone();
        }

        let id = Rc::<TypeKind, NoDealloc>::new_in_bump(ty, &self.arena);
        if let Some(key) = key {
            self.type_cache.insert(key, id.clone());
        }
        id
    }

    fn push_data_no_dedup(&mut self, kind: DataKind, ty: TypeID) -> DataID {
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
        if phi.variants.is_empty() {
            self.push_data(DataKind::Never, ty)
        } else {
            self.push_data(DataKind::Phi { phi }, ty)
        }
    }

    pub fn add_phi_no_dedup(&mut self, phi: PhiID, ty: TypeID) -> DataID {
        self.push_data_no_dedup(DataKind::Phi { phi }, ty)
    }

    pub fn add_boolean(&mut self, boolean: bool) -> DataID {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Bool));
        self.push_data(DataKind::Boolean(boolean), ty)
    }

    pub fn add_builtin_type(&mut self, ty: BuiltinType) -> TypeID {
        self.push_type(TypeKind::BuiltinType(ty))
    }

    pub fn start(&self) -> CtrlID {
        self.start.clone()
    }
    pub fn unit_type(&self) -> TypeID {
        self.unit_type.clone()
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

    pub fn add_never(&mut self) -> DataID {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Never));
        self.push_data(DataKind::Never, ty)
    }
}

impl TypeKind {
    fn key(&self) -> Option<TypeKey> {
        match self {
            TypeKind::Type => Some(TypeKey::Type),
            TypeKind::BuiltinType(builtin_type) => Some(TypeKey::BuiltinType(*builtin_type)),
            TypeKind::DataType { data } => Some(TypeKey::DataType { data: data.addr() }),
            TypeKind::Error => Some(TypeKey::Error),
        }
    }
}

impl DataKind {
    fn key(&self) -> Option<DataKey> {
        match self {
            DataKind::Literal { literal } => Some(DataKey::Literal {
                literal: literal.clone(),
            }),
            DataKind::Quote { quote } => Some(DataKey::Quote {
                quote: quote.clone(),
            }),
            DataKind::Boolean(boolean) => Some(DataKey::Boolean(*boolean)),
            DataKind::Unit => Some(DataKey::Unit),
            DataKind::Never => Some(DataKey::Never),
            DataKind::Unary { op, value: input } => Some(DataKey::Unary {
                op: *op,
                input: input.addr(),
            }),
            DataKind::Binary { op, lhs, rhs } => Some(DataKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            }),
            DataKind::Load { ctrl: mem, addr } => Some(DataKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            }),
            DataKind::Phi { phi } => Some(DataKey::Phi {
                merge: phi.merge.addr(),
                variants: phi.variants.iter().map(|variant| variant.addr()).collect(),
            }),
            DataKind::Type { ty } => Some(DataKey::Type { ty: ty.addr() }),
            DataKind::Error => Some(DataKey::Error),
        }
    }
}

#[cfg(never)]
#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::{CtrlKind, DataKind, Graph};
    use crate::{
        grapher::BuildError,
        literal_parsing::Literal,
        parser::BuiltinType,
        tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    };

    fn new_graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn deduplicates_stable_data_nodes_by_default() {
        let mut graph = new_graph();

        let lit_a = graph.add_literal(Literal::from(7));
        let lit_b = graph.add_literal(Literal::from(7));
        assert_eq!(lit_a.addr(), lit_b.addr());

        let ty_a = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let ty_b = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        assert_eq!(ty_a.addr(), ty_b.addr());

        let unary_a = graph.add_unary(UnaryOp::Neg, lit_a.clone());
        let unary_b = graph.add_unary(UnaryOp::Neg, lit_b.clone());
        assert_eq!(unary_a.addr(), unary_b.addr());

        let binary_a = graph.add_binary(BinaryOp::Add, lit_a.clone(), ty_a.clone());
        let binary_b = graph.add_binary(BinaryOp::Add, lit_b, ty_b);
        assert_eq!(binary_a.addr(), binary_b.addr());
    }

    #[test]
    fn deduplicates_phi_data_when_requested_through_regular_path() {
        let mut graph = new_graph();
        let value = graph.add_literal(Literal::from(1));
        let ctrl = graph.get_ctrl().expect("start ctrl");
        let merge = graph.add_merge(vec![ctrl]);
        let phi = graph.add_phi(merge, vec![value]);

        let data_a = graph.add_data_phi(phi.clone());
        let data_b = graph.add_data_phi(phi);

        assert_eq!(data_a.addr(), data_b.addr());
    }

    #[test]
    fn empty_phi_and_empty_merge_represent_unreachable_flow() {
        let mut graph = new_graph();

        let start = graph.get_ctrl().expect("start ctrl");
        assert!(matches!(*start, CtrlKind::Start));

        let empty_merge = graph.add_merge(vec![]);
        graph.add_merge_to_ctrl(empty_merge.clone());
        assert!(graph.is_unreachable());
        assert!(matches!(graph.get_ctrl(), Err(BuildError::UnreachableCtrl)));

        let empty_phi = graph.add_phi(empty_merge, vec![]);
        let data = graph.add_data_phi(empty_phi);
        assert!(matches!(data.kind, DataKind::Never));
    }

    #[test]
    fn branches_remember_their_input_control_and_condition() {
        let mut graph = new_graph();
        let ctrl = graph.get_ctrl().expect("start ctrl");
        let condition = graph.add_bool(true);

        let (false_ctrl, true_ctrl) = graph.add_branch(ctrl.clone(), condition.clone());
        let CtrlKind::FalseBranch {
            branch: false_branch,
        } = &*false_ctrl
        else {
            panic!("expected false branch control");
        };
        let CtrlKind::TrueBranch {
            branch: true_branch,
        } = &*true_ctrl
        else {
            panic!("expected true branch control");
        };

        assert_eq!(false_branch.addr(), true_branch.addr());
        assert_eq!(false_branch.ctrl.addr(), ctrl.addr());
        assert_eq!(false_branch.condition.addr(), condition.addr());
    }
}
