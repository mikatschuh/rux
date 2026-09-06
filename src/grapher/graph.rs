use std::collections::HashMap;

use crate::{
    grapher::item::ItemID,
    literal_parsing::Literal,
    parser::BuiltinType,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Data(usize);

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct DataPlaceholder(usize);

impl DataPlaceholder {
    pub fn data(&self) -> Data {
        Data(self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Ctrl(usize);

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct CtrlPlaceholder(usize);

impl CtrlPlaceholder {
    pub fn ctrl(&self) -> Ctrl {
        Ctrl(self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct BranchID(usize);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct MergeID(usize);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct PhiID(usize);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Type(usize);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DataNode {
    pub ty: Type,
    pub kind: DataKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch {
    pub ctrl: Ctrl,
    pub condition: Data,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Merge {
    pub branches: Vec<Ctrl>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Phi {
    pub merge: MergeID, // merge always needs to have the same number of branches as the phi variants
    pub variants: Vec<Data>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum CtrlKind {
    Start,
    Merge { merge: MergeID },
    TrueBranch { branch: BranchID },
    FalseBranch { branch: BranchID },
    Placeholder,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeKind {
    Type,
    BuiltinType(BuiltinType),

    TypeData { data: Data },

    Err,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeKey {
    Type,
    BuiltinType(BuiltinType),

    DataType { data: usize },

    Err,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum DataKind {
    Literal { literal: Literal },
    Quote { quote: String },
    Boolean(bool),
    Unit,

    Unary { op: UnaryOp, value: Data },
    Binary { op: BinaryOp, lhs: Data, rhs: Data },
    Load { ctrl: Ctrl, addr: Data },

    Phi { phi: PhiID },

    Type { ty: Type },

    Placeholder,
    Err,
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
    data_nodes: Vec<DataNode>,
    ctrl_nodes: Vec<CtrlKind>,
    branches: Vec<Branch>,
    merges: Vec<Merge>,
    phis: Vec<Phi>,
    types: Vec<TypeKind>,
    type_cache: HashMap<TypeKey, Type>,
}

impl Graph {
    const DEFAULT_TYPES: [TypeKind; 2] = [TypeKind::BuiltinType(BuiltinType::Unit), TypeKind::Err];
    const DEFALT_TYPE_CACHE: [(TypeKey, Type); 2] = [
        (TypeKey::BuiltinType(BuiltinType::Unit), Self::UNIT_TYPE),
        (TypeKey::Err, Self::ERR_TYPE),
    ];
    const DEFAULT_DATA: [DataNode; 2] = [
        DataNode {
            ty: Self::UNIT_TYPE,
            kind: DataKind::Unit,
        },
        DataNode {
            ty: Self::ERR_TYPE,
            kind: DataKind::Err,
        },
    ];

    const START: Ctrl = Ctrl(0);
    const UNIT_TYPE: Type = Type(0);
    const UNIT: Data = Data(0);
    const ERR_TYPE: Type = Type(1);
    const ERR: Data = Data(1);

    pub fn new() -> Self {
        let data_nodes = Vec::from(Self::DEFAULT_DATA);
        let ctrl_nodes = vec![CtrlKind::Start];
        let branches = vec![];
        let merges = vec![];
        let phis = vec![];
        let types = Vec::from(Self::DEFAULT_TYPES);
        let type_cache = HashMap::from(Self::DEFALT_TYPE_CACHE);

        Self {
            data_nodes,
            ctrl_nodes,
            branches,
            merges,
            phis,
            types,
            type_cache,
        }
    }

    pub(super) fn type_ids(&self) -> impl Iterator<Item = Type> + '_ {
        (0..self.types.len()).map(Type)
    }

    fn push_type(&mut self, ty: TypeKind) -> Type {
        let key = ty.key();
        if let Some(existing) = self.type_cache.get(&key) {
            return existing.clone();
        }
        let len = self.types.len();
        self.types.push(ty);
        Type(len)
    }

    fn push_data(&mut self, kind: DataKind, ty: Type) -> Data {
        let len = self.data_nodes.len();
        self.data_nodes.push(DataNode { kind, ty });
        Data(len)
    }

    fn push_ctrl_node(&mut self, node: CtrlKind) -> Ctrl {
        let len = self.ctrl_nodes.len();
        self.ctrl_nodes.push(node);
        Ctrl(len)
    }

    fn push_branch(&mut self, branch: Branch) -> BranchID {
        let len = self.branches.len();
        self.branches.push(branch);
        BranchID(len)
    }

    pub fn add_merge(&mut self, branches: Vec<Ctrl>) -> MergeID {
        let len = self.merges.len();
        self.merges.push(Merge { branches });
        MergeID(len)
    }

    pub fn add_phi(&mut self, merge: MergeID, variants: Vec<Data>) -> PhiID {
        let len = self.phis.len();
        self.phis.push(Phi { merge, variants });
        PhiID(len)
    }

    pub fn type_as_data(&mut self, ty: Type) -> Data {
        let types_type = self.push_type(TypeKind::Type);
        self.push_data(DataKind::Type { ty }, types_type)
    }

    pub fn add_ctrl_merge(&mut self, merge: MergeID) -> Ctrl {
        self.push_ctrl_node(CtrlKind::Merge { merge })
    }

    pub fn add_branch(&mut self, ctrl: Ctrl, condition: Data) -> (Ctrl, Ctrl) {
        let branch = self.push_branch(Branch { ctrl, condition });
        (
            self.push_ctrl_node(CtrlKind::FalseBranch {
                branch: branch.clone(),
            }),
            self.push_ctrl_node(CtrlKind::TrueBranch { branch }),
        )
    }

    pub fn add_load(&mut self, ctrl: Ctrl, addr: Data, ty: Type) -> Data {
        self.push_data(DataKind::Load { ctrl, addr }, ty)
    }

    pub fn add_literal(&mut self, literal: Literal) -> Data {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Complit));
        self.push_data(DataKind::Literal { literal }, ty)
    }

    pub fn add_unary(&mut self, op: UnaryOp, value: Data, ty: Type) -> Data {
        self.push_data(DataKind::Unary { op, value }, ty)
    }

    pub fn add_binary(&mut self, op: BinaryOp, lhs: Data, rhs: Data, ty: Type) -> Data {
        self.push_data(DataKind::Binary { op, lhs, rhs }, ty)
    }

    pub fn add_data_phi(&mut self, phi: PhiID, ty: Type) -> Data {
        self.push_data(DataKind::Phi { phi }, ty)
    }

    pub fn add_boolean(&mut self, boolean: bool) -> Data {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Bool));
        self.push_data(DataKind::Boolean(boolean), ty)
    }

    pub fn add_builtin_type(&mut self, ty: BuiltinType) -> Type {
        self.push_type(TypeKind::BuiltinType(ty))
    }

    pub fn add_placeholder(&mut self, ty: Type) -> DataPlaceholder {
        DataPlaceholder(self.push_data(DataKind::Placeholder, ty).0)
    }

    pub fn add_ctrl_placeholder(&mut self) -> CtrlPlaceholder {
        CtrlPlaceholder(self.push_ctrl_node(CtrlKind::Placeholder).0)
    }

    pub fn start(&self) -> Ctrl {
        Self::START.clone()
    }
    pub fn unit(&self) -> Data {
        Self::UNIT.clone()
    }
    pub fn err(&self) -> Data {
        Self::ERR.clone()
    }
    pub fn error_type(&self) -> Type {
        Self::ERR_TYPE.clone()
    }
}

impl TypeKind {
    fn key(&self) -> TypeKey {
        match self {
            TypeKind::Type => TypeKey::Type,
            TypeKind::BuiltinType(builtin_type) => TypeKey::BuiltinType(*builtin_type),
            TypeKind::TypeData { data } => TypeKey::DataType { data: data.0 },
            TypeKind::Err => TypeKey::Err,
        }
    }
}

impl DataKind {
    fn key(&self, graph: Graph) -> DataKey {
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
                input: input.0,
            },
            DataKind::Binary { op, lhs, rhs } => DataKey::Binary {
                op: *op,
                lhs: lhs.0,
                rhs: rhs.0,
            },
            DataKind::Load { ctrl: mem, addr } => DataKey::Load {
                mem: mem.0,
                addr: addr.0,
            },
            DataKind::Phi { phi } => DataKey::Phi {
                merge: graph[phi].merge.0,
                variants: graph[phi]
                    .variants
                    .iter()
                    .map(|variant| variant.0)
                    .collect(),
            },
            DataKind::Type { ty } => DataKey::Type { ty: ty.0 },
            DataKind::Err => DataKey::Error,
            Self::Placeholder => todo!(),
        }
    }
}

mod graph_indexing {
    use std::ops::{Index, IndexMut};

    use crate::grapher::graph::{CtrlPlaceholder, DataPlaceholder};

    use super::{
        Branch, BranchID, Ctrl, CtrlKind, Data, DataNode, Graph, Merge, MergeID, Phi, PhiID, Type,
        TypeKind,
    };

    impl Index<&Data> for Graph {
        type Output = DataNode;
        fn index(&self, index: &Data) -> &Self::Output {
            &self.data_nodes[index.0]
        }
    }

    impl Index<&DataPlaceholder> for Graph {
        type Output = DataNode;
        fn index(&self, index: &DataPlaceholder) -> &Self::Output {
            &self.data_nodes[index.0]
        }
    }

    impl IndexMut<&DataPlaceholder> for Graph {
        fn index_mut(&mut self, index: &DataPlaceholder) -> &mut Self::Output {
            &mut self.data_nodes[index.0]
        }
    }

    impl Index<&Ctrl> for Graph {
        type Output = CtrlKind;
        fn index(&self, index: &Ctrl) -> &Self::Output {
            &self.ctrl_nodes[index.0]
        }
    }

    impl Index<&CtrlPlaceholder> for Graph {
        type Output = CtrlKind;
        fn index(&self, index: &CtrlPlaceholder) -> &Self::Output {
            &self.ctrl_nodes[index.0]
        }
    }

    impl IndexMut<&CtrlPlaceholder> for Graph {
        fn index_mut(&mut self, index: &CtrlPlaceholder) -> &mut Self::Output {
            &mut self.ctrl_nodes[index.0]
        }
    }

    impl Index<&BranchID> for Graph {
        type Output = Branch;
        fn index(&self, index: &BranchID) -> &Self::Output {
            &self.branches[index.0]
        }
    }

    impl Index<&MergeID> for Graph {
        type Output = Merge;
        fn index(&self, index: &MergeID) -> &Self::Output {
            &self.merges[index.0]
        }
    }

    impl Index<&PhiID> for Graph {
        type Output = Phi;
        fn index(&self, index: &PhiID) -> &Self::Output {
            &self.phis[index.0]
        }
    }

    impl Index<&Type> for Graph {
        type Output = TypeKind;
        fn index(&self, index: &Type) -> &Self::Output {
            &self.types[index.0]
        }
    }
}

#[cfg(any())]
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

        assert!(matches!(&error.kind, DataKind::Err));
        assert!(matches!(*error.ty, TypeKind::Err));
    }
}
