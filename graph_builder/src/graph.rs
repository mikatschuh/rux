use std::collections::HashMap;

use parser::{BinaryOp, BuiltinType, UnaryOp};
use tokenizer::{Literal, TypeSize};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Data(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct DataPlaceholder(usize);

impl DataPlaceholder {
    pub fn data(&self) -> Data {
        Data(self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Ctrl(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct CtrlPlaceholder(usize);

impl CtrlPlaceholder {
    pub fn ctrl(&self) -> Ctrl {
        Ctrl(self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct BranchID(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MergeID(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct PhiID(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Type(usize);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch {
    pub ctrl: Ctrl,
    pub condition: Data,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Merge {
    pub prev: Box<[Ctrl]>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Phi {
    pub merge: MergeID, // merge always needs to have the same number of branches as the phi variants
    pub variants: Box<[Data]>,
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
pub enum DataKind<'src> {
    Literal { literal: Literal<'src> },
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

#[derive(Debug)]
pub struct Graph<'src> {
    datas: Vec<DataKind<'src>>,
    typed: Vec<Type>,
    ctrls: Vec<CtrlKind>,
    branches: Vec<Branch>,
    merges: Vec<Merge>,
    phis: Vec<Phi>,
    types: Vec<TypeKind>,
    type_cache: HashMap<TypeKey, Type>,

    target_ptr_size: TypeSize,
}

impl<'src> Graph<'src> {
    const DEFAULT_TYPES: [TypeKind; 2] = [TypeKind::BuiltinType(BuiltinType::Unit), TypeKind::Err];
    const DEFALT_TYPE_CACHE: [(TypeKey, Type); 2] = [
        (TypeKey::BuiltinType(BuiltinType::Unit), Self::UNIT_TYPE),
        (TypeKey::Err, Self::ERR_TYPE),
    ];
    const DEFAULT_DATA: [DataKind<'src>; 2] = [DataKind::Unit, DataKind::Err];
    const DEFAULT_TYPED: [Type; 2] = [Self::UNIT_TYPE, Self::ERR_TYPE];

    const START: Ctrl = Ctrl(0);
    const UNIT_TYPE: Type = Type(0);
    const UNIT: Data = Data(0);
    const ERR_TYPE: Type = Type(1);
    const ERR: Data = Data(1);

    pub fn new(target_ptr_size: TypeSize) -> Self {
        Self {
            datas: Vec::from(Self::DEFAULT_DATA),
            typed: Vec::from(Self::DEFAULT_TYPED),
            ctrls: vec![CtrlKind::Start],
            branches: vec![],
            merges: vec![],
            phis: vec![],
            types: Vec::from(Self::DEFAULT_TYPES),
            type_cache: HashMap::from(Self::DEFALT_TYPE_CACHE),

            target_ptr_size,
        }
    }

    pub fn get_type(&self, data: Data) -> Type {
        self.typed[data.0]
    }

    pub(super) fn type_ids(&self) -> impl Iterator<Item = Type> + '_ {
        (0..self.types.len()).map(Type)
    }

    fn push_type(&mut self, ty: TypeKind) -> Type {
        let key = ty.key();
        if let Some(existing) = self.type_cache.get(&key) {
            return *existing;
        }
        let type_id = Type(self.types.len());
        self.types.push(ty);
        self.type_cache.insert(key, type_id);
        type_id
    }

    fn push_data(&mut self, kind: DataKind<'src>, ty: Type) -> Data {
        let len = self.datas.len();
        self.datas.push(kind);
        self.typed.push(ty);
        Data(len)
    }

    fn push_ctrl_node(&mut self, ctrl: CtrlKind) -> Ctrl {
        let len = self.ctrls.len();
        self.ctrls.push(ctrl);
        Ctrl(len)
    }

    fn push_branch(&mut self, branch: Branch) -> BranchID {
        let len = self.branches.len();
        self.branches.push(branch);
        BranchID(len)
    }

    pub fn add_merge(&mut self, branches: Box<[Ctrl]>) -> MergeID {
        let len = self.merges.len();
        self.merges.push(Merge { prev: branches });
        MergeID(len)
    }

    pub fn add_phi(&mut self, merge: MergeID, variants: Box<[Data]>) -> PhiID {
        let len = self.phis.len();
        self.phis.push(Phi { merge, variants });
        PhiID(len)
    }

    pub fn add_ctrl_merge(&mut self, merge: MergeID) -> Ctrl {
        self.push_ctrl_node(CtrlKind::Merge { merge })
    }

    pub fn add_branch(&mut self, ctrl: Ctrl, condition: Data) -> (Ctrl, Ctrl) {
        let branch = self.push_branch(Branch { ctrl, condition });
        (
            self.push_ctrl_node(CtrlKind::FalseBranch { branch }),
            self.push_ctrl_node(CtrlKind::TrueBranch { branch }),
        )
    }

    pub fn add_load(&mut self, ctrl: Ctrl, addr: Data, ty: Type) -> Data {
        self.push_data(DataKind::Load { ctrl, addr }, ty)
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> Data {
        let ty = self.push_type(TypeKind::BuiltinType(BuiltinType::Unsigned {
            size: self.target_ptr_size,
        }));
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
        Self::START
    }
    pub fn unit(&self) -> Data {
        Self::UNIT
    }
    pub fn err(&self) -> Data {
        Self::ERR
    }
    pub fn error_type(&self) -> Type {
        Self::ERR_TYPE
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

mod graph_indexing {
    use std::ops::{Index, IndexMut};

    use crate::graph::{CtrlPlaceholder, DataPlaceholder};

    use super::{
        Branch, BranchID, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, MergeID, Phi, PhiID, Type,
        TypeKind,
    };

    impl<'src> Index<Data> for Graph<'src> {
        type Output = DataKind<'src>;
        fn index(&self, index: Data) -> &Self::Output {
            &self.datas[index.0]
        }
    }

    impl<'src> Index<DataPlaceholder> for Graph<'src> {
        type Output = DataKind<'src>;
        fn index(&self, index: DataPlaceholder) -> &Self::Output {
            &self.datas[index.0]
        }
    }

    impl<'src> IndexMut<DataPlaceholder> for Graph<'src> {
        fn index_mut(&mut self, index: DataPlaceholder) -> &mut Self::Output {
            &mut self.datas[index.0]
        }
    }

    impl<'src> Index<Ctrl> for Graph<'src> {
        type Output = CtrlKind;
        fn index(&self, index: Ctrl) -> &Self::Output {
            &self.ctrls[index.0]
        }
    }

    impl<'src> Index<CtrlPlaceholder> for Graph<'src> {
        type Output = CtrlKind;
        fn index(&self, index: CtrlPlaceholder) -> &Self::Output {
            &self.ctrls[index.0]
        }
    }

    impl<'src> IndexMut<CtrlPlaceholder> for Graph<'src> {
        fn index_mut(&mut self, index: CtrlPlaceholder) -> &mut Self::Output {
            &mut self.ctrls[index.0]
        }
    }

    impl<'src> Index<BranchID> for Graph<'src> {
        type Output = Branch;
        fn index(&self, index: BranchID) -> &Self::Output {
            &self.branches[index.0]
        }
    }

    impl<'src> Index<MergeID> for Graph<'src> {
        type Output = Merge;
        fn index(&self, index: MergeID) -> &Self::Output {
            &self.merges[index.0]
        }
    }

    impl<'src> Index<PhiID> for Graph<'src> {
        type Output = Phi;
        fn index(&self, index: PhiID) -> &Self::Output {
            &self.phis[index.0]
        }
    }

    impl<'src> Index<Type> for Graph<'src> {
        type Output = TypeKind;
        fn index(&self, index: Type) -> &Self::Output {
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
