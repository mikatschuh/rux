use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    grapher::{GraphError, GraphResult},
    literal_parsing::Literal,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    type_parsing::AtomicType,
    utilities::{NoDealloc, Rc},
};

pub type DataID<'src> = Rc<DataNode<'src>, NoDealloc>;
pub type CtrlID<'src> = Rc<CtrlKind<'src>, NoDealloc>;
pub type BranchID<'src> = Rc<Branch<'src>, NoDealloc>;
pub type MergeID<'src> = Rc<Merge<'src>, NoDealloc>;
pub type PhiID<'src> = Rc<Phi<'src>, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DataNode<'src> {
    pub kind: DataKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch<'src> {
    pub ctrl: CtrlID<'src>,
    pub condition: DataID<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Merge<'src> {
    pub branches: Vec<CtrlID<'src>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Phi<'src> {
    pub merge: MergeID<'src>, // merge always needs to have the same number of branches as the phi variants
    pub variants: Vec<DataID<'src>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum CtrlKind<'src> {
    Start,
    Merge { merge: MergeID<'src> },
    TrueBranch { branch: BranchID<'src> },
    FalseBranch { branch: BranchID<'src> },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum DataKind<'src> {
    AtomicType {
        ty: AtomicType,
    },
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,
    Never,

    Unary {
        op: UnaryOp,
        input: DataID<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: DataID<'src>,
        rhs: DataID<'src>,
    },
    Load {
        mem: CtrlID<'src>,
        addr: DataID<'src>,
    },

    Phi {
        phi: PhiID<'src>,
    },

    Deferred,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum DataKey<'src> {
    AtomicType {
        ty: AtomicType,
    },
    Literal {
        literal: Literal<'src>,
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
}

#[derive(Debug)]
pub struct Graph<'src> {
    arena: Bump,
    node_cache: HashMap<DataKey<'src>, DataID<'src>>,
    current_ctrl: Option<CtrlID<'src>>,
}
impl<'src> Graph<'src> {
    pub fn new() -> Self {
        let arena = Bump::new();

        Self {
            node_cache: HashMap::new(),
            current_ctrl: Some(Rc::<CtrlKind<'src>, NoDealloc>::new_in_bump(
                CtrlKind::Start,
                &arena,
            )),
            arena,
        }
    }

    fn push_node(&mut self, kind: DataKind<'src>) -> DataID<'src> {
        let key = kind.node_key();
        if let Some(ref key) = key
            && let Some(existing) = self.node_cache.get(key)
        {
            return existing.clone();
        }

        let node: DataNode<'src> = DataNode { kind };
        let node_id = Rc::<DataNode<'src>, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.node_cache.insert(key, node_id.clone());
        }
        node_id
    }

    fn push_node_no_dedup(&mut self, kind: DataKind<'src>) -> DataID<'src> {
        Rc::<DataNode<'src>, NoDealloc>::new_in_bump(DataNode { kind }, &self.arena)
    }

    fn push_ctrl_node(&mut self, node: CtrlKind<'src>) -> CtrlID<'src> {
        Rc::<CtrlKind<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    fn push_branch(&mut self, branch: Branch<'src>) -> BranchID<'src> {
        Rc::<Branch<'src>, NoDealloc>::new_in_bump(branch, &self.arena)
    }

    pub fn add_merge(&mut self, branches: Vec<CtrlID<'src>>) -> MergeID<'src> {
        Rc::<Merge<'src>, NoDealloc>::new_in_bump(Merge { branches }, &self.arena)
    }

    pub fn add_ctrl_merge(&mut self, merge: MergeID<'src>) -> CtrlID<'src> {
        self.push_ctrl_node(CtrlKind::Merge { merge })
    }

    pub fn add_merge_to_ctrl(&mut self, merge: MergeID<'src>) {
        if merge.branches.is_empty() {
            self.current_ctrl = None;
        } else {
            self.current_ctrl = Some(self.push_ctrl_node(CtrlKind::Merge { merge }))
        }
    }

    pub fn add_branch(
        &mut self,
        ctrl: CtrlID<'src>,
        condition: DataID<'src>,
    ) -> GraphResult<'src, (CtrlID<'src>, CtrlID<'src>)> {
        let branch = self.push_branch(Branch { ctrl, condition });
        Ok((
            self.push_ctrl_node(CtrlKind::FalseBranch {
                branch: branch.clone(),
            }),
            self.push_ctrl_node(CtrlKind::TrueBranch { branch }),
        ))
    }

    pub fn add_load(&mut self, addr: DataID<'src>) -> GraphResult<'src, DataID<'src>> {
        Ok(self.push_node(DataKind::Load {
            mem: self.get_ctrl()?,
            addr,
        }))
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> DataID<'src> {
        self.push_node(DataKind::Literal { literal })
    }

    pub fn add_unary(&mut self, op: UnaryOp, input: DataID<'src>) -> DataID<'src> {
        self.push_node(DataKind::Unary { op, input })
    }

    pub fn add_binary(
        &mut self,
        op: BinaryOp,
        lhs: DataID<'src>,
        rhs: DataID<'src>,
    ) -> DataID<'src> {
        self.push_node(DataKind::Binary { op, lhs, rhs })
    }

    pub fn add_phi(&mut self, merge: MergeID<'src>, variants: Vec<DataID<'src>>) -> PhiID<'src> {
        Rc::<Phi<'src>, NoDealloc>::new_in_bump(Phi { merge, variants }, &self.arena)
    }

    pub fn add_data_phi(&mut self, phi: PhiID<'src>) -> DataID<'src> {
        if phi.variants.is_empty() {
            self.push_node(DataKind::Never)
        } else {
            self.push_node(DataKind::Phi { phi })
        }
    }

    pub fn add_phi_no_dedup(&mut self, phi: PhiID<'src>) -> DataID<'src> {
        self.push_node_no_dedup(DataKind::Phi { phi })
    }

    pub fn add_quote(&mut self, quote: String) -> DataID<'src> {
        self.push_node(DataKind::Quote { quote })
    }

    pub fn add_bool(&mut self, boolean: bool) -> DataID<'src> {
        self.push_node(DataKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, type_: AtomicType) -> DataID<'src> {
        self.push_node(DataKind::AtomicType { ty: type_ })
    }

    pub fn add_deferred(&mut self) -> DataID<'src> {
        self.push_node_no_dedup(DataKind::Deferred)
    }

    pub fn add_unit(&mut self) -> DataID<'src> {
        self.push_node(DataKind::Unit)
    }

    pub fn add_never(&mut self) -> DataID<'src> {
        self.push_node(DataKind::Never)
    }

    pub fn is_unreachable(&self) -> bool {
        self.current_ctrl.is_none()
    }

    pub fn make_unreachable(&mut self) {
        self.current_ctrl = None
    }

    pub fn get_ctrl(&self) -> GraphResult<'src, CtrlID<'src>> {
        if let Some(ctrl) = &self.current_ctrl {
            Ok(ctrl.clone())
        } else {
            Err(GraphError::UnreachableCtrl)
        }
    }

    pub fn set_ctrl(&mut self, ctrl: CtrlID<'src>) {
        self.current_ctrl = Some(ctrl)
    }
}

impl<'src> DataKind<'src> {
    fn node_key(&self) -> Option<DataKey<'src>> {
        match self {
            DataKind::AtomicType { ty } => Some(DataKey::AtomicType { ty: *ty }),
            DataKind::Literal { literal } => Some(DataKey::Literal {
                literal: literal.clone(),
            }),
            DataKind::Quote { quote } => Some(DataKey::Quote {
                quote: quote.clone(),
            }),
            DataKind::Boolean(boolean) => Some(DataKey::Boolean(*boolean)),
            DataKind::Unit => Some(DataKey::Unit),
            DataKind::Never => Some(DataKey::Never),
            DataKind::Unary { op, input } => Some(DataKey::Unary {
                op: *op,
                input: input.addr(),
            }),
            DataKind::Binary { op, lhs, rhs } => Some(DataKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            }),
            DataKind::Load { mem, addr } => Some(DataKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            }),
            DataKind::Phi { phi } => Some(DataKey::Phi {
                merge: phi.merge.addr(),
                variants: phi.variants.iter().map(|variant| variant.addr()).collect(),
            }),
            DataKind::Deferred => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{CtrlKind, DataKind, Graph};
    use crate::{
        grapher::GraphError,
        literal_parsing::Literal,
        tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
        type_parsing::AtomicType,
    };

    #[test]
    fn deduplicates_stable_data_nodes_by_default() {
        let mut graph = Graph::new();

        let lit_a = graph.add_literal(Literal::from(7));
        let lit_b = graph.add_literal(Literal::from(7));
        assert_eq!(lit_a.addr(), lit_b.addr());

        let ty_a = graph.add_type(AtomicType::Signed { size: 32 });
        let ty_b = graph.add_type(AtomicType::Signed { size: 32 });
        assert_eq!(ty_a.addr(), ty_b.addr());

        let unary_a = graph.add_unary(UnaryOp::Neg, lit_a.clone());
        let unary_b = graph.add_unary(UnaryOp::Neg, lit_b.clone());
        assert_eq!(unary_a.addr(), unary_b.addr());

        let binary_a = graph.add_binary(BinaryOp::Add, lit_a.clone(), ty_a.clone());
        let binary_b = graph.add_binary(BinaryOp::Add, lit_b, ty_b);
        assert_eq!(binary_a.addr(), binary_b.addr());
    }

    #[test]
    fn does_not_deduplicate_deferred_or_explicit_no_dedup_phis() {
        let mut graph = Graph::new();

        let unknown_a = graph.add_deferred();
        let unknown_b = graph.add_deferred();
        assert_ne!(unknown_a.addr(), unknown_b.addr());
        assert!(matches!(unknown_a.kind, DataKind::Deferred));
        assert!(matches!(unknown_b.kind, DataKind::Deferred));

        let ctrl = graph.get_ctrl().expect("start ctrl");
        let merge = graph.add_merge(vec![ctrl]);
        let phi = graph.add_phi(merge, vec![unknown_a]);
        let data_a = graph.add_phi_no_dedup(phi.clone());
        let data_b = graph.add_phi_no_dedup(phi);
        assert_ne!(data_a.addr(), data_b.addr());
    }

    #[test]
    fn deduplicates_phi_data_when_requested_through_regular_path() {
        let mut graph = Graph::new();
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
        let mut graph = Graph::new();

        let start = graph.get_ctrl().expect("start ctrl");
        assert!(matches!(*start, CtrlKind::Start));

        let empty_merge = graph.add_merge(vec![]);
        graph.add_merge_to_ctrl(empty_merge.clone());
        assert!(graph.is_unreachable());
        assert!(matches!(graph.get_ctrl(), Err(GraphError::UnreachableCtrl)));

        let empty_phi = graph.add_phi(empty_merge, vec![]);
        let data = graph.add_data_phi(empty_phi);
        assert!(matches!(data.kind, DataKind::Never));
    }

    #[test]
    fn branches_remember_their_input_control_and_condition() {
        let mut graph = Graph::new();
        let ctrl = graph.get_ctrl().expect("start ctrl");
        let condition = graph.add_bool(true);

        let (false_ctrl, true_ctrl) = graph
            .add_branch(ctrl.clone(), condition.clone())
            .expect("branch");

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
