use std::slice::from_ref;

use parser::{BinaryOp, UnaryOp};

use crate::{
    dedup::{Dep, UniqueNodes},
    users::{Deps, Users, Uses},
};

pub type Data = Dep<DataKind>;
pub type Ctrl = Dep<CtrlKind>;
pub type Branch = Dep<BranchKind>;
pub type Merge = Dep<MergeKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataKind {
    // Basic Data Types
    Literal {
        literal: (),
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,

    Unary {
        op: UnaryOp,
        value: Data,
    },
    /// `lhs <= rhs`
    Binary {
        op: BinaryOp,
        ops: [Data; 2],
    },
    Phi {
        merge: Merge,
        /// The IDs have to be in increasing order
        variants: Box<[Data]>,
    },
}

impl Deps for DataKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        match self {
            DataKind::Literal { .. }
            | DataKind::Quote { .. }
            | DataKind::Boolean(_)
            | DataKind::Unit => Uses::default(),
            DataKind::Unary { value, .. } => Uses::default().with_data(from_ref(value)),
            DataKind::Binary { ops, .. } => Uses::default().with_data(ops),
            DataKind::Phi { merge, variants } => Uses::default()
                .with_data(variants)
                .with_merge(from_ref(merge)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CtrlKind {
    Entry,
    Branch { branch: Branch, idx: usize },
    Merge(Merge),
}

impl Deps for CtrlKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        match self {
            CtrlKind::Entry => Uses::default(),
            CtrlKind::Branch { branch: value, .. } => Uses::default().with_branch(from_ref(value)),
            CtrlKind::Merge(merge) => Uses::default().with_merge(from_ref(merge)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BranchKind {
    parent: Ctrl,
    condition: Data,
}

impl Deps for BranchKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        Uses::default()
            .with_ctrl(from_ref(&self.parent))
            .with_data(from_ref(&self.condition))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeKind {
    prev: Box<[Ctrl]>,
}

impl Deps for MergeKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        Uses::default().with_ctrl(&self.prev)
    }
}

pub struct Nodes {
    pub data: UniqueNodes<DataKind>,
    pub ctrl: UniqueNodes<CtrlKind>,
    pub branch: UniqueNodes<BranchKind>,
    pub merge: UniqueNodes<MergeKind>,
}

/// `U` stands for Unique and User. In this graph every node is unique and changes
/// can be followed back to the onces depending on the changed node
pub struct UGraph {
    nodes: Nodes,
    users: Users,
}

impl UGraph {
    fn add_data_node(&mut self, node: DataKind) -> Data {
        let entry = self.nodes.data.entry(&node);
        self.users
            .add_data_user(&self.nodes, entry.idx(), node.uses());
        entry.add_node(&mut self.nodes.data, node)
    }

    fn add_ctrl_node(&mut self, node: CtrlKind) -> Ctrl {
        let entry = self.nodes.ctrl.entry(&node);
        self.users
            .add_ctrl_user(&self.nodes, entry.idx(), node.uses());
        entry.add_node(&mut self.nodes.ctrl, node)
    }

    fn add_branch_node(&mut self, node: BranchKind) -> Branch {
        let entry = self.nodes.branch.entry(&node);
        self.users
            .add_branch_user(&self.nodes, entry.idx(), node.uses());
        entry.add_node(&mut self.nodes.branch, node)
    }

    fn add_merge_node(&mut self, node: MergeKind) -> Merge {
        let entry = self.nodes.merge.entry(&node);
        self.users
            .add_merge_user(&self.nodes, entry.idx(), node.uses());
        entry.add_node(&mut self.nodes.merge, node)
    }
}

mod graph_indexing {
    use std::ops::Index;

    use crate::graph::{Data, DataKind, UGraph};

    impl Index<Data> for UGraph {
        type Output = DataKind;
        fn index(&self, index: Data) -> &Self::Output {
            &self.nodes.data[index]
        }
    }
}
