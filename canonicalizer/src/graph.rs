use std::slice::from_ref;

use crate::{
    binary_op::{BinaryOp, ComBinaryOp, OrderedOps},
    canonical::{Dep, UniqueNodes},
    unary_op::UnaryOp,
    users::{ListUses, UserTable, Uses},
};

pub type Data = Dep<DataKind>;
pub type Ctrl = Dep<CtrlKind>;
pub type Branch = Dep<BranchKind>;
pub type Merge = Dep<MergeKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataKind {
    // Basic Data Types
    Unit,
    Literal(usize),
    Quote(String),
    Boolean(bool),

    Unary {
        op: UnaryOp,
        value: Data,
    },
    /// `lhs <= rhs`
    Binary {
        op: BinaryOp,
        ops: [Data; 2],
    },
    ComBinary {
        op: ComBinaryOp,
        ops: OrderedOps,
    },
    Merge {
        merge: Merge,
        /// `variants[i]` is the value arriving through `merge.prev[i]`.
        variants: Box<[Data]>,
    },
}

impl ListUses for DataKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        match self {
            DataKind::Literal(_) | DataKind::Quote(_) | DataKind::Boolean(_) | DataKind::Unit => {
                Uses::default()
            }
            DataKind::Unary { value, .. } => Uses::default().with_data(from_ref(value)),
            DataKind::Binary { ops, .. } => Uses::default().with_data(ops),
            DataKind::ComBinary { ops, .. } => Uses::default().with_data(ops.get()),
            DataKind::Merge { merge, variants } => Uses::default()
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

impl ListUses for CtrlKind {
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
    pub parent: Ctrl,
    pub condition: Data,
}

impl ListUses for BranchKind {
    fn uses<'a>(&'a self) -> Uses<'a> {
        Uses::default()
            .with_ctrl(from_ref(&self.parent))
            .with_data(from_ref(&self.condition))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeKind {
    pub prev: Box<[Ctrl]>,
}

impl ListUses for MergeKind {
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

/// In this graph every node is unique and changes can be followed back to the onces depending on the changed node
pub struct Graph {
    pub nodes: Nodes,
    user_table: UserTable,
}

impl Graph {
    pub fn add_data_node(&mut self, node: DataKind) -> Data {
        self.nodes
            .data
            .new_entry(&self.nodes, &mut self.user_table, node)
            .add_node(&mut self.nodes.data)
    }

    pub fn add_ctrl_node(&mut self, node: CtrlKind) -> Ctrl {
        self.nodes
            .ctrl
            .new_entry(&self.nodes, &mut self.user_table, node)
            .add_node(&mut self.nodes.ctrl)
    }

    pub fn add_branch_node(&mut self, node: BranchKind) -> Branch {
        self.nodes
            .branch
            .new_entry(&self.nodes, &mut self.user_table, node)
            .add_node(&mut self.nodes.branch)
    }

    pub fn add_merge_node(&mut self, node: MergeKind) -> Merge {
        self.nodes
            .merge
            .new_entry(&self.nodes, &mut self.user_table, node)
            .add_node(&mut self.nodes.merge)
    }
}
