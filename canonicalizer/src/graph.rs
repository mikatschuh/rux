use std::slice::from_ref;

use crate::{
    Dir,
    canonical::{Dep, UniqueNodes},
    operator::{BinaryOp, ComBinaryOp, OrderedOps, UnaryOp},
    users::{ListUses, ReportUses, UserTable, Uses},
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

#[derive(Default)]
pub struct Nodes {
    pub data: UniqueNodes<DataKind>,
    pub ctrl: UniqueNodes<CtrlKind>,
    pub branch: UniqueNodes<BranchKind>,
    pub merge: UniqueNodes<MergeKind>,
}

#[derive(Default)]
struct FreeLists {
    data: Vec<Dir<DataKind>>,
    ctrl: Vec<Dir<CtrlKind>>,
    branch: Vec<Dir<BranchKind>>,
    merge: Vec<Dir<MergeKind>>,
}

/// In this graph every node is unique and changes can be followed back to the onces depending on the changed node
#[derive(Default)]
pub struct Graph {
    pub nodes: Nodes,
    free_lists: FreeLists,
    user_table: UserTable,
}

impl Graph {
    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add_data(&mut self, node: DataKind) -> Data {
        let dep = self.nodes.data.allocate();
        self.fill_in_data(dep, node);
        dep
    }

    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add_ctrl(&mut self, node: CtrlKind) -> Ctrl {
        let dep = self.nodes.ctrl.allocate();
        self.fill_in_ctrl(dep, node);
        dep
    }

    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add_branch(&mut self, node: BranchKind) -> Branch {
        let dep = self.nodes.branch.allocate();
        self.fill_in_branch(dep, node);
        dep
    }

    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add_merge(&mut self, node: MergeKind) -> Merge {
        let dep = self.nodes.merge.allocate();
        self.fill_in_merge(dep, node);
        dep
    }

    pub(crate) fn fill_in_data(&mut self, dep: Data, node: DataKind) {
        self.nodes
            .data
            .add_entry(
                &mut self.free_lists.data,
                &self.nodes,
                &mut self.user_table,
                node,
            )
            .fill_in(&mut self.nodes.data, dep)
    }

    pub(crate) fn fill_in_ctrl(&mut self, dep: Ctrl, node: CtrlKind) {
        self.nodes
            .ctrl
            .add_entry(
                &mut self.free_lists.ctrl,
                &self.nodes,
                &mut self.user_table,
                node,
            )
            .fill_in(&mut self.nodes.ctrl, dep)
    }

    pub(crate) fn fill_in_branch(&mut self, dep: Branch, node: BranchKind) {
        self.nodes
            .branch
            .add_entry(
                &mut self.free_lists.branch,
                &self.nodes,
                &mut self.user_table,
                node,
            )
            .fill_in(&mut self.nodes.branch, dep)
    }

    pub(crate) fn fill_in_merge(&mut self, dep: Merge, node: MergeKind) {
        self.nodes
            .merge
            .add_entry(
                &mut self.free_lists.merge,
                &self.nodes,
                &mut self.user_table,
                node,
            )
            .fill_in(&mut self.nodes.merge, dep)
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap_data(&mut self, old: Data, new: Data) {
        let old_dir = self.nodes.data.direct(old);
        if old_dir == self.nodes.data.direct(new) {
            return;
        }
        self.nodes.data[old_dir].remove_node(old_dir, &self.nodes, &mut self.user_table);
        self.nodes
            .data
            .remap_deps(&mut self.free_lists.data, old, new);
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap_ctrl(&mut self, old: Ctrl, new: Ctrl) {
        let old_dir = self.nodes.ctrl.direct(old);
        if old_dir == self.nodes.ctrl.direct(new) {
            return;
        }
        self.nodes.ctrl[old_dir].remove_node(old_dir, &self.nodes, &mut self.user_table);
        self.nodes
            .ctrl
            .remap_deps(&mut self.free_lists.ctrl, old, new);
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap_branch(&mut self, old: Branch, new: Branch) {
        let old_dir = self.nodes.branch.direct(old);
        if old_dir == self.nodes.branch.direct(new) {
            return;
        }
        self.nodes.branch[old_dir].remove_node(old_dir, &self.nodes, &mut self.user_table);
        self.nodes
            .branch
            .remap_deps(&mut self.free_lists.branch, old, new);
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap_merge(&mut self, old: Merge, new: Merge) {
        let old_dir = self.nodes.merge.direct(old);
        if old_dir == self.nodes.merge.direct(new) {
            return;
        }
        self.nodes.merge[old_dir].remove_node(old_dir, &self.nodes, &mut self.user_table);
        self.nodes
            .merge
            .remap_deps(&mut self.free_lists.merge, old, new);
    }
}

mod graph_indexing {
    use std::ops::Index;

    use super::{Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, MergeKind};

    impl Index<Data> for Graph {
        type Output = DataKind;

        fn index(&self, index: Data) -> &Self::Output {
            &self.nodes.data[index]
        }
    }

    impl Index<Ctrl> for Graph {
        type Output = CtrlKind;

        fn index(&self, index: Ctrl) -> &Self::Output {
            &self.nodes.ctrl[index]
        }
    }

    impl Index<Branch> for Graph {
        type Output = BranchKind;

        fn index(&self, index: Branch) -> &Self::Output {
            &self.nodes.branch[index]
        }
    }

    impl Index<Merge> for Graph {
        type Output = MergeKind;

        fn index(&self, index: Merge) -> &Self::Output {
            &self.nodes.merge[index]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{DataKind, Graph};

    #[test]
    fn remaps_all_aliases_and_reuses_freed_nodes() {
        let mut graph = Graph::default();
        let old = graph.nodes.data.allocate();
        graph.fill_in_data(old, DataKind::Literal(1));
        let alias = graph.nodes.data.allocate();
        graph.fill_in_data(alias, DataKind::Literal(1));
        let new = graph.nodes.data.allocate();
        graph.fill_in_data(new, DataKind::Unit);
        let old_dir = graph.nodes.data.direct(old);

        graph.remap_data(old, new);
        assert_eq!(graph.nodes.data.direct(old), graph.nodes.data.direct(new));
        assert_eq!(graph.nodes.data.direct(alias), graph.nodes.data.direct(new));
        graph.remap_data(alias, new);

        let replacement = graph.nodes.data.allocate();
        graph.fill_in_data(replacement, DataKind::Literal(1));
        assert_eq!(graph.nodes.data.direct(replacement), old_dir);
        assert_eq!(graph.nodes.data[replacement], DataKind::Literal(1));
        assert_eq!(graph.nodes.data[old], DataKind::Unit);
        assert_eq!(graph.nodes.data[alias], DataKind::Unit);
    }
}
