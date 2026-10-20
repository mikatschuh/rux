use std::{fmt::Debug, hash::Hash, slice::from_ref};

use crate::{
    Unique,
    canonical::{Canonicalizer, Stable},
    operator::{BinaryOp, ComBinaryOp, OrderedOps, UnaryOp},
    users::{ReportUses, UserTable},
};

pub type Data = Stable<DataKind>;
pub type Ctrl = Stable<CtrlKind>;
pub type Branch = Stable<BranchKind>;
pub type Merge = Stable<MergeKind>;

pub trait ListDeps {
    fn deps<'a>(&'a self) -> NodeDeps<'a>;
}

#[derive(Debug, Default)]
pub struct NodeDeps<'a> {
    pub data: &'a [Data],
    pub ctrl: &'a [Ctrl],
    pub branch: &'a [Branch],
    pub merge: &'a [Merge],
}

impl<'a> NodeDeps<'a> {
    pub fn with_data(self, data: &'a [Data]) -> Self {
        Self { data, ..self }
    }
    pub fn with_ctrl(self, ctrl: &'a [Ctrl]) -> Self {
        Self { ctrl, ..self }
    }
    pub fn with_branch(self, branch: &'a [Branch]) -> Self {
        Self { branch, ..self }
    }
    pub fn with_merge(self, merge: &'a [Merge]) -> Self {
        Self { merge, ..self }
    }
}

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

impl ListDeps for DataKind {
    fn deps<'a>(&'a self) -> NodeDeps<'a> {
        match self {
            DataKind::Literal(_) | DataKind::Quote(_) | DataKind::Boolean(_) | DataKind::Unit => {
                NodeDeps::default()
            }
            DataKind::Unary { value, .. } => NodeDeps::default().with_data(from_ref(value)),
            DataKind::Binary { ops, .. } => NodeDeps::default().with_data(ops),
            DataKind::ComBinary { ops, .. } => NodeDeps::default().with_data(ops.get()),
            DataKind::Merge { merge, variants } => NodeDeps::default()
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

impl ListDeps for CtrlKind {
    fn deps<'a>(&'a self) -> NodeDeps<'a> {
        match self {
            CtrlKind::Entry => NodeDeps::default(),
            CtrlKind::Branch { branch: value, .. } => {
                NodeDeps::default().with_branch(from_ref(value))
            }
            CtrlKind::Merge(merge) => NodeDeps::default().with_merge(from_ref(merge)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BranchKind {
    pub parent: Ctrl,
    pub condition: Data,
}

impl ListDeps for BranchKind {
    fn deps<'a>(&'a self) -> NodeDeps<'a> {
        NodeDeps::default()
            .with_ctrl(from_ref(&self.parent))
            .with_data(from_ref(&self.condition))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MergeKind {
    pub prev: Box<[Ctrl]>,
}

impl ListDeps for MergeKind {
    fn deps<'a>(&'a self) -> NodeDeps<'a> {
        NodeDeps::default().with_ctrl(&self.prev)
    }
}

pub trait Node: Sized + Clone + Eq + Hash + ReportUses + Debug {
    fn nodes(nodes: &Nodes) -> &Canonicalizer<Self>;
    fn nodes_mut(nodes: &mut Nodes) -> &mut Canonicalizer<Self>;
}

impl Node for DataKind {
    fn nodes(nodes: &Nodes) -> &Canonicalizer<Self> {
        &nodes.data
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut Canonicalizer<Self> {
        &mut nodes.data
    }
}

impl Node for CtrlKind {
    fn nodes(nodes: &Nodes) -> &Canonicalizer<Self> {
        &nodes.ctrl
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut Canonicalizer<Self> {
        &mut nodes.ctrl
    }
}

impl Node for BranchKind {
    fn nodes(nodes: &Nodes) -> &Canonicalizer<Self> {
        &nodes.branch
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut Canonicalizer<Self> {
        &mut nodes.branch
    }
}

impl Node for MergeKind {
    fn nodes(nodes: &Nodes) -> &Canonicalizer<Self> {
        &nodes.merge
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut Canonicalizer<Self> {
        &mut nodes.merge
    }
}

#[derive(Default)]
pub struct Nodes {
    pub data: Canonicalizer<DataKind>,
    pub ctrl: Canonicalizer<CtrlKind>,
    pub branch: Canonicalizer<BranchKind>,
    pub merge: Canonicalizer<MergeKind>,
}

/// In this graph every node is unique and changes can be followed back to the onces depending on the changed node
#[derive(Default)]
pub struct Graph {
    pub(crate) nodes: Nodes,
    user_table: UserTable,
}

impl Graph {
    /// Iterates over data users of all aliases of `dep`.
    pub fn data_data_users(
        &self,
        dir: Unique<DataKind>,
    ) -> impl Iterator<Item = Unique<DataKind>> + '_ {
        self.nodes
            .data
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.data[dep].data.iter().copied())
    }

    /// Iterates over branch users of all aliases of `dep`.
    pub fn data_branch_users(
        &self,
        dir: Unique<DataKind>,
    ) -> impl Iterator<Item = Unique<BranchKind>> + '_ {
        self.nodes
            .data
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.data[dep].branch.iter().copied())
    }

    /// Iterates over branch users of all aliases of `dep`.
    pub fn ctrl_branch_users(
        &self,
        dir: Unique<CtrlKind>,
    ) -> impl Iterator<Item = Unique<BranchKind>> + '_ {
        self.nodes
            .ctrl
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.ctrl[dep].branch.iter().copied())
    }

    /// Iterates over merge users of all aliases of `dep`.
    pub fn ctrl_merge_users(
        &self,
        dir: Unique<CtrlKind>,
    ) -> impl Iterator<Item = Unique<MergeKind>> + '_ {
        self.nodes
            .ctrl
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.ctrl[dep].merge.iter().copied())
    }

    /// Iterates over ctrl users of all aliases of `dep`.
    pub fn branch_ctrl_users(
        &self,
        dir: Unique<BranchKind>,
    ) -> impl Iterator<Item = Unique<CtrlKind>> + '_ {
        self.nodes
            .branch
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.branch[dep].ctrl.iter().copied())
    }

    /// Iterates over data users of all aliases of `dep`.
    pub fn merge_data_users(
        &self,
        dir: Unique<MergeKind>,
    ) -> impl Iterator<Item = Unique<DataKind>> + '_ {
        self.nodes
            .merge
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.merge[dep].data.iter().copied())
    }

    /// Iterates over ctrl users of all aliases of `dep`.
    pub fn merge_ctrl_users(
        &self,
        dir: Unique<MergeKind>,
    ) -> impl Iterator<Item = Unique<CtrlKind>> + '_ {
        self.nodes
            .merge
            .stable_deps(dir)
            .flat_map(|dep| self.user_table.merge[dep].ctrl.iter().copied())
    }

    /// Returns the direct handle of a certain dependency.
    pub fn direct<N: Node>(&self, dep: Stable<N>) -> Unique<N> {
        N::nodes(&self.nodes).direct(dep)
    }

    /// Allocates an unfilled dependency and its user-table slot.
    pub fn allocate<N: Node>(&mut self) -> Stable<N> {
        N::nodes_mut(&mut self.nodes).allocate(&mut self.user_table)
    }

    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add<N: Node>(&mut self, node: N) -> Stable<N> {
        let dep = self.allocate();
        self.bind(dep, node);
        dep
    }

    /// Adds a node to an unfilled dependency.
    pub(crate) fn bind<N: Node>(&mut self, dep: Stable<N>, node: N) {
        N::nodes_mut(&mut self.nodes).add_node(&mut self.user_table, dep, node)
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap<N: Node>(&mut self, old: Unique<N>, new: Unique<N>) {
        if old == new {
            return;
        }
        N::nodes_mut(&mut self.nodes)[old].remove_node(old, &mut self.user_table);
        N::nodes_mut(&mut self.nodes).remap_deps(old, new);
    }

    pub fn nodes<'a, N: Node + 'a>(&'a self) -> impl Iterator<Item = Unique<N>> {
        N::nodes(&self.nodes).nodes()
    }
}

mod graph_indexing {
    use std::ops::Index;

    use crate::{Unique, canonical::Stable, graph::Node};

    use super::Graph;

    impl<N: Node> Index<Stable<N>> for Graph {
        type Output = N;

        fn index(&self, index: Stable<N>) -> &Self::Output {
            &N::nodes(&self.nodes)[index]
        }
    }

    impl<N: Node> Index<Unique<N>> for Graph {
        type Output = N;

        fn index(&self, index: Unique<N>) -> &Self::Output {
            &N::nodes(&self.nodes)[index]
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::{BinaryOp, BranchKind, CtrlKind, DataKind, Graph, MergeKind};

    #[test]
    fn collects_users_per_alias_across_remapping() {
        let mut graph = Graph::default();
        let value = graph.add(DataKind::Literal(1));
        let alias = graph.add(DataKind::Literal(1));
        let user = graph.add(DataKind::Binary {
            op: BinaryOp::Less,
            ops: [value, alias],
        });
        // Each alias contributes its user set, so this user is returned twice.
        let expected = [graph.direct(user); 2];
        let value_dir = graph.direct(value);
        let alias_dir = graph.direct(alias);
        assert_eq!(
            graph.data_data_users(value_dir).collect::<Vec<_>>(),
            expected
        );
        assert_eq!(
            graph.data_data_users(alias_dir).collect::<Vec<_>>(),
            expected
        );

        let replacement = graph.add(DataKind::Literal(2));
        graph.remap(value_dir, graph.direct(replacement));
        assert_eq!(
            graph
                .data_data_users(graph.direct(replacement))
                .collect::<Vec<_>>(),
            expected
        );
        assert_eq!(
            graph
                .data_data_users(graph.direct(alias))
                .collect::<Vec<_>>(),
            expected
        );

        let reused = graph.add(DataKind::Literal(3));
        assert_eq!(graph.data_data_users(graph.direct(reused)).count(), 0);
        assert_eq!(
            graph
                .data_data_users(graph.direct(replacement))
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn collects_users_of_every_node_kind() {
        let mut graph = Graph::default();
        let condition = graph.add(DataKind::Boolean(true));
        let entry = graph.add(CtrlKind::Entry);
        let entry_alias = graph.add(CtrlKind::Entry);
        let branch = graph.add(BranchKind {
            parent: entry_alias,
            condition,
        });
        let branch_alias = graph.add(BranchKind {
            parent: entry,
            condition,
        });
        let projection = graph.add(CtrlKind::Branch {
            branch: branch_alias,
            idx: 0,
        });
        let merge = graph.add(MergeKind {
            prev: Box::new([entry]),
        });
        let merge_alias = graph.add(MergeKind {
            prev: Box::new([entry]),
        });
        let merged_ctrl = graph.add(CtrlKind::Merge(merge_alias));
        let merged_data = graph.add(DataKind::Merge {
            merge,
            variants: Box::new([condition]),
        });
        let branch_dir = graph.nodes.branch.direct(branch);
        let alias_dir = graph.nodes.branch.direct(branch_alias);
        assert_eq!(
            graph
                .data_data_users(graph.direct(condition))
                .collect::<Vec<_>>(),
            [graph.nodes.data.direct(merged_data)]
        );
        assert_eq!(
            graph
                .data_branch_users(graph.direct(condition))
                .collect::<HashSet<_>>(),
            HashSet::from([branch_dir, alias_dir])
        );
        assert_eq!(
            graph
                .ctrl_branch_users(graph.direct(entry))
                .collect::<HashSet<_>>(),
            HashSet::from([branch_dir, alias_dir])
        );
        assert_eq!(
            graph
                .ctrl_merge_users(graph.direct(entry))
                .collect::<Vec<_>>(),
            [graph.nodes.merge.direct(merge)]
        );
        assert_eq!(
            graph
                .branch_ctrl_users(graph.direct(branch_alias))
                .collect::<Vec<_>>(),
            [graph.nodes.ctrl.direct(projection)]
        );
        assert_eq!(graph.branch_ctrl_users(graph.direct(branch)).count(), 0);
        assert_eq!(
            graph
                .merge_ctrl_users(graph.direct(merge))
                .collect::<Vec<_>>(),
            [graph.nodes.ctrl.direct(merged_ctrl)]
        );
        assert_eq!(
            graph
                .merge_data_users(graph.direct(merge))
                .collect::<Vec<_>>(),
            [graph.nodes.data.direct(merged_data)]
        );
    }

    #[test]
    fn remaps_all_aliases_and_reuses_freed_nodes() {
        let mut graph = Graph::default();
        let old = graph.allocate();
        graph.bind(old, DataKind::Literal(1));
        let alias = graph.allocate();
        graph.bind(alias, DataKind::Literal(1));
        let new = graph.allocate();
        graph.bind(new, DataKind::Unit);
        let old_dir = graph.nodes.data.direct(old);

        graph.remap(old_dir, graph.direct(new));
        assert_eq!(graph.nodes.data.direct(old), graph.nodes.data.direct(new));
        assert_eq!(graph.nodes.data.direct(alias), graph.nodes.data.direct(new));
        graph.remap(graph.direct(alias), graph.direct(new));

        let replacement = graph.allocate();
        graph.bind(replacement, DataKind::Literal(1));
        assert_eq!(graph.nodes.data.direct(replacement), old_dir);
        assert_eq!(graph.nodes.data[replacement], DataKind::Literal(1));
        assert_eq!(graph.nodes.data[old], DataKind::Unit);
        assert_eq!(graph.nodes.data[alias], DataKind::Unit);
    }
}
