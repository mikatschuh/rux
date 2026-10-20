use std::{fmt::Debug, hash::Hash, slice::from_ref};

use crate::{
    Dir,
    canonical::{Stable, UniqueNodes},
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
    fn nodes(nodes: &Nodes) -> &UniqueNodes<Self>;
    fn nodes_mut(nodes: &mut Nodes) -> &mut UniqueNodes<Self>;
    fn free_list_mut(free_list: &mut FreeLists) -> &mut Vec<Dir<Self>>;
}

impl Node for DataKind {
    fn nodes(nodes: &Nodes) -> &UniqueNodes<Self> {
        &nodes.data
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut UniqueNodes<Self> {
        &mut nodes.data
    }
    fn free_list_mut(free_list: &mut FreeLists) -> &mut Vec<Dir<Self>> {
        &mut free_list.data
    }
}

impl Node for CtrlKind {
    fn nodes(nodes: &Nodes) -> &UniqueNodes<Self> {
        &nodes.ctrl
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut UniqueNodes<Self> {
        &mut nodes.ctrl
    }
    fn free_list_mut(free_list: &mut FreeLists) -> &mut Vec<Dir<Self>> {
        &mut free_list.ctrl
    }
}

impl Node for BranchKind {
    fn nodes(nodes: &Nodes) -> &UniqueNodes<Self> {
        &nodes.branch
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut UniqueNodes<Self> {
        &mut nodes.branch
    }
    fn free_list_mut(free_list: &mut FreeLists) -> &mut Vec<Dir<Self>> {
        &mut free_list.branch
    }
}

impl Node for MergeKind {
    fn nodes(nodes: &Nodes) -> &UniqueNodes<Self> {
        &nodes.merge
    }
    fn nodes_mut(nodes: &mut Nodes) -> &mut UniqueNodes<Self> {
        &mut nodes.merge
    }
    fn free_list_mut(free_list: &mut FreeLists) -> &mut Vec<Dir<Self>> {
        &mut free_list.merge
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
pub struct FreeLists {
    pub data: Vec<Dir<DataKind>>,
    pub ctrl: Vec<Dir<CtrlKind>>,
    pub branch: Vec<Dir<BranchKind>>,
    pub merge: Vec<Dir<MergeKind>>,
}

/// In this graph every node is unique and changes can be followed back to the onces depending on the changed node
#[derive(Default)]
pub struct Graph {
    pub(crate) nodes: Nodes,
    free_lists: FreeLists,
    user_table: UserTable,
}

impl Graph {
    /// Iterates over data users of all aliases of `dep`.
    pub fn data_data_users(&self, dep: Data) -> impl Iterator<Item = Dir<DataKind>> + '_ {
        self.nodes
            .data
            .dependencies(self.nodes.data.direct(dep))
            .flat_map(|dep| self.user_table.data[dep].data.iter().copied())
    }

    /// Iterates over branch users of all aliases of `dep`.
    pub fn data_branch_users(&self, dep: Data) -> impl Iterator<Item = Dir<BranchKind>> + '_ {
        self.nodes
            .data
            .dependencies(self.nodes.data.direct(dep))
            .flat_map(|dep| self.user_table.data[dep].branch.iter().copied())
    }

    /// Iterates over branch users of all aliases of `dep`.
    pub fn ctrl_branch_users(&self, dep: Ctrl) -> impl Iterator<Item = Dir<BranchKind>> + '_ {
        self.nodes
            .ctrl
            .dependencies(self.nodes.ctrl.direct(dep))
            .flat_map(|dep| self.user_table.ctrl[dep].branch.iter().copied())
    }

    /// Iterates over merge users of all aliases of `dep`.
    pub fn ctrl_merge_users(&self, dep: Ctrl) -> impl Iterator<Item = Dir<MergeKind>> + '_ {
        self.nodes
            .ctrl
            .dependencies(self.nodes.ctrl.direct(dep))
            .flat_map(|dep| self.user_table.ctrl[dep].merge.iter().copied())
    }

    /// Iterates over ctrl users of all aliases of `dep`.
    pub fn branch_ctrl_users(&self, dep: Branch) -> impl Iterator<Item = Dir<CtrlKind>> + '_ {
        self.nodes
            .branch
            .dependencies(self.nodes.branch.direct(dep))
            .flat_map(|dep| self.user_table.branch[dep].ctrl.iter().copied())
    }

    /// Iterates over data users of all aliases of `dep`.
    pub fn merge_data_users(&self, dep: Merge) -> impl Iterator<Item = Dir<DataKind>> + '_ {
        self.nodes
            .merge
            .dependencies(self.nodes.merge.direct(dep))
            .flat_map(|dep| self.user_table.merge[dep].data.iter().copied())
    }

    /// Iterates over ctrl users of all aliases of `dep`.
    pub fn merge_ctrl_users(&self, dep: Merge) -> impl Iterator<Item = Dir<CtrlKind>> + '_ {
        self.nodes
            .merge
            .dependencies(self.nodes.merge.direct(dep))
            .flat_map(|dep| self.user_table.merge[dep].ctrl.iter().copied())
    }

    /// Allocates an unfilled dependency and its user-table slot.
    pub fn allocate<N: Node>(&mut self) -> Stable<N> {
        N::nodes_mut(&mut self.nodes).allocate(&mut self.user_table)
    }

    /// Allocates a dependency and fills it with a canonicalized node.
    pub fn add<N: Node>(&mut self, node: N) -> Stable<N> {
        let dep = self.allocate();
        self.fill_in(dep, node);
        dep
    }

    pub(crate) fn fill_in<N: Node>(&mut self, dep: Stable<N>, node: N) {
        N::nodes_mut(&mut self.nodes).add_node(
            N::free_list_mut(&mut self.free_lists),
            &mut self.user_table,
            dep,
            node,
        )
    }

    /// Redirects every dependency on `old` to `new` and frees the old node.
    pub fn remap<N: Node>(&mut self, old: Stable<N>, new: Stable<N>) {
        let old_dir = N::nodes_mut(&mut self.nodes).direct(old);
        let new_dir = N::nodes_mut(&mut self.nodes).direct(new);
        if old_dir == new_dir {
            return;
        }
        N::nodes_mut(&mut self.nodes)[old_dir].remove_node(old_dir, &mut self.user_table);
        N::nodes_mut(&mut self.nodes).remap_deps(
            N::free_list_mut(&mut self.free_lists),
            old_dir,
            new_dir,
        );
    }

    pub fn nodes<'a, N: Node + 'a>(&'a self) -> impl Iterator<Item = Dir<N>> {
        N::nodes(&self.nodes).nodes()
    }
}

mod graph_indexing {
    use std::ops::Index;

    use crate::{Dir, canonical::Stable, graph::Node};

    use super::Graph;

    impl<N: Node> Index<Stable<N>> for Graph {
        type Output = N;

        fn index(&self, index: Stable<N>) -> &Self::Output {
            &N::nodes(&self.nodes)[index]
        }
    }

    impl<N: Node> Index<Dir<N>> for Graph {
        type Output = N;

        fn index(&self, index: Dir<N>) -> &Self::Output {
            &N::nodes(&self.nodes)[index]
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::{BinaryOp, BranchKind, CtrlKind, DataKind, Graph, MergeKind};

    #[test]
    fn collects_distinct_users_across_aliases_and_remapping() {
        let mut graph = Graph::default();
        let value = graph.add(DataKind::Literal(1));
        let alias = graph.add(DataKind::Literal(1));
        let user = graph.add(DataKind::Binary {
            op: BinaryOp::Less,
            ops: [value, alias],
        });
        let expected = [graph.nodes.data.direct(user)];
        assert_eq!(graph.data_data_users(value).collect::<Vec<_>>(), expected);
        assert_eq!(graph.data_data_users(alias).collect::<Vec<_>>(), expected);

        let replacement = graph.add(DataKind::Literal(2));
        graph.remap(value, replacement);
        assert_eq!(
            graph.data_data_users(replacement).collect::<Vec<_>>(),
            expected
        );
        assert_eq!(graph.data_data_users(alias).collect::<Vec<_>>(), expected);

        let reused = graph.add(DataKind::Literal(3));
        assert_eq!(graph.data_data_users(reused).count(), 0);
        assert_eq!(
            graph.data_data_users(replacement).collect::<Vec<_>>(),
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
            graph.data_data_users(condition).collect::<Vec<_>>(),
            [graph.nodes.data.direct(merged_data)]
        );
        assert_eq!(
            graph.data_branch_users(condition).collect::<HashSet<_>>(),
            HashSet::from([branch_dir, alias_dir])
        );
        assert_eq!(
            graph.ctrl_branch_users(entry).collect::<HashSet<_>>(),
            HashSet::from([branch_dir, alias_dir])
        );
        assert_eq!(
            graph.ctrl_merge_users(entry).collect::<Vec<_>>(),
            [graph.nodes.merge.direct(merge)]
        );
        assert_eq!(
            graph.branch_ctrl_users(branch_alias).collect::<Vec<_>>(),
            [graph.nodes.ctrl.direct(projection)]
        );
        assert_eq!(graph.branch_ctrl_users(branch).count(), 0);
        assert_eq!(
            graph.merge_ctrl_users(merge).collect::<Vec<_>>(),
            [graph.nodes.ctrl.direct(merged_ctrl)]
        );
        assert_eq!(
            graph.merge_data_users(merge).collect::<Vec<_>>(),
            [graph.nodes.data.direct(merged_data)]
        );
    }

    #[test]
    fn remaps_all_aliases_and_reuses_freed_nodes() {
        let mut graph = Graph::default();
        let old = graph.allocate();
        graph.fill_in(old, DataKind::Literal(1));
        let alias = graph.allocate();
        graph.fill_in(alias, DataKind::Literal(1));
        let new = graph.allocate();
        graph.fill_in(new, DataKind::Unit);
        let old_dir = graph.nodes.data.direct(old);

        graph.remap(old, new);
        assert_eq!(graph.nodes.data.direct(old), graph.nodes.data.direct(new));
        assert_eq!(graph.nodes.data.direct(alias), graph.nodes.data.direct(new));
        graph.remap(alias, new);

        let replacement = graph.allocate();
        graph.fill_in(replacement, DataKind::Literal(1));
        assert_eq!(graph.nodes.data.direct(replacement), old_dir);
        assert_eq!(graph.nodes.data[replacement], DataKind::Literal(1));
        assert_eq!(graph.nodes.data[old], DataKind::Unit);
        assert_eq!(graph.nodes.data[alias], DataKind::Unit);
    }
}
