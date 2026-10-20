use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use nonempty::{NonEmpty, nonempty};

use crate::{graph::Node, users::UserTable};

/// A type to represent one dependency in the graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Stable<N> {
    _marker: PhantomData<N>,
    idx: usize,
}

impl<N> Stable<N> {
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<N> Clone for Stable<N> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            idx: self.idx,
        }
    }
}
impl<N> Copy for Stable<N> {}

impl<N: PartialEq> PartialOrd for Stable<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.idx.cmp(&other.idx))
    }
}
impl<N: PartialEq + Eq> Ord for Stable<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

/// A type to represent a unique node in the graph.
#[derive(Debug)]
pub struct Unique<N> {
    _marker: PhantomData<N>,
    idx: usize,
}

impl<N> Unique<N> {
    pub(crate) fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<N> Clone for Unique<N> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            idx: self.idx,
        }
    }
}
impl<N> Copy for Unique<N> {}

impl<N> PartialEq<Unique<N>> for Unique<N> {
    fn eq(&self, other: &Unique<N>) -> bool {
        self.idx == other.idx
    }
}
impl<N> Eq for Unique<N> {}

impl<N> Hash for Unique<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state)
    }
}

/// In this graph every node is deduplicated.
pub struct Canonicalizer<N> {
    /// For every dependency `Dep` one direct index `Dir`.
    to_direct: Vec<Unique<N>>,
    /// For every direct index `Dir` all dependencies `Dep` that use it.
    from_direct: Vec<NonEmpty<Stable<N>>>,
    /// Stores the actual nodes
    nodes: Vec<N>,
    /// Caches nodes
    cache: HashMap<N, Unique<N>>,
    free_list: Vec<Unique<N>>,
}

impl<N: Node> Default for Canonicalizer<N> {
    fn default() -> Self {
        Self {
            to_direct: vec![],
            from_direct: vec![],
            nodes: vec![],
            cache: HashMap::new(),
            free_list: vec![],
        }
    }
}

impl<N: Node> Canonicalizer<N> {
    fn bind(&mut self, ind: Stable<N>, dir: Unique<N>) {
        self.from_direct[dir].push(ind);
        self.to_direct[ind] = dir;
    }

    pub(crate) fn add_node(&mut self, user_table: &mut UserTable, stable: Stable<N>, node: N) {
        if let Some(dir) = self.cache.get(&node) {
            self.bind(stable, *dir);
        } else if let Some(free_dir) = self.free_list.pop() {
            node.add_node(free_dir, user_table);
            self.from_direct[free_dir] = nonempty![stable];
            self.to_direct[stable] = free_dir;

            self.nodes[free_dir] = node.clone();
            self.cache.insert(node, free_dir);
        } else {
            let dir = Unique::new(self.from_direct.len());
            node.add_node(dir, user_table);
            self.from_direct.push(nonempty![stable]);
            self.to_direct[stable] = dir;

            self.nodes.push(node.clone());
            self.cache.insert(node, dir);
        }
    }

    pub(crate) fn allocate(&mut self, user_table: &mut UserTable) -> Stable<N> {
        let deps_count = self.to_direct.len();
        self.to_direct.push(Unique::new(usize::MAX));
        N::add_dep(user_table);
        Stable::new(deps_count)
    }

    pub(crate) fn remap_deps(&mut self, old: Unique<N>, new: Unique<N>) {
        if old == new {
            return;
        }
        for ind in [self.from_direct[old].head]
            .into_iter()
            .chain(std::mem::take(&mut self.from_direct[old].tail))
        {
            self.bind(ind, new);
        }
        self.cache.remove(&self.nodes[old]);
        self.free_list.push(old);
    }

    pub(crate) fn direct(&self, dep: Stable<N>) -> Unique<N> {
        self.to_direct[dep]
    }

    pub(crate) fn stable_deps(&self, dir: Unique<N>) -> impl Iterator<Item = Stable<N>> + '_ {
        self.from_direct[dir].iter().copied()
    }

    pub fn nodes(&self) -> impl Iterator<Item = Unique<N>> {
        (0..self.from_direct.len()).map(|i| Unique::new(i))
    }
}

impl<A, B> Index<Unique<A>> for Vec<B> {
    type Output = B;
    fn index(&self, index: Unique<A>) -> &Self::Output {
        &self[index.idx]
    }
}

impl<A, B> IndexMut<Unique<A>> for Vec<B> {
    fn index_mut(&mut self, index: Unique<A>) -> &mut Self::Output {
        &mut self[index.idx]
    }
}

impl<A, B> Index<Stable<A>> for Vec<B> {
    type Output = B;
    fn index(&self, index: Stable<A>) -> &Self::Output {
        &self[index.idx]
    }
}

impl<A, B> IndexMut<Stable<A>> for Vec<B> {
    fn index_mut(&mut self, index: Stable<A>) -> &mut Self::Output {
        &mut self[index.idx]
    }
}

impl<N> Index<Stable<N>> for Canonicalizer<N> {
    type Output = N;
    fn index(&self, index: Stable<N>) -> &Self::Output {
        let dir = self.to_direct[index];
        &self.nodes[dir]
    }
}

impl<N> Index<Unique<N>> for Canonicalizer<N> {
    type Output = N;
    fn index(&self, index: Unique<N>) -> &Self::Output {
        &self.nodes[index]
    }
}
