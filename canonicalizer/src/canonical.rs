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
pub struct Stable<T> {
    _marker: PhantomData<T>,
    idx: usize,
}

impl<T> Stable<T> {
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for Stable<T> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            idx: self.idx,
        }
    }
}
impl<T> Copy for Stable<T> {}

impl<T: PartialEq> PartialOrd for Stable<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.idx.cmp(&other.idx))
    }
}
impl<T: PartialEq + Eq> Ord for Stable<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

/// A type to represent a unique node in the graph.
#[derive(Debug)]
pub struct Dir<T> {
    _marker: PhantomData<T>,
    idx: usize,
}

impl<T> Dir<T> {
    pub(crate) fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for Dir<T> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            idx: self.idx,
        }
    }
}
impl<T> Copy for Dir<T> {}

impl<T> PartialEq<Dir<T>> for Dir<T> {
    fn eq(&self, other: &Dir<T>) -> bool {
        self.idx == other.idx
    }
}
impl<T> Eq for Dir<T> {}

impl<T> Hash for Dir<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state)
    }
}

/// In this graph every node is deduplicated.
pub struct UniqueNodes<N> {
    /// For every dependency `Dep` one direct index `Dir`.
    to_direct: Vec<Dir<N>>,
    /// For every direct index `Dir` all dependencies `Dep` that use it.
    from_direct: Vec<NonEmpty<Stable<N>>>,
    /// Stores the actual nodes
    nodes: Vec<N>,
    /// Caches nodes
    cache: HashMap<N, Dir<N>>,
    free_list: Vec<Dir<N>>,
}

impl<N: Node> Default for UniqueNodes<N> {
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

impl<N: Node> UniqueNodes<N> {
    fn direct_placeholder_to(&mut self, dep: Stable<N>, dir: Dir<N>) {
        self.from_direct[dir].push(dep);
        self.to_direct[dep] = dir;
    }

    pub(crate) fn add_node(&mut self, user_table: &mut UserTable, placeholder: Stable<N>, node: N) {
        if let Some(dir) = self.cache.get(&node) {
            self.direct_placeholder_to(placeholder, *dir);
        } else if let Some(free_dir) = self.free_list.pop() {
            node.add_node(free_dir, user_table);
            self.from_direct[free_dir] = nonempty![placeholder];
            self.to_direct[placeholder] = free_dir;

            self.nodes[free_dir] = node.clone();
            self.cache.insert(node, free_dir);
        } else {
            let dir = Dir::new(self.from_direct.len());
            node.add_node(dir, user_table);
            self.from_direct.push(nonempty![placeholder]);
            self.to_direct[placeholder] = dir;

            self.nodes.push(node.clone());
            self.cache.insert(node, dir);
        }
    }

    pub(crate) fn allocate(&mut self, user_table: &mut UserTable) -> Stable<N> {
        let deps_count = self.to_direct.len();
        self.to_direct.push(Dir::new(usize::MAX));
        N::add_dep(user_table);
        Stable::new(deps_count)
    }

    pub(crate) fn remap_deps(&mut self, old: Dir<N>, new: Dir<N>) {
        if old == new {
            return;
        }
        for dep in [self.from_direct[old].head]
            .into_iter()
            .chain(std::mem::take(&mut self.from_direct[old].tail))
        {
            self.direct_placeholder_to(dep, new);
        }
        self.cache.remove(&self.nodes[old]);
        self.free_list.push(old);
    }

    pub(crate) fn direct(&self, dep: Stable<N>) -> Dir<N> {
        self.to_direct[dep]
    }

    pub(crate) fn stable_deps(&self, dir: Dir<N>) -> impl Iterator<Item = Stable<N>> + '_ {
        self.from_direct[dir].iter().copied()
    }

    pub fn nodes(&self) -> impl Iterator<Item = Dir<N>> {
        (0..self.from_direct.len()).map(|i| Dir::new(i))
    }
}

impl<A, B> Index<Dir<A>> for Vec<B> {
    type Output = B;
    fn index(&self, index: Dir<A>) -> &Self::Output {
        &self[index.idx]
    }
}

impl<A, B> IndexMut<Dir<A>> for Vec<B> {
    fn index_mut(&mut self, index: Dir<A>) -> &mut Self::Output {
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

impl<T> Index<Stable<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Stable<T>) -> &Self::Output {
        let dir = self.to_direct[index];
        &self.nodes[dir]
    }
}

impl<T> Index<Dir<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dir<T>) -> &Self::Output {
        &self.nodes[index]
    }
}
