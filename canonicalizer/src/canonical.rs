use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use nonempty::{NonEmpty, nonempty};

use crate::users::{ReportUses, UserTable};

/// A type to represent one dependency in the graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Dep<T> {
    _marker: PhantomData<T>,
    idx: usize,
}

impl<T> Dep<T> {
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for Dep<T> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData,
            idx: self.idx,
        }
    }
}
impl<T> Copy for Dep<T> {}

impl<T: PartialEq> PartialOrd for Dep<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.idx.cmp(&other.idx))
    }
}
impl<T: PartialEq + Eq> Ord for Dep<T> {
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
    fn new(idx: usize) -> Self {
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

struct Indirect<T> {
    /// For every dependency `Dep` one direct index `Dir`.
    to_direct: Vec<Dir<T>>,
    /// For every direct index `Dir` all dependencies `Dep` that use it.
    from_direct: Vec<NonEmpty<Dep<T>>>,
}

impl<T> Indirect<T> {
    fn direct_dep_to(&mut self, dep: Dep<T>, dir: Dir<T>) {
        self.from_direct[dir].push(dep);
        self.to_direct[dep] = dir;
    }

    /// Remaps all `Dep` that pointed to `replaced` to now point to `new`
    fn remap(&mut self, old: Dir<T>, new: Dir<T>)
    where
        T: Debug,
    {
        assert_ne!(old, new);
        for dep in [self.from_direct[old].head]
            .into_iter()
            .chain(std::mem::take(&mut self.from_direct[old].tail))
        {
            self.direct_dep_to(dep, new);
        }
    }
}

/// In this graph every node is deduplicated.
pub struct UniqueNodes<T> {
    indirect: Indirect<T>,
    /// Stores the actual nodes
    nodes: Vec<T>,
    /// Caches nodes
    cache: HashMap<T, Dep<T>>,
}

impl<T> Default for UniqueNodes<T> {
    fn default() -> Self {
        Self {
            indirect: Indirect {
                to_direct: Vec::new(),
                from_direct: Vec::new(),
            },
            nodes: Vec::new(),
            cache: HashMap::new(),
        }
    }
}

impl<T: Clone + Eq + Hash + ReportUses + Debug> UniqueNodes<T> {
    pub(crate) fn add_node(
        &mut self,
        free_list: &mut Vec<Dir<T>>,
        user_table: &mut UserTable,
        placeholder: Dep<T>,
        node: T,
    ) {
        if let Some(dep) = self.cache.get(&node) {
            let dir = self.direct(*dep);
            self.indirect.direct_dep_to(placeholder, dir);
        } else if let Some(free_dir) = free_list.pop() {
            node.add_node(free_dir, user_table);
            self.indirect.from_direct[free_dir] = nonempty![placeholder];
            self.indirect.to_direct[placeholder] = free_dir;

            self.nodes[free_dir] = node.clone();
            self.cache.insert(node, placeholder);
        } else {
            let dir = Dir::new(self.indirect.from_direct.len());
            node.add_node(dir, user_table);
            self.indirect.from_direct.push(nonempty![placeholder]);
            self.indirect.to_direct[placeholder] = dir;

            self.nodes.push(node.clone());
            self.cache.insert(node, placeholder);
        }
    }

    pub(crate) fn allocate(&mut self, user_table: &mut UserTable) -> Dep<T> {
        let deps_count = self.indirect.to_direct.len();
        self.indirect.to_direct.push(Dir::new(usize::MAX));
        T::add_dep(user_table);
        Dep::new(deps_count)
    }

    pub(crate) fn remap_deps(&mut self, free_list: &mut Vec<Dir<T>>, old: Dep<T>, new: Dep<T>) {
        let old = self.direct(old);
        let new = self.direct(new);
        if old == new {
            return;
        }
        self.indirect.remap(old, new);
        self.cache.remove(&self.nodes[old]);
        free_list.push(old);
    }

    pub fn direct(&self, dep: Dep<T>) -> Dir<T> {
        self.indirect.to_direct[dep]
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

impl<A, B> Index<Dep<A>> for Vec<B> {
    type Output = B;
    fn index(&self, index: Dep<A>) -> &Self::Output {
        &self[index.idx]
    }
}

impl<A, B> IndexMut<Dep<A>> for Vec<B> {
    fn index_mut(&mut self, index: Dep<A>) -> &mut Self::Output {
        &mut self[index.idx]
    }
}

impl<T> Index<Dep<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dep<T>) -> &Self::Output {
        let dir = self.indirect.to_direct[index];
        &self.nodes[dir]
    }
}

impl<T> Index<Dir<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dir<T>) -> &Self::Output {
        &self.nodes[index]
    }
}
