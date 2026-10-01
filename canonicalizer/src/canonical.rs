use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use nonempty::{NonEmpty, nonempty};

use crate::{
    graph::Nodes,
    users::{ReportUses, UserTable},
};

/// A type to represent one dependency in the graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Dep<T> {
    _marker: PhantomData<T>,
    idx: usize,
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
#[derive(Debug, PartialEq, Eq, Hash)]
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

struct Indirect<T> {
    /// For every dependency `Dep` one direct index `Dir`.
    to_direct: Vec<Dir<T>>,
    /// For every direct index `Dir` all dependencies `Dep` that use it.
    from_direct: Vec<NonEmpty<Dep<T>>>,
    free_list: Vec<Dir<T>>,
}

impl<T> Indirect<T> {
    fn new_mapping(&mut self) -> (Dir<T>, Dep<T>) {
        if let Some(free_dir) = self.free_list.pop() {
            let dep = self.next_dep();
            self.to_direct.push(free_dir);
            self.from_direct[free_dir.idx] = nonempty![dep];
            (free_dir, dep)
        } else {
            let dir = Dir::new(self.from_direct.len());
            let dep = self.next_dep();
            self.to_direct.push(dir);
            self.from_direct.push(nonempty![dep]);
            (dir, dep)
        }
    }

    fn next_dir(&self) -> Dir<T> {
        if let Some(free) = self.free_list.last() {
            *free
        } else {
            Dir::new(self.from_direct.len())
        }
    }

    fn next_dep(&self) -> Dep<T> {
        Dep {
            _marker: PhantomData,
            idx: self.to_direct.len(),
        }
    }

    /// Remaps all `Dep` that pointed to `replaced` to now point to `new`
    fn remap(&mut self, old: Dir<T>, new: Dir<T>) {
        for dep in [self.from_direct[old.idx].head]
            .iter()
            .chain(std::mem::take(&mut self.from_direct[old.idx].tail).iter())
        {
            self.to_direct[dep.idx] = new;
            self.from_direct[new.idx].push(*dep)
        }
        self.free_list.push(old)
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

impl<T: Clone + Eq + Hash + ReportUses> UniqueNodes<T> {
    pub(crate) fn new_entry(
        &self,
        indirection: &Nodes,
        user_table: &mut UserTable,
        node: T,
    ) -> Entry<T> {
        if let Some(dep) = self.cache.get(&node) {
            let dir = self.direct(*dep);
            self.nodes[dir].add_uses(dir, indirection, user_table);
            Entry { dir, cached: None }
        } else {
            let dir = self.indirect.next_dir();
            node.add_uses(dir, indirection, user_table);
            Entry {
                dir,
                cached: Some(node),
            }
        }
    }

    pub fn direct(&self, dep: Dep<T>) -> Dir<T> {
        self.indirect.to_direct[dep.idx]
    }

    pub fn remap(
        &mut self,
        indirection: &Nodes,
        user_table: &mut UserTable,
        old: Dir<T>,
        new: Dir<T>,
    ) {
        self.indirect.remap(old, new);
        self.nodes[old.idx].remove_uses(old, indirection, user_table);
    }
}

pub(crate) struct Entry<T> {
    /// Index in the cache
    dir: Dir<T>,
    cached: Option<T>,
}

impl<T: Clone + Eq + Hash> Entry<T> {
    pub fn add_node(self, graph: &mut UniqueNodes<T>) -> Dep<T> {
        match self.cached {
            None => graph.indirect.from_direct[self.dir.idx][0], // reuse existing dependency
            Some(node) => {
                let (_, dep) = graph.indirect.new_mapping();
                graph.nodes.push(node.clone());
                graph.cache.insert(node, dep); // future lookup
                dep
            }
        }
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

impl<T> Index<Dep<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dep<T>) -> &Self::Output {
        let dir = self.indirect.to_direct[index.idx];
        &self.nodes[dir.idx]
    }
}

impl<T> Index<Dir<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dir<T>) -> &Self::Output {
        &self.nodes[index.idx]
    }
}
