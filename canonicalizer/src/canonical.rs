use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use nonempty::{NonEmpty, nonempty};

/// A type to represent one dependency in the graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Dep<T> {
    _unused: PhantomData<T>,
    idx: usize,
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for Dep<T> {
    fn clone(&self) -> Self {
        Self {
            _unused: PhantomData,
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
    _unused: PhantomData<T>,
    idx: usize,
}

impl<T> Dir<T> {
    fn new(idx: usize) -> Self {
        Self {
            _unused: PhantomData,
            idx,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for Dir<T> {
    fn clone(&self) -> Self {
        Self {
            _unused: PhantomData,
            idx: self.idx,
        }
    }
}
impl<T> Copy for Dir<T> {}

struct Indirect<T> {
    /// For every dependency `Dep` one direct index `Dir`.
    to_direct: Vec<Dir<T>>,
    /// For every direct index `Index` all dependencies `Dep` that use it.
    from_direct: Vec<NonEmpty<Dep<T>>>,
    free_list: Vec<Dir<T>>,
}

impl<T> Indirect<T> {
    fn new_mapping(&mut self) -> (Dir<T>, Dep<T>) {
        if let Some(free_dir) = self.free_list.pop() {
            let dep = self.next_dep();
            self.to_direct.push(free_dir);
            self.from_direct[free_dir] = nonempty![dep];
            (free_dir, dep)
        } else {
            let dir = Dir {
                _unused: PhantomData,
                idx: self.from_direct.len(),
            };
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
            Dir {
                _unused: PhantomData,
                idx: self.from_direct.len(),
            }
        }
    }

    fn next_dep(&self) -> Dep<T> {
        Dep {
            _unused: PhantomData,
            idx: self.to_direct.len(),
        }
    }

    /// Remaps all `Dep` that pointed to `replaced` to now point to `new`
    fn remap(&mut self, old: Dir<T>, new: Dir<T>) {
        for dep in [self.from_direct[old].head]
            .iter()
            .chain(std::mem::take(&mut self.from_direct[old].tail).iter())
        {
            self.to_direct[*dep] = new;
            self.from_direct[new].push(*dep)
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
    cache: HashMap<T, Dir<T>>,
}

impl<T: Clone + Eq + Hash> UniqueNodes<T> {
    pub fn entry(&mut self, node: &T) -> Entry<T> {
        if let Some(idx) = self.cache.get(node) {
            return Entry {
                dir: *idx,
                cached: true,
            };
        }

        Entry {
            dir: self.indirect.next_dir(),
            cached: false,
        }
    }

    /// Adds a node `T` into the graph and returns a dependency `Dep<T>` to it.
    /// Alongside that it returns
    pub fn add_node(&mut self, node: T) -> (Dir<T>, Dep<T>) {
        let entry = self.entry(&node);
        (entry.dir(), entry.add_node(self, node))
    }

    pub fn direct(&self, dep: Dep<T>) -> Dir<T> {
        self.indirect.to_direct[dep]
    }

    pub fn replace(&mut self, replaced: Dir<T>, replacement: Dir<T>) {
        self.indirect.remap(replaced, replacement);
    }
}

pub struct Entry<T> {
    /// Index in the cache
    dir: Dir<T>,
    cached: bool,
}

impl<T> Entry<T> {
    pub fn dir(&self) -> Dir<T> {
        self.dir
    }

    pub fn exists(&self) -> bool {
        self.cached
    }
}

impl<T: Clone + Eq + Hash> Entry<T> {
    pub fn add_node(self, graph: &mut UniqueNodes<T>, node: T) -> Dep<T> {
        match self.cached {
            true => graph.indirect.from_direct[self.dir][0], // reuse existing dependency
            false => {
                let (dir, dep) = graph.indirect.new_mapping();
                graph.nodes.push(node.clone());
                graph.cache.insert(node, dir); // future lookup
                dep
            }
        }
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
        let idx = self.indirect.to_direct[index];
        &self.nodes[idx]
    }
}
