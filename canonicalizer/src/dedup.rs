use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

/// A type to represent one dependency in the Graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Dep<T> {
    _unused: std::marker::PhantomData<T>,
    idx: usize,
}

impl<T> Clone for Dep<T> {
    fn clone(&self) -> Self {
        Self {
            _unused: PhantomData::default(),
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

/// A type to represent one dependency in the Graph.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Idx<T> {
    _unused: std::marker::PhantomData<T>,
    idx: usize,
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        Self {
            _unused: PhantomData::default(),
            idx: self.idx,
        }
    }
}
impl<T> Copy for Idx<T> {}

/// In this graph every node is deduplicated.
pub struct UniqueNodes<T> {
    /// One entry for every `Dependency`. Every entry stores that dependency's nodes current index.
    pub deps_indice: Vec<Idx<T>>,
    /// indices_deps.len() == nodes.len()
    indices_deps: Vec<Dep<T>>,
    /// Stores the actual nodes
    nodes: Vec<T>,
    /// Caches nodes
    cache: HashMap<T, Idx<T>>,
}

impl<T: Clone + Eq + Hash> UniqueNodes<T> {
    pub fn entry(&mut self, node: &T) -> Entry<T> {
        if let Some(idx) = self.cache.get(&node) {
            return Entry {
                idx: *idx,
                cached: true,
            };
        }

        Entry {
            idx: self.new_idx(),
            cached: false,
        }
    }

    /// Adds a node `T` into the graph and returns a dependency `Dep<T>` to it.
    /// Alongside that it returns
    pub fn add_node(&mut self, node: T) -> (Idx<T>, Dep<T>) {
        let entry = self.entry(&node);
        (entry.idx(), entry.add_node(self, node))
    }

    pub fn get_idx(&self, dep: Dep<T>) -> Idx<T> {
        self.deps_indice[dep]
    }
}

pub struct Entry<T> {
    /// Index in the cache
    idx: Idx<T>,
    cached: bool,
}

impl<T> Entry<T> {
    pub fn idx(&self) -> Idx<T> {
        self.idx
    }

    pub fn exists(&self) -> bool {
        self.cached
    }
}

impl<T: Clone + Eq + Hash> Entry<T> {
    pub fn add_node(self, graph: &mut UniqueNodes<T>, node: T) -> Dep<T> {
        match self.cached {
            true => graph.indices_deps[self.idx], // reuse existing dependency
            false => {
                let dep = graph.new_dep();
                graph.indices_deps.push(dep);
                graph.deps_indice.push(self.idx);
                graph.nodes.push(node.clone());
                graph.cache.insert(node, self.idx); // future lookup
                dep
            }
        }
    }
}

impl<T> UniqueNodes<T> {
    fn new_idx(&self) -> Idx<T> {
        Idx {
            _unused: PhantomData::default(),
            idx: self.indices_deps.len(),
        }
    }

    fn new_dep(&self) -> Dep<T> {
        Dep {
            _unused: std::marker::PhantomData::default(),
            idx: self.deps_indice.len(),
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

impl<A, B> Index<Idx<A>> for Vec<B> {
    type Output = B;
    fn index(&self, index: Idx<A>) -> &Self::Output {
        &self[index.idx]
    }
}

impl<A, B> IndexMut<Idx<A>> for Vec<B> {
    fn index_mut(&mut self, index: Idx<A>) -> &mut Self::Output {
        &mut self[index.idx]
    }
}

impl<T> Index<Dep<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dep<T>) -> &Self::Output {
        let index = self.deps_indice[index];
        &self.nodes[index]
    }
}
