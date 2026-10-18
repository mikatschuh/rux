use std::{
    collections::HashMap,
    fmt::Debug,
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
    fn add_existing_dep(&mut self, dir: Dir<T>, dep: Dep<T>) {
        self.from_direct[dir].push(dep);
        self.to_direct[dep.idx] = dir;
    }

    /// Remaps all `Dep` that pointed to `replaced` to now point to `new`
    fn remap(&mut self, old: Dir<T>, new: Dir<T>)
    where
        T: Debug,
    {
        assert_ne!(old, new);
        for dep in [self.from_direct[old].head]
            .iter()
            .chain(std::mem::take(&mut self.from_direct[old].tail).iter())
        {
            self.to_direct[dep.idx] = new;
            self.from_direct[new].push(*dep)
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
    pub fn add_entry(
        &self,
        free_list: &mut Vec<Dir<T>>,
        nodes: &Nodes,
        user_table: &mut UserTable,
        node: T,
    ) -> Entry<T> {
        if let Some(dep) = self.cache.get(&node) {
            let dir = self.direct(*dep);
            Entry::Cached { dir }
        } else if let Some(free_dir) = free_list.pop() {
            node.add_node(free_dir, nodes, user_table, true);
            Entry::NewReused { free_dir, node }
        } else {
            node.add_node(
                Dir::new(self.indirect.from_direct.len()),
                nodes,
                user_table,
                false,
            );
            Entry::New(node)
        }
    }

    pub fn allocate(&mut self) -> Dep<T> {
        let deps_count = self.indirect.to_direct.len();
        self.indirect.to_direct.push(Dir::new(usize::MAX));
        Dep::new(deps_count)
    }

    pub fn direct(&self, dep: Dep<T>) -> Dir<T> {
        self.indirect.to_direct[dep.idx]
    }

    pub fn remap(
        &mut self,
        free_list: &mut Vec<Dir<T>>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        old: Dir<T>,
        new: Dir<T>,
    ) {
        if old == new {
            return;
        }
        self.indirect.remap(old, new);
        self.nodes[old].remove_node(old, indirection, user_table);
        free_list.push(old);
    }
}

pub enum Entry<T> {
    Cached { dir: Dir<T> },
    NewReused { free_dir: Dir<T>, node: T },
    New(T),
}

impl<T: Clone + Eq + Hash> Entry<T> {
    pub fn fill_in(self, graph: &mut UniqueNodes<T>, placeholder: Dep<T>) {
        match self {
            Entry::Cached { dir } => graph.indirect.add_existing_dep(dir, placeholder), // reuse existing dependency
            Entry::NewReused { free_dir, node } => {
                graph.indirect.from_direct[free_dir] = nonempty![placeholder];
                graph.indirect.to_direct[placeholder.idx] = free_dir;

                graph.nodes[free_dir] = node.clone();
                graph.cache.insert(node, placeholder);
            }
            Entry::New(node) => {
                let dir = Dir::new(graph.indirect.from_direct.len());
                graph.indirect.from_direct.push(nonempty![placeholder]);
                graph.indirect.to_direct[placeholder.idx] = dir;

                graph.nodes.push(node.clone());
                graph.cache.insert(node, placeholder);
            }
        }
    }

    pub fn add_node(self, graph: &mut UniqueNodes<T>) -> Dep<T> {
        match self {
            Entry::Cached { dir } => graph.indirect.from_direct[dir][0], // reuse existing dependency
            Entry::NewReused { free_dir, node } => {
                let dep = Dep::new(graph.indirect.to_direct.len());
                graph.indirect.to_direct.push(free_dir);
                graph.indirect.from_direct[free_dir] = nonempty![dep];

                graph.nodes[free_dir] = node.clone();
                graph.cache.insert(node, dep);
                dep
            }
            Entry::New(node) => {
                let dir = Dir::new(graph.indirect.from_direct.len());
                let dep = Dep::new(graph.indirect.to_direct.len());
                graph.indirect.to_direct.push(dir);
                graph.indirect.from_direct.push(nonempty![dep]);

                graph.nodes.push(node.clone());
                graph.cache.insert(node, dep);
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
        &self.nodes[dir]
    }
}

impl<T> Index<Dir<T>> for UniqueNodes<T> {
    type Output = T;
    fn index(&self, index: Dir<T>) -> &Self::Output {
        &self.nodes[index]
    }
}
