use std::collections::HashMap;

use nonempty::NonEmpty;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::DataID},
};

pub struct OpenScope(());

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    #[allow(unused)]
    pub type_: DataID<'src>,
    pub value: DataID<'src>,
}

#[derive(Debug)]
pub struct Scope<'src> {
    immutables: HashMap<&'src str, Symbol<'src>>,
    mutables: HashMap<&'src str, Symbol<'src>>,
}

pub struct ScopedSymbolTable<'src> {
    symbol_dump: SymbolDump<'src>,
    scopes: NonEmpty<Scope<'src>>,
    unknowns: HashMap<&'src str, DataID<'src>>, // all nodes that represent the unknown constants
}

#[derive(Clone, Copy)]
pub struct ScopeID(usize);

impl<'src> Scope<'src> {
    fn new() -> Self {
        Self {
            immutables: HashMap::new(),
            mutables: HashMap::new(),
        }
    }
}

impl<'src> ScopedSymbolTable<'src> {
    pub fn new() -> Self {
        Self {
            symbol_dump: SymbolDump::new(),
            scopes: NonEmpty::new(Scope::new()),
            unknowns: HashMap::new(),
        }
    }

    pub fn open_scope_id(&self) -> ScopeID {
        ScopeID(self.scopes.len() - 1)
    }

    fn current_scope(&mut self) -> &mut Scope<'src> {
        self.scopes.last_mut()
    }

    #[must_use]
    pub fn open_scope(&mut self) -> OpenScope {
        self.scopes.push(Scope::new());
        OpenScope(())
    }

    pub fn close_scope(&mut self, _: OpenScope) {
        debug_assert!(self.scopes.len() >= 2);
        let mut scope = self.scopes.pop().unwrap();
        // add all the symbols to the symbol dump
        scope
            .immutables
            .drain()
            .for_each(|i| self.symbol_dump.immutables.push(i));
        scope
            .mutables
            .drain()
            .for_each(|i| self.symbol_dump.mutables.push(i));
    }

    /// is equal to `self.snapshot_state_of_scope(self.opened_scope_id())`
    pub fn snapshot_state(&self) -> Vec<DataID<'src>> {
        self.scopes
            .iter()
            .flat_map(|s| s.mutables.iter())
            .map(|(_, symbol)| symbol.value.clone())
            .collect()
    }

    pub fn snapshot_state_of_scope(&self, scope_id: ScopeID) -> Vec<DataID<'src>> {
        self.scopes
            .iter()
            .take(scope_id.0 + 1)
            .flat_map(|s| s.mutables.iter())
            .map(|(_, symbol)| symbol.value.clone())
            .collect()
    }

    pub(super) fn for_every_mutable(&mut self, mut f: impl FnMut(usize, &mut DataID<'src>)) {
        self.scopes
            .iter_mut()
            .flat_map(|s| s.mutables.iter_mut())
            .enumerate()
            .for_each(|(i, (_, symbol))| f(i, &mut symbol.value));
    }

    pub fn write_symbol(
        &mut self,
        assignment: Span,
        name: &'src str,
        value: DataID<'src>,
    ) -> GraphResult<'src, ()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.immutables.contains_key(name) {
                return Err(GraphError::AssignmentToImmutableIdent { name, assignment });
            }

            match scope.mutables.get_mut(name) {
                Some(symbol) => {
                    symbol.value = value;
                    return Ok(());
                }
                None => continue,
            }
        }

        Err(GraphError::AssignmentToUnknownVar { name, assignment })
    }

    pub fn read_symbol(&mut self, graph: &mut Graph<'src>, name: &'src str) -> DataID<'src> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.immutables.get(name) {
                return symbol.value.clone();
            }
            if let Some(symbol) = scope.mutables.get(name) {
                return symbol.value.clone();
            }
        }

        if let Some(unknown) = self.unknowns.get(name) {
            unknown.clone()
        } else {
            let new_unknown = graph.add_unknown();
            self.unknowns.insert(name, new_unknown.clone());

            new_unknown
        }
    }

    pub fn add_immutable(&mut self, name: &'src str, symbol: Symbol<'src>) {
        self.current_scope().immutables.insert(name, symbol);
    }
    pub fn add_mutable(&mut self, name: &'src str, symbol: Symbol<'src>) {
        self.current_scope().mutables.insert(name, symbol);
    }

    /*pub fn add_constant(
        &mut self,
        decl: Span,
        name: &'src str,
        symbol: Symbol<'src>,
    ) -> GraphResult<'src, ()> {
        if self.current().variables.contains_key(name) {
            return Err(GraphError::ConstShadowing { name, decl });
        }
        if self.current().constants.contains_key(name) {
            return Err(GraphError::ConflictingItems { name, decl });
        }

        if let Some(nodes) = self.current().unknowns.remove(name) {
            for mut node in nodes {
                *node = (*symbol.assignment).clone() // overwrite every node referring to this unknown with the const's value
            }
        }

        self.current().constants.insert(name, symbol);
        Ok(())
    }*/

    pub fn all_symbols(mut self) -> SymbolDump<'src> {
        self.scopes.iter_mut().for_each(|s| {
            s.immutables
                .drain()
                .for_each(|i| self.symbol_dump.immutables.push(i));
            s.mutables
                .drain()
                .for_each(|i| self.symbol_dump.mutables.push(i));
        });

        self.symbol_dump
    }
}

pub struct SymbolDump<'src> {
    pub immutables: Vec<(&'src str, Symbol<'src>)>,
    pub mutables: Vec<(&'src str, Symbol<'src>)>,
}

impl<'src> SymbolDump<'src> {
    fn new() -> Self {
        Self {
            immutables: Vec::new(),
            mutables: Vec::new(),
        }
    }
}
