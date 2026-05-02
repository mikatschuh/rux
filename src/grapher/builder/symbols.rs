use std::{collections::HashMap, ops::Index, vec::IntoIter};

use nonempty::NonEmpty;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::DataID},
};

#[must_use]
pub struct OpenScope(());

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    #[allow(unused)]
    pub ty: DataID<'src>,
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

#[derive(Clone, Copy)]
pub struct MutableIdx(usize);

pub struct MutableState<'src>(Vec<DataID<'src>>);

impl<'src> Index<MutableIdx> for MutableState<'src> {
    type Output = DataID<'src>;
    fn index(&self, index: MutableIdx) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<'src> MutableState<'src> {
    pub fn into_iter(self) -> IntoIter<DataID<'src>> {
        self.0.into_iter()
    }
}

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

    pub fn open_scope(&mut self) -> OpenScope {
        self.scopes.push(Scope::new());
        OpenScope(())
    }

    pub fn close_scope(&mut self, _: OpenScope) {
        debug_assert!(self.scopes.len() >= 2);
        let scope = self.scopes.pop().unwrap();
        // add all the symbols to the symbol dump
        self.symbol_dump.append_scope(scope);
    }

    /// is equal to `self.snapshot_state_of_scope(self.opened_scope_id())`
    pub(super) fn snapshot_state(&self) -> MutableState<'src> {
        MutableState(
            self.scopes
                .iter()
                .flat_map(|s| s.mutables.iter())
                .map(|(_, symbol)| symbol.value.clone())
                .collect(),
        )
    }

    pub(super) fn snapshot_state_of_scope(&self, scope_id: ScopeID) -> MutableState<'src> {
        MutableState(
            self.scopes
                .iter()
                .take(scope_id.0 + 1)
                .flat_map(|s| s.mutables.iter())
                .map(|(_, symbol)| symbol.value.clone())
                .collect(),
        )
    }

    pub(super) fn for_every_mutable(&mut self, mut f: impl FnMut(MutableIdx, &mut DataID<'src>)) {
        self.scopes
            .iter_mut()
            .flat_map(|s| s.mutables.iter_mut())
            .enumerate()
            .for_each(|(i, (_, symbol))| f(MutableIdx(i), &mut symbol.value));
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
        self.scopes.into_iter().for_each(|scope| {
            self.symbol_dump.append_scope(scope);
        });

        self.symbol_dump
    }
}

pub struct SymbolDump<'src> {
    pub immutables: HashMap<&'src str, Symbol<'src>>,
    pub mutables: HashMap<&'src str, Symbol<'src>>,
}

impl<'src> SymbolDump<'src> {
    fn new() -> Self {
        Self {
            immutables: HashMap::new(),
            mutables: HashMap::new(),
        }
    }

    fn append_scope(&mut self, mut scope: Scope<'src>) {
        scope.immutables.drain().for_each(|(name, immutable)| {
            self.immutables.insert(name, immutable);
        });
        scope.mutables.drain().for_each(|(name, mutable)| {
            self.mutables.insert(name, mutable);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::{ScopedSymbolTable, Symbol};
    use crate::{
        error::Span,
        grapher::{Graph, GraphError},
        literal_parsing::Literal,
        type_parsing::AtomicType,
    };

    fn symbol<'src>(
        ty: crate::grapher::graph::DataID<'src>,
        value: crate::grapher::graph::DataID<'src>,
    ) -> Symbol<'src> {
        Symbol { ty, value }
    }

    #[test]
    fn reads_nearest_symbol_and_restores_outer_symbol_after_scope_closes() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_type(AtomicType::Signed { size: 32 });
        let outer = graph.add_literal(Literal::from(1));
        let inner = graph.add_literal(Literal::from(2));

        table.add_immutable("setting", symbol(ty.clone(), outer.clone()));
        assert_eq!(
            table.read_symbol(&mut graph, "setting").addr(),
            outer.addr()
        );

        let scope = table.open_scope();
        table.add_immutable("setting", symbol(ty, inner.clone()));
        assert_eq!(
            table.read_symbol(&mut graph, "setting").addr(),
            inner.addr()
        );

        table.close_scope(scope);
        assert_eq!(
            table.read_symbol(&mut graph, "setting").addr(),
            outer.addr()
        );
    }

    #[test]
    fn caches_unknown_reads_by_name() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();

        let first = table.read_symbol(&mut graph, "future_constant");
        let second = table.read_symbol(&mut graph, "future_constant");
        let other = table.read_symbol(&mut graph, "other_constant");

        assert_eq!(first.addr(), second.addr());
        assert_ne!(first.addr(), other.addr());
    }

    #[test]
    fn writes_to_nearest_mutable_and_rejects_immutables() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_type(AtomicType::Signed { size: 32 });
        let initial = graph.add_literal(Literal::from(1));
        let overwritten = graph.add_literal(Literal::from(2));
        let immutable = graph.add_literal(Literal::from(3));

        table.add_mutable("counter", symbol(ty.clone(), initial));
        table.add_immutable("limit", symbol(ty, immutable));

        table
            .write_symbol(Span::beginning(), "counter", overwritten.clone())
            .expect("write mutable");
        assert_eq!(
            table.read_symbol(&mut graph, "counter").addr(),
            overwritten.addr()
        );

        let err = table
            .write_symbol(
                Span::beginning(),
                "limit",
                graph.add_literal(Literal::from(4)),
            )
            .expect_err("immutable write should fail");
        assert!(matches!(
            err,
            GraphError::AssignmentToImmutableIdent { name: "limit", .. }
        ));
    }

    #[test]
    fn reports_writes_to_unknown_variables() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();
        let value = graph.add_literal(Literal::from(1));

        let err = table
            .write_symbol(Span::beginning(), "missing", value)
            .expect_err("unknown write should fail");

        assert!(matches!(
            err,
            GraphError::AssignmentToUnknownVar {
                name: "missing",
                ..
            }
        ));
    }

    #[test]
    fn snapshots_and_restores_mutable_state_in_scope_order() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_type(AtomicType::Signed { size: 32 });
        let first = graph.add_literal(Literal::from(1));
        let second = graph.add_literal(Literal::from(2));
        let first_new = graph.add_literal(Literal::from(10));
        let second_new = graph.add_literal(Literal::from(20));

        table.add_mutable("first", symbol(ty.clone(), first.clone()));
        let scope = table.open_scope();
        table.add_mutable("second", symbol(ty, second.clone()));

        let snapshot = table.snapshot_state();
        table
            .write_symbol(Span::beginning(), "first", first_new.clone())
            .expect("write first");
        table
            .write_symbol(Span::beginning(), "second", second_new.clone())
            .expect("write second");

        table.for_every_mutable(|idx, value| *value = snapshot[idx].clone());

        assert_eq!(table.read_symbol(&mut graph, "first").addr(), first.addr());
        assert_eq!(
            table.read_symbol(&mut graph, "second").addr(),
            second.addr()
        );

        let outer_snapshot = table.snapshot_state_of_scope(table.open_scope_id());
        assert_eq!(outer_snapshot.into_iter().count(), 2);
        table.close_scope(scope);
    }

    #[test]
    fn all_symbols_includes_open_and_closed_scopes() {
        let mut graph = Graph::new();
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_type(AtomicType::Signed { size: 32 });
        let global = graph.add_literal(Literal::from(1));
        let local = graph.add_literal(Literal::from(2));

        table.add_immutable("global", symbol(ty.clone(), global.clone()));
        let scope = table.open_scope();
        table.add_mutable("local", symbol(ty, local.clone()));
        table.close_scope(scope);

        let dump = table.all_symbols();
        assert_eq!(dump.immutables["global"].value.addr(), global.addr());
        assert_eq!(dump.mutables["local"].value.addr(), local.addr());
    }
}
