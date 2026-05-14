use std::{collections::HashMap, ops::Index, vec::IntoIter};

use crate::{
    error::Span,
    grapher::{
        builder::{Error, Result},
        graph::{DataID, TypeID},
    },
    parser::Symbol,
};

#[must_use]
pub struct OpenScope(());

#[derive(Debug, Clone)]
pub struct Binding {
    #[allow(unused)]
    pub ty: TypeID,
    pub value: DataID,
}

#[derive(Debug)]
pub struct Scope {
    immutables: HashMap<Symbol, Binding>,
    mutables: HashMap<Symbol, Binding>,
}

pub struct ScopedSymbolTable {
    symbol_dump: SymbolDump,
    scopes: Vec<Scope>,
}

#[derive(Clone, Copy)]
pub struct ScopeID(usize);

#[derive(Clone, Copy)]
pub struct MutableIdx(usize);

pub struct MutableState(Vec<DataID>);

impl Index<MutableIdx> for MutableState {
    type Output = DataID;
    fn index(&self, index: MutableIdx) -> &Self::Output {
        &self.0[index.0]
    }
}

impl MutableState {
    pub fn into_iter(self) -> IntoIter<DataID> {
        self.0.into_iter()
    }
}

impl Scope {
    fn new() -> Self {
        Self {
            immutables: HashMap::new(),
            mutables: HashMap::new(),
        }
    }
}

impl ScopedSymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_dump: SymbolDump::new(),
            scopes: vec![],
        }
    }

    pub fn open_scope_id(&self) -> ScopeID {
        ScopeID(self.scopes.len() - 1)
    }

    pub fn open_scope(&mut self) -> OpenScope {
        self.scopes.push(Scope::new());
        OpenScope(())
    }

    pub fn close_scope(&mut self, _: OpenScope) {
        let scope = self.scopes.pop().unwrap();
        // add all the symbols to the symbol dump
        self.symbol_dump.append_scope(scope);
    }

    /// is equal to `self.snapshot_state_of_scope(self.opened_scope_id())`
    pub(super) fn snapshot_state(&self) -> MutableState {
        MutableState(
            self.scopes
                .iter()
                .flat_map(|s| s.mutables.iter())
                .map(|(_, symbol)| symbol.value.clone())
                .collect(),
        )
    }

    pub(super) fn snapshot_state_of_scope(&self, scope_id: ScopeID) -> MutableState {
        MutableState(
            self.scopes
                .iter()
                .take(scope_id.0 + 1)
                .flat_map(|s| s.mutables.iter())
                .map(|(_, symbol)| symbol.value.clone())
                .collect(),
        )
    }

    pub(super) fn for_every_mutable(&mut self, mut f: impl FnMut(MutableIdx, &mut DataID)) {
        self.scopes
            .iter_mut()
            .flat_map(|s| s.mutables.iter_mut())
            .enumerate()
            .for_each(|(i, (_, symbol))| f(MutableIdx(i), &mut symbol.value));
    }

    pub fn add_symbol(
        &mut self,
        mutable: bool,
        keyword: Span,
        symbol: Symbol,
        binding: Binding,
    ) -> Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            if mutable {
                scope.mutables.insert(symbol, binding);
                scope.immutables.remove(&symbol);
            } else {
                scope.immutables.insert(symbol, binding);
                scope.mutables.remove(&symbol);
            }
            Ok(())
        } else {
            Err(Error::BindingOutsideOfScope {
                binding: keyword,
                symbol,
            })
        }
    }

    pub fn write_symbol(&mut self, equal: Span, symbol: Symbol, value: DataID) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.immutables.contains_key(&symbol) {
                return Err(Error::AssignmentToImmutableIdent { symbol, equal });
            }

            match scope.mutables.get_mut(&symbol) {
                Some(symbol) => {
                    symbol.value = value;
                    return Ok(());
                }
                None => continue,
            }
        }

        Err(Error::AssignmentToUnknownVar { symbol, equal })
    }

    pub fn read_symbol(&mut self, symbol: Symbol) -> Option<DataID> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.immutables.get(&symbol) {
                return Some(symbol.value.clone());
            }
            if let Some(symbol) = scope.mutables.get(&symbol) {
                return Some(symbol.value.clone());
            }
        }
        None
    }

    pub fn symbol_dump(mut self) -> SymbolDump {
        self.scopes.into_iter().for_each(|scope| {
            self.symbol_dump.append_scope(scope);
        });

        self.symbol_dump
    }
}

pub struct SymbolDump {
    pub immutables: HashMap<Symbol, Binding>,
    pub mutables: HashMap<Symbol, Binding>,
}

impl SymbolDump {
    fn new() -> Self {
        Self {
            immutables: HashMap::new(),
            mutables: HashMap::new(),
        }
    }

    fn append_scope(&mut self, mut scope: Scope) {
        scope.immutables.drain().for_each(|(symbol, immutable)| {
            self.immutables.insert(symbol, immutable);
        });
        scope.mutables.drain().for_each(|(name, mutable)| {
            self.mutables.insert(name, mutable);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::{Binding, ScopedSymbolTable};
    use crate::{
        error::Span,
        grapher::{
            Graph,
            builder::Error,
            graph::{DataID, TypeID},
        },
        literal_parsing::Literal,
        parser::{BuiltinType, Interner},
    };
    use bumpalo::Bump;

    fn graph() -> (Interner, Graph) {
        (Interner::new(), Graph::new(Bump::new()))
    }

    fn symbol(ty: TypeID, value: DataID) -> Binding {
        Binding { ty, value }
    }

    #[test]
    fn reads_nearest_symbol_and_restores_outer_symbol_after_scope_closes() {
        let (mut interner, mut graph) = graph();
        let setting = interner.get("setting");

        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let outer = graph.add_literal(Literal::from(1));
        let inner = graph.add_literal(Literal::from(2));

        table.add_symbol(
            false,
            Span::beginning(),
            setting,
            symbol(ty.clone(), outer.clone()),
        );
        assert_eq!(table.read_symbol(setting).unwrap().addr(), outer.addr());

        let scope = table.open_scope();
        table.add_symbol(false, Span::beginning(), setting, symbol(ty, inner.clone()));
        assert_eq!(table.read_symbol(setting).unwrap().addr(), inner.addr());

        table.close_scope(scope);
        assert_eq!(table.read_symbol(setting).unwrap().addr(), outer.addr());
    }

    #[test]
    fn writes_to_nearest_mutable_and_rejects_immutables() {
        let (mut interner, mut graph) = graph();
        let counter = interner.get("counter");
        let limit = interner.get("limit");

        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let initial = graph.add_literal(Literal::from(1));
        let overwritten = graph.add_literal(Literal::from(2));
        let immutable = graph.add_literal(Literal::from(3));

        table.add_symbol(
            true,
            Span::beginning(),
            counter,
            symbol(ty.clone(), initial),
        );
        table.add_symbol(false, Span::beginning(), limit, symbol(ty, immutable));

        table
            .write_symbol(Span::beginning(), counter, overwritten.clone())
            .expect("write mutable");
        assert_eq!(
            table.read_symbol(counter).expect("immutable").addr(),
            overwritten.addr()
        );

        let err = table
            .write_symbol(
                Span::beginning(),
                limit,
                graph.add_literal(Literal::from(4)),
            )
            .expect_err("immutable write should fail");
        assert!(matches!(
            err,
            Error::AssignmentToImmutableIdent { symbol: limit, .. }
        ));
    }

    #[test]
    fn reports_writes_to_unknown_variables() {
        let (mut interner, mut graph) = graph();
        let missing = interner.get("missing");

        let mut table = ScopedSymbolTable::new();
        let value = graph.add_literal(Literal::from(1));

        let err = table
            .write_symbol(Span::beginning(), missing, value)
            .expect_err("unknown write should fail");

        assert!(matches!(
            err,
            Error::AssignmentToUnknownVar {
                symbol: missing,
                ..
            }
        ));
    }

    #[test]
    fn snapshots_and_restores_mutable_state_in_scope_order() {
        let (mut interner, mut graph) = graph();
        let first = interner.get("first");
        let second = interner.get("second");

        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let first_value = graph.add_literal(Literal::from(1));
        let second_value = graph.add_literal(Literal::from(2));
        let first_new = graph.add_literal(Literal::from(10));
        let second_new = graph.add_literal(Literal::from(20));

        table.add_symbol(
            true,
            Span::beginning(),
            first,
            symbol(ty.clone(), first_value.clone()),
        );
        let scope = table.open_scope();
        table.add_symbol(
            true,
            Span::beginning(),
            second,
            symbol(ty, second_value.clone()),
        );

        let snapshot = table.snapshot_state();
        table
            .write_symbol(Span::beginning(), first, first_new.clone())
            .expect("write first");
        table
            .write_symbol(Span::beginning(), second, second_new.clone())
            .expect("write second");

        table.for_every_mutable(|idx, value| *value = snapshot[idx].clone());

        assert_eq!(
            table.read_symbol(first).expect("first").addr(),
            first_value.addr()
        );
        assert_eq!(
            table.read_symbol(second).expect("second").addr(),
            second_value.addr()
        );

        let outer_snapshot = table.snapshot_state_of_scope(table.open_scope_id());
        assert_eq!(outer_snapshot.into_iter().count(), 2);
        table.close_scope(scope);
    }

    #[test]
    fn all_symbols_includes_open_and_closed_scopes() {
        let (mut interner, mut graph) = graph();
        let global = interner.get("global");
        let local = interner.get("local");

        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let global_value = graph.add_literal(Literal::from(1));
        let local_value = graph.add_literal(Literal::from(2));

        table.add_symbol(
            false,
            Span::beginning(),
            global,
            symbol(ty.clone(), global_value.clone()),
        );
        let scope_tok = table.open_scope();
        table.add_symbol(
            true,
            Span::beginning(),
            local,
            symbol(ty, local_value.clone()),
        );
        table.close_scope(scope_tok);

        let dump = table.symbol_dump();
        assert_eq!(dump.immutables[&global].value.addr(), global_value.addr());
        assert_eq!(dump.mutables[&local].value.addr(), local_value.addr());
    }
}
