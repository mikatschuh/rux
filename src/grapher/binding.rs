use std::collections::HashMap;

use crate::{
    grapher::graph::{DataID, TypeID},
    parser::Symbol,
};

#[derive(Debug, Clone)]
pub struct Binding {
    pub mutable: bool,
    pub ty: TypeID,
    pub id: StateID,
}

pub struct Scope {
    size: usize,
    bindings: HashMap<Symbol, Binding>,
}

pub struct SymbolTableStack {
    scopes: Vec<Scope>,
}

pub type StateID = usize;
pub type MutableState = Vec<Option<DataID>>;

#[must_use]
pub struct OpenScope(());

impl SymbolTableStack {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn open_scope(&mut self) -> OpenScope {
        self.scopes.push(Scope {
            size: 0,
            bindings: HashMap::new(),
        });
        OpenScope(())
    }

    pub fn close_scope_and_state(&mut self, _: OpenScope, state: &mut MutableState) {
        let scope = self.scopes.pop().unwrap(); // safe because of OpenScope
        state.truncate(state.len() - scope.size); // remove the mutables from the state
    }

    pub fn close_scope(&mut self, _: OpenScope) {
        self.scopes.pop().unwrap(); // safe because of OpenScope
    }

    pub fn add_symbol(
        &mut self,
        mutable: bool,
        symbol: Symbol,
        ty: TypeID,
        state: &mut MutableState,
    ) -> Option<StateID> {
        if let Some(scope) = self.scopes.last_mut() {
            let id = state.len();
            state.push(None);
            scope.bindings.insert(symbol, Binding { mutable, ty, id });
            scope.size += 1;
            Some(id)
        } else {
            None
        }
    }

    pub fn get_binding(&mut self, symbol: Symbol) -> Option<&Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.bindings.get(&symbol) {
                return Some(binding);
            }
        }
        None
    }
}

#[cfg(never)]
#[cfg(test)]
mod tests {
    use super::{Binding, SymbolTableStack};
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

    fn interner_and_graph() -> (Interner, Graph) {
        (Interner::new(), Graph::new(Bump::new()))
    }

    fn symbol(ty: TypeID, value: DataID) -> ImmutableBinding {
        ImmutableBinding { ty, value }
    }

    #[test]
    fn reads_nearest_symbol_and_restores_outer_symbol_after_scope_closes() {
        let (mut interner, mut graph) = interner_and_graph();
        let setting = interner.get("setting");

        let mut table = SymbolTableStack::new();
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
        let (mut interner, mut graph) = interner_and_graph();
        let counter = interner.get("counter");
        let limit = interner.get("limit");

        let mut table = SymbolTableStack::new();
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
        let (mut interner, mut graph) = interner_and_graph();
        let missing = interner.get("missing");

        let mut table = SymbolTableStack::new();
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
        let (mut interner, mut graph) = interner_and_graph();
        let first = interner.get("first");
        let second = interner.get("second");

        let mut table = SymbolTableStack::new();
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
        let (mut interner, mut graph) = interner_and_graph();
        let global = interner.get("global");
        let local = interner.get("local");

        let mut table = SymbolTableStack::new();
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
