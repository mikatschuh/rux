use std::collections::HashMap;

use crate::{grapher::graph::TypeID, parser::Symbol};

#[derive(Debug, Clone)]
pub struct Binding {
    pub mutable: bool,
    pub ty: TypeID,
    pub id: VariableID,
}

pub struct Scope {
    bindings: HashMap<Symbol, Binding>,
}

pub struct SymbolTableStack {
    state_id: usize, // monotonic increasing
    scopes: Vec<Scope>,
}

/// This is a proof that the variable exists
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableID(usize);

#[must_use]
pub struct OpenScope(());

impl SymbolTableStack {
    pub fn new() -> Self {
        Self {
            state_id: 0,
            scopes: vec![],
        }
    }

    pub fn open_scope(&mut self) -> OpenScope {
        self.scopes.push(Scope {
            bindings: HashMap::new(),
        });
        OpenScope(())
    }

    pub fn close_scope(&mut self, _: OpenScope) {
        self.scopes.pop().unwrap(); // safe because of OpenScope
    }

    pub fn add_symbol(&mut self, mutable: bool, symbol: Symbol, ty: TypeID) -> Option<VariableID> {
        if let Some(scope) = self.scopes.last_mut() {
            let id = VariableID(self.state_id);
            scope.bindings.insert(symbol, Binding { mutable, ty, id });
            self.state_id += 1;
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
    use bumpalo::Bump;

    use super::SymbolTableStack;
    use crate::{
        grapher::Graph,
        literal_parsing::Literal,
        parser::{BuiltinType, Interner},
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn rejects_bindings_without_an_open_scope() {
        let mut graph = graph();
        let mut interner = Interner::new();
        let symbol = interner.get("outside");
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let mut state = Vec::new();

        let id = SymbolTableStack::new().add_symbol(false, symbol, ty, &mut state);

        assert!(id.is_none());
        assert!(state.is_empty());
    }

    #[test]
    fn lookup_prefers_inner_scope_and_restores_outer_after_close() {
        let mut graph = graph();
        let mut interner = Interner::new();
        let symbol = interner.get("value");
        let outer_ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let inner_ty = graph.add_builtin_type(BuiltinType::Unsigned { size: 32 });
        let mut table = SymbolTableStack::new();
        let mut state = Vec::new();

        let outer_scope = table.open_scope();
        let outer_id = table
            .add_symbol(false, symbol, outer_ty.clone(), &mut state)
            .expect("outer binding");
        let inner_scope = table.open_scope();
        let inner_id = table
            .add_symbol(true, symbol, inner_ty.clone(), &mut state)
            .expect("inner binding");

        let inner = table.get_binding(symbol).expect("inner lookup");
        assert_eq!(inner.id, inner_id);
        assert!(inner.mutable);
        assert_eq!(inner.ty.addr(), inner_ty.addr());

        table.close_scope_and_state(inner_scope, &mut state);
        let outer = table.get_binding(symbol).expect("outer lookup");
        assert_eq!(outer.id, outer_id);
        assert!(!outer.mutable);
        assert_eq!(outer.ty.addr(), outer_ty.addr());

        table.close_scope_and_state(outer_scope, &mut state);
        assert!(table.get_binding(symbol).is_none());
        assert!(state.is_empty());
    }

    #[test]
    fn duplicate_bindings_in_one_scope_truncate_all_allocated_state_slots() {
        let mut graph = graph();
        let mut interner = Interner::new();
        let symbol = interner.get("shadowed");
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        let mut table = SymbolTableStack::new();
        let mut state = Vec::new();

        let scope = table.open_scope();
        let first = table
            .add_symbol(false, symbol, ty.clone(), &mut state)
            .expect("first");
        let second = table
            .add_symbol(false, symbol, ty, &mut state)
            .expect("second");
        state[first] = Some(graph.add_literal(Literal::from(1)));
        state[second] = Some(graph.add_literal(Literal::from(2)));

        assert_eq!(table.get_binding(symbol).expect("binding").id, second);
        table.close_scope_and_state(scope, &mut state);

        assert!(state.is_empty());
        assert!(table.get_binding(symbol).is_none());
    }
}
