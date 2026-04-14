use std::collections::HashMap;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::ValueID},
};

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub type_: ValueID<'src>,
    pub assignment: ValueID<'src>,
}

#[derive(Debug)]
struct Scope<'src> {
    variables: HashMap<&'src str, Symbol<'src>>,
    mutables: HashMap<&'src str, Symbol<'src>>,

    constants: HashMap<&'src str, Symbol<'src>>,
    unknowns: HashMap<&'src str, Vec<ValueID<'src>>>, // all nodes that represent the unknown
}

pub struct Scopes<'src> {
    scopes: Vec<Scope<'src>>,
}

impl<'src> Scope<'src> {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            mutables: HashMap::new(),
            constants: HashMap::new(),
            unknowns: HashMap::new(),
        }
    }
}

impl<'src> Scopes<'src> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    fn current(&mut self) -> &mut Scope<'src> {
        self.scopes.last_mut().unwrap()
    }

    fn pop_scope(&mut self) -> Scope<'src> {
        self.scopes.pop().unwrap()
    }

    pub fn open_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn register_symbol(
        &mut self,
        graph: &mut Graph<'src>,
        name: &'src str,
    ) -> Result<Symbol<'src>, ValueID<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Ok(symbol.clone());
            }
            if let Some(symbol) = scope.mutables.get(name) {
                return Ok(symbol.clone());
            }
            if let Some(symbol) = scope.constants.get(name) {
                return Ok(symbol.clone());
            }
        }

        if let Some(unknown) = self.current().unknowns.get(name) {
            let unknown = &unknown[0];
            Err(unknown.clone())
        } else {
            let new_unknown = graph.add_unknown();
            self.current()
                .unknowns
                .insert(name, vec![new_unknown.clone()]);

            Err(new_unknown)
        }
    }

    pub fn add_variable(
        &mut self,
        decl: Span,
        name: &'src str,
        symbol: Symbol<'src>,
    ) -> GraphResult<'src, ()> {
        if self.current().constants.contains_key(name) {
            return Err(GraphError::ShadowingConst { name, decl });
        }
        self.current().mutables.remove(name);

        self.current().variables.insert(name, symbol);
        Ok(())
    }

    pub fn add_mutable(
        &mut self,
        decl: Span,
        name: &'src str,
        symbol: Symbol<'src>,
    ) -> GraphResult<'src, ()> {
        if self.current().constants.contains_key(name) {
            return Err(GraphError::ShadowingConst { name, decl });
        }
        self.current().variables.remove(name);

        self.current().mutables.insert(name, symbol);
        Ok(())
    }

    pub fn add_constant(
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
    }

    pub fn write_mutable(
        &mut self,
        span: Span,
        name: &'src str,
        value: ValueID<'src>,
    ) -> GraphResult<'src, ()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(mutable) = scope.mutables.get_mut(name) {
                mutable.assignment = value;
                return Ok(());
            }
        }
        Err(GraphError::AssignmentToUnknownVar { name, span })
    }

    pub fn close_scope(&mut self) {
        let old_unknowns = self.pop_scope().unknowns;

        for mut unknown in old_unknowns {
            if let Some(nodes) = self.current().unknowns.get_mut(&unknown.0) {
                nodes.append(&mut unknown.1)
            } else {
                self.current().unknowns.insert(unknown.0, unknown.1);
            }
        }
    }

    pub fn symbol(&self, name: &'src str) -> Option<Symbol<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Some(symbol.clone());
            }
            if let Some(symbol) = scope.mutables.get(name) {
                return Some(symbol.clone());
            }
            if let Some(symbol) = scope.constants.get(name) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn variable(&self, name: &'src str) -> Option<Symbol<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.variables.get(name) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn mutable(&self, name: &'src str) -> Option<Symbol<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.mutables.get(name) {
                return Some(symbol.clone());
            }
        }

        None
    }

    pub fn constant(&self, name: &'src str) -> Option<Symbol<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.constants.get(name) {
                return Some(symbol.clone());
            }
        }

        None
    }
}
