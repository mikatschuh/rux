use std::collections::HashMap;

use crate::grapher::{Graph, graph::NodeID};

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub type_: NodeID<'src>,
    pub assignment: NodeID<'src>,
}

#[derive(Debug)]
struct Scope<'src> {
    symbols: HashMap<&'src str, Symbol<'src>>,
    unknowns: HashMap<&'src str, Vec<NodeID<'src>>>, // all nodes that represent the unknown
}

pub struct Scopes<'src> {
    scopes: Vec<Scope<'src>>,
}

impl<'src> Scope<'src> {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
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

    pub fn get_symbol(
        &mut self,
        graph: &mut Graph<'src>,
        name: &'src str,
    ) -> Result<Symbol<'src>, NodeID<'src>> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
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

    pub fn add_symbol(&mut self, name: &'src str, symbol: Symbol<'src>) {
        self.current().symbols.insert(name, symbol);
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
}
