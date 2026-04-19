use std::{collections::HashMap, iter::Rev};

use nonempty::NonEmpty;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::ValueID},
};

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub mutable: bool,
    pub type_: ValueID<'src>,
    pub assignment: ValueID<'src>,
}

#[derive(Debug)]
pub struct Scope<'src> {
    variables: HashMap<&'src str, Symbol<'src>>,
}

pub struct Scopes<'src> {
    scopes: NonEmpty<Scope<'src>>,
    unknowns: HashMap<&'src str, Vec<ValueID<'src>>>, // all nodes that represent the unknown constants
}

impl<'src> Scope<'src> {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

impl<'src> Scopes<'src> {
    pub fn new() -> Self {
        Self {
            scopes: NonEmpty::new(Scope::new()),
            unknowns: HashMap::new(),
        }
    }

    fn current(&mut self) -> &mut Scope<'src> {
        self.scopes.last_mut()
    }

    fn scopes(&self) -> Rev<impl DoubleEndedIterator<Item = &Scope<'src>> + '_> {
        self.scopes.iter().rev()
    }

    fn scopes_mut(&mut self) -> Rev<impl DoubleEndedIterator<Item = &mut Scope<'src>> + '_> {
        self.scopes.iter_mut().rev()
    }

    pub fn open_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn close_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn use_symbol(
        &mut self,
        graph: &mut Graph<'src>,
        name: &'src str,
    ) -> Result<Symbol<'src>, ValueID<'src>> {
        for scope in self.scopes() {
            if let Some(symbol) = scope.variables.get(name) {
                return Ok(symbol.clone());
            }
        }

        if let Some(unknown) = self.unknowns.get(name) {
            let unknown = &unknown[0];
            Err(unknown.clone())
        } else {
            let new_unknown = graph.add_unknown();
            self.unknowns.insert(name, vec![new_unknown.clone()]);

            Err(new_unknown)
        }
    }

    pub fn add_symbol(&mut self, name: &'src str, symbol: Symbol<'src>) {
        self.current().variables.insert(name, symbol);
    }

    pub fn get_mutable_in_this_branch<'a>(
        &'a mut self,
        name: &'src str,
        scopes_within_branch: usize,
    ) -> Option<&'a mut Symbol<'src>> {
        for (i, scope) in self.scopes_mut().enumerate() {
            if i >= scopes_within_branch {
                return None;
            }
            if let Some(symbol) = scope.variables.get_mut(name)
                && symbol.mutable
            {
                return Some(symbol);
            }
        }
        None
    }

    pub fn get_symbol_to_assign(
        &mut self,
        span: Span,
        name: &'src str,
    ) -> GraphResult<'src, &mut Symbol<'src>> {
        for scope in self.scopes_mut() {
            if let Some(symbol) = scope.variables.get_mut(name) {
                return if symbol.mutable {
                    Ok(symbol)
                } else {
                    Err(GraphError::AssignmentToImmutableIdent {
                        name,
                        assigment: span,
                    })
                };
            }
        }

        Err(GraphError::AssignmentToUnknownVar {
            name,
            assignment: span,
        })
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
        SymbolDump {
            variables: self.current().variables.drain().collect(),
        }
    }
}

pub struct SymbolDump<'src> {
    pub variables: Vec<(&'src str, Symbol<'src>)>,
}
