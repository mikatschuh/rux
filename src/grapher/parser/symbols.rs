use std::{collections::HashMap, iter::Rev};

use nonempty::NonEmpty;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::ValueID},
};

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub type_: ValueID<'src>,
    pub value: ValueID<'src>,
}

#[derive(Debug)]
pub struct Scope<'src> {
    immutables: HashMap<&'src str, Symbol<'src>>,
    mutables: HashMap<&'src str, Symbol<'src>>,
}

pub type Overwrites<'src> = HashMap<*mut ValueID<'src>, ValueID<'src>>;

#[derive(Debug)]
pub struct Branch<'src> {
    pub scopes: Vec<Scope<'src>>,
    pub overwrites: Overwrites<'src>,
}

pub struct ScopedSymbolTable<'src> {
    pub branches: NonEmpty<Branch<'src>>,
    unknowns: HashMap<&'src str, ValueID<'src>>, // all nodes that represent the unknown constants
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
            branches: NonEmpty::new(Branch {
                scopes: vec![Scope::new()],
                overwrites: HashMap::new(),
            }),
            unknowns: HashMap::new(),
        }
    }

    pub fn branches(&mut self) -> Rev<impl DoubleEndedIterator<Item = &mut Branch<'src>> + '_> {
        self.branches.iter_mut().rev()
    }

    pub fn current_scope(&mut self) -> &mut Scope<'src> {
        self.branches.last_mut().scopes.last_mut().unwrap()
    }

    pub fn open_scope(&mut self) {
        self.branches.last_mut().scopes.push(Scope::new());
    }

    pub fn close_scope(&mut self) {
        self.branches.last_mut().scopes.pop();
    }

    pub fn open_branch(&mut self) {
        self.branches.push(Branch {
            scopes: vec![],
            overwrites: HashMap::new(),
        });
    }

    #[must_use]
    pub fn close_branch(&mut self) -> Overwrites<'src> {
        self.branches.pop().unwrap().overwrites
    }

    pub fn write_symbol(
        &mut self,
        assignment: Span,
        name: &'src str,
        value: ValueID<'src>,
    ) -> GraphResult<'src, ()> {
        let mut mutable_found: Option<(ValueID, *mut ValueID<'src>, usize)> = None;

        'outer: for (i, branch) in self.branches.iter_mut().rev().enumerate() {
            for scope in branch.scopes.iter_mut().rev() {
                if scope.immutables.contains_key(name) {
                    return Err(GraphError::AssignmentToImmutableIdent { name, assignment });
                }

                match scope.mutables.get_mut(name) {
                    Some(symbol) => {
                        if i == 0 {
                            // symbol is within current branch
                            symbol.value = value;
                            return Ok(());
                        } else {
                            mutable_found = Some((
                                symbol.value.clone(),
                                &mut symbol.value as *mut ValueID<'src>,
                                i,
                            ));
                            break 'outer;
                        }
                    }
                    None => continue,
                }
            }
        }

        if let Some((mut prev_value, mut outer_scope_overwrite, i)) = mutable_found {
            let start = self.branches.len() - i;
            for branch in self.branches.iter_mut().skip(start) {
                if let Some(overwrite) = branch.overwrites.get_mut(&outer_scope_overwrite) {
                    outer_scope_overwrite = overwrite as *mut ValueID;
                    prev_value = overwrite.clone();
                } else {
                    branch
                        .overwrites
                        .insert(outer_scope_overwrite, prev_value.clone());
                    outer_scope_overwrite =
                        branch.overwrites.get_mut(&outer_scope_overwrite).unwrap() as *mut ValueID;
                }
            }
            unsafe { *outer_scope_overwrite = value }

            return Ok(());
        }

        Err(GraphError::AssignmentToUnknownVar { name, assignment })
    }

    pub fn use_symbol(&mut self, graph: &mut Graph<'src>, name: &'src str) -> ValueID<'src> {
        let mut mutable_found: Option<(*mut ValueID<'src>, usize)> = None;

        'outer: for (i, branch) in self.branches.iter_mut().rev().enumerate() {
            for scope in branch.scopes.iter_mut().rev() {
                if let Some(symbol) = scope.immutables.get(name) {
                    return symbol.value.clone();
                }
                if let Some(symbol) = scope.mutables.get_mut(name) {
                    mutable_found = Some((&mut symbol.value as *mut ValueID<'src>, i));
                    break 'outer;
                }
            }
        }

        if let Some((symbol_ptr, i)) = mutable_found {
            for branch in self.branches.iter().rev().take(i) {
                if let Some(overwrite) = branch.overwrites.get(&symbol_ptr) {
                    return overwrite.clone();
                }
            }

            return unsafe { (*symbol_ptr).clone() };
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
        SymbolDump {
            immutables: self
                .branches
                .last_mut()
                .scopes
                .last_mut()
                .map_or(vec![], |s| s.immutables.drain().collect()),
            mutables: self
                .branches
                .last_mut()
                .scopes
                .last_mut()
                .map_or(vec![], |s| s.mutables.drain().collect()),
        }
    }
}

pub struct SymbolDump<'src> {
    pub immutables: Vec<(&'src str, Symbol<'src>)>,
    pub mutables: Vec<(&'src str, Symbol<'src>)>,
}
