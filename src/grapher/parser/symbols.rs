use std::collections::HashMap;

use nonempty::NonEmpty;

use crate::{
    error::Span,
    grapher::{Graph, GraphError, GraphResult, graph::DataID, parser::alias::Alias},
};

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub type_: DataID<'src>,
    pub value: DataID<'src>,
}

#[derive(Debug)]
pub struct Scope<'src> {
    immutables: HashMap<&'src str, Symbol<'src>>,
    mutables: HashMap<&'src str, Symbol<'src>>,
}

pub type Overwrites<'src> = HashMap<Alias<'src>, DataID<'src>>;

#[derive(Debug)]
pub struct Branch<'src> {
    pub scopes: Vec<Scope<'src>>,
    pub overwrites: Overwrites<'src>,
}

pub struct ScopedSymbolTable<'src> {
    pub symbol_dump: SymbolDump<'src>,
    pub branches: NonEmpty<Branch<'src>>,
    unknowns: HashMap<&'src str, DataID<'src>>, // all nodes that represent the unknown constants
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
            branches: NonEmpty::new(Branch {
                scopes: vec![Scope::new()],
                overwrites: HashMap::new(),
            }),
            unknowns: HashMap::new(),
        }
    }

    pub fn current_scope(&mut self) -> &mut Scope<'src> {
        self.branches.last_mut().scopes.last_mut().unwrap()
    }

    pub fn open_scope(&mut self) {
        self.branches.last_mut().scopes.push(Scope::new());
    }

    pub fn close_scope(&mut self) {
        let mut scope = self.branches.last_mut().scopes.pop().unwrap();
        scope
            .immutables
            .drain()
            .for_each(|i| self.symbol_dump.immutables.push(i));
        scope
            .mutables
            .drain()
            .for_each(|i| self.symbol_dump.mutables.push(i));
    }

    pub fn open_branch(&mut self) {
        self.branches.push(Branch {
            scopes: vec![],
            overwrites: HashMap::new(),
        });
    }

    pub fn collect_mutables(&mut self) -> Overwrites<'src> {
        let mut overwrites = Overwrites::new();

        for mutable in self
            .branches
            .iter()
            .flat_map(|b| b.scopes.iter())
            .flat_map(|s| s.mutables.iter())
        {
            overwrites.insert(Alias::new(&mutable.1.value), mutable.1.value.clone());
        }
        overwrites
    }

    pub fn open_branch_with_overwrites(&mut self, overwrites: Overwrites<'src>) {
        self.branches.push(Branch {
            scopes: vec![],
            overwrites,
        });
    }

    #[must_use]
    pub fn close_branch(&mut self) -> Overwrites<'src> {
        debug_assert!(self.branches.last().scopes.is_empty());
        self.branches.pop().unwrap().overwrites
    }

    pub fn collect_overwrites_until_branch(&mut self, branch: usize) -> Overwrites<'src> {
        let mut branches = self.branches.iter().skip(branch);
        let mut overwrites = branches.next().unwrap().overwrites.clone();
        branches
            .flat_map(|b| b.overwrites.iter())
            .for_each(|(alias, overwrite)| {
                if let Some(prev_value) = overwrites.get_mut(alias) {
                    *prev_value = overwrite.clone();
                }
            });
        overwrites
    }

    fn build_alias_chain(
        &mut self,
        mut outer_scope_overwrite: Alias<'src>,
        branch_depth: usize,
    ) -> Alias<'src> {
        let mut prev_value = outer_scope_overwrite.read_current_value();

        let start = self.branches.len() - branch_depth;
        for branch in self.branches.iter_mut().skip(start) {
            if let Some(overwrite) = branch.overwrites.get(&outer_scope_overwrite) {
                outer_scope_overwrite = Alias::new(overwrite);
                prev_value = overwrite.clone();
            } else {
                branch
                    .overwrites
                    .insert(outer_scope_overwrite.clone(), prev_value.clone());
                outer_scope_overwrite =
                    Alias::new(branch.overwrites.get(&outer_scope_overwrite).unwrap());
            }
        }
        outer_scope_overwrite
    }

    fn resolve_overwrite_ptr(
        &self,
        mut symbol_ptr: Alias<'src>,
        branch_depth: usize,
    ) -> Alias<'src> {
        let start = self.branches.len() - branch_depth;
        for branch in self.branches.iter().skip(start) {
            if let Some(overwrite) = branch.overwrites.get(&symbol_ptr) {
                symbol_ptr = Alias::new(overwrite);
            }
        }

        symbol_ptr
    }

    pub fn write_symbol(
        &mut self,
        assignment: Span,
        name: &'src str,
        value: DataID<'src>,
    ) -> GraphResult<'src, ()> {
        let mut mutable_found: Option<(Alias<'src>, usize)> = None;

        'outer: for (depth, branch) in self.branches.iter_mut().rev().enumerate() {
            for scope in branch.scopes.iter_mut().rev() {
                if scope.immutables.contains_key(name) {
                    return Err(GraphError::AssignmentToImmutableIdent { name, assignment });
                }

                match scope.mutables.get_mut(name) {
                    Some(symbol) => {
                        if depth == 0 {
                            // symbol is within current branch
                            symbol.value = value;
                            return Ok(());
                        } else {
                            mutable_found = Some((Alias::new(&symbol.value), depth));
                            break 'outer;
                        }
                    }
                    None => continue,
                }
            }
        }

        if let Some((outer_scope_overwrite, branch_depth)) = mutable_found {
            self.build_alias_chain(outer_scope_overwrite, branch_depth)
                .over_write(value);
            return Ok(());
        }

        Err(GraphError::AssignmentToUnknownVar { name, assignment })
    }

    pub fn read_symbol(&mut self, graph: &mut Graph<'src>, name: &'src str) -> DataID<'src> {
        let mut mutable_found: Option<(Alias<'src>, usize)> = None;

        'outer: for (depth, branch) in self.branches.iter().rev().enumerate() {
            for scope in branch.scopes.iter().rev() {
                if let Some(symbol) = scope.immutables.get(name) {
                    return symbol.value.clone();
                }
                if let Some(symbol) = scope.mutables.get(name) {
                    mutable_found = Some((Alias::new(&symbol.value), depth));
                    break 'outer;
                }
            }
        }

        if let Some((symbol_ptr, branch_depth)) = mutable_found {
            let symbol_ptr = self.resolve_overwrite_ptr(symbol_ptr, branch_depth);
            return symbol_ptr.read_current_value();
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
        self.branches.iter_mut().for_each(|b| {
            b.scopes.iter_mut().for_each(|s| {
                s.immutables
                    .drain()
                    .for_each(|i| self.symbol_dump.immutables.push(i));
                s.mutables
                    .drain()
                    .for_each(|i| self.symbol_dump.mutables.push(i));
            })
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

#[cfg(test)]
mod tests {
    use super::{ScopedSymbolTable, Symbol};
    use crate::{error::Span, grapher::Graph};

    #[test]
    fn read_symbol_follows_nested_branch_overwrite_chain() {
        let mut graph = Graph::new();
        let type_ = graph.add_unit();
        let before = graph.add_quote("before".into());
        let second = graph.add_quote("second".into());
        let third_clause = graph.add_quote("third-clause".into());
        let thirdish_clause = graph.add_quote("thirdish-clause".into());

        let mut symbols = ScopedSymbolTable::new();
        symbols.add_mutable(
            "mutable",
            Symbol {
                type_: type_.clone(),
                value: before,
            },
        );

        symbols.open_branch();
        symbols.open_scope();
        symbols
            .write_symbol(Span::beginning(), "mutable", second)
            .expect("first branch write");

        symbols.open_branch();
        symbols.open_scope();
        symbols
            .write_symbol(Span::beginning(), "mutable", third_clause)
            .expect("nested branch write");
        symbols
            .write_symbol(Span::beginning(), "mutable", thirdish_clause.clone())
            .expect("nested branch overwrite");

        let value = symbols.read_symbol(&mut graph, "mutable");
        assert!(value.ptr_cmp(&thirdish_clause));
    }
}
