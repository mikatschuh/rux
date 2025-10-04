use std::{collections::HashMap, marker::PhantomData};

use crate::{
    comp,
    parser::{intern::Symbol, tree::NodeBox},
};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct Variable<'table> {
    _marker: PhantomData<&'table ()>,
    idx: usize,
}
impl Variable<'_> {
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData::default(),
            idx,
        }
    }
}

pub type Type<'src> = NodeBox<'src>;

pub struct VarTable<'src> {
    table: HashMap<Variable<'src>, Type<'src>>,
    next_var_idx: usize,
    scopes: comp::Vec<HashMap<Symbol<'src>, Variable<'src>>, 1>,
}

impl<'src> VarTable<'src> {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            next_var_idx: 0,
            scopes: comp::Vec::new([HashMap::new()]),
        }
    }

    pub fn add(&mut self, sym: Symbol<'src>, ty: Type<'src>) {
        let var = Variable::new(self.next_var_idx);
        self.next_var_idx += 1;

        self.table.insert(var, ty);
        self.scopes.last_mut().insert(sym, var);
    }

    pub fn get(&self, sym: Symbol<'src>) -> Option<Variable<'src>> {
        for sym_var_table in self.scopes.iter().rev() {
            if let Some(var) = sym_var_table.get(&sym) {
                return Some(*var);
            }
        }
        None
    }

    pub fn open_new_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    pub fn close_last_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("VarTable::close_last_scope was called even though there was no scope to close")
        }
    }
}
