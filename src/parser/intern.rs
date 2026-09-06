use std::collections::HashMap;

use crate::{
    parser::ast::{Ident, Spanned},
    tokenizing::span::Span,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(usize);

#[derive(Debug)]
pub struct Interner {
    names: Vec<&'static str>,
    hashmap: HashMap<&'static str, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            names: vec![],
            hashmap: HashMap::new(),
        }
    }

    pub fn get(&mut self, ident: &'static str) -> Symbol {
        match self.hashmap.get(ident).cloned() {
            Some(symbol) => symbol,
            None => {
                let symbol = Symbol(self.names.len());

                self.names.push(ident);
                self.hashmap.insert(ident, symbol);

                symbol
            }
        }
    }

    pub fn get_ident(&mut self, span: Span, ident: &'static str) -> Ident {
        Spanned {
            span,
            val: self.get(ident),
        }
    }

    #[allow(unused)]
    pub fn resolve(&self, symbol: Symbol) -> &'static str {
        self.names[symbol.0]
    }
}
