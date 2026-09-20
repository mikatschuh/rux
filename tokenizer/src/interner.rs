use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(usize);

#[derive(Debug)]
pub struct Interner {
    names: Vec<&'static str>,
    hashmap: HashMap<&'static str, Symbol>,
}

impl Interner {
    #[allow(clippy::new_without_default)]
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

    #[allow(unused)]
    pub fn resolve(&self, symbol: Symbol) -> &'static str {
        self.names[symbol.0]
    }
}
