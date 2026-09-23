use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(usize);

#[derive(Debug)]
pub struct Interner<'src> {
    names: Vec<&'src str>,
    hashmap: HashMap<&'src str, Symbol>,
}

impl<'src> Interner<'src> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            names: vec![],
            hashmap: HashMap::new(),
        }
    }

    pub fn get(&mut self, ident: &'src str) -> Symbol {
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
    pub fn resolve(&self, symbol: Symbol) -> &'src str {
        self.names[symbol.0]
    }
}
