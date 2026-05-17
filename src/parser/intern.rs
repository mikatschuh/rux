use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(usize);

impl Symbol {
    pub const fn unknown() -> Self {
        Self(0)
    }
}

#[derive(Debug)]
pub struct Interner {
    names: Vec<&'static str>,
    hashmap: HashMap<&'static str, Symbol>,
    counter: usize,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            names: vec!["{unknown}"],
            hashmap: HashMap::from([("{unknown}", Symbol(0))]),
            counter: 1,
        }
    }

    pub fn get(&mut self, ident: &'static str) -> Symbol {
        match self.hashmap.get(ident).cloned() {
            Some(symbol) => symbol,
            None => {
                let symbol = Symbol(self.counter);

                self.names.push(ident);
                self.hashmap.insert(ident, symbol);

                self.counter += 1;
                symbol
            }
        }
    }

    #[allow(unused)]
    pub fn resolve(&self, symbol: Symbol) -> &'static str {
        self.names[symbol.0]
    }
}
