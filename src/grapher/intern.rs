use std::{collections::HashMap, marker::PhantomData};

const EMPTY: &str = "{empty}";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol<'internalizer> {
    _marker: PhantomData<&'internalizer ()>,
    idx: usize,
}

pub struct Internalizer<'src> {
    map: HashMap<&'src str, Symbol<'src>>,
    vec: Vec<&'src str>, // Optional: zum Zurückübersetzen
}

impl<'src> Internalizer<'src> {
    pub fn new() -> Self {
        let map = HashMap::from([(
            EMPTY,
            Symbol {
                _marker: PhantomData,
                idx: 0,
            },
        )]);
        let vec = vec![EMPTY];
        Self { map, vec }
    }

    pub fn get(&mut self, name: &'src str) -> Symbol<'src> {
        if let Some(&sym) = self.map.get(name) {
            return sym;
        }
        let id = Symbol {
            _marker: PhantomData,
            idx: self.vec.len(),
        };
        self.vec.push(name);
        self.map.insert(name, id);
        id
    }

    #[inline]
    pub const fn empty(&mut self) -> Symbol<'src> {
        Symbol {
            _marker: PhantomData,
            idx: 0,
        }
    }

    pub fn resolve(&self, sym: Symbol) -> &str {
        self.vec[sym.idx]
    }
}
