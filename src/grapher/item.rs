use std::ops::Index;

use crate::grapher::graph::Data;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ItemID(usize);

#[derive(Clone, Debug)]
pub struct ItemTypes {
    types: Vec<Data>,
}

impl Index<ItemID> for ItemTypes {
    type Output = Data;
    fn index(&self, index: ItemID) -> &Self::Output {
        &self.types[index.0]
    }
}

impl ItemTypes {
    pub fn new() -> Self {
        Self { types: vec![] }
    }

    pub fn add(&mut self, ty: Data) -> ItemID {
        let id = self.types.len();
        self.types.push(ty);
        ItemID(id)
    }
}
