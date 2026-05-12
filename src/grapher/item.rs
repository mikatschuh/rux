use std::ops::Index;

use crate::grapher::graph::DataID;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ItemID(usize);

#[derive(Clone, Debug)]
pub struct ItemTypes {
    types: Vec<DataID>,
}

impl Index<ItemID> for ItemTypes {
    type Output = DataID;
    fn index(&self, index: ItemID) -> &Self::Output {
        &self.types[index.0]
    }
}

impl ItemTypes {
    pub fn new() -> Self {
        Self { types: vec![] }
    }

    pub fn add(&mut self, ty: DataID) -> ItemID {
        let id = self.types.len();
        self.types.push(ty);
        ItemID(id)
    }
}
