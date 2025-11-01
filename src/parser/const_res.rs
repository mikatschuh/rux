use std::{fmt, marker::PhantomData, ops::Index};

use crate::parser::tree::NodeBox;

pub struct Const<'src> {
    _marker: PhantomData<&'src ()>,
    idx: usize,
}

impl Const<'_> {
    pub fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData::default(),
            idx,
        }
    }
}

impl fmt::Display for Const<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const[ {} ]", self.idx)
    }
}

pub type ConstTable<'src> = Vec<NodeBox<'src>>;

impl<'src> Index<Const<'src>> for ConstTable<'src> {
    type Output = NodeBox<'src>;
    fn index(&self, index: Const<'src>) -> &Self::Output {
        &self[index.idx]
    }
}
