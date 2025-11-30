use std::fmt::Display;

use crate::{
    grapher::intern::Internalizer,
    parser::tree::{NodeBox, TreeDisplay},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumberKind {
    Unsigned,
    Signed,
    Float,
    Arbitrary,
}
use NumberKind::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberType {
    pub kind: NumberKind,
    pub size: Option<usize>,
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            match self.kind {
                Unsigned => "u",
                Signed => "i",
                Float => "f",
                Arbitrary => "()",
            },
            match &self.size {
                Some(size) => size.to_string(),
                None if self.kind != Arbitrary => "_".to_owned(),
                None => "".to_owned(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type<'tree> {
    Number(NumberType),
    Expr(NodeBox<'tree>),
}
use Type::*;

impl<'tree> From<NumberType> for Type<'tree> {
    fn from(value: NumberType) -> Self {
        Number(value)
    }
}

impl<'tree> From<NodeBox<'tree>> for Type<'tree> {
    fn from(value: NodeBox<'tree>) -> Self {
        Expr(value)
    }
}

impl<'tree> Type<'tree> {
    pub fn display(&self, internalizer: &Internalizer<'tree>, indentation: &String) -> String {
        match self {
            Number(num) => format!("{num}"),
            Expr(node) => node.display(internalizer, indentation),
        }
    }
}
