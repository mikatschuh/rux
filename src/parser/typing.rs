use std::fmt::Display;

use crate::{
    grapher::intern::Internalizer,
    parser::tree::{NodeBox, TreeDisplay},
    tokenizing::num,
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

#[cfg(target_pointer_width = "64")]
const SYSTEM_SIZE: usize = 64;

#[cfg(target_pointer_width = "32")]
const SYSTEM_SIZE: usize = 32;

#[cfg(target_pointer_width = "16")]
const SYSTEM_SIZE: usize = 16;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeParser {
    target_ptr_size: usize,
}

impl TypeParser {
    pub const fn new() -> Self {
        Self {
            target_ptr_size: SYSTEM_SIZE,
        }
    }
    pub fn parse_number_type(&self, mut input: &[u8]) -> Option<NumberType> {
        if input.is_empty() {
            return None;
        }

        let kind = match input[0] {
            b'u' => Unsigned,
            b'i' => Signed,
            b'f' => Float,
            _ => return None,
        };
        input = &input[1..];
        if input.is_empty() {
            return None;
        };
        Some(NumberType {
            kind,
            size: match input {
                b"x" => Some(self.target_ptr_size),
                _ => Some({
                    let mut parsed_size =
                        num::parse_number(&mut input).1?.to_u64_digits().into_iter();
                    if !input.is_empty() {
                        return None;
                    }
                    let size = parsed_size.next()?;
                    if !parsed_size.all(|digit| digit == 0) {
                        return None;
                    }
                    size as usize
                }),
            },
        })
    }
    pub fn parse_type_suffix(&self, input: &mut &[u8]) -> Option<NumberType> {
        let original_input = *input;
        if input.is_empty() {
            return None;
        }
        let kind = match input[0] {
            b'u' => Unsigned,
            b'i' => Signed,
            b'f' => Float,
            _ => return None,
        };
        *input = &input[1..];
        Some(NumberType {
            kind,
            size: Some(if input.starts_with(&[b'x']) {
                self.target_ptr_size
            } else if !input.is_empty() && input[0].is_ascii_digit() {
                let (_, parsed_size) = num::parse_number(input);
                let Some(parsed_size) = parsed_size else {
                    *input = original_input;
                    return None;
                };
                let mut parsed_size = parsed_size.to_u64_digits().into_iter();

                let size = parsed_size.next()?;
                if !parsed_size.all(|digit| digit == 0) {
                    *input = original_input;
                    return None;
                }
                size as usize
            } else {
                return Some(NumberType { kind, size: None });
            }),
        })
    }
}
