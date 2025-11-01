use crate::parser::{
    tokenizing::{token::TokenKind, Tokenizer},
    typing::{NumberType, TypeParser},
};
#[allow(unused)]
use crate::{
    error::*,
    format_error_quote_arg,
    parser::{
        binary_op::BinaryOp,
        tree::{Node, NodeBox, NodeWrapper, Note},
    },
};
use num::{bigint::Sign, BigInt, BigUint};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Literal {
    pub digits: BigUint,
    pub base: Base,
    pub digits_after_dot: usize,
    pub exponent: Option<BigInt>,
    pub type_suffix: Option<NumberType>,
}

impl<'src> Tokenizer<'src> {
    /// Function to parse any literal into an AST-Node.
    /// Format:
    /// - whitespaces are only for formatting and precedence
    /// - content in brackets is optional
    /// ```
    /// N = (0  b/s/o/d/x) DIGITS
    ///
    /// numbers = N  .DIGITS  ((u/i/f N) / e / p)
    /// ```
    /// Meaning of prefixes:
    /// ```
    /// b => Binary
    /// s => Seximal
    /// o => Octal
    /// d => Dozenal
    /// x => Hexadecimal
    /// ```
    /// If instead of a base specifier, a digit is given, its assumed that the leading zero
    /// was just a typo. The number will be continued regulary.
    pub(super) fn try_to_parse_literal(
        &mut self,
        mut ident: &'src [u8],
        type_parser: TypeParser,
    ) -> (usize, Option<Literal>) {
        let original_len = ident.len();
        let (base, mut digits) = parse_number(&mut ident);

        let digits_after_dot = if !ident.is_empty() && ident[0] == b'.' {
            ident = &ident[1..];
            parse_digits(&mut digits, base as u8, &mut ident)
        } else {
            0
        };

        let Some(digits) = digits else {
            return (original_len - ident.len(), None);
        };

        let exponent = if !ident.is_empty() && (ident[0] == b'e' || ident[0] == b'p') {
            ident = &ident[1..];

            if ident.is_empty() {
                return (original_len - ident.len(), None);
            }

            let sign = match ident[0] {
                b'+' => {
                    ident = &ident[1..];
                    Sign::Plus
                }
                b'-' => {
                    ident = &ident[1..];
                    Sign::Minus
                }
                _ => Sign::Plus,
            };
            let (_, number) = parse_number(&mut ident);

            let Some(number) = number else {
                return (original_len - ident.len(), None);
            };

            Some(BigInt::new(sign, number.to_u32_digits()))
        } else {
            None
        };

        let type_suffix = type_parser.parse_type_suffix(&mut ident);

        let ident = unsafe { str::from_utf8_unchecked(ident) };

        if ident.starts_with("\"")
            || ident.starts_with(|first: char| first.is_whitespace())
            || ident.starts_with(|first: char| TokenKind::new(first).is_some())
            || ident.is_empty()
        {
            (
                original_len - ident.len(),
                Some(Literal {
                    digits,
                    base,
                    digits_after_dot,
                    exponent,
                    type_suffix,
                }),
            )
        } else {
            (original_len - ident.len(), None)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Base {
    Binary = 2,
    Seximal = 6,
    Octal = 8,
    Decimal = 10,
    Dozenal = 12,
    Hexadecimal = 16,
}
use Base::*;

/// Defaults to Decimal
fn parse_base_prefix(num: &mut &[u8]) -> Base {
    if num.len() >= 2 {
        let base = match &num[..2] {
            b"0b" => Binary,
            b"0s" => Seximal,
            b"0o" => Octal,
            b"0d" => Dozenal,
            b"0x" => Hexadecimal,
            _ => return Decimal,
        };
        *num = &num[2..];
        base
    } else {
        Decimal
    }
}

fn parse_digits(number: &mut Option<BigUint>, radix: u8, input: &mut &[u8]) -> usize {
    let mut num_digits = 0;

    while !input.is_empty() {
        if let Some(digit) = to_digit(input[0], radix) {
            if let Some(number) = number {
                *number *= BigUint::from(radix);
                *number += BigUint::from(digit);
            } else {
                *number = Some(BigUint::from(digit));
            }
            num_digits += 1;
            *input = &input[1..];
        } else if input[0] == b'_' {
            *input = &input[1..];
        } else {
            return num_digits;
        }
    }
    num_digits
}

fn to_digit(c: u8, radix: u8) -> Option<u8> {
    debug_assert!(radix <= 36);

    if c.is_ascii_digit() && c <= b'0' - 1 + radix {
        return Some(c - b'0');
    }

    let c = c | 0b0010_0000; // OR the third bit into c - this has the equivalent of making it lowercase

    if c.is_ascii_lowercase() && c <= b'a' - 11 + radix {
        return Some(c - b'a' + 10);
    }
    None
}

pub fn parse_number(num: &mut &[u8]) -> (Base, Option<BigUint>) {
    let base = parse_base_prefix(num);
    let mut number = None;
    parse_digits(&mut number, base as u8, num);
    (base, number)
}
