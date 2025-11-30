use crate::tokenizing::{token::TokenKind, whitespace_at_start_or_empty};
use num::{bigint::Sign, BigInt, BigUint, FromPrimitive};
use std::slice;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal<'src> {
    pub base: Base,
    pub digits: BigUint,
    pub num_digits_after_dot: usize,
    pub exponent: Option<BigInt>,
    pub suffix: &'src str,
}

impl<'src, N: Into<u128>> From<N> for Literal<'src> {
    fn from(num: N) -> Self {
        Self {
            base: Base::Decimal,
            digits: BigUint::from_u128(num.into()).expect("BigUint"),
            num_digits_after_dot: 0,
            exponent: None,
            suffix: "",
        }
    }
}

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
pub(super) fn parse_literal<'src>(mut ident: &'src [u8]) -> Option<(usize, Literal<'src>)> {
    let original_len = ident.len();
    let (base, mut digits) = parse_number(&mut ident);

    let num_digits_after_dot = if !ident.is_empty() && ident[0] == b'.' {
        ident = &ident[1..];
        parse_digits(&mut digits, base as u8, &mut ident)
    } else {
        0
    };

    let Some(digits) = digits else {
        return None;
    };

    let exponent = 'blk: {
        // parse exponent
        if !ident.is_empty() && (ident[0] == b'e' || ident[0] == b'p') {
            let original_slice = ident;

            ident = &ident[1..];

            if ident.is_empty() {
                ident = original_slice;
                break 'blk None;
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
                ident = original_slice;
                break 'blk None;
            };

            Some(BigInt::new(sign, number.to_u32_digits()))
        } else {
            None
        }
    };

    let suffix_start = ident.as_ptr();
    let mut suffix_len = 0_usize;
    while !ident.is_empty() {
        if ident[0] == b'\"'
            || whitespace_at_start_or_empty(ident)
            || TokenKind::new(ident[0]).is_some()
        {
            break;
        }

        ident = &ident[1..];
        suffix_len += 1;
    }

    Some((
        original_len - ident.len(),
        Literal {
            digits,
            base,
            num_digits_after_dot,
            exponent,
            suffix: unsafe {
                str::from_utf8_unchecked(slice::from_raw_parts(suffix_start, suffix_len))
            },
        },
    ))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
