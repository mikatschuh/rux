use crate::{
    error::{ErrorCode, Errors, Span},
    tokenizing::parse_tok::push_over_until_none_identifier_char,
};
use num::{BigInt, BigUint, bigint::Sign};

mod error;
mod literal;

pub use error::Error;
pub use literal::{Base, Literal};

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
pub fn parse_literal<'src>(
    text: &mut &'src [u8],
    span: &mut Span,
    errors: &mut Errors,
) -> Option<Literal<'src>> {
    let original_len = text.len();

    let (base, mut digits) = parse_integer(text);
    let num_digits_after_dot = if !text.is_empty() && text[0] == b'.' {
        *text = &text[1..];
        Some(parse_digits(&mut digits, base as u8, text))
    } else {
        None
    };

    let Some(digits) = digits else {
        return None;
    };
    span.end += original_len - text.len();
    // at this point we know for a fact that the user wanted to input a literal
    // that means we are definitely returning Some

    // parse exponent
    let exponent = if !text.is_empty()
        && (((base == Base::Binary || base == Base::Decimal) && text[0] == b'e')
            || (base == Base::Hexadecimal && text[0] == b'p'))
    {
        let original_len = text.len();
        *text = &text[1..];

        match 'try_parsing: {
            if text.is_empty() {
                break 'try_parsing None;
            }

            let sign = match text[0] {
                b'+' => {
                    *text = &text[1..];
                    Sign::Plus
                }
                b'-' => {
                    *text = &text[1..];
                    Sign::Minus
                }
                _ => Sign::Plus,
            };

            let exponent = parse_integer(text);
            span.end += original_len - text.len();

            match exponent {
                (_, Some(number)) => Some(BigInt::new(sign, number.to_u32_digits())),
                _ => None,
            }
        } {
            Some(exponent) => Some(exponent),
            None => {
                _ = push_over_until_none_identifier_char(text, span);

                errors.push(
                    *span,
                    ErrorCode::LiteralParsingError(Error::MissingExponent),
                );
                return Some(Literal {
                    base,
                    digits,
                    num_digits_after_dot,
                    exponent: None,
                    suffix: "",
                });
            }
        }
    } else {
        None
    };

    Some(Literal {
        digits,
        base,
        num_digits_after_dot,
        exponent,
        suffix: push_over_until_none_identifier_char(text, span).to_str(),
    })
}

use Base::*;

/// Defaults to Decimal
pub fn parse_base_prefix(text: &mut &[u8]) -> Base {
    if text.len() >= 2 {
        let base = match &text[..2] {
            b"0b" => Binary,
            b"0s" => Seximal,
            b"0o" => Octal,
            b"0d" => Dozenal,
            b"0x" => Hexadecimal,
            _ => return Decimal,
        };
        *text = &text[2..];
        base
    } else {
        Decimal
    }
}

pub fn parse_digits(number: &mut Option<BigUint>, radix: u8, text: &mut &[u8]) -> usize {
    let mut num_digits = 0;

    while !text.is_empty() {
        if let Some(digit) = to_digit(text[0], radix) {
            if let Some(number) = number {
                *number *= BigUint::from(radix);
                *number += BigUint::from(digit);
            } else {
                *number = Some(BigUint::from(digit));
            }
            num_digits += 1;
            *text = &text[1..];
        } else if text[0] == b'_' {
            *text = &text[1..];
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

pub fn parse_integer(text: &mut &[u8]) -> (Base, Option<BigUint>) {
    let base = parse_base_prefix(text);
    let mut number = None;
    parse_digits(&mut number, base as u8, text);
    (base, number)
}
