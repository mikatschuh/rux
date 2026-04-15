use num::BigUint;

use crate::{
    error::{ErrorCode, Errors, Span},
    literal,
    types::error::TypeResult,
};

mod error;
pub use error::Error;

pub type TypeSize = u128;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum AtomicType {
    Unsigned { size: TypeSize },
    Signed { size: TypeSize },
    Float(FloatPrecision),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum FloatPrecision {
    Half = 16,
    Full = 32,
    Double = 64,
    DoubleDouble = 128,
}

pub fn parse_type(
    mut ident: &[u8],
    span: Span,
    errors: &mut Errors,

    target_ptr_size: TypeSize,
) -> Option<AtomicType> {
    match ident[0] {
        b'u' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Some(AtomicType::Unsigned {
                    size: target_ptr_size,
                });
            }

            let size = parse_size(&mut ident)?.unwrap_or_else(|e| {
                errors.push(span, ErrorCode::TypeParsingError(e));
                target_ptr_size
            });

            if ident.is_empty() {
                Some(AtomicType::Unsigned { size })
            } else {
                None
            }
        }
        b'i' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Some(AtomicType::Signed {
                    size: target_ptr_size,
                });
            }

            let size = parse_size(&mut ident)?.unwrap_or_else(|e| {
                errors.push(span, ErrorCode::TypeParsingError(e));
                target_ptr_size
            });

            if ident.is_empty() {
                Some(AtomicType::Signed { size })
            } else {
                None
            }
        }

        _ if ident == b"f16" => Some(AtomicType::Float(FloatPrecision::Half)),
        _ if ident == b"f32" => Some(AtomicType::Float(FloatPrecision::Full)),
        _ if ident == b"f64" => Some(AtomicType::Float(FloatPrecision::Double)),
        _ if ident == b"f128" => Some(AtomicType::Float(FloatPrecision::DoubleDouble)),

        _ => None,
    }
}

fn parse_size(input: &mut &[u8]) -> Option<TypeResult<u128>> {
    match literal::parse_integer(input) {
        (_, Some(integer)) => Some(
            <BigUint as TryInto<u128>>::try_into(integer).map_err(|_| Error::TooLargeIntegerSize),
        ),
        _ => None,
    }
}
