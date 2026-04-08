use num::BigUint;

use crate::{literals, types::error::TypeResult};

mod error;
pub use error::Error;

pub type TypeSize = u128;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum PrimitiveType {
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

pub fn parse_type(mut ident: &[u8], target_ptr_size: TypeSize) -> TypeResult<PrimitiveType> {
    match ident[0] {
        b'u' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Ok(PrimitiveType::Unsigned {
                    size: target_ptr_size,
                });
            }

            Ok(PrimitiveType::Unsigned {
                size: parse_size(&mut ident)?,
            })
        }
        b'i' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Ok(PrimitiveType::Signed {
                    size: target_ptr_size,
                });
            }

            Ok(PrimitiveType::Signed {
                size: parse_size(&mut ident)?,
            })
        }

        _ if ident == b"f16" => Ok(PrimitiveType::Float(FloatPrecision::Half)),
        _ if ident == b"f32" => Ok(PrimitiveType::Float(FloatPrecision::Full)),
        _ if ident == b"f64" => Ok(PrimitiveType::Float(FloatPrecision::Double)),
        _ if ident == b"f128" => Ok(PrimitiveType::Float(FloatPrecision::DoubleDouble)),

        _ => Err(Error::NotAType),
    }
}

fn parse_size(input: &mut &[u8]) -> TypeResult<u128> {
    match literals::parse_integer(input) {
        (_, Some(integer)) => {
            <BigUint as TryInto<u128>>::try_into(integer).map_err(|_| Error::TooLargeIntegerSize)
        }
        _ => Err(Error::NotAType),
    }
}
