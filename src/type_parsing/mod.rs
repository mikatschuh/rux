use num::BigUint;

use crate::{
    error::{ErrorCode, Errors, Span},
    literal_parsing,
    type_parsing::error::TypeResult,
};

mod error;
pub use error::Error;

pub type TypeSize = u128;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum IntegerType {
    Unsigned { size: TypeSize },
    Signed { size: TypeSize },
}

pub fn parse_type(
    mut ident: &[u8],
    span: Span,
    errors: &mut Errors,

    target_ptr_size: TypeSize,
) -> Option<IntegerType> {
    match ident[0] {
        b'u' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Some(IntegerType::Unsigned {
                    size: target_ptr_size,
                });
            }

            let size = parse_size(&mut ident)?.unwrap_or_else(|e| {
                errors.push(span, ErrorCode::TypeParsingError(e));
                target_ptr_size
            });

            if ident.is_empty() {
                Some(IntegerType::Unsigned { size })
            } else {
                None
            }
        }
        b'i' => {
            ident = &ident[1..];

            if ident == b"size" {
                return Some(IntegerType::Signed {
                    size: target_ptr_size,
                });
            }

            let size = parse_size(&mut ident)?.unwrap_or_else(|e| {
                errors.push(span, ErrorCode::TypeParsingError(e));
                target_ptr_size
            });

            if ident.is_empty() {
                Some(IntegerType::Signed { size })
            } else {
                None
            }
        }

        _ => None,
    }
}

fn parse_size(input: &mut &[u8]) -> Option<TypeResult<u128>> {
    match literal_parsing::parse_integer(input) {
        (_, Some(integer)) => Some(
            <BigUint as TryInto<u128>>::try_into(integer).map_err(|_| Error::TooLargeIntegerSize),
        ),
        _ => None,
    }
}
