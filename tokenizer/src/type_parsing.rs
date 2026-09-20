use crate::literal_parsing;
use num::BigUint;

pub type TypeSize = u16;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum IntegerType {
    Unsigned { size: TypeSize },
    Signed { size: TypeSize },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    TooLargeSize,
}

pub fn parse_type(
    mut ident: &[u8],
    target_ptr_size: TypeSize,
) -> Result<IntegerType, Option<(IntegerType, Error)>> {
    match ident[0] {
        b'u' => {
            ident = &ident[1..];
            let default = IntegerType::Unsigned {
                size: target_ptr_size,
            };

            if ident == b"size" {
                return Ok(default);
            }

            let size = parse_size(&mut ident);
            if !ident.is_empty() {
                return Err(None);
            }

            Ok(IntegerType::Unsigned {
                size: size.map_err(|e| e.map(|e| (default, e)))?,
            })
        }
        b'i' => {
            ident = &ident[1..];
            let default = IntegerType::Signed {
                size: target_ptr_size,
            };

            if ident == b"size" {
                return Ok(default);
            }

            let size = parse_size(&mut ident);
            if !ident.is_empty() {
                return Err(None);
            }

            Ok(IntegerType::Signed {
                size: size.map_err(|e| e.map(|e| (default, e)))?,
            })
        }

        _ => Err(None),
    }
}

fn parse_size(input: &mut &[u8]) -> Result<TypeSize, Option<Error>> {
    match literal_parsing::parse_integer(input).1 {
        Some(integer) => match <BigUint as TryInto<TypeSize>>::try_into(integer).ok() {
            Some(size) => Ok(size),
            None => Err(Some(Error::TooLargeSize)),
        },
        None => Err(None),
    }
}
