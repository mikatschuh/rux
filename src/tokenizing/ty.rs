use num::BigUint;

use crate::tokenizing::parse_literal;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum Type {
    Unsigned { size: usize },
    Signed { size: usize },
    Float,
    DoublePrecision,
}

pub(super) fn parse_type<'src>(
    mut ident: &'src [u8],
    target_ptr_size: usize,
) -> Option<(usize, Type)> {
    match ident[0] {
        b'u' => {
            ident = &ident[1..];

            if ident[0] == b'x' {
                return Some((
                    2,
                    Type::Unsigned {
                        size: target_ptr_size,
                    },
                ));
            }

            match parse_size(ident) {
                Some((lit_bytes, size)) => Some((1 + lit_bytes, Type::Unsigned { size })),
                None => None,
            }
        }
        b'i' => {
            ident = &ident[1..];

            if ident[0] == b'x' {
                return Some((
                    2,
                    Type::Signed {
                        size: target_ptr_size,
                    },
                ));
            }

            match parse_size(ident) {
                Some((lit_bytes, size)) => Some((1 + lit_bytes, Type::Signed { size })),
                None => None,
            }
        }
        _ if ident.starts_with(b"f32") => Some((3, Type::Float)),
        _ if ident.starts_with(b"f64") => Some((3, Type::DoublePrecision)),
        _ => None,
    }
}

fn parse_size<'src>(input: &'src [u8]) -> Option<(usize, usize)> {
    if let Some((used_bytes, literal)) = parse_literal(input) {
        if literal.num_digits_after_dot != 0 || literal.exponent != None || literal.suffix != "" {
            return None;
        }

        if let Ok(size) = <BigUint as TryInto<usize>>::try_into(literal.digits) {
            return Some((used_bytes, size));
        }
    }
    None
}
