use crate::{LiteralParsingError, TypeParsingError};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    Literal(LiteralParsingError),
    Type(TypeParsingError),
    InvalidUTF8,
    UnknownEscapeSequence(String),
    NoClosingQuotes,
}
