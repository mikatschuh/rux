use std::fmt;

pub type TypeResult<T> = Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    TooLargeIntegerSize,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::TooLargeIntegerSize =>
                    "the maximal size for an integer type of 2^128-1 was exceeded".to_string(),
            }
        )
    }
}
