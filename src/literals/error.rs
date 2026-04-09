use std::fmt;

/// Number of bytes correct and the error message
pub(super) type LiteralResult<T> = Result<T, Error>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    MissingExponent,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::MissingExponent => "after `e` or `p` there wasn't a number".to_string(),
            }
        )
    }
}
