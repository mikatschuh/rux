use std::fmt;

pub(super) type LiteralResult<T> = Result<T, (usize, Error)>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    NoDigitsAtBeginning,
    MissingExponent,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::NoDigitsAtBeginning => "didn't have digits".to_string(),
                Self::MissingExponent => "after `e` or `p` there wasn't a number".to_string(),
            }
        )
    }
}
