use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    BaseWithoutBody,
    MissingExponent,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::BaseWithoutBody => "after a base prefix there was simply no body".to_string(),
                Self::MissingExponent => "after `e` or `p` there wasn't a number".to_string(),
            }
        )
    }
}
