use std::fmt;

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
