use std::{any::Any, error, fmt, path::Path};

#[derive(Debug)]
pub enum CliError {
    Io(std::io::Error),
    CommandLine(&'static str),
    ThreadPanic(String),
    NotValidUTF8(&'static Path),
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::ThreadPanic(e) => e.clone(),
                Self::Io(e) => e.to_string(),
                Self::NotValidUTF8(path) =>
                    format!("the file {:?} did not contain valid UTF-8", path),
                Self::CommandLine(mes) => mes.to_string(),
            }
        )
    }
}

// implentation of error
impl error::Error for CliError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            CliError::Io(e) => Some(e),
            _ => None,
        }
    }
}

// convertion of other error types
impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> CliError {
        CliError::Io(err)
    }
}
impl From<Box<dyn Any + Send + 'static>> for CliError {
    fn from(err: Box<dyn Any + Send + 'static>) -> CliError {
        if let Some(panic_msg) = err.downcast_ref::<String>() {
            CliError::ThreadPanic(panic_msg.clone())
        } else {
            CliError::ThreadPanic("unknown-panic-error".to_string())
        }
    }
}
