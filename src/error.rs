use colored::*;
use std::any::Any;
use std::error;
use std::fmt;
use std::fmt::Debug;
use std::path::Path;
use std::sync::{Arc, RwLock};

use crate::grapher::Graph;
use crate::grapher::Type;
use crate::parser::Interner;
use crate::parser::Symbol;
use crate::tokenizing::span::Span;
use crate::tokenizing::token::Bracket;
use crate::{
    literal_parsing::Error as LiteralError, type_parsing::Error as PrimitiveTypeParsingError,
};

/// A shared diagnostic collection. Clones append to the same collection.
#[derive(Clone, Debug)]
pub struct Errors<'src> {
    file: &'src Path,
    errors: Arc<RwLock<Vec<Error>>>,
}

impl<'src> Errors<'src> {
    pub fn new(path: &'src Path, pos: Span, error: ErrorCode) -> Self {
        Self {
            file: path,
            errors: Arc::new(RwLock::new(vec![Error::new(pos, error)])),
        }
    }

    pub fn empty(path: &'src Path) -> Self {
        Self {
            file: path,
            errors: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors
            .read()
            .expect("diagnostic lock poisoned")
            .is_empty()
    }

    pub fn push(&self, pos: Span, error: ErrorCode) {
        self.errors
            .write()
            .expect("diagnostic lock poisoned")
            .push(Error::new(pos, error))
    }

    pub fn display(&self, interner: &Interner, graph: &Graph) -> String {
        let mut string = String::new();
        for err in self.errors.read().expect("diagnostic lock poisoned").iter() {
            string += &err.display(self.file, interner, graph);
            string += "\n"
        }

        string
    }
}

impl PartialEq for Errors<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.file != other.file {
            return false;
        }
        if Arc::ptr_eq(&self.errors, &other.errors) {
            return true;
        }
        // Compare snapshots without holding two locks at once.
        let errors = self
            .errors
            .read()
            .expect("diagnostic lock poisoned")
            .clone();
        let other_errors = other.errors.read().expect("diagnostic lock poisoned");
        errors == *other_errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{sync::Barrier, thread};

    #[test]
    fn clones_share_diagnostics_and_outlive_original() {
        let errors = Errors::empty(Path::new("example.rx"));
        let shared = errors.clone();
        shared.push(Span::beginning(), ErrorCode::ExpectedIdent);
        assert!(!errors.is_empty());
        assert_eq!(errors, shared);
        drop(errors);
        assert_eq!(
            shared,
            Errors::new(
                Path::new("example.rx"),
                Span::beginning(),
                ErrorCode::ExpectedIdent,
            )
        );
    }

    #[test]
    fn collects_all_diagnostics_from_concurrent_writers() {
        let errors = Errors::empty(Path::new("example.rx"));
        let barrier = Barrier::new(8);
        thread::scope(|scope| {
            for worker in 0..8 {
                let errors = errors.clone();
                let barrier = &barrier;
                scope.spawn(move || {
                    barrier.wait();
                    for _ in 0..256 {
                        errors.push(
                            Span::beginning(),
                            ErrorCode::UnknownEscapeSequence {
                                given: worker.to_string(),
                            },
                        );
                        assert!(!errors.is_empty());
                    }
                });
            }
        });
        let collected = errors.errors.read().unwrap();
        assert_eq!(collected.len(), 8 * 256);
        for worker in 0..8 {
            let expected = ErrorCode::UnknownEscapeSequence {
                given: worker.to_string(),
            };
            assert_eq!(
                collected.iter().filter(|err| err.error == expected).count(),
                256
            );
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub span: Span,
    pub error: ErrorCode,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    // tokenizing
    InvalidUTF8,
    UnknownEscapeSequence { given: String },
    NoClosingQuotes,
    LiteralParsingError(LiteralError),
    TypeParsingError(PrimitiveTypeParsingError),

    // parsing
    ExpectedExpr,
    ExpectedIdent,
    ExpectedTerminator,
    ExpectedComma,
    ExpectedAssignment,
    ExpectedItemDeclaration,

    // bracket
    ExpectedOpenParen,
    ExpectedClosedBracket { opened: Bracket },
    LonelyClosedBracket { closed: Bracket },

    // semantic
    MissingEntryPoint { entry: &'static str },

    // variable
    BindingOutsideScope,
    UnknownIdent { symbol: Symbol },
    AssignmentToUnknownIdent { symbol: Symbol },
    AssignmentToImmutableIdent { symbol: Symbol },
    ReadUnitializedOrMoved,

    // control flow
    ContinueOutsideLoop,
    ContinueWithUnknownLabel { label: Symbol },
    BreakOutsideLoop,
    BreakWithUnknownLabel { label: Symbol },
    DivergentControlFlow,

    BindingWithNeitherTypeNorValue,

    // initialization / ownership
    MovedInLoop,

    // type checking
    ExpectedType,
    WrongType { expected: Type, got: Type },
}
impl Error {
    pub fn new(span: Span, error: ErrorCode) -> Self {
        Self { span, error }
    }
}

static ERROR: std::sync::LazyLock<String> =
    std::sync::LazyLock::new(|| format!("{}{}", "ERROR".bold().red(), ":".bold()));
macro_rules! format_error {
    // version without tip
    ($pos:expr, $msg:expr, [$($arg:expr),*]) => {
        format!(
            "\n{}\t{} {}\n",
            *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos
        )
    };
    // version without tip and without arguments
    ($pos:expr, $msg:expr) => {
        format!(
            "\n{}\t{} {}\n",
            *ERROR,
            $msg,
            $pos
        )
    };

    // version with tip
    ($pos:expr, $msg:expr, [$($arg:expr),*], $tip:expr, [$($tip_arg:expr),*]) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos,
            "Tip".bold(),
            format!($tip, $(format!(" {} ", $tip_arg.bold())),*)
        )
    };
    // version with tip but without arguments
    ($pos:expr, $msg:expr, $tip:expr) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            $msg,
            $pos,
            "Tip".bold(),
            $tip
        )
    };
    // version with tip but without arguments for tip
    ($pos:expr, $msg:expr, [$($arg:expr),*], $tip:expr) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos,
            "Tip".bold(),
            $tip
        )
    };
    // version without tip and without position
    ($msg:expr, [$($arg:expr),*]) => {
        format!(
            "\n{}\t{}\n",
            *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
        )
    };
    // version without tip and without position and without arguments
    ($msg:expr) => {
        format!(
            "\n{}\t{}\n",
            *ERROR,
            $msg,
        )
    };

    // version with tip and without position
    ($msg:expr, [$($arg:expr),*], $tip:expr, [$($tip_arg:expr),*]) => {
        format!(
            "\n{}\t{}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            "Tip".bold(),
            format!($tip, $(format!(" {} ", $tip_arg.bold())),*)
        )
    };
}
#[macro_export]
macro_rules! format_error_quote_arg {
    () => {
        format!(" \"\" ").bold()
    };
    ($($es:expr),+) => {{
        use std::fmt::Write;
        format!(" \"{}\" ", concat_display!{ $($es),+ }).bold()
    }};
}
#[macro_export]
macro_rules! format_error_arg {
    () => {
        format!("  ").bold()
    };
    ($($es:expr),+) => {{
        use std::fmt::Write;
        format!(" {} ", concat_display!{ $($es),+ })
    }};
}

impl Error {
    fn display(&self, path: &Path, interner: &Interner, graph: &Graph) -> String {
        use ErrorCode::*;
        (match &self.error {
            InvalidUTF8 => {
                format_error!(self.span.to_string(path), "found invalid UTF-8 character")
            }
            UnknownEscapeSequence { given } => {
                format_error!(
                    self.span.to_string(path),
                    "unknown escape sequence: {}",
                    [given]
                )
            }
            NoClosingQuotes => format_error!(
                self.span.to_string(path),
                "the ending quotes of the quote were missing"
            ),
            LiteralParsingError(err) => {
                format_error!(
                    self.span.to_string(path),
                    "literal error: {}",
                    [err.to_string()]
                )
            }
            TypeParsingError(err) => {
                format_error!(
                    self.span.to_string(path),
                    "primitive type parsing error: {}",
                    [err.to_string()]
                )
            }

            ExpectedExpr => format_error!(self.span.to_string(path), "expected a value"),
            ExpectedIdent => format_error!(
                self.span.to_string(path),
                "expected an identifier",
                "you have to always put an identifier behind a tick"
            ),
            ExpectedTerminator => format_error!(
                self.span.to_string(path),
                "expected a comma or any closed bracket"
            ),
            ExpectedComma => format_error!(self.span.to_string(path), "expected a comma"),
            ExpectedAssignment => {
                format_error!(self.span.to_string(path), "expected an assignment")
            }
            ExpectedItemDeclaration => format_error!(
                self.span.to_string(path),
                "expected an item declaration with {}",
                ["::"]
            ),

            ExpectedOpenParen => {
                format_error!(
                    self.span.to_string(path),
                    "expected open parentheses {}",
                    ["("]
                )
            }
            ExpectedClosedBracket { opened } => {
                format_error!(
                    self.span.to_string(path),
                    "expected a closed bracket {}",
                    [opened.display_closed()]
                )
            }
            LonelyClosedBracket { closed } => {
                format_error!(
                    self.span.to_string(path),
                    "found a closed bracket {} with no opened one before",
                    [closed.display_closed()]
                )
            }

            MissingEntryPoint { entry } => {
                format_error!(self.span.to_string(path), "entry point {} missing", [entry])
            }
            BindingOutsideScope => {
                format_error!(
                    self.span.to_string(path),
                    "found a binding outside of any scope"
                )
            }
            UnknownIdent { symbol } => format_error!(
                self.span.to_string(path),
                "found an unknown identifier {}",
                [interner.resolve(*symbol)]
            ),
            AssignmentToUnknownIdent { symbol } => format_error!(
                self.span.to_string(path),
                "assignment to unknown variable {}",
                [interner.resolve(*symbol)]
            ),
            AssignmentToImmutableIdent { symbol } => format_error!(
                self.span.to_string(path),
                "assignment to immutable, initialized variable {}",
                [interner.resolve(*symbol)]
            ),
            ReadUnitializedOrMoved => format_error!(
                self.span.to_string(path),
                "tried to read uninitialized/moved variable"
            ),
            ContinueOutsideLoop => format_error!(
                self.span.to_string(path),
                "found continue outside of a loop/block"
            ),
            ContinueWithUnknownLabel { label } => format_error!(
                self.span.to_string(path),
                "found continue with {}, an unknown label",
                [interner.resolve(*label)]
            ),
            BreakOutsideLoop => format_error!(
                self.span.to_string(path),
                "found break outside of a loop/block"
            ),
            BreakWithUnknownLabel { label } => format_error!(
                self.span.to_string(path),
                "found break with {}, an unknown label",
                [interner.resolve(*label)]
            ),

            DivergentControlFlow => format_error!(
                self.span.to_string(path),
                "the control flow inside a larger block converges at this point"
            ),

            BindingWithNeitherTypeNorValue => format_error!(
                self.span.to_string(path),
                "a binding contained neither a type nor a value"
            ),

            MovedInLoop => format_error!(
                self.span.to_string(path),
                "value moved in loop and then still used"
            ),

            ExpectedType => format_error!(self.span.to_string(path), "expected a type expression"),
            WrongType { expected, got } => {
                format_error!(
                    self.span.to_string(path),
                    "expected {} got {}",
                    [
                        format!("{:?}", graph[expected]),
                        format!("{:?}", graph[got])
                    ]
                )
            }
        })
        .to_string()
    }
}
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
                Self::ThreadPanic(e) => format_error!(e),
                Self::Io(e) => format_error!(e),
                Self::NotValidUTF8(path) => {
                    format_error!(
                        "the file  {}  did not contain valid UTF-8",
                        format!("{:?}", path)
                    )
                }
                Self::CommandLine(mes) => format_error!(mes),
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
