use std::path::Path;

use crate::{
    error::Span,
    parser::{Interner, Spanned, Symbol},
};

pub type Result<T> = std::result::Result<T, Error>;

#[allow(unused)]
#[derive(Debug)]
pub enum Error {
    BindingOutsideOfScope {
        binding: Span,
        symbol: Symbol,
    },

    AssignmentToUnknownVar {
        symbol: Symbol,
        equal: Span,
    },
    AssignmentToImmutableIdent {
        symbol: Symbol,
        equal: Span,
    },

    TriedToReadUnitialized {
        symbol: Spanned<Symbol>,
    },

    ContinueWithUnknownLabel {
        keyword: Span,
        label: Spanned<Symbol>,
    },
    BreakWithUnknownLabel {
        keyword: Span,
        label: Spanned<Symbol>,
    },
    ContinueOutsideLoop {
        keyword: Span,
    },
    BreakOutsideLoop {
        keyword: Span,
    },
    UnreachableCtrl,
}

impl Error {
    fn to_string(&self, path: &Path, interner: &Interner) -> String {
        use Error::*;
        match self {
            BindingOutsideOfScope { binding, symbol } => {
                format!(
                    "found a binding binding {} outside of any scope at {}",
                    interner.resolve(*symbol),
                    binding.to_string(path)
                )
            }
            AssignmentToUnknownVar {
                symbol,
                equal: assignment,
            } => format!(
                "assignment to unknown identifier '{}' at {}",
                interner.resolve(*symbol),
                assignment.to_string(path)
            ),
            AssignmentToImmutableIdent {
                symbol,
                equal: assigment,
            } => format!(
                "assignment to immutable identifier '{}' at {}",
                interner.resolve(*symbol),
                assigment.to_string(path)
            ),

            TriedToReadUnitialized { symbol } => {
                format!(
                    "identifier '{}' read without being ever assigned at {}",
                    interner.resolve(symbol.val),
                    symbol.span.to_string(path)
                )
            }

            ContinueWithUnknownLabel { keyword, label } => format!(
                "used unknown label {} at {} in a continue at {}",
                interner.resolve(label.val),
                label.span.to_string(path),
                (*keyword - label.span).to_string(path)
            ),
            BreakWithUnknownLabel { keyword, label } => format!(
                "used unknown label {} at {} in a break at {}",
                interner.resolve(label.val),
                label.span.to_string(path),
                (*keyword - label.span).to_string(path)
            ),
            ContinueOutsideLoop { keyword } => format!(
                "continue used outside of a loop at {}",
                keyword.to_string(path)
            ),
            BreakOutsideLoop { keyword } => format!(
                "break used outside of a loop at {}",
                keyword.to_string(path)
            ),
            UnreachableCtrl => format!("tried to make control flow which is unreachable",),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "consider using `to_string` instead")
    }
}
impl std::error::Error for Error {}
