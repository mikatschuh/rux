use crate::tokenizing::token::Token;
use std::fmt::{self};

mod graph;
// #[allow(unused)]
// mod graph_dump;
mod parser;
#[cfg(test)]
mod test;

pub use graph::Graph;

pub type GraphResult<'src, T> = Result<T, GraphError<'src>>;

#[derive(Debug)]
pub enum GraphError<'src> {
    UnexpectedToken {
        expected: &'static str,
        found: Token<'src>,
    },
    AssignmentToUnknownIdent {
        ident: Token<'src>,
    },
    AssignmentToImmutableIdent {
        ident: Token<'src>,
    },
    IdentWithoutAssignment {
        ident: Token<'src>,
    },
    InvalidLiteral {
        token: Token<'src>,
    },
    ExpectedExpression {
        found: Token<'src>,
    },
    MismatchedBracket {
        opener: Token<'src>,
        closer: Token<'src>,
    },
    JumpOutsideLoop {
        keyword: Token<'src>,
    },
    UnreachableStatementAfterJump {
        jump: Token<'src>,
        statement: Token<'src>,
    },
}

impl fmt::Display for GraphError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use GraphError::*;
        match self {
            UnexpectedToken { expected, found } => write!(
                f,
                "expected {}, found '{}' at {:?}",
                expected, found.src, found.span
            ),
            AssignmentToUnknownIdent { ident } => write!(
                f,
                "assignment to unknown identifier '{}' at {:?}",
                ident.src, ident.span
            ),
            AssignmentToImmutableIdent { ident } => write!(
                f,
                "assignment to immutable identifier '{}' at {:?}",
                ident.src, ident.span
            ),
            IdentWithoutAssignment { ident } => {
                write!(
                    f,
                    "identifier '{}' read without being ever assigned at {:?}",
                    ident.src, ident.span
                )
            }
            InvalidLiteral { token } => {
                write!(f, "invalid literal '{}' at {:?}", token.src, token.span)
            }
            ExpectedExpression { found } => write!(f, "expected expression near {:?}", found.span),
            MismatchedBracket { opener, closer } => write!(
                f,
                "mismatched brackets: opened at {:?}, closed with '{}' at {:?}",
                opener.span, closer.src, closer.span
            ),
            JumpOutsideLoop { keyword } => write!(
                f,
                "'{}' used outside of a loop at {:?}",
                keyword.src, keyword.span
            ),
            UnreachableStatementAfterJump { jump, statement } => write!(
                f,
                "statement '{}' at {:?} is unreachable after '{}' at {:?}",
                statement.src, statement.span, jump.src, jump.span
            ),
        }
    }
}

impl std::error::Error for GraphError<'_> {}
