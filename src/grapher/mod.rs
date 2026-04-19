use crate::{
    error::Span,
    grapher::parser::{GraphBuilder, Scopes},
    tokenizing::{TokenStream, token::Token},
};
use std::fmt::{self};

mod graph;
pub mod graph_dump;
mod parser;
#[cfg(test)]
mod test;

pub use graph::Graph;

pub fn build_graph<'src>(tokens: &mut impl TokenStream<'src>) -> GraphResult<'src, Graph<'src>> {
    GraphBuilder::new(tokens).build()
}

pub fn build_debug_graph<'src>(
    tokens: &mut impl TokenStream<'src>,
) -> GraphResult<'src, (Graph<'src>, Scopes<'src>)> {
    GraphBuilder::new(tokens).debug_build()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentToken<'src> {
    src: &'src str,
    span: Span,
}

pub type GraphResult<'src, T> = Result<T, GraphError<'src>>;

#[derive(Debug)]
pub enum GraphError<'src> {
    ExpectedItem {
        found: Token<'src>,
    },
    ExpectedStatement {
        found: Token<'src>,
    },
    ExpectedExpression {
        found: Token<'src>,
    },
    ExpectedName {
        found: Token<'src>,
    },
    ExpectedAssignment {
        found: Token<'src>,
    },

    UnexpectedToken {
        expected: &'static str,
        found: Token<'src>,
    },

    ConstShadowing {
        name: &'src str,
        decl: Span,
    },
    ShadowingConst {
        name: &'src str,
        decl: Span,
    },
    ConflictingItems {
        name: &'src str,
        decl: Span,
    },

    AssignmentToUnknownVar {
        name: &'src str,
        assignment: Span,
    },
    AssignmentToImmutableIdent {
        name: &'src str,
        assigment: Span,
    },

    TriedToReadUnitialized {
        name: IdentToken<'src>,
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
            ExpectedItem { found } => write!(f, "expected item at {:?}", found.span),
            ExpectedStatement { found } => write!(f, "expected statement at {:?}", found.span),
            ExpectedExpression { found } => write!(f, "expected expression at {:?}", found.span),
            ExpectedName { found } => write!(f, "expected name at {:?}", found.span),
            ExpectedAssignment { found } => write!(f, "expected an assignment at {:?}", found.span),

            UnexpectedToken { expected, found } => write!(
                f,
                "expected {}, found '{}' at {:?}",
                expected, found.src, found.span
            ),

            ConstShadowing { name, decl } => {
                write!(f, "const {name} shadowing variable at {:?}", decl)
            }
            ShadowingConst { name, decl } => {
                write!(f, "variable {name} shadowing const at {:?}", decl)
            }
            ConflictingItems { name, decl } => {
                write!(f, "const {name} conflicting with const at {:?}", decl)
            }

            AssignmentToUnknownVar { name, assignment } => write!(
                f,
                "assignment to unknown identifier '{}' at {:?}",
                name, assignment
            ),
            AssignmentToImmutableIdent { name, assigment } => write!(
                f,
                "assignment to immutable identifier '{}' at {:?}",
                name, assigment
            ),

            TriedToReadUnitialized { name } => {
                write!(
                    f,
                    "identifier '{}' read without being ever assigned at {:?}",
                    name.src, name.span
                )
            }
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
