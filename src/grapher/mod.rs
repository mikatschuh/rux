use crate::{
    error::Span,
    grapher::builder::{GraphBuilder, ScopedSymbolTable},
    tokenizing::{TokenStream, token::Token},
};
use std::fmt::{self};

mod builder;
#[allow(unused)]
mod graph;
pub mod graph_dump;
mod parser;
#[allow(unused)]
#[cfg(test)]
mod test;
pub use graph::Graph;

#[allow(unused)]
pub fn build_graph(tokens: &mut impl TokenStream) -> GraphResult<Graph> {
    GraphBuilder::new(tokens).build()
}

pub fn build_debug_graph(tokens: &mut impl TokenStream) -> GraphResult<(Graph, ScopedSymbolTable)> {
    GraphBuilder::new(tokens).debug_build()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IdentToken {
    src: &'static str,
    span: Span,
}

pub type GraphResult<T> = Result<T, GraphError>;

#[allow(unused)]
#[derive(Debug)]
pub enum GraphError {
    UnexpectedToken {
        expected: &'static str,
        found: Token,
    },
    MismatchedBracket {
        opener: Token,
        closer: Token,
    },

    ExpectedItem {
        found: Token,
    },
    ExpectedStatement {
        found: Token,
    },
    ExpectedExpression {
        found: Token,
    },
    ExpectedIdent {
        found: Token,
    },
    ExpectedAssignment {
        found: Token,
    },

    ConstShadowing {
        name: &'static str,
        decl: Span,
    },
    ShadowingConst {
        name: &'static str,
        decl: Span,
    },
    ConflictingItems {
        name: &'static str,
        decl: Span,
    },

    AssignmentToUnknownVar {
        name: &'static str,
        assignment: Span,
    },
    AssignmentToImmutableIdent {
        name: &'static str,
        assignment: Span,
    },

    TriedToReadUnitialized {
        name: IdentToken,
    },

    UnknownLabel {
        keyword: Token,
        label: IdentToken,
    },
    JumpOutsideLoop {
        keyword: Token,
    },
    UnreachableCtrl,
}

impl fmt::Display for GraphError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use GraphError::*;
        match self {
            ExpectedItem { found } => write!(f, "expected item at {:?}", found.span),
            ExpectedStatement { found } => write!(f, "expected statement at {:?}", found.span),
            ExpectedExpression { found } => write!(f, "expected expression at {:?}", found.span),
            ExpectedIdent { found } => write!(f, "expected name at {:?}", found.span),
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
            AssignmentToImmutableIdent {
                name,
                assignment: assigment,
            } => write!(
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

            UnknownLabel { keyword, label } => {
                write!(
                    f,
                    "used unknown label {} in a {} at {:?}",
                    label.src,
                    keyword.src,
                    keyword.span - label.span
                )
            }
            JumpOutsideLoop { keyword } => write!(
                f,
                "'{}' used outside of a loop at {:?}",
                keyword.src, keyword.span
            ),
            UnreachableCtrl => write!(f, "tried to make control flow which is unreachable",),
        }
    }
}

impl std::error::Error for GraphError {}
