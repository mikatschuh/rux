use crate::{
    grapher::parser::{GraphBuilder, Parser},
    tokenizing::{
        binary_op::BinaryOp, num::Literal, token::Token, ty::Type, unary_op::UnaryOp, TokenStream,
    },
    utilities::{MocAllocator, Rc},
};
use bumpalo::Bump;
use std::{collections::HashMap, fmt};

mod parser;
#[cfg(test)]
mod test;

pub type GraphResult<'src, T> = Result<T, GraphError<'src>>;

pub type NodeId<'src> = Rc<Node<'src>, MocAllocator>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Node<'src> {
    pub kind: NodeKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum NodeKind<'src> {
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    PrimitiveType {
        ty: Type,
    },

    Unary {
        op: UnaryOp,
        input: NodeId<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: NodeId<'src>,
        rhs: NodeId<'src>,
    },

    Phi {
        condition: NodeId<'src>,
        when_true: NodeId<'src>,
        when_false: NodeId<'src>,
    },

    UnknownIdent {
        name: &'src str,
    },
}

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub ty: NodeId<'src>,
    pub last_value: Option<NodeId<'src>>,
}

#[derive(Debug, Default)]
pub struct Graph<'src> {
    arena: Bump,
    symbols: HashMap<&'src str, Symbol<'src>>,
    node_cache: HashMap<NodeKind<'src>, NodeId<'src>>,
}

impl<'src> Graph<'src> {
    pub fn from_stream<'tokens>(
        tokens: &'tokens mut impl TokenStream<'src>,
    ) -> GraphResult<'src, Graph<'src>> {
        GraphBuilder::new(tokens).build()
    }

    pub fn symbol(&self, name: &str) -> Option<&Symbol<'src>> {
        self.symbols.get(name)
    }

    fn push_node(&mut self, kind: NodeKind<'src>) -> NodeId<'src> {
        if let Some(existing) = self.node_cache.get(&kind) {
            return existing.clone();
        }

        let key = kind.clone();
        let node: Node<'src> = Node { kind };
        let node_id = Rc::<Node<'src>, MocAllocator>::new_in_bump(node, &self.arena);
        self.node_cache.insert(key, node_id.clone());
        node_id
    }

    fn add_literal(&mut self, literal: Literal<'src>) -> NodeId<'src> {
        self.push_node(NodeKind::Literal { literal })
    }

    fn add_unary(&mut self, op: UnaryOp, input: NodeId<'src>) -> NodeId<'src> {
        self.push_node(NodeKind::Unary { op, input })
    }

    fn add_binary(&mut self, op: BinaryOp, lhs: NodeId<'src>, rhs: NodeId<'src>) -> NodeId<'src> {
        self.push_node(NodeKind::Binary { op, lhs, rhs })
    }

    fn add_phi(
        &mut self,
        condition: NodeId<'src>,
        when_true: NodeId<'src>,
        when_false: NodeId<'src>,
    ) -> NodeId<'src> {
        self.push_node(NodeKind::Phi {
            condition,
            when_true,
            when_false,
        })
    }

    fn add_quote(&mut self, quote: String) -> NodeId<'src> {
        self.push_node(NodeKind::Quote { quote })
    }

    fn add_type(&mut self, ty: Type) -> NodeId<'src> {
        self.push_node(NodeKind::PrimitiveType { ty })
    }

    fn declare_variable(&mut self, name: Token<'src>, ty: NodeId<'src>) {
        self.symbols.insert(
            name.src,
            Symbol {
                ty,
                last_value: None,
            },
        );
    }

    fn assign_variable(&mut self, name: Token<'src>, value: NodeId<'src>) -> GraphResult<'src, ()> {
        if !self.symbols.contains_key(name.src) {
            return Err(GraphError::AssignmentToUnknownIdent { ident: name });
        }

        if let Some(symbol) = self.symbols.get_mut(name.src) {
            symbol.last_value = Some(value);
        }
        Ok(())
    }

    fn read_variable(&mut self, ident: Token<'src>) -> GraphResult<'src, NodeId<'src>> {
        let Some(symbol) = self.symbols.get(ident.src) else {
            let unknown_id = self.push_node(NodeKind::UnknownIdent { name: ident.src });
            return Ok(unknown_id);
        };

        match symbol.last_value {
            Some(ref last_value) => Ok(last_value.clone()),
            None => Err(GraphError::IdentWithoutAssignment { ident }),
        }
    }

    fn snapshot_symbols(&self) -> HashMap<&'src str, Symbol<'src>> {
        self.symbols.clone()
    }

    fn replace_symbols(&mut self, snapshot: HashMap<&'src str, Symbol<'src>>) {
        self.symbols = snapshot;
    }
}

#[derive(Debug)]
pub enum GraphError<'src> {
    UnexpectedToken {
        expected: &'static str,
        found: Token<'src>,
    },
    AssignmentToUnknownIdent {
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
        }
    }
}

impl std::error::Error for GraphError<'_> {}
