use crate::{
    tokenizing::{
        binary_op::BinaryOp,
        binding_pow,
        num::Literal,
        token::{Token, TokenKind},
        ty::Type,
        unary_op::UnaryOp,
        TokenStream,
    },
    utilities::{MocAllocator, Rc},
};
use bumpalo::Bump;
use std::{collections::HashMap, fmt};

#[cfg(test)]
mod test;

pub type GraphResult<'src, T> = Result<T, GraphError<'src>>;

pub type NodeId<'src> = Rc<Node<'src>, MocAllocator>;

#[derive(Debug, PartialEq, Eq)]
pub struct Node<'src> {
    pub kind: NodeKind<'src>,
}

#[derive(Debug, PartialEq, Eq)]
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
    UnknownIdent {
        name: &'src str,
    },
}

#[derive(Debug)]
pub struct Symbol<'src> {
    pub ty: NodeId<'src>,
    pub last_value: Option<NodeId<'src>>,
}

#[derive(Debug, Default)]
pub struct Graph<'src> {
    arena: Bump,
    symbols: HashMap<&'src str, Symbol<'src>>,
}

impl<'src> Graph<'src> {
    pub fn from_stream(tokens: &mut impl TokenStream<'src>) -> GraphResult<'src, Graph<'src>> {
        GraphBuilder::new(tokens).build()
    }

    pub fn symbol(&self, name: &str) -> Option<&Symbol<'src>> {
        self.symbols.get(name)
    }

    fn push_node(&mut self, kind: NodeKind<'src>) -> NodeId<'src> {
        let node: Node<'src> = Node { kind };
        Rc::<Node<'src>, MocAllocator>::new_in_bump(node, &self.arena)
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

struct GraphBuilder<'src, 'tokens, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    graph: Graph<'src>,
}

impl<'src, 'tokens, T: TokenStream<'src>> GraphBuilder<'src, 'tokens, T> {
    fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::default(),
        }
    }

    fn build(mut self) -> GraphResult<'src, Graph<'src>> {
        loop {
            let token = self.peek();
            match token.kind {
                TokenKind::EOF => return Ok(self.graph),
                TokenKind::Semicolon => {
                    self.advance();
                }
                TokenKind::Ident => {
                    let name = self.advance();
                    self.parse_variable_tail(name, 1)?
                }
                _ => {
                    return Err(GraphError::UnexpectedToken {
                        expected: "identifier",
                        found: token,
                    })
                }
            }
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> GraphResult<'src, NodeId<'src>> {
        let mut lhs = self.parse_primary(min_bp)?;

        // connect tokens to this one
        loop {
            let tok = self.peek();
            if tok.binding_pow() < min_bp {
                return Ok(lhs);
            }

            if let Some(op) = tok.as_infix() {
                self.advance();
                let rhs = self.parse_expr(op.binding_pow())?;

                lhs = self.graph.add_binary(op, lhs, rhs);
            } else if let Some(op) = tok.as_postfix() {
                self.advance();
                lhs = self.graph.add_unary(op, lhs)
            } else {
                return Ok(lhs);
            }
        }
    }

    fn parse_primary(&mut self, min_bp: u8) -> GraphResult<'src, NodeId<'src>> {
        let tok = self.peek();
        match tok.kind {
            TokenKind::Literal => {
                let literal = self.tokens.get_literal();
                self.advance();
                Ok(self.graph.add_literal(literal))
            }
            TokenKind::Quote => {
                let quote = self.tokens.get_quote();
                self.advance();
                Ok(self.graph.add_quote(quote))
            }
            TokenKind::Type => {
                let ty = self.tokens.get_type();
                self.advance();
                Ok(self.graph.add_type(ty))
            }
            TokenKind::Ident => {
                let name = self.advance();
                self.parse_variable_tail(name, min_bp)?;
                self.graph.read_variable(name)
            }
            TokenKind::Open(open) => {
                self.advance();
                let expr = self.parse_expr(0)?;
                let closer = self.advance();
                match closer.kind {
                    TokenKind::Closed(closed) if closed == open => Ok(expr),
                    _ => Err(GraphError::MismatchedBracket {
                        opener: tok,
                        closer,
                    }),
                }
            }
            _ => match tok.as_prefix() {
                Some(op) => {
                    self.advance();
                    let node = self.parse_primary(op.binding_pow())?;
                    Ok(self.graph.add_unary(UnaryOp::Neg, node))
                }
                None => {
                    self.advance();
                    Err(GraphError::ExpectedExpression { found: tok })
                }
            },
        }
    }

    fn parse_variable_tail(&mut self, name: Token<'src>, min_bp: u8) -> GraphResult<'src, ()> {
        if binding_pow::WRITE < min_bp {
            return Ok(());
        }

        if min_bp <= 1 && self.peek().binding_pow() == 0 {
            let ty = self.parse_expr(if min_bp == 0 { 0 } else { binding_pow::LABEL })?;

            self.graph.declare_variable(name, ty);
        }

        loop {
            let next_tok = self.peek();
            if next_tok.kind == TokenKind::Equal {
                self.advance();
                let rhs = self.parse_expr(binding_pow::WRITE_RIGHT)?;

                self.graph.assign_variable(name, rhs)?;
            } else if let Some(op) = next_tok.as_assign() {
                self.advance();
                let rhs = self.parse_expr(binding_pow::WRITE_RIGHT)?;
                let lhs = self.graph.read_variable(name)?;

                let new_val = self.graph.add_binary(op, lhs, rhs);
                self.graph.assign_variable(name, new_val)?;
            } else if let Some(op) = next_tok.as_inc_or_dec() {
                self.advance();
                let rhs = self.graph.add_literal(Literal::from(1_u8));
                let lhs = self.graph.read_variable(name)?;

                let new_val = self.graph.add_binary(op, lhs, rhs);
                self.graph.assign_variable(name, new_val)?
            } else {
                return Ok(());
            }
        }
    }

    fn advance(&mut self) -> Token<'src> {
        let tok = self.tokens.peek();
        self.tokens.consume();
        tok
    }

    fn peek(&mut self) -> Token<'src> {
        self.tokens.peek()
    }
}
