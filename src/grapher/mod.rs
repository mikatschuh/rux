use crate::{
    tokenizing::{
        binary_op::BinaryOp,
        binding_pow,
        num::Literal,
        token::{Token, TokenKind},
        unary_op::UnaryOp,
        TokenStream,
    },
    utilities::{MocAllocator, Rc},
};
use bumpalo::Bump;
use num::ToPrimitive;
use std::{collections::HashMap, fmt};

pub mod intern;
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
    Constant {
        literal: &'src str,
        value: i64,
    },
    Quote {
        quote: String,
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
    VariableDecl {
        name: &'src str,
        ty: NodeId<'src>,
    },
    Assign {
        name: &'src str,
        value: NodeId<'src>,
    },
    Read {
        name: &'src str,
        source: NodeId<'src>,
    },
}

#[derive(Debug)]
pub struct Symbol<'src> {
    pub ty: NodeId<'src>,
    pub decl: NodeId<'src>,
    pub last_value: NodeId<'src>,
    pub version: usize,
}

#[derive(Debug, Default)]
pub struct Graph<'src> {
    arena: Bump,
    symbols: HashMap<&'src str, Vec<Symbol<'src>>>,
}

impl<'src> Graph<'src> {
    pub fn from_stream(tokens: &mut impl TokenStream<'src>) -> GraphResult<'src, Graph<'src>> {
        GraphBuilder::new(tokens).build()
    }

    pub fn symbol(&self, name: &str) -> Option<&Symbol<'src>> {
        self.symbols.get(name).and_then(|stack| stack.last())
    }

    #[allow(dead_code)]
    pub fn symbol_versions(&self, name: &str) -> Option<&[Symbol<'src>]> {
        self.symbols.get(name).map(|stack| stack.as_slice())
    }

    #[allow(dead_code)]
    pub fn symbols(&self) -> impl Iterator<Item = &Symbol<'src>> {
        self.symbols.values().flat_map(|stack| stack.iter())
    }

    fn push_node(&mut self, kind: NodeKind<'src>) -> NodeId<'src> {
        let node: Node<'src> = Node { kind };
        Rc::<Node<'src>, MocAllocator>::new_in_bump(node, &self.arena)
    }

    fn add_constant(&mut self, literal: &'src str, value: i64) -> NodeId<'src> {
        self.push_node(NodeKind::Constant { literal, value })
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

    fn declare_variable(&mut self, name: Token<'src>, ty: NodeId<'src>) -> NodeId<'src> {
        let decl = self.push_node(NodeKind::VariableDecl {
            name: name.src,
            ty: ty.clone(),
        });
        let stack = self.symbols.entry(name.src).or_default();
        let version = stack.len();
        stack.push(Symbol {
            ty,
            decl: decl.clone(),
            last_value: decl.clone(),
            version,
        });
        decl
    }

    fn assign_variable(&mut self, name: Token<'src>, value: NodeId<'src>) -> GraphResult<'src, ()> {
        let has_symbol = matches!(
            self.symbols.get(name.src),
            Some(stack) if !stack.is_empty()
        );
        if !has_symbol {
            return Err(GraphError::UnknownVariable { token: name });
        }

        let assignment = self.push_node(NodeKind::Assign {
            name: name.src,
            value,
        });
        if let Some(symbol) = self
            .symbols
            .get_mut(name.src)
            .and_then(|stack| stack.last_mut())
        {
            symbol.last_value = assignment.clone();
        }
        Ok(())
    }

    fn read_variable(&mut self, ident: Token<'src>) -> GraphResult<'src, NodeId<'src>> {
        let Some(stack) = self.symbols.get(ident.src) else {
            return Err(GraphError::UnknownVariable { token: ident });
        };
        let Some(symbol) = stack.last() else {
            return Err(GraphError::UnknownVariable { token: ident });
        };
        Ok(self.push_node(NodeKind::Read {
            name: ident.src,
            source: symbol.last_value.clone(),
        }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphError<'src> {
    UnexpectedToken {
        expected: &'static str,
        found: Token<'src>,
    },
    InvalidLiteral {
        token: Token<'src>,
    },
    UnknownVariable {
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
            InvalidLiteral { token } => {
                write!(f, "invalid literal '{}' at {:?}", token.src, token.span)
            }
            UnknownVariable { token } => {
                write!(f, "unknown variable '{}' at {:?}", token.src, token.span)
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
            }
        }
    }

    fn parse_primary(&mut self, min_bp: u8) -> GraphResult<'src, NodeId<'src>> {
        let tok = self.peek();
        match tok.kind {
            TokenKind::Literal => {
                let literal = self.tokens.get_literal();
                self.advance();
                self.emit_literal(tok, literal)
            }
            TokenKind::Quote => {
                let quote = self.tokens.get_quote();
                self.advance();
                Ok(self.graph.add_quote(quote))
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
                self.graph.assign_variable(name, new_val);
            }

            return Ok(());
        }
    }

    fn emit_literal(
        &mut self,
        token: Token<'src>,
        literal: Literal<'src>,
    ) -> GraphResult<'src, NodeId<'src>> {
        if literal.num_digits_after_dot != 0
            || literal.exponent.is_some()
            || !literal.suffix.is_empty()
        {
            return Err(GraphError::InvalidLiteral { token });
        }
        let value_u64 = literal
            .digits
            .to_u64()
            .ok_or(GraphError::InvalidLiteral { token })?;
        if value_u64 > i64::MAX as u64 {
            return Err(GraphError::InvalidLiteral { token });
        }
        let value = value_u64 as i64;
        Ok(self.graph.add_constant(token.src, value))
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
