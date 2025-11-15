use crate::{
    tokenizing::{
        num::Literal,
        token::{Token, TokenKind},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeKind<'src> {
    Constant {
        literal: &'src str,
        value: i64,
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
        ty: &'src str,
        initializer: Option<NodeId<'src>>,
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

#[allow(dead_code)]
#[derive(Debug)]
pub struct Symbol<'src> {
    pub name: &'src str,
    pub ty: &'src str,
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

    fn declare_variable(
        &mut self,
        name: Token<'src>,
        ty: Token<'src>,
        initializer: Option<NodeId<'src>>,
    ) -> NodeId<'src> {
        let decl = self.push_node(NodeKind::VariableDecl {
            name: name.src,
            ty: ty.src,
            initializer: initializer.clone(),
        });
        let last_value = initializer.unwrap_or(decl.clone());

        let stack = self.symbols.entry(name.src).or_default();
        let version = stack.len();
        stack.push(Symbol {
            name: name.src,
            ty: ty.src,
            decl: decl.clone(),
            last_value,
            version,
        });
        decl
    }

    fn assign_variable(
        &mut self,
        name: Token<'src>,
        value: NodeId<'src>,
    ) -> GraphResult<'src, NodeId<'src>> {
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
        Ok(assignment)
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
                TokenKind::EOF => break,
                TokenKind::Semicolon => {
                    _ = self.advance();
                }
                TokenKind::Ident => self.parse_statement()?,
                _ => {
                    return Err(GraphError::UnexpectedToken {
                        expected: "identifier",
                        found: token,
                    })
                }
            }
        }
        Ok(self.graph)
    }

    fn parse_statement(&mut self) -> GraphResult<'src, ()> {
        let name = self.advance();
        match self.peek().kind {
            TokenKind::Equal => {
                self.advance();
                let value = self.parse_expression()?;
                self.graph.assign_variable(name, value)?;
            }
            TokenKind::Ident | TokenKind::Keyword(_) => {
                let ty = self.advance();
                let initializer = if matches!(self.peek().kind, TokenKind::Equal) {
                    self.advance();
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                self.graph.declare_variable(name, ty, initializer);
            }
            _ => {
                return Err(GraphError::UnexpectedToken {
                    expected: "type name or '='",
                    found: self.peek(),
                })
            }
        }
        Ok(())
    }

    fn parse_expression(&mut self) -> GraphResult<'src, NodeId<'src>> {
        let lhs = self.parse_term()?;
        self.parse_additive_rhs(lhs)
    }

    fn parse_additive_rhs(&mut self, mut lhs: NodeId<'src>) -> GraphResult<'src, NodeId<'src>> {
        loop {
            lhs = match self.peek().kind {
                TokenKind::Plus => {
                    self.advance();
                    let rhs = self.parse_term()?;
                    self.graph.add_binary(BinaryOp::Add, lhs, rhs)
                }
                TokenKind::Dash => {
                    self.advance();
                    let rhs = self.parse_term()?;
                    self.graph.add_binary(BinaryOp::Sub, lhs, rhs)
                }
                _ => break,
            };
        }
        Ok(lhs)
    }

    fn parse_term(&mut self) -> GraphResult<'src, NodeId<'src>> {
        let mut node = self.parse_unary()?;
        loop {
            node = match self.peek().kind {
                TokenKind::Star => {
                    self.advance();
                    let rhs = self.parse_unary()?;
                    self.graph.add_binary(BinaryOp::Mul, node, rhs)
                }
                TokenKind::Slash => {
                    self.advance();
                    let rhs = self.parse_unary()?;
                    self.graph.add_binary(BinaryOp::Div, node, rhs)
                }
                _ => break,
            };
        }
        Ok(node)
    }

    fn parse_unary(&mut self) -> GraphResult<'src, NodeId<'src>> {
        match self.peek().kind {
            TokenKind::Plus => {
                self.advance();
                self.parse_unary()
            }
            TokenKind::Dash => {
                self.advance();
                let node = self.parse_unary()?;
                Ok(self.graph.add_unary(UnaryOp::Neg, node))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> GraphResult<'src, NodeId<'src>> {
        match self.peek().kind {
            TokenKind::Literal => {
                let token = self.peek();
                let literal = self.tokens.get_literal();
                self.tokens.consume();
                self.emit_literal(token, literal)
            }
            _ => {
                let token = self.advance();
                match token.kind {
                    TokenKind::Ident => self.graph.read_variable(token),
                    TokenKind::Open(open) => {
                        let expr = self.parse_expression()?;
                        let closer = self.advance();
                        match closer.kind {
                            TokenKind::Closed(closed) if closed == open => Ok(expr),
                            _ => Err(GraphError::MismatchedBracket {
                                opener: token,
                                closer,
                            }),
                        }
                    }
                    _ => Err(GraphError::ExpectedExpression { found: token }),
                }
            }
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
