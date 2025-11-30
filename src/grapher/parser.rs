use crate::{
    grapher::{Graph, GraphError, GraphResult, NodeId},
    tokenizing::{
        binding_pow,
        num::Literal,
        token::{Token, TokenKind},
        unary_op::UnaryOp,
        TokenStream,
    },
};
pub trait Parser<'tokens, 'src, T: TokenStream<'src>> {
    fn new(tokens: &'tokens mut T) -> Self;
    fn build(self) -> GraphResult<'src, Graph<'src>>;
}

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    graph: Graph<'src>,
}

impl<'tokens, 'src, T: TokenStream<'src>> Parser<'tokens, 'src, T>
    for GraphBuilder<'tokens, 'src, T>
{
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
}

impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
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
