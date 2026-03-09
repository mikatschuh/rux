use crate::{
    grapher::{Graph, GraphError, GraphResult, MemNodeID, NodeID, NodeKind, Symbol},
    tokenizing::{
        binding_pow,
        num::Literal,
        token::{Bracket, Keyword, Token, TokenKind},
        TokenStream,
    },
};
use std::collections::HashMap;
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
            graph: Graph::new(),
        }
    }

    fn build(mut self) -> GraphResult<'src, Graph<'src>> {
        loop {
            let token = self.peek();
            if token.kind == TokenKind::EOF {
                return Ok(self.graph);
            }

            self.parse_statement()?;
        }
    }
}

impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    fn parse_statement(&mut self) -> GraphResult<'src, ()> {
        let token = self.peek();
        match token.kind {
            TokenKind::EOF => return Ok(()),
            TokenKind::Semicolon => {
                self.advance();
                Ok(())
            }
            TokenKind::Ident => {
                let name = self.advance();
                self.parse_variable_tail(name, 1)
            }
            TokenKind::Open(Bracket::Curly) => {
                let open_curly = self.advance();
                self.parse_block(open_curly)
            }
            TokenKind::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenKind::Keyword(Keyword::Loop) => self.parse_loop_statement(),
            _ => Err(GraphError::UnexpectedToken {
                expected: "statement",
                found: token,
            }),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> GraphResult<'src, NodeID<'src>> {
        let lhs = self.parse_primary(min_bp)?;
        self.parse_expr_tail(lhs, min_bp)
    }

    fn parse_expr_tail(
        &mut self,
        mut lhs: NodeID<'src>,
        min_bp: u8,
    ) -> GraphResult<'src, NodeID<'src>> {
        // connect tokens to lhs
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

    fn parse_primary(&mut self, min_bp: u8) -> GraphResult<'src, NodeID<'src>> {
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
            TokenKind::Keyword(Keyword::If) => self.parse_if_expression(),
            _ => match tok.as_prefix() {
                Some(op) => {
                    self.advance();
                    let node = self.parse_primary(op.binding_pow())?;
                    Ok(self.graph.add_unary(op, node))
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

        if min_bp <= binding_pow::STATEMENT && self.peek().binding_pow() == binding_pow::ALL {
            let ty = self.parse_expr(if min_bp == binding_pow::ALL {
                min_bp
            } else {
                binding_pow::LABEL
            })?;

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

    fn parse_if_expression(&mut self) -> GraphResult<'src, NodeID<'src>> {
        self.advance(); // consume 'if'
        let condition = self.parse_expr(binding_pow::EXPRESSION)?;
        let when_true = self.parse_expr(binding_pow::EXPRESSION)?;

        let else_token = self.peek();
        match else_token.kind {
            TokenKind::Keyword(Keyword::Else) => {
                self.advance();
            }
            _ => {
                return Err(GraphError::UnexpectedToken {
                    expected: "else",
                    found: else_token,
                })
            }
        }

        let when_false = self.parse_expr(binding_pow::EXPRESSION)?;

        Ok(self.graph.add_phi(condition, when_true, when_false))
    }

    fn parse_if_statement(&mut self) -> GraphResult<'src, ()> {
        self.advance(); // consume 'if'
        let condition = self.parse_expr(binding_pow::EXPRESSION)?;
        let before_symbols = self.graph.snapshot_symbols();
        let before_mem = self.graph.snapshot_mem();

        let then_block_open = self.expect_open_curly()?;
        self.parse_block(then_block_open)?;
        let when_true_symbols = self.graph.snapshot_symbols();
        let when_true_mem = self.graph.snapshot_mem();

        self.graph.replace_symbols(before_symbols.clone());
        self.graph.replace_mem(before_mem.clone());

        let (when_false_symbols, when_false_mem) =
            if matches!(self.peek().kind, TokenKind::Keyword(Keyword::Else)) {
                self.advance();
                let else_block_open = self.expect_open_curly()?;
                self.parse_block(else_block_open)?;
                (self.graph.snapshot_symbols(), self.graph.snapshot_mem())
            } else {
                (before_symbols.clone(), before_mem.clone())
            };

        self.merge_symbol_versions(
            condition.clone(),
            before_symbols,
            when_true_symbols,
            when_false_symbols,
        );
        self.merge_mem_versions(condition, when_true_mem, when_false_mem);
        Ok(())
    }

    fn parse_loop_statement(&mut self) -> GraphResult<'src, ()> {
        self.advance(); // consume 'loop'

        let condition = if self.peek().kind == TokenKind::Ident {
            let name = self.advance();
            if self.peek().kind == TokenKind::Open(Bracket::Curly) {
                self.graph.read_variable(name)? // there is already the body, we cannot parse any further
            } else {
                self.parse_variable_tail(name, binding_pow::STATEMENT)?;

                if self.peek().kind == TokenKind::Colon {
                    self.advance();
                    self.parse_expr(binding_pow::EXPRESSION)
                } else {
                    let lhs = self.graph.read_variable(name)?;
                    self.parse_expr_tail(lhs, binding_pow::EXPRESSION)
                }?
            }
        } else {
            self.parse_expr(binding_pow::EXPRESSION)?
        };

        let has_step_clause = if self.peek().kind == TokenKind::Colon {
            self.advance();
            true
        } else {
            false
        };

        let before_symbols = self.graph.snapshot_symbols();
        let before_mem = self.graph.snapshot_mem();
        let loop_head = self.graph.add_loop_head(before_mem.clone());
        self.graph.replace_mem(loop_head.clone());

        if has_step_clause {
            self.parse_statement()?;
        }

        let body_open = self.expect_open_curly()?;
        self.parse_block(body_open)?;

        let when_loop_symbols = self.graph.snapshot_symbols();
        let when_loop_mem = self.graph.snapshot_mem();
        self.graph
            .set_loop_backedge(loop_head.clone(), when_loop_mem);

        self.merge_symbol_versions(
            condition,
            before_symbols.clone(),
            when_loop_symbols,
            before_symbols,
        );
        self.graph.replace_mem(loop_head);
        Ok(())
    }

    fn expect_open_curly(&mut self) -> GraphResult<'src, Token<'src>> {
        let token = self.advance();
        match token.kind {
            TokenKind::Open(Bracket::Curly) => Ok(token),
            _ => Err(GraphError::UnexpectedToken {
                expected: "'{'",
                found: token,
            }),
        }
    }

    fn parse_block(&mut self, opener: Token<'src>) -> GraphResult<'src, ()> {
        loop {
            let token = self.peek();
            match token.kind {
                TokenKind::Closed(closed) if closed == Bracket::Curly => {
                    self.advance();
                    return Ok(());
                }
                TokenKind::Closed(_) | TokenKind::EOF => {
                    return Err(GraphError::MismatchedBracket {
                        opener,
                        closer: token,
                    })
                }
                _ => self.parse_statement()?,
            }
        }
    }

    fn merge_symbol_versions(
        &mut self,
        condition: NodeID<'src>,
        mut base: HashMap<&'src str, Symbol<'src>>,
        when_true: HashMap<&'src str, Symbol<'src>>,
        when_false: HashMap<&'src str, Symbol<'src>>,
    ) {
        for (name, symbol) in base.iter_mut() {
            let base_value = symbol.assignment.clone();
            if base_value.kind != NodeKind::UnInitialized {
                continue;
            }

            let true_value = when_true
                .get(name)
                .map(|symbol| symbol.assignment.clone())
                .unwrap_or_else(|| base_value.clone());
            let false_value = when_false
                .get(name)
                .map(|symbol| symbol.assignment.clone())
                .unwrap_or_else(|| base_value.clone());

            let new_value = if true_value.ptr_cmp(&false_value) {
                true_value
            } else {
                self.graph
                    .add_phi(condition.clone(), true_value, false_value)
            };

            symbol.assignment = new_value;
        }

        self.graph.replace_symbols(base);
    }

    fn merge_mem_versions(
        &mut self,
        condition: NodeID<'src>,
        when_true_mem: MemNodeID<'src>,
        when_false_mem: MemNodeID<'src>,
    ) {
        let true_mem = when_true_mem;
        let false_mem = when_false_mem;

        if true_mem.ptr_cmp(&false_mem) {
            self.graph.replace_mem(true_mem);
        } else {
            let merged = self.graph.add_mem_merge(condition, true_mem, false_mem);
            self.graph.replace_mem(merged);
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
