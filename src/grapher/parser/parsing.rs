use std::collections::HashMap;

use crate::{
    error::Span,
    grapher::{GraphError, GraphResult, IdentToken, graph::DataID, parser::GraphBuilder},
    literal_parsing::Literal,
    tokenizing::{
        TokenStream,
        token::{Bracket, Keyword, Token, TokenKind},
    },
};

impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn parse_file(&mut self) -> GraphResult<'src, ()> {
        while self.peek().kind != TokenKind::Eof {
            self.parse_statement()?;
        }
        Ok(())
    }

    fn parse_item(&mut self) -> GraphResult<'src, ()> {
        match self.peek().kind {
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::Let => {
                    /*
                        let const_ = self.advance();
                        self.parse_const(const_)
                    */

                    todo!()
                }
                Keyword::Fn => todo!(),
                Keyword::Enum => todo!(),
                Keyword::Struct => todo!(),

                _ => Err(GraphError::ExpectedItem { found: self.peek() }),
            },
            _ => Err(GraphError::ExpectedItem { found: self.peek() }),
        }
    }

    fn parse_statement(&mut self) -> GraphResult<'src, DataID<'src>> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.advance();
                Ok(self.graph.add_unit())
            }
            TokenKind::Name => {
                let name = self.expect_name().unwrap();
                self.parse_assignment(name)?;
                Ok(self.graph.add_unit())
            }
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::Fn => todo!(),
                Keyword::Enum => todo!(),
                Keyword::Struct => todo!(),

                Keyword::Let => {
                    let let_ = self.advance();
                    self.parse_decl(let_.span, false)?;
                    Ok(self.graph.add_unit())
                }
                Keyword::Var => {
                    let var = self.advance();
                    self.parse_decl(var.span, true)?;
                    Ok(self.graph.add_unit())
                }

                Keyword::Continue => todo!(),
                Keyword::Break => todo!(),

                _ => self.parse_expr(0),
            },

            _ => self.parse_expr(0),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> GraphResult<'src, DataID<'src>> {
        let lhs = self.parse_primary()?;
        self.parse_expr_tail(lhs, min_bp)
    }

    fn parse_primary(&mut self) -> GraphResult<'src, DataID<'src>> {
        match self.peek().kind {
            TokenKind::Type => {
                let type_ = self.tokens.get_type();
                self.advance();
                Ok(self.graph.add_type(type_))
            }
            TokenKind::Literal => {
                let literal = self.tokens.get_literal();
                self.advance();
                Ok(self.graph.add_literal(literal))
            }
            TokenKind::Quote { .. } => {
                let quote = self.tokens.get_quote();
                self.advance();
                Ok(self.graph.add_quote(quote))
            }
            TokenKind::Boolean(boolean) => {
                self.advance();
                Ok(self.graph.add_bool(boolean))
            }
            TokenKind::Name => {
                let name = self.expect_name()?;
                Ok(self.read_variable(name))
            }
            TokenKind::Open(Bracket::Curly) => {
                let opener = self.advance();
                self.parse_block(opener)
            }
            TokenKind::Open(open) => {
                self.advance();
                if self.peek().kind == TokenKind::Open(open) {
                    self.advance();
                    return Ok(self.graph.add_unit());
                }

                let expr = self.parse_expr(0)?;
                let closer = self.advance();
                match closer.kind {
                    TokenKind::Closed(closed) if closed == open => Ok(expr),
                    _ => Err(GraphError::MismatchedBracket {
                        opener: self.peek(),
                        closer,
                    }),
                }
            }
            TokenKind::Keyword(Keyword::If) => {
                let if_ = self.advance();
                self.parse_if(if_)
            }
            TokenKind::Keyword(Keyword::Loop) => {
                let loop_ = self.advance();
                self.parse_loop(loop_)
            }
            _ => match self.peek().as_prefix() {
                Some(op) => {
                    self.advance();
                    let node = self.parse_expr(op.binding_pow())?;
                    Ok(self.graph.add_unary(op, node))
                }
                None => {
                    self.advance();
                    Err(GraphError::ExpectedExpression { found: self.peek() })
                }
            },
        }
    }

    fn parse_expr_tail(
        &mut self,
        mut lhs: DataID<'src>,
        min_bp: u8,
    ) -> GraphResult<'src, DataID<'src>> {
        loop {
            if self.peek().binding_pow() < min_bp {
                return Ok(lhs);
            }

            if let Some(op) = self.peek().as_infix() {
                self.advance();
                let rhs = self.parse_expr(op.binding_pow())?;

                lhs = self.graph.add_binary(op, lhs, rhs);
            } else if let Some(op) = self.peek().as_postfix() {
                self.advance();
                lhs = self.graph.add_unary(op, lhs)
            } else {
                return Ok(lhs);
            }
        }
    }

    fn parse_block(&mut self, _opener: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        self.symbols.open_scope();
        if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
            self.advance();
            self.symbols.close_scope();
            return Ok(self.graph.add_unit());
        }
        loop {
            let value = self.parse_statement()?;
            if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
                self.advance();
                self.symbols.close_scope();
                return Ok(value);
            }
        }
    }

    fn parse_decl(&mut self, _binding: Span, mutable: bool) -> GraphResult<'src, ()> {
        let name = self.expect_name()?.src;

        if self.peek().kind == TokenKind::Equal {
            // type inference
            todo!()
        } else {
            // no type inference
            let type_ = self.parse_expr(0)?;

            if self.peek().kind == TokenKind::Equal {
                self.advance();
                let value = self.parse_expr(0)?;

                self.declare_variable(name, type_, value, mutable);
                Ok(())
            } else {
                Err(GraphError::ExpectedAssignment { found: self.peek() })
            }
        }
    }

    /*fn parse_const(&mut self, const_: Token<'src>) -> GraphResult<'src, ()> {
        let name = self.expect_name()?.src;

        if self.peek().kind == TokenKind::Equal {
            // type inference
            todo!()
        } else {
            // no type inference
            let type_ = self.parse_expr(0)?;

            if self.peek().kind == TokenKind::Equal {
                self.advance();
                let value = self.parse_expr(0)?;

                self.declare_const(const_.span - self.last_pos(), name, type_, value)
            } else {
                Err(GraphError::ExpectedAssignment { found: self.peek() })
            }
        }
    }*/

    fn parse_assignment(&mut self, name: IdentToken<'src>) -> GraphResult<'src, DataID<'src>> {
        loop {
            if self.peek().kind == TokenKind::Equal {
                let span = self.advance().span;
                let value = self.parse_expr(0)?;
                self.symbols
                    .write_symbol(span - self.tokens.last_pos(), name.src, value)?;
            } else if let Some(op) = self.peek().as_assign() {
                let span = self.advance().span;
                let rhs = self.parse_expr(0)?;
                let lhs = self.read_variable(name);
                let value = self.graph.add_binary(op, lhs, rhs);

                self.symbols
                    .write_symbol(span - self.tokens.last_pos(), name.src, value)?;
            } else if let Some(op) = self.peek().as_inc_or_dec() {
                let span = self.advance().span;
                let lhs = self.read_variable(name);
                let rhs = self.graph.add_literal(Literal::from(1_u8));
                let value = self.graph.add_binary(op, lhs, rhs);

                self.symbols.write_symbol(span, name.src, value)?;
            } else {
                return Ok(self.read_variable(name));
            }
        }
    }

    fn parse_if(&mut self, _if: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        let condition = self.parse_expr(0)?;
        let (false_branch, true_branch) = self.graph.add_branch(condition.clone());

        self.graph.current_mem = true_branch;

        self.symbols.open_branch();
        let when_true = self.parse_expr(0)?;
        let overwrites_when_true = self.symbols.close_branch();
        let true_branch = self.graph.current_mem();

        if self.peek().kind == TokenKind::Keyword(Keyword::Else) {
            self.advance();

            self.graph.current_mem = false_branch;

            self.symbols.open_branch();
            let when_false = self.parse_expr(0)?;
            let overwrites_when_false = self.symbols.close_branch();
            let false_branch = self.graph.current_mem();

            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.graph.current_mem = self.graph.add_ctrl_merge(merge.clone());

            self.merge_overwrites(merge.clone(), overwrites_when_true, overwrites_when_false)?;

            Ok(self.graph.add_phi(merge, when_true, when_false))
        } else {
            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.graph.current_mem = self.graph.add_ctrl_merge(merge.clone());

            self.merge_overwrites(merge.clone(), overwrites_when_true, HashMap::new())?;

            let unit = self.graph.add_unit();
            Ok(self.graph.add_phi(merge, when_true, unit))
        }
    }

    fn parse_loop(&mut self, _loop: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        let condition = self.parse_expr(0)?;

        let _before_loop = self.graph.current_mem();
        let mut loop_head = self.graph.add_loop_head(condition);
        self.graph.current_mem = loop_head.clone();

        self.symbols.open_branch();
        let _body = self.parse_expr(0);
        let _overwrites = self.symbols.close_branch();

        loop_head.set_backedge(self.graph.current_mem());

        Ok(self.graph.add_unit())
    }
}
