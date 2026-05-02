use crate::{
    error::Span,
    grapher::{GraphError, GraphResult, IdentToken, builder::GraphBuilder, graph::DataID},
    literal_parsing::Literal,
    tokenizing::{
        TokenStream,
        token::{Bracket, Token, TokenKind},
    },
};

impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn parse_file(&mut self) -> GraphResult<'src, ()> {
        while self.peek().kind != TokenKind::Eof {
            self.parse_decl_or_statement()?;
        }
        Ok(())
    }

    #[allow(unused)]
    fn parse_item(&mut self) -> GraphResult<'src, ()> {
        match self.peek().kind {
            TokenKind::Let => {
                /*
                    let const_ = self.advance();
                    self.parse_const(const_)
                */

                todo!()
            }
            TokenKind::Fn => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Struct => todo!(),

            _ => Err(GraphError::ExpectedItem { found: self.peek() }),
        }
    }

    fn parse_decl_or_statement(&mut self) -> GraphResult<'src, DataID<'src>> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.advance();
                Ok(self.graph.add_unit())
            }

            TokenKind::Fn => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Struct => todo!(),

            TokenKind::Let => {
                let let_keyword = self.advance();
                self.parse_decl(let_keyword.span, false)?;
                Ok(self.graph.add_unit())
            }
            TokenKind::Var => {
                let var_keyword = self.advance();
                self.parse_decl(var_keyword.span, true)?;
                Ok(self.graph.add_unit())
            }

            _ => self.parse_stmt_expr(),
        }
    }

    fn parse_stmt_expr(&mut self) -> GraphResult<'src, DataID<'src>> {
        match self.peek().kind {
            TokenKind::Ident => {
                let name = self.expect_name().unwrap();
                self.parse_name_pattern(name)
            }
            _ => self.parse_expr(0),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> GraphResult<'src, DataID<'src>> {
        let lhs = self.parse_primary()?;
        self.parse_expr_tail(lhs, min_bp)
    }

    fn parse_optional_expr(&mut self, min_bp: u8) -> GraphResult<'src, Option<DataID<'src>>> {
        let lhs = match self.parse_primary() {
            Ok(lhs) => lhs,
            Err(e) => match e {
                GraphError::ExpectedExpression { .. } => return Ok(None),
                _ => return Err(e),
            },
        };
        self.parse_expr_tail(lhs, min_bp).map(Some)
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
            TokenKind::Ident => {
                let name = self.expect_name()?;
                Ok(self.read_variable(name))
            }
            TokenKind::Open(Bracket::Curly) => {
                let opener = self.advance();
                self.parse_block(opener)
            }
            TokenKind::Open(open) => {
                let opener = self.advance();
                if self.peek().kind == TokenKind::Closed(open) {
                    self.advance();
                    return Ok(self.graph.add_unit());
                }

                let expr = self.parse_expr(0)?;
                let closer = self.advance();
                match closer.kind {
                    TokenKind::Closed(closed) if closed == open => Ok(expr),
                    _ => Err(GraphError::MismatchedBracket { opener, closer }),
                }
            }
            TokenKind::If => {
                let if_ = self.advance();
                self.parse_if(if_)
            }
            TokenKind::Loop => {
                let loop_ = self.advance();
                self.parse_loop(loop_)
            }
            TokenKind::Continue => {
                let keyword = self.advance();
                self.parse_continue(keyword)
            }
            TokenKind::Break => {
                let keyword = self.advance();
                self.parse_break(keyword)
            }
            TokenKind::Unreachable => {
                self.advance();
                self.graph.make_unreachable();
                Ok(self.graph.add_never())
            }

            _ => match self.peek().as_prefix() {
                Some(op) => {
                    self.advance();
                    let node = self.parse_expr(op.binding_pow())?;
                    Ok(self.graph.add_unary(op, node))
                }
                None => Err(GraphError::ExpectedExpression { found: self.peek() }),
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
        let open_scope = self.symbol_table.open_scope();
        if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
            self.advance();
            self.symbol_table.close_scope(open_scope);
            return Ok(self.graph.add_unit());
        }
        loop {
            let value = self.parse_decl_or_statement()?;
            if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
                self.advance();
                self.symbol_table.close_scope(open_scope);
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

    fn parse_name_pattern(&mut self, name: IdentToken<'src>) -> GraphResult<'src, DataID<'src>> {
        // assignments:
        if self.peek().kind == TokenKind::Equal {
            let span = self.advance().span;
            let value = self.parse_expr(0)?;
            self.symbol_table
                .write_symbol(span - self.tokens.last_pos(), name.src, value)?;
            return Ok(self.graph.add_unit());
        } else if let Some(op) = self.peek().as_assign() {
            let span = self.advance().span;
            let rhs = self.parse_expr(0)?;
            let lhs = self.read_variable(name);
            let value = self.graph.add_binary(op, lhs, rhs);

            self.symbol_table
                .write_symbol(span - self.tokens.last_pos(), name.src, value)?;
            return Ok(self.graph.add_unit());
        } else if let Some(op) = self.peek().as_inc_or_dec() {
            let span = self.advance().span;
            let lhs = self.read_variable(name);
            let rhs = self.graph.add_literal(Literal::from(1));
            let value = self.graph.add_binary(op, lhs, rhs);

            self.symbol_table.write_symbol(span, name.src, value)?;
            return Ok(self.graph.add_unit());
        }

        // if we actually didnt have a pattern here
        // labels: `name: blk`
        if self.peek().kind == TokenKind::Colon {
            let colon = self.advance();
            return self.parse_label(name, colon);
        }

        let lhs = self.read_variable(name);
        self.parse_expr_tail(lhs, 0)
    }

    fn parse_if(&mut self, _if: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        let condition = self.parse_expr(0)?;

        let (state_before_branch, before_branch) = self.current_state()?;
        let (false_branch, true_branch) = self
            .graph
            .add_branch(before_branch.clone(), condition.clone())?;

        self.graph.set_ctrl(true_branch);
        let when_true = self.parse_stmt_expr()?;

        let Ok((state_when_true, true_branch)) = self.current_state() else {
            self.restore_state(state_before_branch);
            self.graph.set_ctrl(false_branch);
            return if self.peek().kind == TokenKind::Else {
                self.advance();

                let when_false = self.parse_stmt_expr()?;
                if self.graph.is_unreachable() {
                    return Ok(self.graph.add_never());
                }

                Ok(when_false)
            } else {
                Ok(self.graph.add_unit())
            };
        };

        if self.peek().kind == TokenKind::Else {
            self.advance();

            self.restore_state(state_before_branch);
            self.graph.set_ctrl(false_branch);
            let when_false = self.parse_stmt_expr()?;

            let Ok((state_when_false, false_branch)) = self.current_state() else {
                self.restore_state(state_when_true);
                self.graph.set_ctrl(true_branch);
                return Ok(when_true);
            };

            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.merge_states(merge.clone(), vec![state_when_false, state_when_true]);
            let phi = self
                .graph
                .add_phi(merge.clone(), vec![when_false, when_true]);

            self.graph.add_merge_to_ctrl(merge);
            Ok(self.graph.add_data_phi(phi))
        } else {
            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.merge_states(merge.clone(), vec![state_before_branch, state_when_true]);

            self.graph.add_merge_to_ctrl(merge);
            Ok(self.graph.add_unit())
        }
    }

    fn parse_label(
        &mut self,
        name: IdentToken<'src>,
        _colon: Token<'src>,
    ) -> GraphResult<'src, DataID<'src>> {
        let entry = self.graph.get_ctrl()?;
        let (loop_head, loop_phis) = self.set_up_loop_merge(entry);
        let branch = self.open_branch();
        self.labels.insert(name.src, branch);

        let value = self.parse_stmt_expr()?; // parse the hole body
        self.labels.remove(name.src);
        self.close_loop(Some(value), loop_head, loop_phis)
    }

    fn parse_continue(&mut self, keyword: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        if self.peek().kind == TokenKind::Colon {
            self.advance();
            let name = self.expect_name()?;
            return self.add_continue_to(keyword, name);
        }
        self.add_continue(keyword)
    }

    fn parse_break(&mut self, keyword: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        if self.peek().kind == TokenKind::Colon {
            self.advance();
            let name = self.expect_name()?;
            let value = self
                .parse_optional_expr(0)?
                .unwrap_or(self.graph.add_unit());
            return self.add_break_to(keyword, name, value);
        }

        let value = self
            .parse_optional_expr(0)?
            .unwrap_or(self.graph.add_unit());
        self.add_break(keyword, value)
    }

    fn parse_loop(&mut self, _loop: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        // let condition = self.parse_expr(0)?;

        let entry = self.graph.get_ctrl()?;
        let (loop_head, loop_phis) = self.set_up_loop_merge(entry);
        self.open_branch(); // jump branches

        let _ = self.parse_stmt_expr()?; // parse the hole body

        self.close_loop(None, loop_head, loop_phis)
    }
}
