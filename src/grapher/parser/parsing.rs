use crate::{
    error::Span,
    grapher::{GraphError, GraphResult, IdentToken, graph::DataID, parser::GraphBuilder},
    literal_parsing::Literal,
    tokenizing::{
        TokenStream,
        token::{Bracket, Token, TokenKind},
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

    fn parse_statement(&mut self) -> GraphResult<'src, DataID<'src>> {
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

            TokenKind::Ident => {
                let name = self.expect_name().unwrap();
                self.parse_assignment(name)
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
                if self.peek().kind == TokenKind::Colon {
                    let colon = self.advance();
                    self.parse_label(name, colon)
                } else {
                    Ok(self.read_variable(name))
                }
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

    fn parse_assignment(&mut self, name: IdentToken<'src>) -> GraphResult<'src, DataID<'src>> {
        if self.peek().kind == TokenKind::Colon {
            let colon = self.advance();
            return self.parse_label(name, colon);
        }
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
        let before_branch = self.graph.get_ctrl()?;
        let (false_branch, true_branch) = self
            .graph
            .add_branch(before_branch.clone(), condition.clone())?;

        let state_before_branch = self.symbols.snapshot_state();

        self.graph.set_ctrl(true_branch);
        let when_true = self.parse_expr(0)?;

        if self.graph.is_unreachable() {
            self.graph.set_ctrl(false_branch);
            return if self.peek().kind == TokenKind::Else {
                self.advance();

                let when_false = self.parse_expr(0)?;
                if self.graph.is_unreachable() {
                    return Ok(self.graph.add_never());
                }

                Ok(when_false)
            } else {
                self.go_back_to_state(state_before_branch);
                Ok(self.graph.add_unit())
            };
        }

        let true_branch = self.graph.get_ctrl().unwrap();
        let state_when_true = self.symbols.snapshot_state();

        if self.peek().kind == TokenKind::Else {
            self.advance();

            self.graph.set_ctrl(false_branch);
            let when_false = self.parse_expr(0)?;
            if self.graph.is_unreachable() {
                self.graph.set_ctrl(true_branch);
                self.go_back_to_state(state_when_true);
                return Ok(when_true);
            }

            let false_branch = self.graph.get_ctrl().unwrap();
            let state_when_false = self.symbols.snapshot_state();

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
        self.jumps.open_branch(self.symbols.open_scope_id());
        self.labels.insert(name.src, self.jumps.open_branch_id());

        let value = self.parse_expr(0)?; // parse the hole body
        self.labels.remove(name.src);
        self.close_loop(Some(value), loop_head, loop_phis)
    }

    fn parse_continue(&mut self, keyword: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        if self.peek().kind == TokenKind::Colon {
            self.advance();
            let name = self.expect_name()?;
            let Some(branch) = self.labels.get(name.src).cloned() else {
                return Err(GraphError::UnknownLabel { label: name });
            };
            let scope = self.jumps.scope_of(branch);
            self.jumps.add_continue_to(
                branch,
                self.graph.get_ctrl()?,
                self.symbols.snapshot_state_of_scope(scope),
            );
            self.graph.make_unreachable();
            return Ok(self.graph.add_never());
        }

        if let Some(current_loop) = self.jumps.scope() {
            self.jumps.add_continue(
                self.graph.get_ctrl()?,
                self.symbols.snapshot_state_of_scope(current_loop),
            );
            self.graph.make_unreachable();
            Ok(self.graph.add_never())
        } else {
            Err(GraphError::JumpOutsideLoop { keyword })
        }
    }

    fn parse_break(&mut self, keyword: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        if self.peek().kind == TokenKind::Colon {
            self.advance();
            let name = self.expect_name()?;
            let Some(branch) = self.labels.get(name.src).cloned() else {
                return Err(GraphError::UnknownLabel { label: name });
            };

            let value = self
                .parse_optional_expr(0)?
                .unwrap_or(self.graph.add_unit());

            let scope = self.jumps.scope_of(branch);
            self.jumps.add_break_to(
                branch,
                self.graph.get_ctrl()?,
                self.symbols.snapshot_state_of_scope(scope),
                value,
            );
            self.graph.make_unreachable();
            return Ok(self.graph.add_never());
        }

        if let Some(current_loop) = self.jumps.scope() {
            let value = self
                .parse_optional_expr(0)?
                .unwrap_or(self.graph.add_unit());

            self.jumps.add_break(
                self.graph.get_ctrl()?,
                self.symbols.snapshot_state_of_scope(current_loop),
                value,
            );
            self.graph.make_unreachable();
            Ok(self.graph.add_never())
        } else {
            Err(GraphError::JumpOutsideLoop { keyword })
        }
    }

    fn parse_loop(&mut self, _loop: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        // let condition = self.parse_expr(0)?;

        let entry = self.graph.get_ctrl()?;
        let (loop_head, loop_phis) = self.set_up_loop_merge(entry);
        self.jumps.open_branch(self.symbols.open_scope_id()); // jump branches

        let _ = self.parse_expr(0)?; // parse the hole body

        self.close_loop(None, loop_head, loop_phis)
    }
}
