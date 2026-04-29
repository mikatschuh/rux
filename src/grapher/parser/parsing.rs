use crate::{
    error::Span,
    grapher::{
        GraphError, GraphResult, IdentToken,
        graph::DataID,
        parser::{BreakJump, GraphBuilder},
    },
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
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::If => {
                    let if_ = self.advance();
                    self.parse_if(if_)
                }
                Keyword::Loop => {
                    let loop_ = self.advance();
                    self.parse_loop(loop_)
                }
                Keyword::Continue => {
                    let keyword = self.advance().span;
                    self.parse_continue(keyword).map(|_| self.graph.add_never())
                }
                Keyword::Break => {
                    let keyword = self.advance().span;
                    self.parse_break(keyword).map(|_| self.graph.add_never())
                }
                _ => Err(GraphError::ExpectedExpression { found: self.peek() }),
            },
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

    fn parse_continue(&mut self, _keyword: Span) -> GraphResult<'src, ()> {
        if let Some(current_loop) = self.loops.last().cloned() {
            self.continue_jumps.add_jump(
                self.graph.current_ctrl(),
                self.symbols.snapshot_state_of_scope(current_loop),
            );
            Ok(())
        } else {
            Err(GraphError::JumpOutsideLoop {
                keyword: self.peek(),
            })
        }
    }

    fn parse_break(&mut self, _keyword: Span) -> GraphResult<'src, ()> {
        if let Some(current_loop) = self.loops.last().cloned() {
            let value = self.parse_expr(0).or_else(|e| match e {
                GraphError::ExpectedExpression { .. } => Ok(self.graph.add_unit()),
                _ => Err(e),
            })?;
            self.break_jumps.add_jump(
                self.graph.current_ctrl(),
                BreakJump {
                    state: self.symbols.snapshot_state_of_scope(current_loop),
                    value,
                },
            );
            Ok(())
        } else {
            Err(GraphError::JumpOutsideLoop {
                keyword: self.peek(),
            })
        }
    }

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

        self.graph.current_ctrl = true_branch;

        let state_before_branch = self.symbols.snapshot_state();

        let when_true = self.parse_expr(0)?;
        let true_branch = self.graph.current_ctrl();
        let state_when_true = self.symbols.snapshot_state();

        if self.peek().kind == TokenKind::Keyword(Keyword::Else) {
            self.advance();

            self.graph.current_ctrl = false_branch;

            let when_false = self.parse_expr(0)?;
            let false_branch = self.graph.current_ctrl();
            let state_when_false = self.symbols.snapshot_state();

            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.graph.current_ctrl = self.graph.add_ctrl_merge(merge.clone());

            self.merge_states(merge.clone(), vec![state_when_true, state_when_false]);

            let phi = self.graph.add_phi(merge, vec![when_false, when_true]);
            Ok(self.graph.add_data_phi(phi))
        } else {
            let merge = self.graph.add_merge(vec![false_branch, true_branch]);
            self.graph.current_ctrl = self.graph.add_ctrl_merge(merge.clone());

            self.merge_states(merge.clone(), vec![state_when_true, state_before_branch]);

            let unit = self.graph.add_unit();
            let phi = self.graph.add_phi(merge, vec![unit, when_true]);
            Ok(self.graph.add_data_phi(phi))
        }
    }

    fn parse_loop(&mut self, _loop: Token<'src>) -> GraphResult<'src, DataID<'src>> {
        // let condition = self.parse_expr(0)?;

        // ctrl node structure setup
        let entry = self.graph.current_ctrl();
        let mut loop_head = self.graph.add_merge(vec![entry]);
        self.graph.current_ctrl = self.graph.add_ctrl_merge(loop_head.clone()); // with this the body has it as control flow dependency

        self.loops.push(self.symbols.open_scope_id()); // this is a marker that marks the loop scope
        self.continue_jumps.open_branch(); // jump branches
        self.break_jumps.open_branch();

        let mut loop_phis = vec![];
        self.symbols.for_every_mutable(|_, data| {
            let phi = self.graph.add_phi(loop_head.clone(), vec![data.clone()]);
            *data = self.graph.add_phi_no_dedup(phi.clone());
            loop_phis.push(phi);
        }); // replace every variable with a phi and keep track of the phi's

        let _ = self.parse_expr(0)?; // parse the hole body
        let body_end = self.graph.current_ctrl();
        let regular_backedge_state = self.symbols.snapshot_state(); // the state of every symbol after the hole body

        let (mut continue_points, mut continue_jumps_states) = self.continue_jumps.close_branch();
        continue_points.push(body_end); // add the regular backedge
        continue_jumps_states.push(regular_backedge_state); // add the regular backedge state

        let (mut break_points, break_jump_data) = self.break_jumps.close_branch();
        let mut break_values: Vec<DataID<'src>> =
            break_jump_data.iter().map(|j| j.value.clone()).collect();
        let mut break_jump_states: Vec<Vec<DataID<'src>>> =
            break_jump_data.into_iter().map(|j| j.state).collect();

        loop_head.branches.append(&mut continue_points); // add the continuation points as backedges
        continue_jumps_states
            .into_iter()
            .flat_map(|overwrites| overwrites.into_iter().enumerate())
            .for_each(|(i, backedge)| loop_phis[i].variants.push(backedge)); // add thoses backedges to the phi nodes of mutable variables outside the loop for every jump

        if break_points.len() == 1 {
            self.graph.current_ctrl = break_points.pop().unwrap();

            let state = break_jump_states.pop().unwrap();
            self.symbols
                .for_every_mutable(|i, value| *value = state[i].clone());

            Ok(break_values.pop().unwrap())
        } else {
            let exit_merge = self.graph.add_merge(break_points); // merge them
            self.graph.current_ctrl = self.graph.add_ctrl_merge(exit_merge.clone()); // make it the control flow of the code after that

            self.merge_states(exit_merge.clone(), break_jump_states);

            let phi = self.graph.add_phi(exit_merge, break_values);
            Ok(self.graph.add_data_phi(phi))
        }
    }
}
