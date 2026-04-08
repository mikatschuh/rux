use crate::{
    grapher::{
        Graph, GraphError, GraphResult,
        graph::{MemNodeID, NodeID, NodeKind, Symbol},
    },
    literals::Literal,
    tokenizing::{
        TokenStream, binding_pow,
        token::{Bracket, Keyword, Token, TokenKind},
    },
};
use std::collections::HashMap;

#[derive(Clone)]
struct LoopContext<'src> {
    loop_head: MemNodeID<'src>,
    break_mem: Option<MemNodeID<'src>>,
    continue_mem: Option<MemNodeID<'src>>,
}

#[derive(Clone)]
struct ParserState<'src> {
    symbols: HashMap<&'src str, Symbol<'src>>,
    mem: MemNodeID<'src>,
    loops: Vec<LoopContext<'src>>,
    reachable: bool,
    last_jump: Option<Token<'src>>,
}

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    graph: Graph<'src>,
    loops: Vec<LoopContext<'src>>,
    reachable: bool,
    last_jump: Option<Token<'src>>,
}

impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::new(),
            loops: Vec::new(),
            reachable: true,
            last_jump: None,
        }
    }

    pub fn build(mut self) -> GraphResult<'src, Graph<'src>> {
        loop {
            let token = self.peek();
            if token.kind == TokenKind::Eof {
                return Ok(self.graph);
            }

            self.parse_statement()?;
        }
    }

    fn parse_statement(&mut self) -> GraphResult<'src, ()> {
        let token = self.peek();
        match token.kind {
            TokenKind::Eof => return Ok(()),
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
            TokenKind::Keyword(Keyword::Continue) => self.parse_continue_statement(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_statement(),
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

        if min_bp <= binding_pow::STMT && self.peek().binding_pow() == binding_pow::ALL {
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
        let condition = self.parse_expr(binding_pow::EXPR)?;
        let when_true = self.parse_expr(binding_pow::EXPR)?;

        let else_token = self.peek();
        match else_token.kind {
            TokenKind::Keyword(Keyword::Else) => {
                self.advance();
            }
            _ => {
                return Err(GraphError::UnexpectedToken {
                    expected: "else",
                    found: else_token,
                });
            }
        }

        let when_false = self.parse_expr(binding_pow::EXPR)?;

        Ok(self.graph.add_phi(condition, when_true, when_false))
    }

    fn parse_if_statement(&mut self) -> GraphResult<'src, ()> {
        self.advance(); // consume 'if'
        let condition = self.parse_expr(binding_pow::EXPR)?;
        let before_state = self.snapshot_state();
        let when_true_state = self.parse_branch_block()?;

        self.restore_state(before_state.clone());

        let when_false_state = if matches!(self.peek().kind, TokenKind::Keyword(Keyword::Else)) {
            self.advance();
            self.parse_branch_block()?
        } else {
            before_state.clone()
        };

        self.finish_branch_merge(condition, before_state, when_true_state, when_false_state);
        Ok(())
    }

    fn parse_loop_statement(&mut self) -> GraphResult<'src, ()> {
        self.advance(); // consume 'loop'
        let before_loop = self.graph.latest_mem.clone();
        let loop_head = self.parse_loop_header()?;

        let step_clause = if self.peek().kind == TokenKind::Colon {
            self.advance();
            let step_clause = self.graph.add_step_clause(before_loop.clone());
            self.parse_statement()?;
            Some(step_clause)
        } else {
            None
        };

        self.graph.latest_mem = loop_head.clone();
        self.loops.push(LoopContext {
            loop_head: loop_head.clone(),
            break_mem: None,
            continue_mem: None,
        });
        self.reachable = true;

        let body_open = self.expect_open_curly()?; // loop body
        self.parse_block(body_open)?;

        let mut loop_ctx = self.loops.pop().expect("loop context");
        let mut backedge_mem = loop_ctx.continue_mem;
        if self.reachable {
            backedge_mem = self.merge_optional_mem(backedge_mem, Some(self.graph.latest_mem()));
        }

        if let Some(mut when_loop_mem) = backedge_mem {
            if let Some(mut step_clause) = step_clause {
                step_clause.set_entry(when_loop_mem.clone());
                when_loop_mem = step_clause;
            }

            loop_ctx.loop_head.set_backedge(when_loop_mem);
        }

        let after_loop_mem = self
            .merge_optional_mem(Some(loop_ctx.loop_head), loop_ctx.break_mem)
            .expect("loop exit memory");
        self.graph.latest_mem = after_loop_mem;
        self.reachable = true;
        self.last_jump = None;
        Ok(())
    }

    fn parse_loop_header(&mut self) -> GraphResult<'src, MemNodeID<'src>> {
        let placeholder = self.graph.add_literal(Literal::from(0_u8));
        let before_loop = self.graph.latest_mem();

        let placeholder_mem = self.graph.add_placeholder(before_loop.clone());
        self.graph.latest_mem = placeholder_mem.clone();

        let first_clause = if self.peek().kind == TokenKind::Ident {
            let name = self.advance();

            if self.peek().kind == TokenKind::Open(Bracket::Curly) {
                let mut loop_head = self.graph.add_loop_head(placeholder, before_loop);

                let condition = self.graph.read_variable(name)?;
                loop_head.set_condition(condition);
                self.graph.latest_mem = loop_head.clone();
                return Ok(loop_head);
            }

            self.parse_variable_tail(name, binding_pow::STMT)?;
            let lhs = self.graph.read_variable(name)?;
            self.parse_expr_tail(lhs, binding_pow::EXPR)?
        } else {
            self.parse_expr(binding_pow::EXPR)?
        };

        if self.peek().kind != TokenKind::Colon {
            let loop_head = self.graph.add_loop_head(first_clause, before_loop);

            self.graph.latest_mem = loop_head.clone();
            return Ok(loop_head);
        } else {
            self.advance();
        }

        if self.peek().kind == TokenKind::Ident {
            let name = self.advance();

            if self.peek().kind == TokenKind::Open(Bracket::Curly) {
                let condition = self.graph.read_variable(name)?;
                let loop_head = self.graph.add_loop_head(condition, before_loop);
                self.graph.latest_mem = loop_head.clone();
                return Ok(loop_head);
            }

            self.parse_variable_tail(name, binding_pow::STMT)?;
            let lhs = self.graph.read_variable(name)?;
            self.parse_expr_tail(lhs, binding_pow::EXPR)?
        } else {
            self.parse_expr(binding_pow::EXPR)?
        };

        todo!();
    }

    fn parse_branch_block(&mut self) -> GraphResult<'src, ParserState<'src>> {
        let block_open = self.expect_open_curly()?;
        self.parse_block(block_open)?;
        Ok(self.snapshot_state())
    }

    fn finish_branch_merge(
        &mut self,
        condition: NodeID<'src>,
        before_state: ParserState<'src>,
        when_true_state: ParserState<'src>,
        when_false_state: ParserState<'src>,
    ) {
        self.merge_loop_versions(
            &before_state.loops,
            &when_true_state.loops,
            &when_false_state.loops,
        );

        match (when_true_state.reachable, when_false_state.reachable) {
            (true, true) => {
                self.merge_symbol_versions(
                    condition.clone(),
                    before_state.symbols,
                    when_true_state.symbols,
                    when_false_state.symbols,
                );
                self.merge_mem_versions(condition, when_true_state.mem, when_false_state.mem);
                self.reachable = true;
                self.last_jump = None;
            }
            (true, false) => {
                self.graph.replace_symbols(when_true_state.symbols);
                self.graph.latest_mem = when_true_state.mem;
                self.reachable = true;
                self.last_jump = None;
            }
            (false, true) => {
                self.graph.replace_symbols(when_false_state.symbols);
                self.graph.latest_mem = when_false_state.mem;
                self.reachable = true;
                self.last_jump = None;
            }
            (false, false) => {
                self.graph.replace_symbols(before_state.symbols);
                self.graph.latest_mem = before_state.mem;
                self.reachable = false;
            }
        }
    }

    fn parse_continue_statement(&mut self) -> GraphResult<'src, ()> {
        let keyword = self.advance();
        let mut layers = 0;
        while matches!(self.peek().kind, TokenKind::Keyword(Keyword::Continue)) {
            self.advance();
            layers += 1;
        }

        let current_mem = self.graph.latest_mem();
        let existing = self
            .loop_target(layers)
            .and_then(|loop_ctx| loop_ctx.continue_mem.clone());
        let merged = self.merge_optional_mem(existing, Some(current_mem));

        let Some(target_loop) = self.loop_target_mut(layers) else {
            return Err(GraphError::JumpOutsideLoop { keyword });
        };
        target_loop.continue_mem = merged;
        self.reachable = false;
        self.last_jump = Some(keyword);
        Ok(())
    }

    fn parse_break_statement(&mut self) -> GraphResult<'src, ()> {
        let keyword = self.advance();
        let mut layers = 0;
        while matches!(self.peek().kind, TokenKind::Keyword(Keyword::Break)) {
            self.advance();
            layers += 1;
        }

        let current_mem = self.graph.latest_mem();
        let existing = self
            .loop_target(layers)
            .and_then(|loop_ctx| loop_ctx.break_mem.clone());
        let merged = self.merge_optional_mem(existing, Some(current_mem));

        let Some(target_loop) = self.loop_target_mut(layers) else {
            return Err(GraphError::JumpOutsideLoop { keyword });
        };
        target_loop.break_mem = merged;
        self.reachable = false;
        self.last_jump = Some(keyword);
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
                TokenKind::Closed(_) | TokenKind::Eof => {
                    return Err(GraphError::MismatchedBracket {
                        opener,
                        closer: token,
                    });
                }
                _ => {
                    if !self.reachable {
                        return Err(GraphError::UnreachableStatementAfterJump {
                            jump: self
                                .last_jump
                                .expect("jump token recorded for unreachable path"),
                            statement: token,
                        });
                    }

                    self.parse_statement()?;
                }
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
            self.graph.latest_mem = true_mem;
        } else {
            let merged = self.graph.add_mem_merge(condition, true_mem, false_mem);
            self.graph.latest_mem = merged;
        }
    }

    fn merge_loop_versions(
        &mut self,
        base: &[LoopContext<'src>],
        when_true: &[LoopContext<'src>],
        when_false: &[LoopContext<'src>],
    ) {
        let mut merged = base.to_vec();

        for index in 0..merged.len() {
            merged[index].break_mem = self.merge_optional_mem(
                merged[index].break_mem.clone(),
                self.merged_branch_jump(base, when_true, index, true),
            );
            merged[index].break_mem = self.merge_optional_mem(
                merged[index].break_mem.clone(),
                self.merged_branch_jump(base, when_false, index, true),
            );

            merged[index].continue_mem = self.merge_optional_mem(
                merged[index].continue_mem.clone(),
                self.merged_branch_jump(base, when_true, index, false),
            );
            merged[index].continue_mem = self.merge_optional_mem(
                merged[index].continue_mem.clone(),
                self.merged_branch_jump(base, when_false, index, false),
            );
        }

        self.loops = merged;
    }

    fn merged_branch_jump(
        &self,
        base: &[LoopContext<'src>],
        branch: &[LoopContext<'src>],
        index: usize,
        is_break: bool,
    ) -> Option<MemNodeID<'src>> {
        let base_jump = if is_break {
            base[index].break_mem.as_ref()
        } else {
            base[index].continue_mem.as_ref()
        };
        let branch_jump = if is_break {
            branch[index].break_mem.as_ref()
        } else {
            branch[index].continue_mem.as_ref()
        };

        match (base_jump, branch_jump) {
            (Some(base_mem), Some(branch_mem)) if !base_mem.ptr_cmp(branch_mem) => {
                Some(branch_mem.clone())
            }
            (None, Some(branch_mem)) => Some(branch_mem.clone()),
            _ => None,
        }
    }

    fn merge_optional_mem(
        &mut self,
        current: Option<MemNodeID<'src>>,
        next: Option<MemNodeID<'src>>,
    ) -> Option<MemNodeID<'src>> {
        match (current, next) {
            (None, None) => None,
            (Some(mem), None) | (None, Some(mem)) => Some(mem),
            (Some(a), Some(b)) if a.ptr_cmp(&b) => Some(a),
            (Some(a), Some(b)) => {
                let merge_condition = self.graph.add_literal(Literal::from(1_u8));
                Some(self.graph.add_mem_merge(merge_condition, a, b))
            }
        }
    }

    fn loop_target_mut(&mut self, layers: usize) -> Option<&mut LoopContext<'src>> {
        let index = self.loops.len().checked_sub(layers + 1)?;
        self.loops.get_mut(index)
    }

    fn loop_target(&self, layers: usize) -> Option<&LoopContext<'src>> {
        let index = self.loops.len().checked_sub(layers + 1)?;
        self.loops.get(index)
    }

    fn snapshot_state(&self) -> ParserState<'src> {
        ParserState {
            symbols: self.graph.snapshot_symbols(),
            mem: self.graph.latest_mem(),
            loops: self.loops.clone(),
            reachable: self.reachable,
            last_jump: self.last_jump,
        }
    }

    fn restore_state(&mut self, state: ParserState<'src>) {
        self.graph.replace_symbols(state.symbols);
        self.graph.latest_mem = state.mem;
        self.loops = state.loops;
        self.reachable = state.reachable;
        self.last_jump = state.last_jump;
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
