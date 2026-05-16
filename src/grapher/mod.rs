use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        binding::ScopedSymbolTable,
        builder::{Cursor, DataCursor},
        graph::TypeID,
        jumps::{JumpTableStack, LoopID},
    },
    parser::{
        DeclStmt, DeclStmtKind, Expr, ExprKind, ExprStmt, ExprStmtKind, Item, Label, ParserOutput,
        Spanned, Symbol,
    },
    utilities::Rc,
};

mod binding;
mod builder;
#[allow(unused)]
mod graph;
pub mod graph_dump;
mod item;
mod jumps;
// mod parser;

use bumpalo::Bump;
pub use graph::Graph;
use nonempty::NonEmpty;

pub fn build_graph_debug<'errors>(
    ParserOutput {
        arena,
        mut interner,
        mut item_table,
    }: ParserOutput,
    starting_point: &'static str,
    mut errors: Rc<Errors<'errors>>,
) -> Option<String> {
    let starting_point_symbol = interner.get(starting_point);

    let Item { ty, expr } = match item_table.remove(&starting_point_symbol) {
        Some(item) => item,
        None => {
            errors.push(
                Span::beginning(),
                ErrorCode::MissingEntryPoint {
                    entry: starting_point,
                },
            );
            return None;
        }
    };

    let (mut builder, cursor) = GraphBuilder::new(errors, arena, item_table);
    let cursor = builder.expr(cursor, expr);

    Some(graph_dump::dump_text(
        builder.graph.destruct(),
        Some(cursor),
    ))
}

pub struct GraphBuilder<'errors> {
    errors: Rc<Errors<'errors>>,
    graph: Graph,
    symbol_table: ScopedSymbolTable,

    pub labels: HashMap<Symbol, LoopID>,
    pub jump_stack: JumpTableStack,

    raw_item_table: HashMap<Symbol, Item>,
}

impl<'errors> GraphBuilder<'errors> {
    fn new(
        errors: Rc<Errors<'errors>>,
        arena: Bump,
        raw_item_table: HashMap<Symbol, Item>,
    ) -> (Self, Cursor) {
        let graph = Graph::new(arena);
        let cursor = Cursor::new(&graph);
        (
            Self {
                errors,
                graph,
                symbol_table: ScopedSymbolTable::new(),
                labels: HashMap::new(),
                jump_stack: JumpTableStack::new(),
                raw_item_table,
            },
            cursor,
        )
    }

    fn decl_stmt_pot_divergent(&mut self, cursor: Cursor, stmt: DeclStmt) -> Option<DataCursor> {
        match (*stmt.val).clone() {
            DeclStmtKind::ExprStmt { expr_stmt } => self.expr_stmt_pot_divergent(cursor, expr_stmt),
            _ => Some(self.decl_stmt(cursor, stmt)),
        }
    }

    fn decl_stmt(&mut self, cursor: Cursor, stmt: DeclStmt) -> DataCursor {
        match (*stmt.val).clone() {
            DeclStmtKind::Binding {
                mutable,
                keyword,
                symbol,
                ty,
                assignment,
            } => self
                .binding(keyword, mutable, symbol, ty, assignment, cursor)
                .with_data(self.graph.unit()),
            DeclStmtKind::ExprStmt { expr_stmt } => self.expr_stmt(cursor, expr_stmt),
        }
    }

    fn expr_stmt(&mut self, cursor: Cursor, expr_stmt: ExprStmt) -> DataCursor {
        match (*expr_stmt.val).clone() {
            ExprStmtKind::Assignment {
                symbol,
                equal,
                value,
            } => self
                .assignment(symbol, equal, value, cursor)
                .with_data(self.graph.unit()),
            ExprStmtKind::Expr { expr } => self.expr(cursor, expr),
            _ => self.divergent_control_flow(expr_stmt.span, cursor),
        }
    }

    fn expr_stmt_pot_divergent(
        &mut self,
        cursor: Cursor,
        expr_stmt: ExprStmt,
    ) -> Option<DataCursor> {
        match (*expr_stmt.val).clone() {
            ExprStmtKind::Continue { keyword, label } => {
                self.continue_stmt(keyword, label, cursor);
                None
            }
            ExprStmtKind::Break {
                keyword,
                label,
                value,
            } => {
                self.break_stmt(keyword, label, value, cursor);
                None
            }
            ExprStmtKind::Return { keyword, value } => todo!(),
            ExprStmtKind::Unreachable => None,
            ExprStmtKind::Expr { expr } => match (*expr.val).clone() {
                ExprKind::Block { statements } => {
                    let open_scope = self.symbol_table.open_scope();

                    let mut statements = statements.into_iter().peekable();
                    let mut cursor = cursor;
                    loop {
                        let stmt = statements.next().unwrap();
                        if statements.peek().is_none() {
                            match self.decl_stmt_pot_divergent(cursor, stmt) {
                                Some(mut value) => {
                                    self.symbol_table
                                        .close_scope_and_state(open_scope, &mut value.state);
                                    return Some(value);
                                }
                                None => {
                                    self.symbol_table.close_scope(open_scope);
                                    return None;
                                }
                            }
                        } else {
                            cursor = self.decl_stmt(cursor, stmt).cursor();
                        }
                    }
                }
                ExprKind::If {
                    keyword,
                    condition,
                    when_body,
                    else_body,
                } => self.if_stmt_pot_divergent(keyword, condition, when_body, else_body, cursor),
                ExprKind::Label { label, body } => self.label(label, body, cursor),
                ExprKind::Loop { keyword, body } => self.loop_stmt(keyword, body, cursor),
                _ => Some(self.expr(cursor, expr)),
            },
            _ => Some(self.expr_stmt(cursor, expr_stmt)),
        }
    }

    fn expr(&mut self, cursor: Cursor, expr: Expr) -> DataCursor {
        match (*expr.val).clone() {
            ExprKind::BuiltinType(builtin_type) => {
                let ty = self.graph.add_builtin_type(builtin_type);
                cursor.with_data(self.graph.type_as_data(ty))
            }
            ExprKind::Literal(literal) => cursor.with_data(self.graph.add_literal(literal)),
            ExprKind::Boolean(boolean) => cursor.with_data(self.graph.add_boolean(boolean)),
            ExprKind::Quote(quote) => todo!(),
            ExprKind::Unit => cursor.with_data(self.graph.unit()),

            ExprKind::Unary { op, value: input } => {
                let (cursor, value) = self.expr(cursor, input).split_data();
                let ty = value.ty.clone();
                cursor.with_data(self.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(cursor, lhs).split_data();
                let (cursor, rhs) = self.expr(cursor, rhs).split_data();
                let ty = lhs.ty.clone();
                cursor.with_data(self.graph.add_binary(op.val, lhs, rhs, ty))
            }

            ExprKind::Ident(symbol) => match self.symbol_table.read_symbol(symbol, &cursor.state) {
                Some(value) => cursor.with_data(value),
                None => match self.raw_item_table.get(&symbol) {
                    Some(_) => todo!(),
                    None => {
                        self.errors
                            .push(expr.span, ErrorCode::UnknownIdent { symbol });
                        cursor.with_data(self.graph.error())
                    }
                },
            },

            ExprKind::Function {
                keyword,
                parameters,
                output,
                body,
            } => todo!(),

            ExprKind::Block { statements } => {
                let open_scope = self.symbol_table.open_scope();

                let mut statements = statements.into_iter().peekable();
                let mut cursor = cursor;
                loop {
                    let stmt = statements.next().unwrap();
                    if statements.peek().is_none() {
                        let DataCursor {
                            mut state,
                            ctrl,
                            data,
                        } = self.decl_stmt(cursor, stmt);
                        self.symbol_table
                            .close_scope_and_state(open_scope, &mut state);

                        return DataCursor { state, ctrl, data };
                    } else {
                        cursor = self.decl_stmt(cursor, stmt).cursor();
                    }
                }
            }
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_body,
            } => self.if_stmt(keyword, condition, when_body, else_body, cursor),
            ExprKind::Label { label, body } => match self.label(label, body, cursor.clone()) {
                Some(cursor) => cursor,
                None => self.divergent_control_flow(label.span(), cursor),
            },
            ExprKind::Loop { keyword, body } => match self.loop_stmt(keyword, body, cursor.clone())
            {
                Some(cursor) => cursor,
                None => self.divergent_control_flow(keyword, cursor),
            },
        }
    }

    fn type_expr(&mut self, expr: Expr) -> TypeID {
        match (*expr.val).clone() {
            ExprKind::BuiltinType(builtin_type) => self.graph.add_builtin_type(builtin_type),
            _ => {
                self.errors.push(expr.span, ErrorCode::ExpectedType);
                self.graph.error_type()
            }
        }
    }

    fn binding(
        &mut self,
        keyword: Span,
        mutable: bool,
        symbol: Spanned<Symbol>,
        ty: Option<Expr>,
        assignment: Option<(Span, Expr)>,

        mut cursor: Cursor,
    ) -> Cursor {
        match ty {
            Some(ty) => {
                let ty = self.type_expr(ty);
                if let Some(id) =
                    self.symbol_table
                        .add_symbol(mutable, symbol.val, ty, &mut cursor.state)
                {
                    match assignment {
                        Some((_, value)) => {
                            let DataCursor {
                                mut state,
                                ctrl,
                                data,
                            } = self.expr(cursor, value);

                            state[id] = Some(data);
                            Cursor { state, ctrl }
                        }
                        None => cursor,
                    }
                } else {
                    self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                    cursor
                }
            }
            None => match assignment {
                Some((_, value)) => {
                    let DataCursor {
                        mut state,
                        ctrl,
                        data,
                    } = self.expr(cursor, value);

                    if let Some(id) = self.symbol_table.add_symbol(
                        mutable,
                        symbol.val,
                        data.ty.clone(),
                        &mut state,
                    ) {
                        state[id] = Some(data);
                        Cursor { state, ctrl }
                    } else {
                        self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                        Cursor { state, ctrl }
                    }
                }
                None => {
                    self.errors
                        .push(keyword, ErrorCode::BindingWithNeitherTypeNorValue);
                    cursor
                }
            },
        }
    }

    fn assignment(
        &mut self,
        symbol: Spanned<Symbol>,
        equal: Span,
        value: Expr,
        cursor: Cursor,
    ) -> Cursor {
        let DataCursor {
            mut state,
            ctrl,
            data,
        } = self.expr(cursor, value);
        if let Some(binding) = self.symbol_table.get_binding(symbol.val) {
            if binding.mutable || state[binding.id].is_none() {
                state[binding.id] = Some(data);
            } else {
                self.errors.push(
                    equal,
                    ErrorCode::AssignmentToImmutableIdent { symbol: symbol.val },
                )
            }
        } else {
            self.errors.push(
                symbol.span,
                ErrorCode::AssignmentToUnknownIdent { symbol: symbol.val },
            );
        }
        Cursor { state, ctrl }
    }

    fn continue_stmt(&mut self, keyword: Span, label: Option<Label>, cursor: Cursor) {
        let Cursor { state, ctrl } = cursor;
        match label {
            Some(label) => {
                let Some(branch) = self.labels.get(&label.label.val).cloned() else {
                    self.errors.push(
                        keyword,
                        ErrorCode::ContinueWithUnknownLabel {
                            label: label.label.val,
                        },
                    );
                    return;
                };

                let state_size = self.jump_stack.state_size_of(branch);
                self.jump_stack.add_continue_to(
                    branch,
                    Cursor {
                        state: state[..state_size].to_vec(),
                        ctrl,
                    },
                );
            }
            None => {
                let Some(state_size) = self.jump_stack.currents_state_size() else {
                    self.errors.push(keyword, ErrorCode::ContinueOutsideLoop);
                    return;
                };

                self.jump_stack.add_continue(Cursor {
                    state: state[..state_size].to_vec(),
                    ctrl,
                });
            }
        }
    }

    fn break_stmt(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
        cursor: Cursor,
    ) {
        let DataCursor { state, ctrl, data } = match value {
            Some(value) => self.expr(cursor, value),
            None => cursor.with_data(self.graph.unit()),
        };
        match label {
            Some(label) => {
                let Some(branch) = self.labels.get(&label.label.val).cloned() else {
                    self.errors.push(
                        keyword,
                        ErrorCode::BreakWithUnknownLabel {
                            label: label.label.val,
                        },
                    );
                    return;
                };

                let state_size = self.jump_stack.state_size_of(branch);
                self.jump_stack.add_break_to(
                    branch,
                    DataCursor {
                        state: state[..state_size].to_vec(),
                        ctrl,
                        data,
                    },
                );
            }
            None => {
                let Some(state_size) = self.jump_stack.currents_state_size() else {
                    self.errors.push(keyword, ErrorCode::BreakOutsideLoop);
                    return;
                };

                self.jump_stack.add_break(DataCursor {
                    state: state[..state_size].to_vec(),
                    ctrl,
                    data,
                });
            }
        }
    }

    fn if_stmt_pot_divergent(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
        cursor: Cursor,
    ) -> Option<DataCursor> {
        let condition_cursor = self.expr(cursor, condition);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor.clone());

        let Some(cursor_when_true) = self.expr_stmt_pot_divergent(true_branch, when_body) else {
            return if let Some((_, else_body)) = else_body {
                self.expr_stmt_pot_divergent(false_branch, else_body)
            } else {
                Some(false_branch.with_data(self.graph.unit()))
            };
        };

        if let Some((_, else_body)) = else_body {
            let Some(cursor_when_false) = self.expr_stmt_pot_divergent(false_branch, else_body)
            else {
                return Some(cursor_when_true);
            };

            self.graph
                .merge_pot_diverge(vec![cursor_when_false, cursor_when_true])
        } else {
            self.graph
                .merge_pot_diverge(vec![condition_cursor, cursor_when_true])
        }
    }

    fn if_stmt(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
        cursor: Cursor,
    ) -> DataCursor {
        let condition_cursor = self.expr(cursor, condition);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor.clone());

        let Some(cursor_when_true) = self.expr_stmt_pot_divergent(true_branch, when_body) else {
            return if let Some((_, else_body)) = else_body {
                self.expr_stmt(false_branch, else_body)
            } else {
                false_branch.with_data(self.graph.unit())
            };
        };

        if let Some((_, else_body)) = else_body {
            let Some(cursor_when_false) = self.expr_stmt_pot_divergent(false_branch, else_body)
            else {
                return cursor_when_true;
            };

            self.graph.merge(NonEmpty {
                head: cursor_when_false,
                tail: vec![cursor_when_true],
            })
        } else {
            self.graph.merge(NonEmpty {
                head: condition_cursor,
                tail: vec![cursor_when_true],
            })
        }
    }

    fn label(&mut self, label: Label, body: ExprStmt, cursor: Cursor) -> Option<DataCursor> {
        let (body_cursor, incomplete) = self.graph.open_loop(cursor);
        let (tok, loop_id) = self.jump_stack.open_block(body_cursor.state.len()); // store how many variables belonged to the previous state

        self.labels.insert(label.label.val, loop_id);
        let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body
        self.labels.remove(&label.label.val);

        let jumps = self.jump_stack.close_block(tok);
        self.graph
            .close_loop(jumps, body, false, incomplete, &mut self.errors)
    }

    fn loop_stmt(&mut self, _keyword: Span, body: ExprStmt, cursor: Cursor) -> Option<DataCursor> {
        let (body_cursor, incomplete) = self.graph.open_loop(cursor);
        let (tok, _) = self.jump_stack.open_block(body_cursor.state.len()); // store how many variables belonged to the previous state

        let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body

        let jumps = self.jump_stack.close_block(tok);
        self.graph
            .close_loop(jumps, body, true, incomplete, &mut self.errors)
    }

    fn divergent_control_flow(&mut self, span: Span, cursor: Cursor) -> DataCursor {
        self.errors.push(span, ErrorCode::DivergentControlFlow);
        cursor.with_data(self.graph.error())
    }
}
