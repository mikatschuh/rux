use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        builder::{Builder, Cursor, DataCursor},
        graph::TypeID,
    },
    parser::{
        DeclStmt, DeclStmtKind, Expr, ExprKind, ExprStmt, ExprStmtKind, Item, Label, ParserOutput,
        Spanned, Symbol,
    },
    utilities::Rc,
};

mod builder;
#[allow(unused)]
mod graph;
pub mod graph_dump;
mod item;
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
        &builder.builder.graph,
        Some(cursor),
        &interner,
    ))
}

pub struct GraphBuilder<'errors> {
    errors: Rc<Errors<'errors>>,
    builder: Builder,
    raw_item_table: HashMap<Symbol, Item>,
}

impl<'errors> GraphBuilder<'errors> {
    fn new(
        errors: Rc<Errors<'errors>>,
        arena: Bump,
        raw_item_table: HashMap<Symbol, Item>,
    ) -> (Self, Cursor) {
        let builder = Builder::new(arena);
        let cursor = Cursor::new(&builder.graph);
        (
            Self {
                errors,
                builder,
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
                .with_data(self.builder.graph.unit()),
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
                .with_data(self.builder.graph.unit()),
            ExprStmtKind::Expr { expr } => self.expr(cursor, expr),
            _ => {
                self.errors
                    .push(expr_stmt.span, ErrorCode::DivergentControlFlow);
                cursor.with_data(self.builder.graph.error())
            }
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
                    let open_scope = self.builder.symbol_table.open_scope();

                    let mut statements = statements.into_iter().peekable();
                    let mut cursor = cursor;
                    loop {
                        let stmt = statements.next().unwrap();
                        if statements.peek().is_none() {
                            match self.decl_stmt_pot_divergent(cursor, stmt) {
                                Some(mut value) => {
                                    self.builder
                                        .symbol_table
                                        .close_scope_and_state(open_scope, &mut value.state);
                                    return Some(value);
                                }
                                None => {
                                    self.builder.symbol_table.close_scope(open_scope);
                                    return None;
                                }
                            }
                        } else {
                            cursor = self.decl_stmt(cursor, stmt).cursor();
                        }
                    }
                }
                ExprKind::If {
                    condition,
                    when_body,
                    else_body,
                    ..
                } => {
                    let condition_cursor = self.expr(cursor, condition);

                    let (false_branch, true_branch) = self.builder.branch(condition_cursor.clone());

                    let Some(cursor_when_true) =
                        self.expr_stmt_pot_divergent(true_branch, when_body)
                    else {
                        return if let Some((_, else_body)) = else_body {
                            self.expr_stmt_pot_divergent(false_branch, else_body)
                        } else {
                            Some(false_branch.with_data(self.builder.graph.unit()))
                        };
                    };

                    if let Some((_, else_body)) = else_body {
                        let Some(cursor_when_false) =
                            self.expr_stmt_pot_divergent(false_branch, else_body)
                        else {
                            return Some(cursor_when_true);
                        };

                        self.builder
                            .merge_pot_diverge(vec![cursor_when_false, cursor_when_true])
                    } else {
                        self.builder
                            .merge_pot_diverge(vec![condition_cursor, cursor_when_true])
                    }
                }

                ExprKind::Label { label, body } => {
                    let (body_cursor, incomplete, tok, loop_id) =
                        self.builder.open_loop(cursor.clone());

                    self.builder.labels.insert(label.label.val, loop_id);
                    let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body
                    self.builder.labels.remove(&label.label.val);

                    self.builder
                        .close_loop(body, tok, false, incomplete, &mut self.errors)
                }
                ExprKind::Loop { body, .. } => {
                    let (body_cursor, incomplete, tok, _) = self.builder.open_loop(cursor.clone());

                    let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body

                    self.builder
                        .close_loop(body, tok, true, incomplete, &mut self.errors)
                }
                _ => Some(self.expr(cursor, expr)),
            },
            _ => Some(self.expr_stmt(cursor, expr_stmt)),
        }
    }

    fn expr(&mut self, cursor: Cursor, expr: Expr) -> DataCursor {
        match (*expr.val).clone() {
            ExprKind::BuiltinType(builtin_type) => {
                let ty = self.builder.graph.add_builtin_type(builtin_type);
                cursor.with_data(self.builder.graph.type_as_data(ty))
            }
            ExprKind::Literal(literal) => cursor.with_data(self.builder.graph.add_literal(literal)),
            ExprKind::Boolean(boolean) => cursor.with_data(self.builder.graph.add_boolean(boolean)),
            ExprKind::Quote(quote) => todo!(),
            ExprKind::Unit => cursor.with_data(self.builder.graph.unit()),

            ExprKind::Unary { op, value: input } => {
                let (cursor, value) = self.expr(cursor, input).split_data();
                let ty = value.ty.clone();
                cursor.with_data(self.builder.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(cursor, lhs).split_data();
                let (cursor, rhs) = self.expr(cursor, rhs).split_data();
                let ty = lhs.ty.clone();
                cursor.with_data(self.builder.graph.add_binary(op.val, lhs, rhs, ty))
            }

            ExprKind::Ident(symbol) => {
                match self.builder.symbol_table.read_symbol(symbol, &cursor.state) {
                    Some(value) => cursor.with_data(value),
                    None => match self.raw_item_table.get(&symbol) {
                        Some(_) => todo!(),
                        /*Some(Item { ty, expr }) => match ty {
                            Some(ty) => {
                                let ty = self.expr(*ty);
                                let item_ty = self.item_types.add(ty);
                                self.builder.graph.add_item(item_ty)
                            }
                            None => todo!(),
                        },*/
                        None => {
                            self.errors
                                .push(expr.span, ErrorCode::UnknownIdent { symbol });
                            cursor.with_data(self.builder.graph.error())
                        }
                    },
                }
            }
            ExprKind::Function {
                keyword,
                parameters,
                output,
                body,
            } => todo!(),

            ExprKind::Block { statements } => {
                let open_scope = self.builder.symbol_table.open_scope();

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
                        self.builder
                            .symbol_table
                            .close_scope_and_state(open_scope, &mut state);

                        return DataCursor { state, ctrl, data };
                    } else {
                        cursor = self.decl_stmt(cursor, stmt).cursor();
                    }
                }
            }
            ExprKind::If {
                condition,
                when_body,
                else_body,
                ..
            } => {
                let condition_cursor = self.expr(cursor, condition);

                let (false_branch, true_branch) = self.builder.branch(condition_cursor.clone());

                let Some(cursor_when_true) = self.expr_stmt_pot_divergent(true_branch, when_body)
                else {
                    return if let Some((_, else_body)) = else_body {
                        self.expr_stmt(false_branch, else_body)
                    } else {
                        false_branch.with_data(self.builder.graph.unit())
                    };
                };

                if let Some((_, else_body)) = else_body {
                    let Some(cursor_when_false) =
                        self.expr_stmt_pot_divergent(false_branch, else_body)
                    else {
                        return cursor_when_true;
                    };

                    self.builder.merge(NonEmpty {
                        head: cursor_when_false,
                        tail: vec![cursor_when_true],
                    })
                } else {
                    self.builder.merge(NonEmpty {
                        head: condition_cursor,
                        tail: vec![cursor_when_true],
                    })
                }
            }
            ExprKind::Label { label, body } => {
                let (body_cursor, incomplete, tok, loop_id) =
                    self.builder.open_loop(cursor.clone());

                self.builder.labels.insert(label.label.val, loop_id);
                let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body
                self.builder.labels.remove(&label.label.val);

                match self
                    .builder
                    .close_loop(body, tok, false, incomplete, &mut self.errors)
                {
                    Some(cursor) => cursor,
                    None => cursor.with_data(self.builder.graph.error()),
                }
            }
            ExprKind::Loop { body, .. } => {
                let (body_cursor, incomplete, tok, _) = self.builder.open_loop(cursor.clone());

                let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body

                match self
                    .builder
                    .close_loop(body, tok, true, incomplete, &mut self.errors)
                {
                    Some(cursor) => cursor,
                    None => cursor.with_data(self.builder.graph.error()),
                }
            }
        }
    }

    fn type_expr(&mut self, expr: Expr) -> TypeID {
        match (*expr.val).clone() {
            ExprKind::BuiltinType(builtin_type) => {
                self.builder.graph.add_builtin_type(builtin_type)
            }
            _ => {
                self.errors.push(expr.span, ErrorCode::ExpectedType);
                self.builder.graph.error_type()
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
                    self.builder
                        .symbol_table
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

                    if let Some(id) = self.builder.symbol_table.add_symbol(
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
        if let Some(binding) = self.builder.symbol_table.get_binding(symbol.val) {
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
                let Some(branch) = self.builder.labels.get(&label.label.val).cloned() else {
                    self.errors.push(
                        keyword,
                        ErrorCode::ContinueWithUnknownLabel {
                            label: label.label.val,
                        },
                    );
                    return;
                };

                let state_size = self.builder.jump_stack.state_size_of(branch);
                self.builder.jump_stack.add_continue_to(
                    branch,
                    Cursor {
                        state: state[..state_size].to_vec(),
                        ctrl,
                    },
                );
            }
            None => {
                let Some(state_size) = self.builder.jump_stack.currents_state_size() else {
                    self.errors.push(keyword, ErrorCode::ContinueOutsideLoop);
                    return;
                };

                self.builder.jump_stack.add_continue(Cursor {
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
            None => cursor.with_data(self.builder.graph.unit()),
        };
        match label {
            Some(label) => {
                let Some(branch) = self.builder.labels.get(&label.label.val).cloned() else {
                    self.errors.push(
                        keyword,
                        ErrorCode::BreakWithUnknownLabel {
                            label: label.label.val,
                        },
                    );
                    return;
                };

                let state_size = self.builder.jump_stack.state_size_of(branch);
                self.builder.jump_stack.add_break_to(
                    branch,
                    DataCursor {
                        state: state[..state_size].to_vec(),
                        ctrl,
                        data,
                    },
                );
            }
            None => {
                let Some(state_size) = self.builder.jump_stack.currents_state_size() else {
                    self.errors.push(keyword, ErrorCode::BreakOutsideLoop);
                    return;
                };

                self.builder.jump_stack.add_break(DataCursor {
                    state: state[..state_size].to_vec(),
                    ctrl,
                    data,
                });
            }
        }
    }
}
