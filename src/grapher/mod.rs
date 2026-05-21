use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        binding::{Binding, SymbolTableStack},
        builder::{CtrlCursor, Cursor, DataCursor},
        graph::{DataKind, TypeID},
        loops::JumpTableStack,
        type_check::require_type,
    },
    parser::{
        DeclStmt, DeclStmtKind, Expr, ExprKind, ExprStmt, ExprStmtKind, Interner, Item, Label,
        ParserOutput, Spanned, Symbol,
    },
    ref_count::Rc,
};

mod binding;
mod builder;
mod graph;
pub mod graph_dump;
mod item;
mod loops;
mod type_check;

use bumpalo::Bump;
pub use graph::Graph;

pub fn build_graph_debug<'errors>(
    ParserOutput {
        arena,
        mut interner,
        mut item_table,
    }: ParserOutput,
    starting_point: &'static str,
    mut errors: Rc<Errors<'errors>>,
) -> Option<(String, Interner)> {
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

    Some((
        graph_dump::dump_text(builder.graph.destruct(), Some(cursor)),
        interner,
    ))
}

pub struct GraphBuilder<'errors> {
    errors: Rc<Errors<'errors>>,
    graph: Graph,

    symbol_table: SymbolTableStack,
    jump_table: JumpTableStack,

    raw_item_table: HashMap<Symbol, Item>,
}

impl<'errors> GraphBuilder<'errors> {
    fn new(
        errors: Rc<Errors<'errors>>,
        arena: Bump,
        raw_item_table: HashMap<Symbol, Item>,
    ) -> (Self, CtrlCursor) {
        let graph = Graph::new(arena);
        let cursor = Cursor::new().with_ctrl(graph.start());
        (
            Self {
                errors,
                graph,
                symbol_table: SymbolTableStack::new(),
                jump_table: JumpTableStack::new(),
                raw_item_table,
            },
            cursor,
        )
    }

    fn decl_stmt_pot_divergent(
        &mut self,
        cursor: CtrlCursor,
        stmt: DeclStmt,
    ) -> Option<DataCursor> {
        match (*stmt.val).clone() {
            DeclStmtKind::ExprStmt { expr_stmt } => self.expr_stmt_pot_divergent(cursor, expr_stmt),
            _ => Some(self.decl_stmt(cursor, stmt)),
        }
    }

    fn decl_stmt(&mut self, cursor: CtrlCursor, stmt: DeclStmt) -> DataCursor {
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

    fn expr_stmt(&mut self, cursor: CtrlCursor, expr_stmt: ExprStmt) -> DataCursor {
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
        cursor: CtrlCursor,
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
                                Some(value) => {
                                    self.symbol_table.close_scope(open_scope);
                                    return Some(value);
                                }
                                None => {
                                    self.symbol_table.close_scope(open_scope);
                                    return None;
                                }
                            }
                        } else {
                            cursor = self.decl_stmt(cursor, stmt).without_data();
                        }
                    }
                }
                ExprKind::If {
                    keyword,
                    condition,
                    when_body,
                    else_body,
                } => self.if_stmt_pot_divergent(keyword, condition, when_body, else_body, cursor),
                ExprKind::Label { label, body } => self.loop_stmt(Some(label), body, cursor),
                ExprKind::Loop { keyword, body } => self.loop_stmt(None, body, cursor),
                _ => Some(self.expr(cursor, expr)),
            },
            _ => Some(self.expr_stmt(cursor, expr_stmt)),
        }
    }

    fn expr(&mut self, mut cursor: CtrlCursor, expr: Expr) -> DataCursor {
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
                let (cursor, value) = self.expr(cursor, input).split();
                let ty = value.ty.clone();
                cursor.with_data(self.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(cursor, lhs).split();
                let (cursor, rhs) = self.expr(cursor, rhs).split();
                let ty = lhs.ty.clone();
                cursor.with_data(self.graph.add_binary(op.val, lhs, rhs, ty))
            }

            ExprKind::Ident(symbol) => match self.symbol_table.get_binding(symbol) {
                Some(Binding { id, ty, .. }) => {
                    match cursor.cursor.get_value(
                        *id,
                        ty.clone(),
                        expr.clone(),
                        &mut self.graph,
                        &mut self.jump_table,
                    ) {
                        Some(mut value) => {
                            if value.kind == DataKind::Error {
                                // Placeholder because of Loop
                                value.ty = ty.clone();
                            }
                            cursor.with_data(value)
                        }
                        None => self.uninitialized_moved_variable(expr.span, cursor),
                    }
                }
                None => match self.raw_item_table.get(&symbol) {
                    Some(_) => todo!(),
                    None => self.unknown_identifier(expr.span, symbol, cursor),
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
                        let DataCursor { cursor, ctrl, data } = self.decl_stmt(cursor, stmt);
                        self.symbol_table.close_scope(open_scope);

                        return DataCursor { cursor, ctrl, data };
                    } else {
                        cursor = self.decl_stmt(cursor, stmt).without_data();
                    }
                }
            }
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_body,
            } => self.if_stmt(keyword, condition, when_body, else_body, cursor),
            ExprKind::Label { label, body } => {
                match self.loop_stmt(Some(label), body, cursor.clone()) {
                    Some(cursor) => cursor,
                    None => self.divergent_control_flow(label.span(), cursor),
                }
            }
            ExprKind::Loop { keyword, body } => match self.loop_stmt(None, body, cursor.clone()) {
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

        cursor: CtrlCursor,
    ) -> CtrlCursor {
        match ty {
            Some(ty) => {
                let ty = self.type_expr(ty);
                if let Some(id) = self
                    .symbol_table
                    .add_symbol(mutable, symbol.val, ty.clone())
                {
                    match assignment {
                        Some((_, value)) => {
                            let DataCursor {
                                mut cursor,
                                ctrl,
                                data,
                            } = self.expr(cursor, value.clone());

                            cursor.state.insert(
                                id,
                                require_type(&self.graph, value.span, ty, data, &mut self.errors),
                            );
                            CtrlCursor { cursor, ctrl }
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
                        mut cursor,
                        ctrl,
                        data,
                    } = self.expr(cursor, value);

                    if let Some(id) =
                        self.symbol_table
                            .add_symbol(mutable, symbol.val, data.ty.clone())
                    {
                        cursor.state.insert(id, data);
                        CtrlCursor { cursor, ctrl }
                    } else {
                        self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                        CtrlCursor { cursor, ctrl }
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
        cursor: CtrlCursor,
    ) -> CtrlCursor {
        let DataCursor {
            mut cursor,
            ctrl,
            data,
        } = self.expr(cursor, value.clone());
        if let Some(binding) = self.symbol_table.get_binding(symbol.val) {
            if binding.mutable {
                let ty = binding.ty.clone();
                cursor.state.insert(
                    binding.id,
                    require_type(&self.graph, value.span, ty, data, &mut self.errors),
                );
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
        CtrlCursor { cursor, ctrl }
    }

    fn continue_stmt(&mut self, keyword: Span, label: Option<Label>, cursor: CtrlCursor) {
        let Some(block) = self.jump_table.get(label.map(|l| l.label.val)) else {
            self.errors.push(
                keyword,
                label.map_or(ErrorCode::ContinueOutsideLoop, |l| {
                    ErrorCode::ContinueWithUnknownLabel { label: l.label.val }
                }),
            );
            return;
        };

        let CtrlCursor { cursor, ctrl } = cursor;
        block.continue_jumps.push(CtrlCursor { cursor, ctrl });
    }

    fn break_stmt(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
        cursor: CtrlCursor,
    ) {
        let DataCursor { cursor, ctrl, data } = match value {
            Some(value) => self.expr(cursor, value),
            None => cursor.with_data(self.graph.unit()),
        };
        let Some(block) = self.jump_table.get(label.map(|l| l.label.val)) else {
            self.errors.push(
                keyword,
                label.map_or(ErrorCode::BreakOutsideLoop, |l| {
                    ErrorCode::BreakWithUnknownLabel { label: l.label.val }
                }),
            );
            return;
        };

        block.break_jumps.push(DataCursor { cursor, ctrl, data });
    }

    fn if_stmt(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
        cursor: CtrlCursor,
    ) -> DataCursor {
        let condition_cursor = self.expr(cursor, condition);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor);

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

            self.graph
                .merge(vec![cursor_when_false, cursor_when_true])
                .unwrap() // we dont put in an empty vec
        } else {
            self.graph
                .merge(vec![
                    false_branch.with_data(self.graph.unit()),
                    cursor_when_true,
                ])
                .unwrap()
        }
    }

    fn if_stmt_pot_divergent(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let condition_cursor = self.expr(cursor, condition);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor);

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

            self.graph.merge(vec![cursor_when_false, cursor_when_true])
        } else {
            self.graph.merge(vec![
                false_branch.with_data(self.graph.unit()),
                cursor_when_true,
            ])
        }
    }

    fn loop_stmt(
        &mut self,
        label: Option<Label>,
        body: ExprStmt,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let tok = self.jump_table.open_loop(label.map(|l| l.label.val));
        let (loop_head, entry, body_cursor) = self.graph.open_loop(cursor, &tok);

        let body = self.expr_stmt_pot_divergent(body_cursor, body); // parse the hole body

        let closed_loop = self.jump_table.close_loop(tok);
        self.graph.close_loop(
            loop_head,
            entry,
            closed_loop,
            &mut self.jump_table,
            body,
            false,
            &mut self.errors,
        )
    }

    fn divergent_control_flow(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.push(span, ErrorCode::DivergentControlFlow);
        cursor.with_data(self.graph.error())
    }

    fn uninitialized_moved_variable(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.push(span, ErrorCode::ReadUnitializedOrMoved);
        cursor.with_data(self.graph.error())
    }

    fn unknown_identifier(&mut self, span: Span, symbol: Symbol, cursor: CtrlCursor) -> DataCursor {
        self.errors.push(span, ErrorCode::UnknownIdent { symbol });
        cursor.with_data(self.graph.error())
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use crate::{
        error::{Errors, Span},
        grapher::graph::{DataID, DataKind},
        literal_parsing::Literal,
        parser::{AstBuilder, BuiltinType, Expr, Interner, Label, Spanned, Symbol},
        ref_count::Rc,
    };

    use super::GraphBuilder;

    fn span() -> Span {
        Span::beginning()
    }

    fn spanned(symbol: Symbol) -> Spanned<Symbol> {
        Spanned {
            span: span(),
            val: symbol,
        }
    }

    fn with_built_expr<R>(expr: Expr, arena: bumpalo::Bump, f: impl FnOnce(&DataID) -> R) -> R {
        let errors = Rc::new(Errors::empty(Path::new("grapher-test.rx")));
        let (mut builder, cursor) = GraphBuilder::new(errors, arena, HashMap::new());
        let data = builder.expr(cursor, expr).data;

        f(&data)
    }

    fn literal_value(node: &DataID) -> Option<Literal> {
        match &node.kind {
            DataKind::Literal { literal } => Some(literal.clone()),
            _ => None,
        }
    }

    fn kind_name(node: &DataID) -> &'static str {
        match &node.kind {
            DataKind::Literal { .. } => "literal",
            DataKind::Quote { .. } => "quote",
            DataKind::Boolean(_) => "boolean",
            DataKind::Unit => "unit",
            DataKind::Unary { .. } => "unary",
            DataKind::Binary { .. } => "binary",
            DataKind::Load { .. } => "load",
            DataKind::Phi { .. } => "phi",
            DataKind::Type { .. } => "type",
            DataKind::Error => "error",
            DataKind::Placeholder => "placeholder",
        }
    }

    #[test]
    fn if_without_else_merges_false_unit_path_and_true_value_path() {
        let mut ast = AstBuilder::new();
        let condition = ast.add_boolean(span(), true);
        let one = ast.add_literal(span(), Literal::from(1));
        let when_body = ast.expr_as_expr_stmt(one);
        let expr = ast.add_if(span(), condition, when_body, None);
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            let DataKind::Phi { phi } = &value.kind else {
                panic!("if expression should produce a phi");
            };

            assert_eq!(phi.merge.branches.len(), 2);
            assert!(
                phi.variants
                    .iter()
                    .any(|variant| matches!(&variant.kind, DataKind::Unit))
            );
            assert!(
                phi.variants
                    .iter()
                    .any(|variant| literal_value(variant).as_ref() == Some(&Literal::from(1)))
            );
        });
    }

    #[test]
    fn uninitialized_binding_read_recovers_with_error_node() {
        let mut interner = Interner::new();
        let x = interner.get("x");
        let mut ast = AstBuilder::new();
        let ty = ast.add_type(span(), BuiltinType::Signed { size: 32 });
        let binding = ast.add_binding(false, span(), spanned(x), Some(ty), None);
        let read = ast.add_ident(spanned(x));
        let read = ast.expr_as_stmt(read);
        let expr = ast.add_block(
            span(),
            nonempty::NonEmpty {
                head: binding,
                tail: vec![read],
            },
        );
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            assert!(
                matches!(&value.kind, DataKind::Error),
                "got {}",
                kind_name(value)
            );
        });
    }

    #[test]
    fn unknown_identifier_recovers_with_error_node() {
        let mut interner = Interner::new();
        let mut ast = AstBuilder::new();
        let expr = ast.add_ident(spanned(interner.get("missing")));
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            assert!(
                matches!(&value.kind, DataKind::Error),
                "got {}",
                kind_name(value)
            );
        });
    }

    #[test]
    fn divergent_loop_in_expression_position_recovers_with_error_node() {
        let mut ast = AstBuilder::new();
        let unit = ast.add_unit(span());
        let body = ast.expr_as_expr_stmt(unit);
        let expr = ast.add_loop(span(), body);
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            assert!(
                matches!(&value.kind, DataKind::Error),
                "got {}",
                kind_name(value)
            );
        });
    }

    #[test]
    fn labelled_block_can_break_to_outer_block_from_nested_loop() {
        let mut interner = Interner::new();
        let outer = interner.get("outer");
        let mut ast = AstBuilder::new();
        let value = ast.add_literal(span(), Literal::from(42));
        let break_stmt = ast.add_break(
            span(),
            Some(Label {
                colon: span(),
                label: spanned(outer),
            }),
            Some(value),
        );
        let loop_expr = ast.add_loop(span(), break_stmt);
        let loop_stmt = ast.expr_as_expr_stmt(loop_expr);
        let expr = ast.add_label(
            Label {
                colon: span(),
                label: spanned(outer),
            },
            loop_stmt,
        );
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            assert_eq!(literal_value(value), Some(Literal::from(42)));
        });
    }

    #[test]
    fn if_assignment_merges_mutable_state() {
        let mut interner = Interner::new();
        let x = interner.get("x");
        let mut ast = AstBuilder::new();
        let zero = ast.add_literal(span(), Literal::from(0));
        let binding = ast.add_binding(true, span(), spanned(x), None, Some((span(), zero)));
        let condition = ast.add_boolean(span(), true);
        let one = ast.add_literal(span(), Literal::from(1));
        let assignment = ast.add_assignment(spanned(x), span(), one);
        let if_expr = ast.add_if(span(), condition, assignment, None);
        let if_stmt = ast.expr_as_stmt(if_expr);
        let read = ast.add_ident(spanned(x));
        let read = ast.expr_as_stmt(read);
        let expr = ast.add_block(
            span(),
            nonempty::NonEmpty {
                head: binding,
                tail: vec![if_stmt, read],
            },
        );
        let arena = ast.arena();
        with_built_expr(expr, arena, |value| {
            assert!(matches!(&value.kind, DataKind::Phi { .. }));
        });
    }
}
