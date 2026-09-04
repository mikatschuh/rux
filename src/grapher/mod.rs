use std::{collections::HashMap, vec};

use crate::{
    error::{ErrorCode, Errors},
    grapher::{
        binding::{Binding, SymbolTableStack},
        builder::{Cfg, CtrlCursor, DataCursor},
        graph::{DataID, DataKind, TypeID},
        loops::JumpTableStack,
        type_check::require_type,
    },
    parser::{
        Assignment, AstBuilder, ControlStruct, Definition, Expr, ExprKind, Ident, Interner, Item,
        JumpStruct, Label, ParserOutput, ScopeStmt, ScopeStmtKind, Spanned, StmtExpr, StmtExprKind,
        Symbol,
    },
    ref_count::Rc,
    tokenizing::span::Span,
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
        ast,
        mut interner,
        mut item_table,
        err_expr,
        incomplete_bindings,
    }: ParserOutput,
    starting_point: &'static str,
    mut errors: Rc<Errors<'errors>>,
) -> Option<(String, Interner)> {
    let starting_point_symbol = interner.get(starting_point);

    let Item::Constant {
        ident,
        definition:
            Definition::Assignment {
                ty: Some(ty),
                assignment: Assignment { value, .. },
            },
        ..
    } = (match item_table.remove(&starting_point_symbol) {
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
    })
    else {
        todo!()
    };

    let (mut builder, cursor) = GraphBuilder::new(ast, errors, Bump::new(), item_table);
    let cursor = builder.expr(value, cursor);

    Some((
        graph_dump::dump_text(
            builder.graph.destruct(),
            builder.symbol_dump,
            Some(cursor),
            &interner,
        ),
        interner,
    ))
}

pub struct GraphBuilder<'errors> {
    ast: AstBuilder,
    errors: Rc<Errors<'errors>>,

    graph: Graph,

    cfg: Cfg,
    symbol_table: SymbolTableStack,
    symbol_dump: Vec<(Symbol, DataID)>,
    jump_table: JumpTableStack,

    raw_item_table: HashMap<Symbol, Item>,
}

impl<'errors> GraphBuilder<'errors> {
    fn new(
        ast: AstBuilder,
        errors: Rc<Errors<'errors>>,
        arena: Bump,
        raw_item_table: HashMap<Symbol, Item>,
    ) -> (Self, CtrlCursor) {
        let graph = Graph::new(arena);
        let (cfg, start) = Cfg::new();
        let start = CtrlCursor {
            block: start,
            ctrl: graph.start(),
        };
        (
            Self {
                ast,
                errors,
                graph,
                cfg,
                symbol_table: SymbolTableStack::new(),
                symbol_dump: vec![],
                jump_table: JumpTableStack::new(),
                raw_item_table,
            },
            start,
        )
    }

    fn scope_stmt_pot_divergent(
        &mut self,
        stmt: ScopeStmt,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        match self.ast.scope_stmt(stmt).val.clone() {
            ScopeStmtKind::StmtExpr(stmt_expr) => self.stmt_expr_pot_divergent(stmt_expr, cursor),
            _ => Some(self.scope_stmt(stmt, cursor)),
        }
    }

    fn scope_stmt(&mut self, stmt: ScopeStmt, cursor: CtrlCursor) -> DataCursor {
        match self.ast.scope_stmt(stmt).val.clone() {
            ScopeStmtKind::Binding {
                keyword,
                mutable,
                ident,
                definition,
            } => self
                .binding(keyword, mutable, ident, definition, cursor)
                .with_data(self.graph.unit()),
            ScopeStmtKind::StmtExpr(stmt_expr) => self.stmt_expr(stmt_expr, cursor),
            _ => todo!(),
        }
    }

    fn stmt_expr(&mut self, stmt_expr: StmtExpr, cursor: CtrlCursor) -> DataCursor {
        let stmt_expr = self.ast.stmt_expr(stmt_expr);
        match stmt_expr.val.clone() {
            StmtExprKind::Assignment {
                ident,
                assignment: Assignment { equal, value },
            } => self
                .assignment(ident, equal, value, cursor)
                .with_data(self.graph.unit()),
            StmtExprKind::Expr(expr) => self.expr(expr, cursor),
            _ => self.divergent_control_flow(stmt_expr.span, cursor),
        }
    }

    fn stmt_expr_pot_divergent(
        &mut self,
        stmt_expr: StmtExpr,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        match self.ast.stmt_expr(stmt_expr).val.clone() {
            StmtExprKind::Continue(JumpStruct {
                keyword,
                label,
                value,
            }) => {
                self.continue_stmt(keyword, label, cursor);
                None
            }
            StmtExprKind::Break(JumpStruct {
                keyword,
                label,
                value,
            }) => {
                self.break_stmt(keyword, label, value, cursor);
                None
            }
            StmtExprKind::Return(JumpStruct {
                keyword,
                label,
                value,
            }) => todo!(),
            StmtExprKind::Unreachable => None,
            StmtExprKind::Expr(expr) => match self.ast.expr(expr).val.clone() {
                ExprKind::Block { stmts } => {
                    let open_scope = self.symbol_table.open_scope();

                    let mut stmts = stmts.into_iter().peekable();
                    let mut cursor = cursor;
                    loop {
                        let stmt = stmts.next().unwrap();
                        if stmts.peek().is_none() {
                            match self.scope_stmt_pot_divergent(stmt, cursor.clone()) {
                                Some(value) => {
                                    self.symbol_table.close_scope(
                                        open_scope,
                                        &mut self.symbol_dump,
                                        |ty, var| {
                                            self.cfg.read_variable(
                                                ty,
                                                cursor.block,
                                                var,
                                                expr.clone(),
                                                &mut self.graph,
                                            )
                                        },
                                    );
                                    return Some(value);
                                }
                                None => {
                                    self.symbol_table.close_scope(
                                        open_scope,
                                        &mut self.symbol_dump,
                                        |ty, var| {
                                            self.cfg.read_variable(
                                                ty,
                                                cursor.block,
                                                var,
                                                expr.clone(),
                                                &mut self.graph,
                                            )
                                        },
                                    );
                                    return None;
                                }
                            }
                        } else {
                            cursor = self.scope_stmt(stmt, cursor).without_data();
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
                ExprKind::Loop(ControlStruct { body, .. }) => self.loop_stmt(None, body, cursor),
                _ => Some(self.expr(expr, cursor)),
            },
            _ => Some(self.stmt_expr(stmt_expr, cursor)),
        }
    }

    fn expr(&mut self, expr: Expr, cursor: CtrlCursor) -> DataCursor {
        let expression = self.ast.expr(expr);
        match expression.val.clone() {
            ExprKind::BuiltinType(builtin_type) => {
                let ty = self.graph.add_builtin_type(builtin_type);
                cursor.with_data(self.graph.type_as_data(ty))
            }
            ExprKind::Literal(literal) => cursor.with_data(self.graph.add_literal(literal)),
            ExprKind::Boolean(boolean) => cursor.with_data(self.graph.add_boolean(boolean)),
            ExprKind::Quote(quote) => todo!(),
            ExprKind::Unit => cursor.with_data(self.graph.unit()),

            ExprKind::Unary { op, value: input } => {
                let (cursor, value) = self.expr(input, cursor).split();
                let ty = value.ty.clone();
                cursor.with_data(self.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(lhs, cursor).split();
                let (cursor, rhs) = self.expr(rhs, cursor).split();
                let ty = lhs.ty.clone();
                cursor.with_data(self.graph.add_binary(op.val, lhs, rhs, ty))
            }

            ExprKind::Ident(symbol) => match self.symbol_table.get_binding(symbol) {
                Some(Binding { id, ty, .. }) => {
                    match self.cfg.read_variable(
                        ty.clone(),
                        cursor.block,
                        *id,
                        expr.clone(),
                        &mut self.graph,
                    ) {
                        Some(mut value) => {
                            if value.kind == DataKind::Error {
                                // Placeholder because of Loop
                                value.ty = ty.clone();
                            }
                            cursor.with_data(value)
                        }
                        None => self.uninitialized_moved_variable(expression.span, cursor),
                    }
                }
                None => match self.raw_item_table.get(&symbol) {
                    Some(_) => todo!(),
                    None => self.unknown_identifier(
                        Spanned {
                            span: expression.span,
                            val: symbol,
                        },
                        cursor,
                    ),
                },
            },

            ExprKind::Block { stmts: statements } => {
                let open_scope = self.symbol_table.open_scope();

                let mut statements = statements.into_iter().peekable();
                let mut cursor = cursor;
                loop {
                    let stmt = statements.next().unwrap();
                    if statements.peek().is_none() {
                        let DataCursor {
                            block: cursor,
                            ctrl,
                            data,
                        } = self.scope_stmt(stmt, cursor);
                        self.symbol_table.close_scope(
                            open_scope,
                            &mut self.symbol_dump,
                            |ty, var| {
                                self.cfg.read_variable(
                                    ty,
                                    cursor,
                                    var,
                                    expr.clone(),
                                    &mut self.graph,
                                )
                            },
                        );

                        return DataCursor {
                            block: cursor,
                            ctrl,
                            data,
                        };
                    } else {
                        cursor = self.scope_stmt(stmt, cursor).without_data();
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
                let label_span = label.at_sign - label.ident.span;
                match self.loop_stmt(Some(label), body, cursor.clone()) {
                    Some(cursor) => cursor,
                    None => self.divergent_control_flow(label_span, cursor),
                }
            }
            ExprKind::Loop(ControlStruct { keyword, body }) => {
                match self.loop_stmt(None, body, cursor.clone()) {
                    Some(cursor) => cursor,
                    None => self.divergent_control_flow(keyword, cursor),
                }
            }
            ExprKind::Err => cursor.with_data(self.graph.error()),
        }
    }

    fn type_expr(&mut self, expr: Expr) -> TypeID {
        let expression = self.ast.expr(expr);
        match expression.val.clone() {
            ExprKind::BuiltinType(builtin_type) => self.graph.add_builtin_type(builtin_type),
            _ => {
                self.errors.push(expression.span, ErrorCode::ExpectedType);
                self.graph.error_type()
            }
        }
    }

    fn binding(
        &mut self,
        keyword: Span,
        mutable: bool,
        ident: Ident,
        definition: Definition,

        cursor: CtrlCursor,
    ) -> CtrlCursor {
        match definition {
            Definition::Type { ty, assignment } => {
                let ty = self.type_expr(ty);
                if let Some(var) =
                    self.symbol_table
                        .add_symbol_to_scope(mutable, ident.val, ty.clone())
                {
                    match assignment {
                        Some(Assignment { value, .. }) => {
                            let span = self.ast.expr(value).span;
                            let DataCursor { block, ctrl, data } = self.expr(value.clone(), cursor);
                            let value = require_type(&self.graph, span, ty, data, &mut self.errors);

                            self.cfg.assign_variable(block, var, value.clone());
                            CtrlCursor { block, ctrl }
                        }
                        None => cursor,
                    }
                } else {
                    self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                    cursor
                }
            }
            Definition::Assignment(Assignment { equal, value }) => {
                let DataCursor { block, ctrl, data } = self.expr(value, cursor);

                if let Some(var) =
                    self.symbol_table
                        .add_symbol_to_scope(mutable, ident.val, data.ty.clone())
                {
                    self.cfg.assign_variable(block, var, data);
                    CtrlCursor { block, ctrl }
                } else {
                    self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                    CtrlCursor { block, ctrl }
                }
            }
        }
    }

    fn assignment(
        &mut self,
        ident: Ident,
        equal: Span,
        value: Expr,
        cursor: CtrlCursor,
    ) -> CtrlCursor {
        let span = self.ast.expr(value).span;
        let DataCursor { block, ctrl, data } = self.expr(value.clone(), cursor);
        if let Some(binding) = self.symbol_table.get_binding(ident.val) {
            if binding.mutable {
                let ty = binding.ty.clone();
                let value = require_type(&self.graph, span, ty, data, &mut self.errors);
                self.cfg.assign_variable(block, binding.id, value.clone());
            } else {
                self.errors.push(
                    equal,
                    ErrorCode::AssignmentToImmutableIdent { symbol: ident.val },
                )
            }
        } else {
            self.errors.push(
                ident.span,
                ErrorCode::AssignmentToUnknownIdent { symbol: ident.val },
            );
        }
        CtrlCursor { block, ctrl }
    }

    fn continue_stmt(&mut self, keyword: Span, label: Option<Label>, cursor: CtrlCursor) {
        let Some(block) = self.jump_table.get(label.as_ref().map(|l| l.ident.val)) else {
            self.errors.push(
                keyword,
                label.map_or(ErrorCode::ContinueOutsideLoop, |l| {
                    ErrorCode::ContinueWithUnknownLabel { label: l.ident.val }
                }),
            );
            return;
        };

        let CtrlCursor {
            block: cursor,
            ctrl,
        } = cursor;
        block.continue_jumps.push(CtrlCursor {
            block: cursor,
            ctrl,
        });
    }

    fn break_stmt(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
        cursor: CtrlCursor,
    ) {
        let DataCursor {
            block: cursor,
            ctrl,
            data,
        } = match value {
            Some(value) => self.expr(value, cursor),
            None => cursor.with_data(self.graph.unit()),
        };
        let Some(block) = self.jump_table.get(label.as_ref().map(|l| l.ident.val)) else {
            self.errors.push(
                keyword,
                label.map_or(ErrorCode::BreakOutsideLoop, |l| {
                    ErrorCode::BreakWithUnknownLabel { label: l.ident.val }
                }),
            );
            return;
        };

        block.break_jumps.push(DataCursor {
            block: cursor,
            ctrl,
            data,
        });
    }

    fn if_stmt(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<ControlStruct>,
        cursor: CtrlCursor,
    ) -> DataCursor {
        let condition_cursor = self.expr(condition, cursor);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor, &mut self.cfg);

        let Some(cursor_when_true) = self.stmt_expr_pot_divergent(when_body, true_branch) else {
            return if let Some(ControlStruct {
                body: else_body, ..
            }) = else_body
            {
                self.stmt_expr(else_body, false_branch)
            } else {
                false_branch.with_data(self.graph.unit())
            };
        };

        if let Some(ControlStruct {
            body: else_body, ..
        }) = else_body
        {
            let Some(cursor_when_false) = self.stmt_expr_pot_divergent(else_body, false_branch)
            else {
                return cursor_when_true;
            };

            self.graph
                .merge(vec![cursor_when_false, cursor_when_true], &mut self.cfg)
                .unwrap() // we dont put in an empty vec
        } else {
            self.graph
                .merge(
                    vec![false_branch.with_data(self.graph.unit()), cursor_when_true],
                    &mut self.cfg,
                )
                .unwrap()
        }
    }

    fn if_stmt_pot_divergent(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<ControlStruct>,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let condition_cursor = self.expr(condition, cursor);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor, &mut self.cfg);

        let Some(cursor_when_true) = self.stmt_expr_pot_divergent(when_body, true_branch) else {
            return if let Some(ControlStruct {
                body: else_body, ..
            }) = else_body
            {
                self.stmt_expr_pot_divergent(else_body, false_branch)
            } else {
                Some(false_branch.with_data(self.graph.unit()))
            };
        };

        if let Some(ControlStruct {
            body: else_body, ..
        }) = else_body
        {
            let Some(cursor_when_false) = self.stmt_expr_pot_divergent(else_body, false_branch)
            else {
                return Some(cursor_when_true);
            };

            self.graph
                .merge(vec![cursor_when_false, cursor_when_true], &mut self.cfg)
        } else {
            self.graph.merge(
                vec![false_branch.with_data(self.graph.unit()), cursor_when_true],
                &mut self.cfg,
            )
        }
    }

    fn loop_stmt(
        &mut self,
        label: Option<Label>,
        body: StmtExpr,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let tok = self
            .jump_table
            .open_loop(label.as_ref().map(|l| l.ident.val));
        let body_cursor = self.graph.open_loop(&tok, &mut self.cfg);
        let header_block = body_cursor.block;

        let body = self.stmt_expr_pot_divergent(body, body_cursor); // parse the hole body

        let backedges = self.jump_table.close_loop(tok);
        self.graph.close_loop(
            cursor,
            body,
            header_block,
            backedges,
            label.is_none(),
            &mut self.cfg,
            &mut self.errors,
            &self.ast,
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

    fn unknown_identifier(&mut self, ident: Ident, cursor: CtrlCursor) -> DataCursor {
        self.errors
            .push(ident.span, ErrorCode::UnknownIdent { symbol: ident.val });
        cursor.with_data(self.graph.error())
    }
}

#[cfg(never)]
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
        let when_body = ast.expr_as_stmt_expr(one);
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
        let body = ast.expr_as_stmt_expr(unit);
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
        let loop_stmt = ast.expr_as_stmt_expr(loop_expr);
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
