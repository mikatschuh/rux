use std::{collections::HashMap, vec};

use parser::{
    Assignment, AstBuilder, ControlStruct, Definition, Expr, ExprKind, Ident, Item, JumpStruct,
    Label, ScopeStmt, ScopeStmtKind, Spanned, StmtExpr, StmtExprKind,
};
use tokenizer::{Interner, Span, Symbol, TypeSize};

use crate::{
    binding::{Binding, SymbolTableStack},
    builder::{Cfg, CtrlCursor, CtrlCursors, DataCursor},
    jumps::{Jumps, LoopBlockStack},
    type_check::require_type,
};

mod binding;
mod builder;
mod error;
mod graph;
pub mod graph_dump;
mod jumps;
mod type_check;

pub use error::Error;
pub use graph::{Data, Graph, Type};

pub trait Diagnostics {
    fn add(&mut self, span: Span, err: Error);
}

pub fn build_graph_debug<D: Diagnostics>(
    ast: AstBuilder,
    mut item_table: HashMap<Symbol, Item>,
    interner: Interner,
    starting_point: Symbol,
    mut errors: D,
    target_ptr_size: TypeSize,
) -> (String, D) {
    let Item::Constant {
        ident: _,
        definition:
            Definition::Type {
                ty: _,
                assignment: Some(Assignment { value, .. }),
            },
        ..
    } = (match item_table.remove(&starting_point) {
        Some(item) => item,
        None => {
            errors.add(
                Span::beginning(),
                Error::MissingEntryPoint {
                    entry: starting_point,
                },
            );
            return ("".to_string(), errors);
        }
    })
    else {
        todo!()
    };

    let (mut builder, cursor) = GraphBuilder::new(ast, errors, item_table, target_ptr_size);
    let cursor = builder.expr(value, cursor);

    (
        graph_dump::dump_text(&builder.graph, builder.symbol_dump, Some(cursor), &interner),
        builder.errors,
    )
}

struct GraphBuilder<D: Diagnostics> {
    ast: AstBuilder,
    errors: D,

    graph: Graph,

    cfg: Cfg,
    symbol_table: SymbolTableStack,
    symbol_dump: Vec<(Symbol, Data)>,
    jump_table: LoopBlockStack,

    raw_item_table: HashMap<Symbol, Item>,
}

impl<D: Diagnostics> GraphBuilder<D> {
    fn new(
        ast: AstBuilder,
        errors: D,
        raw_item_table: HashMap<Symbol, Item>,
        target_ptr_size: TypeSize,
    ) -> (Self, CtrlCursor) {
        let graph = Graph::new(target_ptr_size);
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
                jump_table: LoopBlockStack::new(),
                raw_item_table,
            },
            start,
        )
    }

    fn scope_stmt_could_diverge(
        &mut self,
        stmt: ScopeStmt,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        match &self.ast[&stmt].val {
            ScopeStmtKind::StmtExpr(stmt_expr) => {
                self.stmt_expr_could_diverge(stmt_expr.clone(), cursor)
            }
            _ => Some(self.scope_stmt(stmt, cursor)),
        }
    }

    fn scope_stmt(&mut self, stmt: ScopeStmt, cursor: CtrlCursor) -> DataCursor {
        match &self.ast[&stmt].val {
            ScopeStmtKind::Binding {
                keyword,
                mutable,
                ident,
                definition,
            } => self
                .binding(
                    *keyword,
                    *mutable,
                    ident.clone(),
                    definition.clone(),
                    cursor,
                )
                .with_data(self.graph.unit()),
            ScopeStmtKind::StmtExpr(stmt_expr) => self.stmt_expr(stmt_expr.clone(), cursor),
            _ => todo!(),
        }
    }

    fn stmt_expr(&mut self, stmt_expr: StmtExpr, cursor: CtrlCursor) -> DataCursor {
        let stmt_expr = &self.ast[&stmt_expr];
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

    fn stmt_expr_could_diverge(
        &mut self,
        stmt_expr: StmtExpr,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        match self.ast[&stmt_expr].val.clone() {
            StmtExprKind::Continue(JumpStruct {
                keyword,
                label,
                value: _,
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
                keyword: _,
                label: _,
                value: _,
            }) => todo!(),
            StmtExprKind::Unreachable => None,
            StmtExprKind::Expr(expr) => match self.ast[&expr].val.clone() {
                ExprKind::Block { stmts } => {
                    let open_scope = self.symbol_table.open_scope();

                    let mut stmts = stmts.into_iter().peekable();
                    let mut cursor = cursor;
                    loop {
                        let stmt = stmts.next().unwrap();
                        if stmts.peek().is_none() {
                            let final_value =
                                self.scope_stmt_could_diverge(stmt.clone(), cursor.clone());

                            self.symbol_table.close_scope(
                                open_scope,
                                &mut self.symbol_dump,
                                |ty, var| {
                                    self.cfg.get_definition(
                                        var,
                                        final_value
                                            .as_ref()
                                            .map_or(cursor.block.clone(), |v| v.block.clone()),
                                        ty,
                                        expr.clone(),
                                        &mut self.graph,
                                    )
                                },
                            );
                            return final_value;
                        } else {
                            cursor = self.scope_stmt(stmt.clone(), cursor).without_data();
                        }
                    }
                }
                ExprKind::If {
                    keyword,
                    condition,
                    when_body,
                    else_clause: else_body,
                } => self.if_stmt_could_diverge(keyword, condition, when_body, else_body, cursor),
                ExprKind::Label { label, body } => self.loop_stmt(Some(label), body, cursor),
                ExprKind::Loop(ControlStruct { body, .. }) => self.loop_stmt(None, body, cursor),
                _ => Some(self.expr(expr, cursor)),
            },
            _ => Some(self.stmt_expr(stmt_expr, cursor)),
        }
    }

    fn expr(&mut self, expr: Expr, cursor: CtrlCursor) -> DataCursor {
        let expression = &self.ast[&expr];
        match expression.val.clone() {
            ExprKind::BuiltinType(builtin_type) => {
                let ty = self.graph.add_builtin_type(builtin_type);
                cursor.with_data(self.graph.type_as_data(ty))
            }
            ExprKind::Literal(literal) => cursor.with_data(self.graph.add_literal(literal)),
            ExprKind::Boolean(boolean) => cursor.with_data(self.graph.add_boolean(boolean)),
            ExprKind::Quote(..) => todo!("implement quotes"),
            ExprKind::Unit => cursor.with_data(self.graph.unit()),

            ExprKind::Unary { op, value: input } => {
                let (cursor, value) = self.expr(input, cursor).split();
                let ty = self.graph[&value].ty.clone();
                cursor.with_data(self.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(lhs, cursor).split();
                let (cursor, rhs) = self.expr(rhs, cursor).split();
                let ty = self.graph[&lhs].ty.clone();
                cursor.with_data(self.graph.add_binary(op.val, lhs, rhs, ty))
            }
            ExprKind::FieldAccess { .. } => todo!("implement fields"),

            ExprKind::Ident(symbol) => match self.symbol_table.get_binding(symbol) {
                Some(Binding { id, ty, .. }) => {
                    match self.cfg.get_definition(
                        id.clone(),
                        cursor.block.clone(),
                        ty.clone(),
                        expr.clone(),
                        &mut self.graph,
                    ) {
                        Some(value) => cursor.with_data(value),
                        None => self.uninitialized_or_moved_variable(expression.span, cursor),
                    }
                }
                None => match self.raw_item_table.get(&symbol) {
                    Some(_) => todo!("implement item lookup"),
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
                        let DataCursor { block, ctrl, data } = self.scope_stmt(stmt, cursor);
                        self.symbol_table.close_scope(
                            open_scope,
                            &mut self.symbol_dump,
                            |ty, var| {
                                self.cfg.get_definition(
                                    var,
                                    block.clone(),
                                    ty,
                                    expr.clone(),
                                    &mut self.graph,
                                )
                            },
                        );

                        return DataCursor {
                            block: block.clone(),
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
                else_clause: else_body,
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
            ExprKind::Err => cursor.with_data(self.graph.err()),
        }
    }

    fn type_expr(&mut self, expr: Expr) -> Type {
        let expression = &self.ast[&expr];
        match expression.val.clone() {
            ExprKind::BuiltinType(builtin_type) => self.graph.add_builtin_type(builtin_type),
            _ => {
                self.errors.add(expression.span, Error::ExpectedType);
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
                            let span = self.ast[&value].span;
                            let DataCursor { block, ctrl, data } = self.expr(value.clone(), cursor);
                            let value = require_type(&self.graph, span, ty, data, &mut self.errors);

                            self.cfg.assign_variable(block.clone(), var, value.clone());
                            CtrlCursor { block, ctrl }
                        }
                        None => cursor,
                    }
                } else {
                    self.errors.add(keyword, Error::BindingOutsideScope);
                    cursor
                }
            }
            Definition::Assignment(Assignment { value, .. }) => {
                let DataCursor { block, ctrl, data } = self.expr(value, cursor);

                if let Some(var) = self.symbol_table.add_symbol_to_scope(
                    mutable,
                    ident.val,
                    self.graph[&data].ty.clone(),
                ) {
                    self.cfg.assign_variable(block.clone(), var, data);
                    CtrlCursor { block, ctrl }
                } else {
                    self.errors.add(keyword, Error::BindingOutsideScope);
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
        let span = self.ast[&value].span;
        let DataCursor { block, ctrl, data } = self.expr(value.clone(), cursor);
        if let Some(binding) = self.symbol_table.get_binding(ident.val) {
            if binding.mutable {
                let ty = binding.ty.clone();
                let value = require_type(&self.graph, span, ty, data, &mut self.errors);
                self.cfg
                    .assign_variable(block.clone(), binding.id.clone(), value.clone());
            } else {
                self.errors.add(
                    equal,
                    Error::AssignmentToImmutableIdent { symbol: ident.val },
                )
            }
        } else {
            self.errors.add(
                ident.span,
                Error::AssignmentToUnknownIdent { symbol: ident.val },
            );
        }
        CtrlCursor { block, ctrl }
    }

    fn continue_stmt(&mut self, keyword: Span, label: Option<Label>, cursor: CtrlCursor) {
        let Some(block) = self.jump_table.get(label.as_ref().map(|l| l.ident.val)) else {
            self.errors.add(
                keyword,
                label.map_or(Error::ContinueOutsideLoop, |l| {
                    Error::ContinueWithUnknownLabel { label: l.ident.val }
                }),
            );
            return;
        };

        let CtrlCursor {
            block: cursor,
            ctrl,
        } = cursor;
        block.continues.push(CtrlCursor {
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
            self.errors.add(
                keyword,
                label.map_or(Error::BreakOutsideLoop, |l| Error::BreakWithUnknownLabel {
                    label: l.ident.val,
                }),
            );
            return;
        };

        block.breaks.push(DataCursor {
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

        let Some(cursor_when_true) = self.stmt_expr_could_diverge(when_body, true_branch) else {
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
            let Some(cursor_when_false) = self.stmt_expr_could_diverge(else_body, false_branch)
            else {
                return cursor_when_true;
            };

            self.graph
                .merge(cursor_when_false.and(cursor_when_true), &mut self.cfg)
                .unwrap() // we dont put in an empty vec
        } else {
            self.graph
                .merge(
                    false_branch
                        .with_data(self.graph.unit())
                        .and(cursor_when_true),
                    &mut self.cfg,
                )
                .unwrap()
        }
    }

    fn if_stmt_could_diverge(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<ControlStruct>,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let condition_cursor = self.expr(condition, cursor);

        let (false_branch, true_branch) = self.graph.branch(condition_cursor, &mut self.cfg);

        let Some(cursor_when_true) = self.stmt_expr_could_diverge(when_body, true_branch) else {
            return if let Some(ControlStruct {
                body: else_body, ..
            }) = else_body
            {
                self.stmt_expr_could_diverge(else_body, false_branch)
            } else {
                Some(false_branch.with_data(self.graph.unit()))
            };
        };

        if let Some(ControlStruct {
            body: else_body, ..
        }) = else_body
        {
            let Some(cursor_when_false) = self.stmt_expr_could_diverge(else_body, false_branch)
            else {
                return Some(cursor_when_true);
            };

            self.graph
                .merge(cursor_when_false.and(cursor_when_true), &mut self.cfg)
        } else {
            self.graph.merge(
                false_branch
                    .with_data(self.graph.unit())
                    .and(cursor_when_true),
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
        // first create a location jumps can go to
        let tok = self
            .jump_table
            .open_loop_block(label.as_ref().map(|l| l.ident.clone()), &mut self.errors);
        let (unsealed_block, ctrl_placeholder) = self.cfg.add_unsealed(&mut self.graph);

        let end_of_body = self.stmt_expr_could_diverge(
            body,
            CtrlCursor {
                block: unsealed_block.clone(),
                ctrl: ctrl_placeholder,
            },
        ); // parse the body

        let Jumps {
            continues: mut backedges,
            breaks: mut exits,
        } = self.jump_table.close_loop_block(tok); // get the jumps out

        if let Some(end_of_body) = end_of_body {
            if label.is_none() {
                backedges.push(end_of_body.without_data());
            } else {
                exits.push(end_of_body);
            }
        }

        let CtrlCursors {
            blocks: mut entry_blocks,
            ctrls: mut entry_ctrls,
        } = backedges;

        entry_blocks.push(cursor.block);
        entry_ctrls.push(cursor.ctrl);

        self.cfg.seal_block(
            unsealed_block,
            entry_blocks,
            entry_ctrls,
            &mut self.graph,
            &mut self.errors,
            &self.ast,
        );

        self.graph.merge(exits, &mut self.cfg)
    }

    fn divergent_control_flow(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.add(span, Error::DivergentControlFlow);
        cursor.with_data(self.graph.err())
    }

    fn uninitialized_or_moved_variable(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.add(span, Error::ReadUnitializedOrMoved);
        cursor.with_data(self.graph.err())
    }

    fn unknown_identifier(&mut self, ident: Ident, cursor: CtrlCursor) -> DataCursor {
        self.errors
            .add(ident.span, Error::UnknownIdent { symbol: ident.val });
        cursor.with_data(self.graph.err())
    }
}

#[cfg(any())]
#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::Path};

    use crate::{
        error::{Errors, Span},
        grapher::graph::{Data, DataKind},
        literal_parsing::Literal,
        parser::{AstBuilder, BuiltinType, Expr, Interner, Label, Spanned, Symbol},
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

    fn with_built_expr<R>(expr: Expr, arena: bumpalo::Bump, f: impl FnOnce(&Data) -> R) -> R {
        let errors = Errors::empty(Path::new("grapher-test.rx"));
        let (mut builder, cursor) = GraphBuilder::new(errors, arena, HashMap::new());
        let data = builder.expr(cursor, expr).data;

        f(&data)
    }

    fn literal_value(node: &Data) -> Option<Literal> {
        match &node.kind {
            DataKind::Literal { literal } => Some(literal.clone()),
            _ => None,
        }
    }

    fn kind_name(node: &Data) -> &'static str {
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
            DataKind::Err => "error",
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
