use std::{collections::HashMap, vec};

use parser::{
    Assignment, AstBuilder, ControlStruct, Definition, Expr, ExprKind, Ident, Item, JumpStruct,
    Label, ScopeStmt, ScopeStmtKind, Spanned, StmtExpr, StmtExprKind,
};
use tokenizer::{Interner, Span, Symbol, TypeSize};

use crate::{
    binding::{Binding, Mutability, SymbolTableStack},
    builder::{Cfg, CtrlCursor, DataCursor},
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
pub use graph::{
    Branch, BranchKind, Ctrl, CtrlKind, CtrlPlaceholder, Data, DataKind, DataPlaceholder, Graph,
    Merge, MergeKind, Type, TypeKey, TypeKind,
};

pub trait Diagnostics {
    fn add(&mut self, span: Span, err: Error);
}

pub fn build_graph_debug<'src, D: Diagnostics>(
    ast: &AstBuilder<'src>,
    mut item_table: HashMap<Symbol, Item>,
    interner: Interner<'src>,
    starting_point: Symbol,
    errors: D,
    target_ptr_size: TypeSize,
) -> Option<(String, D)> {
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
            return None;
        }
    })
    else {
        todo!()
    };

    let (mut builder, cursor) = GraphBuilder::new(ast, errors, item_table, target_ptr_size);
    let cursor = builder.expr(value, cursor);

    Some((
        graph_dump::dump_text(&builder.graph, builder.symbol_dump, Some(cursor), &interner),
        builder.errors,
    ))
}

struct GraphBuilder<'ast, 'src, D: Diagnostics> {
    // Keep AST borrows independent of mutable graph-building state.
    ast: &'ast AstBuilder<'src>,
    errors: D,

    graph: Graph<'src>,

    cfg: Cfg,
    symbol_table: SymbolTableStack,
    symbol_dump: Vec<(Symbol, Data)>,
    jump_table: LoopBlockStack,

    raw_item_table: HashMap<Symbol, Item>,
}

impl<'ast, 'src, D: Diagnostics> GraphBuilder<'ast, 'src, D> {
    fn new(
        ast: &'ast AstBuilder<'src>,
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
        match &self.ast[stmt].val {
            ScopeStmtKind::StmtExpr(stmt_expr) => self.stmt_expr_could_diverge(*stmt_expr, cursor),
            _ => Some(self.scope_stmt(stmt, cursor)),
        }
    }

    fn scope_stmt(&mut self, stmt: ScopeStmt, cursor: CtrlCursor) -> DataCursor {
        match &self.ast[stmt].val {
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
            ScopeStmtKind::StmtExpr(stmt_expr) => self.stmt_expr(*stmt_expr, cursor),
            _ => todo!(),
        }
    }

    fn stmt_expr(&mut self, stmt_expr: StmtExpr, cursor: CtrlCursor) -> DataCursor {
        let stmt_expr = &self.ast[stmt_expr];
        match &stmt_expr.val {
            StmtExprKind::Assignment {
                ident,
                assignment: Assignment { equal, value },
            } => self
                .assignment(ident.clone(), *equal, *value, cursor)
                .with_data(self.graph.unit()),
            StmtExprKind::Expr(expr) => self.expr(*expr, cursor),
            _ => self.divergent_control_flow(stmt_expr.span, cursor),
        }
    }

    fn stmt_expr_could_diverge(
        &mut self,
        stmt_expr: StmtExpr,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        match &self.ast[stmt_expr].val {
            StmtExprKind::Continue(JumpStruct {
                keyword,
                label,
                value: _,
            }) => {
                self.continue_stmt(*keyword, label.clone(), cursor);
                None
            }
            StmtExprKind::Break(JumpStruct {
                keyword,
                label,
                value,
            }) => {
                self.break_stmt(*keyword, label.clone(), *value, cursor);
                None
            }
            StmtExprKind::Return(JumpStruct {
                keyword: _,
                label: _,
                value: _,
            }) => todo!(),
            StmtExprKind::Unreachable => None,
            StmtExprKind::Expr(expr) => self.expr_could_diverge(*expr, cursor),
            _ => Some(self.stmt_expr(stmt_expr, cursor)),
        }
    }

    fn expr_could_diverge(&mut self, expr: Expr, cursor: CtrlCursor) -> Option<DataCursor> {
        match &self.ast[expr].val {
            ExprKind::Block { stmts } => {
                let open_scope = self.symbol_table.open_scope();

                let mut stmts = stmts.iter().cloned().peekable();
                let mut cursor = cursor;
                loop {
                    let stmt = stmts.next().unwrap(); // starts always nonempty
                    if stmts.peek().is_none() {
                        let current_block = cursor.block;
                        let last_stmt = self.scope_stmt_could_diverge(stmt, cursor);
                        let current_block = last_stmt.as_ref().map_or(current_block, |v| v.block);

                        self.symbol_table.close_scope(
                            open_scope,
                            &mut self.symbol_dump,
                            |ty, var| {
                                self.cfg.get_definition(
                                    current_block,
                                    var,
                                    ty,
                                    expr,
                                    &mut self.graph,
                                )
                            },
                        );
                        return last_stmt;
                    } else {
                        cursor = self.scope_stmt(stmt, cursor).without_data();
                    }
                }
            }
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_clause,
            } => self.if_stmt_could_diverge(
                *keyword,
                *condition,
                *when_body,
                else_clause.clone(),
                cursor,
            ),
            ExprKind::Label { label, body } => self.loop_stmt(Some(label.clone()), *body, cursor),
            ExprKind::Loop(ControlStruct { body, .. }) => self.loop_stmt(None, *body, cursor),
            _ => Some(self.expr(expr, cursor)),
        }
    }

    fn expr(&mut self, expr: Expr, cursor: CtrlCursor) -> DataCursor {
        let expression = &self.ast[expr];
        match &expression.val {
            ExprKind::BuiltinType(_) => todo!(),
            ExprKind::Literal(literal) => cursor.with_data(self.graph.add_literal(literal.clone())),
            ExprKind::Boolean(boolean) => cursor.with_data(self.graph.add_boolean(*boolean)),
            ExprKind::Quote(..) => todo!("implement quotes"),
            ExprKind::Unit => cursor.with_data(self.graph.unit()),

            ExprKind::Unary { op, value: input } => {
                let (cursor, value) = self.expr(*input, cursor).split();
                let ty = self.graph.get_type(value);
                cursor.with_data(self.graph.add_unary(op.val, value, ty))
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let (cursor, lhs) = self.expr(*lhs, cursor).split();
                let (cursor, rhs) = self.expr(*rhs, cursor).split();
                let ty = self.graph.get_type(lhs); // todo
                cursor.with_data(self.graph.add_binary(op.val, lhs, rhs, ty))
            }
            ExprKind::FieldAccess { .. } => todo!("implement fields"),

            ExprKind::Ident(symbol) => match self.symbol_table.get_binding(*symbol) {
                Some(Binding { var, ty, .. }) => {
                    match self
                        .cfg
                        .get_definition(cursor.block, *var, *ty, expr, &mut self.graph)
                    {
                        Some(value) => cursor.with_data(value),
                        None => self.uninitialized_or_moved_variable(expression.span, cursor),
                    }
                }
                None => match self.raw_item_table.get(symbol) {
                    Some(_) => todo!("implement item lookup"),
                    None => self.unknown_identifier(
                        Spanned {
                            span: expression.span,
                            val: *symbol,
                        },
                        cursor,
                    ),
                },
            },

            ExprKind::Block { stmts: statements } => {
                let open_scope = self.symbol_table.open_scope();

                let mut statements = statements.iter().copied().peekable();
                let mut cursor = cursor;
                loop {
                    let stmt = statements.next().unwrap();
                    if statements.peek().is_none() {
                        let last_stmt = self.scope_stmt(stmt, cursor);
                        self.symbol_table.close_scope(
                            open_scope,
                            &mut self.symbol_dump,
                            |ty, var| {
                                self.cfg.get_definition(
                                    last_stmt.block,
                                    var,
                                    ty,
                                    expr,
                                    &mut self.graph,
                                )
                            },
                        );

                        return last_stmt;
                    } else {
                        cursor = self.scope_stmt(stmt, cursor).without_data();
                    }
                }
            }
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_clause,
            } => self.if_stmt(
                *keyword,
                *condition,
                *when_body,
                else_clause.clone(),
                cursor,
            ),
            ExprKind::Label { label, body } => {
                match self.loop_stmt(Some(label.clone()), *body, cursor.clone()) {
                    Some(cursor) => cursor,
                    None => self.divergent_control_flow(label.at_sign - label.ident.span, cursor),
                }
            }
            ExprKind::Loop(ControlStruct { keyword, body }) => {
                match self.loop_stmt(None, *body, cursor.clone()) {
                    Some(cursor) => cursor,
                    None => self.divergent_control_flow(*keyword, cursor),
                }
            }
            ExprKind::Err => cursor.with_data(self.graph.err()),
        }
    }

    fn type_expr(&mut self, expr: Expr) -> Type {
        let expression = &self.ast[expr];
        match &expression.val {
            ExprKind::BuiltinType(builtin_type) => self.graph.add_builtin_type(*builtin_type),
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
                if let Some(var) = self
                    .symbol_table
                    .add_symbol_to_scope(mutable, ident.val, ty)
                {
                    if let Some(Assignment { value: expr, .. }) = assignment {
                        let (cursor, value) = self.expr(expr, cursor).split();

                        let span = self.ast[expr].span;
                        let value = require_type(&self.graph, span, ty, value, &mut self.errors);
                        self.cfg.assign_variable(cursor.block, var, value);

                        return cursor;
                    }
                } else {
                    self.errors.add(keyword, Error::BindingOutsideScope);
                }
                cursor
            }
            Definition::Assignment(Assignment { value: expr, .. }) => {
                let (cursor, value) = self.expr(expr, cursor).split();

                if let Some(var) = self.symbol_table.add_symbol_to_scope(
                    mutable,
                    ident.val,
                    self.graph.get_type(value),
                ) {
                    self.cfg.assign_variable(cursor.block, var, value);
                } else {
                    self.errors.add(keyword, Error::BindingOutsideScope);
                }
                cursor
            }
        }
    }

    fn assignment(
        &mut self,
        ident: Ident,
        equal: Span,
        expr: Expr,
        cursor: CtrlCursor,
    ) -> CtrlCursor {
        let (cursor, value) = self.expr(expr, cursor).split();

        if let Some(binding) = self.symbol_table.get_binding_mut(ident.val) {
            let span = self.ast[expr].span;
            let value = require_type(&self.graph, span, binding.ty, value, &mut self.errors);

            match binding.mutability {
                Mutability::Mutable => {
                    self.cfg.assign_variable(cursor.block, binding.var, value);
                }
                Mutability::ImmutableUninitialized => {
                    self.cfg.assign_variable(cursor.block, binding.var, value);
                    binding.mutability = Mutability::ImmutableInitialized; // lock the assignment in place
                }
                Mutability::ImmutableInitialized => self.errors.add(
                    equal,
                    Error::AssignmentToImmutableIdent { symbol: ident.val },
                ),
            }
        } else {
            self.errors.add(
                ident.span,
                Error::AssignmentToUnknownIdent { symbol: ident.val },
            );
        }
        cursor
    }

    fn continue_stmt(&mut self, keyword: Span, label: Option<Label>, cursor: CtrlCursor) {
        let Some(jumps) = self.jump_table.get(label.as_ref().map(|l| l.ident.val)) else {
            self.errors.add(
                keyword,
                label.map_or(Error::ContinueOutsideLoop, |l| {
                    Error::ContinueWithUnknownLabel { label: l.ident.val }
                }),
            );
            return;
        };

        jumps.continues.push(cursor);
    }

    fn break_stmt(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
        cursor: CtrlCursor,
    ) {
        let value = match value {
            Some(value) => self.expr(value, cursor),
            None => cursor.with_data(self.graph.unit()),
        };
        let Some(jumps) = self.jump_table.get(label.as_ref().map(|l| l.ident.val)) else {
            self.errors.add(
                keyword,
                label.map_or(Error::BreakOutsideLoop, |l| Error::BreakWithUnknownLabel {
                    label: l.ident.val,
                }),
            );
            return;
        };

        jumps.breaks.push(value);
    }

    fn if_stmt(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_clause: Option<ControlStruct>,
        cursor: CtrlCursor,
    ) -> DataCursor {
        let (false_branch, true_branch) = self
            .expr(condition, cursor)
            .branch(&mut self.graph, &mut self.cfg);

        let Some(cursor_when_true) = self.stmt_expr_could_diverge(when_body, true_branch) else {
            if let Some(ControlStruct { body, .. }) = else_clause {
                return self.stmt_expr(body, false_branch);
            } else {
                return false_branch.with_data(self.graph.unit());
            };
        };

        if let Some(ControlStruct { body, .. }) = else_clause {
            let Some(cursor_when_false) = self.stmt_expr_could_diverge(body, false_branch) else {
                return cursor_when_true;
            };

            cursor_when_false
                .and(cursor_when_true)
                .merge(&mut self.graph, &mut self.cfg)
        } else {
            false_branch
                .with_data(self.graph.unit())
                .and(cursor_when_true)
                .merge(&mut self.graph, &mut self.cfg)
        }
    }

    fn if_stmt_could_diverge(
        &mut self,
        _keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_clause: Option<ControlStruct>,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        let (false_branch, true_branch) = self
            .expr(condition, cursor)
            .branch(&mut self.graph, &mut self.cfg);

        let Some(cursor_when_true) = self.stmt_expr_could_diverge(when_body, true_branch) else {
            return if let Some(ControlStruct { body, .. }) = else_clause {
                self.stmt_expr_could_diverge(body, false_branch)
            } else {
                Some(false_branch.with_data(self.graph.unit()))
            };
        };

        if let Some(ControlStruct { body, .. }) = else_clause {
            let Some(cursor_when_false) = self.stmt_expr_could_diverge(body, false_branch) else {
                return Some(cursor_when_true);
            };

            let merged = cursor_when_false
                .and(cursor_when_true)
                .merge(&mut self.graph, &mut self.cfg);
            Some(merged)
        } else {
            let merged = false_branch
                .with_data(self.graph.unit())
                .and(cursor_when_true)
                .merge(&mut self.graph, &mut self.cfg);
            Some(merged)
        }
    }

    fn loop_stmt(
        &mut self,
        label: Option<Label>,
        body: StmtExpr,
        cursor: CtrlCursor,
    ) -> Option<DataCursor> {
        // first create a location jumps can go to
        let (tok, default_backedge) = match label {
            Some(label) => {
                let Some(tok) = self.jump_table.open_loop_block_labeled(label.ident.val) else {
                    self.errors.add(
                        label.at_sign - label.ident.span,
                        Error::LabelOverwrite {
                            label: label.ident.val,
                        },
                    );
                    return self.stmt_expr_could_diverge(body, cursor);
                };
                (tok, false)
            }
            None => (self.jump_table.open_loop_block(), true),
        };
        let (unsealed_block, ctrl_placeholder) = self.cfg.add_unsealed(&mut self.graph);

        let end_of_body = self.stmt_expr_could_diverge(
            body,
            CtrlCursor {
                block: unsealed_block,
                ctrl: ctrl_placeholder,
            },
        ); // parse the body

        let Jumps {
            continues: mut entrys,
            breaks: mut exits,
        } = self.jump_table.close_loop_block(tok); // get the jumps out

        if let Some(end_of_body) = end_of_body {
            if default_backedge {
                entrys.push(end_of_body.without_data());
            } else {
                exits.push(end_of_body);
            }
        }

        entrys.push(cursor);

        self.cfg.seal_block(
            unsealed_block,
            entrys,
            &mut self.graph,
            &mut self.errors,
            self.ast,
        );

        exits.merge(&mut self.graph, &mut self.cfg)
    }

    fn divergent_control_flow(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.add(span, Error::DivergentControlFlow);
        cursor.with_data(self.graph.err())
    }

    fn uninitialized_or_moved_variable(&mut self, span: Span, cursor: CtrlCursor) -> DataCursor {
        self.errors.add(span, Error::ReadEitherUnitializedOrMoved);
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

#[cfg(test)]
mod lifetime_tests {
    use super::*;

    struct NoErrors;

    impl tokenizer::Diagnostics for NoErrors {
        fn add(&mut self, span: Span, err: tokenizer::Error) {
            panic!("unexpected tokenizer error at {span:?}: {err:?}");
        }
    }

    impl parser::Diagnostics for NoErrors {
        fn add(&mut self, span: Span, err: parser::Error) {
            panic!("unexpected parser error at {span:?}: {err:?}");
        }
    }

    impl Diagnostics for NoErrors {
        fn add(&mut self, span: Span, err: Error) {
            panic!("unexpected graph error at {span:?}: {err:?}");
        }
    }

    #[test]
    fn graph_borrows_source_independently_of_ast() {
        let source = String::from("let main u64 = { 123suffix }");
        let (graph, data) = {
            let tokens = tokenizer::Tokenizer::new(&source, NoErrors, 64);
            let mut parsed = parser::parse(tokens, NoErrors);
            let main = parsed.interner.get("main");
            let Item::Constant {
                definition:
                    Definition::Type {
                        assignment: Some(Assignment { value, .. }),
                        ..
                    },
                ..
            } = parsed.item_table.remove(&main).unwrap()
            else {
                panic!("expected initialized constant");
            };
            let (mut builder, cursor) =
                GraphBuilder::new(&parsed.ast, NoErrors, parsed.item_table, 64);
            let data = builder.expr(value, cursor).data;
            (builder.graph, data)
        };

        // The parser output and AST are gone; the source still owns the suffix bytes.
        let graph::DataKind::Literal { literal } = &graph[data] else {
            panic!("expected literal");
        };
        assert_eq!(literal.suffix, "suffix");
        let suffix_offset = source.find("suffix").unwrap();
        assert_eq!(literal.suffix.as_ptr(), source[suffix_offset..].as_ptr());
    }
}
