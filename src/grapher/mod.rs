use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        builder::{Binding, Builder, binding::MutableState},
        graph::{CtrlID, DataID, TypeID},
    },
    parser::{Expr, ExprKind, Item, ParserOutput, Stmt, StmtExpr, StmtExprKind, StmtKind, Symbol},
    utilities::Rc,
};

mod builder;
#[allow(unused)]
mod graph;
pub mod graph_dump;
mod item;
// mod parser;

use bumpalo::Bump;
pub use {
    builder::{Error as BuildError, Result as BuildResult},
    graph::Graph,
};

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

    let mut builder = GraphBuilder::new(errors, arena, item_table);
    let value = builder.expr(expr);

    Some(graph_dump::dump_text(
        &builder.builder.graph,
        vec![value],
        builder.builder.symbol_table.symbol_dump(),
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
    ) -> Self {
        Self {
            errors,
            builder: Builder::new(arena),
            raw_item_table,
        }
    }

    fn stmt(&mut self, stmt: Stmt) -> DataID {
        match (*stmt.val).clone() {
            StmtKind::Binding {
                mutable,
                keyword,
                symbol,
                ty,
                equal,
                value,
            } => match ty {
                Some(ty) => {
                    let ty = self.type_expr(ty);
                    let value = self.expr(value);
                    if self
                        .builder
                        .symbol_table
                        .add_symbol(mutable, keyword, symbol.val, Binding { ty, value })
                        .is_err()
                    {
                        self.errors.push(keyword, ErrorCode::BindingOutsideScope);
                    };
                    self.builder.graph.add_unit()
                }
                None => todo!(),
            },
            StmtKind::StmtExpr { stmt_expr } => self.stmt_expr(stmt_expr),
        }
    }

    fn stmt_expr(&mut self, stmt_expr: StmtExpr) -> DataID {
        match (*stmt_expr.val).clone() {
            StmtExprKind::Assignment {
                symbol,
                equal,
                value,
            } => {
                let value = self.expr(value);
                self.builder
                    .symbol_table
                    .write_symbol(stmt_expr.span, symbol.val, value);
                self.builder.graph.add_unit()
            }
            StmtExprKind::Continue { keyword, label } => {
                match label {
                    Some(label) => todo!(), // self.builder.add_continue_to(keyword, label.label.val),
                    None => todo!(),        // self.builder.add_continue(keyword),
                };
                self.builder.graph.add_never()
            }
            StmtExprKind::Break {
                keyword,
                label,
                value,
            } => {
                let value = match value {
                    Some(value) => self.expr(value),
                    None => self.builder.graph.add_unit(),
                };
                match label {
                    Some(label) => todo!(), // self.builder.add_break_to(keyword, label.label.val, value),
                    None => todo!(),        // self.builder.add_break(keyword, value),
                };
                self.builder.graph.add_never()
            }
            StmtExprKind::Return { keyword, value } => todo!(),
            StmtExprKind::Unreachable => {
                self.builder.graph.make_unreachable();
                self.builder.graph.add_never()
            }
            StmtExprKind::ExprStmt { expr } => self.expr(expr),
        }
    }

    fn expr(&mut self, expr: Expr) -> DataID {
        match (*expr.val).clone() {
            ExprKind::BuiltinType(builtin_type) => {
                let ty = self.builder.graph.add_builtin_type(builtin_type);
                self.builder.graph.type_as_data(ty)
            }
            ExprKind::Literal(literal) => self.builder.graph.add_literal(literal),
            ExprKind::Boolean(boolean) => self.builder.graph.add_boolean(boolean),
            ExprKind::Quote(quote) => todo!(),
            ExprKind::Unit => self.builder.graph.add_unit(),

            ExprKind::Unary { op, value: input } => {
                let value = self.expr(input);
                let ty = value.ty.clone();
                self.builder.graph.add_unary(op.val, value, ty)
            }
            ExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);
                let ty = lhs.ty.clone();
                self.builder.graph.add_binary(op.val, lhs, rhs, ty)
            }

            ExprKind::Ident(symbol) => match self.builder.symbol_table.read_symbol(symbol) {
                Some(value) => value,
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
                    None => todo!(),
                },
            },

            ExprKind::Block { statements } => {
                let open_scope = self.builder.symbol_table.open_scope();

                let mut statements = statements.into_iter().peekable();
                loop {
                    let value = self.stmt(statements.next().unwrap());
                    if statements.peek().is_none() {
                        self.builder.symbol_table.close_scope(open_scope);
                        break value;
                    }
                }
            }
            ExprKind::Function {
                keyword,
                parameters,
                output,
                body,
            } => todo!(),

            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_body,
            } => {
                let condition = self.expr(condition);

                let (state_before_branch, before_branch) = self.current_state();
                let (false_branch, true_branch) = self
                    .builder
                    .graph
                    .add_branch(before_branch.clone(), condition.clone());

                self.builder.graph.set_ctrl(true_branch);
                let when_true = self.stmt_expr(when_body);

                let Some((state_when_true, true_branch)) = self.try_current_state() else {
                    self.builder.restore_state(state_before_branch);
                    self.builder.graph.set_ctrl(false_branch);
                    return if let Some((else_keyword, else_body)) = else_body {
                        let when_false = self.stmt_expr(else_body);
                        if self.builder.graph.is_unreachable() {
                            return self.builder.graph.add_never();
                        }
                        when_false
                    } else {
                        self.builder.graph.add_unit()
                    };
                };

                if let Some((else_keyword, else_body)) = else_body {
                    self.builder.restore_state(state_before_branch);
                    self.builder.graph.set_ctrl(false_branch);
                    let when_false = self.stmt_expr(else_body);

                    let Some((state_when_false, false_branch)) = self.try_current_state() else {
                        self.builder.restore_state(state_when_true);
                        self.builder.graph.set_ctrl(true_branch);
                        return when_true;
                    };

                    let ty = when_true.ty.clone();

                    let merge = self
                        .builder
                        .graph
                        .add_merge(vec![false_branch, true_branch]);
                    self.builder
                        .merge_states(merge.clone(), vec![state_when_false, state_when_true]);
                    let phi = self
                        .builder
                        .graph
                        .add_phi(merge.clone(), vec![when_false, when_true]);

                    self.builder.graph.add_merge_to_ctrl(merge);
                    self.builder.graph.add_data_phi(phi, ty)
                } else {
                    let merge = self
                        .builder
                        .graph
                        .add_merge(vec![false_branch, true_branch]);
                    self.builder
                        .merge_states(merge.clone(), vec![state_before_branch, state_when_true]);

                    self.builder.graph.add_merge_to_ctrl(merge);
                    self.builder.graph.add_unit()
                }
            }
            ExprKind::Label { label, body } => {
                let entry = self.get_ctrl();
                let (loop_head, loop_phis, tok, loop_id) = self.builder.set_up_loop(entry);
                self.builder.labels.insert(label.label.val, loop_id);

                let value = self.stmt_expr(body); // parse the hole body
                self.builder.labels.remove(&label.label.val);
                self.builder
                    .close_loop(tok, Some(value), loop_head, loop_phis)
            }
            ExprKind::Loop { keyword, body } => {
                let entry = self.get_ctrl();
                let (loop_head, loop_phis, tok, _) = self.builder.set_up_loop(entry);

                let _ = self.stmt_expr(body); // parse the hole body

                self.builder.close_loop(tok, None, loop_head, loop_phis)
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
                self.builder.graph.add_type_error()
            }
        }
    }

    fn get_ctrl(&mut self) -> CtrlID {
        match self.builder.graph.get_ctrl() {
            Ok(ctrl) => ctrl,
            Err(_) => todo!(),
        }
    }

    pub fn current_state(&mut self) -> (MutableState, CtrlID) {
        (self.builder.snapshot_state(), self.get_ctrl())
    }

    pub fn try_current_state(&mut self) -> Option<(MutableState, CtrlID)> {
        let Ok(ctrl) = self.builder.graph.get_ctrl() else {
            return None;
        };
        Some((self.builder.snapshot_state(), ctrl))
    }
}
