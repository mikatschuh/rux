use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::builder::Builder,
    parser::{Expr, Interner, Item, Stmt, StmtExpr, Symbol, SymbolTable},
    utilities::Rc,
};

mod builder;
#[allow(unused)]
mod graph;
pub mod graph_dump;
mod parser;
#[allow(unused)]
use builder::{Error as BuildError, Result as BuildResult};

use bumpalo::Bump;
pub use graph::Graph;

pub fn build_son<'errors>(
    mut errors: Rc<Errors<'errors>>,
    SymbolTable {
        arena,
        mut interner,
        mut item_table,
    }: SymbolTable,
    starting_point: &'static str,
) {
    let starting_point_symbol = interner.get(starting_point);

    let Item { ty, value } = match item_table.remove(&starting_point_symbol) {
        Some(item) => item,
        None => {
            errors.push(
                Span::beginning(),
                ErrorCode::MissingEntryPoint {
                    entry: starting_point,
                },
            );
            return;
        }
    };

    let builder = GraphBuilder::new(errors, arena, interner, item_table);
}

pub struct GraphBuilder<'errors> {
    errors: Rc<Errors<'errors>>,
    builder: Builder,
    interner: Interner,
    item_table: HashMap<Symbol, Item>,
}

impl<'errors> GraphBuilder<'errors> {
    fn new(
        errors: Rc<Errors<'errors>>,
        arena: Bump,
        interner: Interner,
        item_table: HashMap<Symbol, Item>,
    ) -> Self {
        Self {
            errors,
            builder: Builder::new(arena),
            interner,
            item_table,
        }
    }

    fn process_stmt(stmt: Stmt) {}

    fn process_stmt_expr(stmt_expr: StmtExpr) {}

    fn process_expr(expr: Expr) {}
}
