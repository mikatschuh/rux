use std::collections::{HashMap, HashSet};

use super::{Graph, GraphError};
use crate::{
    error::Errors,
    grapher::{
        GraphResult,
        builder::{GraphBuilder, ScopedSymbolTable, Symbol},
        graph::DataID,
    },
    literal_parsing::{Base, Literal, parse_integer},
    tokenizing::{Tokenizer, binary_op::BinaryOp, unary_op::UnaryOp},
    type_parsing::{AtomicType, TypeSize},
    utilities::Rc,
};
use std::path::Path;

const TARGET_PTR_SIZE: TypeSize = 64;

fn tokenizer_for(source: &'static str) -> Tokenizer<'static> {
    let errors = Rc::new(Errors::empty(Path::new("example.rx")));
    Tokenizer::new(source.as_bytes(), errors, TARGET_PTR_SIZE)
}

fn parse(
    source: &'static str,
) -> GraphResult<'static, (Graph<'static>, ScopedSymbolTable<'static>)> {
    let mut tokenizer = tokenizer_for(source);
    GraphBuilder::new(&mut tokenizer).debug_build()
}

macro_rules! parse_correct {
    ($source:literal) => {{
        let (graph, symbols) = parse($source).expect("graph");
        let symbol_dump = symbols.all_symbols();
        (graph, symbol_dump.immutables, symbol_dump.mutables)
    }};
}

#[test]
fn allows_declaration_and_reading() {
    let (mut graph, immutables, mutables) = parse_correct!(
        "
let first i32 = 0
let second i32 = 21

{
    let first i32 = 99
}

let second_read i32 = second
let first_read i32 = first
        "
    );

    let ty = graph.add_type(AtomicType::Signed { size: 32 });
    let first_val = graph.add_literal(Literal::from(0));
    let second_val = graph.add_literal(Literal::from(21));

    symbol(&immutables, "first", &ty, &first_val);
    symbol(&immutables, "second", &ty, &second_val);
}

#[test]
fn allows_overwriting() {
    let (mut graph, immutables, mutables) = parse_correct!(
        "
var first i32 = 0
{
    var first i32 = 1
    var first_first_read i32 = first // trying mutable read
    first = 2
    let second_first_read u33 = first
}
let first_outer_first_read i32 = first
first = 999
let second_outer_first_read i32 = first
        "
    );

    let ty = graph.add_type(AtomicType::Signed { size: 32 });
    let ty2 = graph.add_type(AtomicType::Unsigned { size: 33 });
    let val1 = graph.add_literal(Literal::from(0));
    let val2 = graph.add_literal(Literal::from(1));
    let val3 = graph.add_literal(Literal::from(2));
    let val4 = graph.add_literal(Literal::from(999));

    symbol(&immutables, "first_outer_first_read", &ty, &val1);
    symbol(&immutables, "second_outer_first_read", &ty, &val4);
    symbol(&mutables, "first_first_read", &ty, &val2);
    symbol(&immutables, "second_first_read", &ty2, &val3);
}

fn symbol<'src>(
    symbols: &HashMap<&'src str, Symbol<'src>>,
    name: &'src str,
    ty: &DataID<'src>,

    val: &DataID<'src>,
) {
    let symbol = symbols.get(name).expect("symbol");
    assert_eq!(symbol.ty.addr(), ty.addr());
    assert_eq!(symbol.value.addr(), val.addr());
}
