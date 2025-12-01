use num::{BigUint, FromPrimitive};

use super::{BinaryOp, Graph, NodeId, NodeKind};
use crate::{
    error::Errors,
    tokenizing::{
        num::{Base, Literal},
        ty::Type,
        Tokenizer,
    },
    utilities::Rc,
};
use std::{panic, path::Path};

const TARGET_PTR_SIZE: usize = 64;

fn tokenizer_for(source: &'static str) -> Tokenizer<'static> {
    let errors = Rc::new(Errors::empty(Path::new("example.rx")));
    Tokenizer::new(source, errors, TARGET_PTR_SIZE)
}

const ARITHMETIC_EXAMPLE: &str = include_str!("arithmetic_example.rx");

#[test]
fn builds_graph_for_example_program() {
    let mut tokenizer = tokenizer_for(ARITHMETIC_EXAMPLE);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    expect_primitive_type(&x_symbol.ty, Type::Signed { size: 32 });

    let x_value = x_symbol.last_value.as_ref().expect("x's last_value");
    let (mul_lhs, mul_rhs) = expect_binary(x_value, BinaryOp::Mul);
    expect_literal(&mul_rhs, Literal::from(20_u8));

    let (add_lhs, add_rhs) = expect_binary(&mul_lhs, BinaryOp::Add);
    expect_literal(&add_lhs, Literal::from(10_u8));
    expect_literal(&add_rhs, Literal::from(10_u8));

    let y_symbol = graph.symbol("y").expect("y symbol");
    expect_primitive_type(&y_symbol.ty, Type::Signed { size: 32 });
    expect_literal(
        y_symbol.last_value.as_ref().expect("y's last value"),
        Literal::from(11_u8),
    );

    let a_symbol = graph.symbol("a").expect("a symbol");
    expect_primitive_type(&a_symbol.ty, Type::Unsigned { size: 32 });
    assert!(a_symbol.last_value.is_none());

    let abc_symbol = graph.symbol("abc").expect("abc symbol");
    expect_unknown_ident(&abc_symbol.ty, "str");
    expect_quote(
        abc_symbol.last_value.as_ref().expect("abc's last value"),
        "abc",
    );

    let complex_symbol = graph.symbol("complex").expect("complex symbol");
    expect_primitive_type(
        &complex_symbol.ty,
        Type::Unsigned {
            size: TARGET_PTR_SIZE,
        },
    );
    let (lhs, rhs) = expect_binary(
        complex_symbol
            .last_value
            .as_ref()
            .expect("complex's last value"),
        BinaryOp::Add,
    );

    assert!(x_value.ptr_cmp(&lhs));
    expect_literal(&rhs, Literal::from(1_u8));
}

#[test]
fn assignment_updates_variable_versions() {
    const PROGRAM: &str = "x i32 = 1\nx = x + 1\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x versions");
    expect_primitive_type(&x_symbol.ty, Type::Signed { size: 32 });

    let (lhs, rhs) = expect_binary(
        x_symbol.last_value.as_ref().expect("x's last value"),
        BinaryOp::Add,
    );

    expect_literal(&lhs, Literal::from(1_u8));
    expect_literal(&rhs, Literal::from(1_u8));
}

#[test]
fn parses_non_decimal_literals() {
    const PROGRAM: &str = "value i32 = 0x20\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let symbol = graph.symbol("value").expect("value symbol");
    expect_primitive_type(&symbol.ty, Type::Signed { size: 32 });
    expect_literal(
        &symbol.last_value.as_ref().expect("value's last value"),
        Literal {
            base: Base::Hexadecimal,
            digits: BigUint::from_u8(32_u8).expect("BigUint"),
            num_digits_after_dot: 0,
            exponent: None,
            suffix: "",
        },
    );
}

#[test]
fn deduplicates_types_and_literal_nodes() {
    const PROGRAM: &str = "x i32 = 1\ny i32 = 1\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let y_symbol = graph.symbol("y").expect("y symbol");

    assert!(x_symbol.ty.ptr_cmp(&y_symbol.ty));

    let x_value = x_symbol.last_value.as_ref().expect("x value");
    let y_value = y_symbol.last_value.as_ref().expect("y value");
    assert!(x_value.ptr_cmp(y_value));

    expect_literal(x_value, Literal::from(1_u8));

    drop(graph);
    drop(tokenizer);
}

#[test]
fn deduplicates_binary_nodes_for_identical_expressions() {
    const PROGRAM: &str = "x i32 = (10 + 2) * 3\ny i32 = (10 + 2) * 3\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_value = graph
        .symbol("x")
        .and_then(|symbol| symbol.last_value.as_ref())
        .expect("x value");
    let y_value = graph
        .symbol("y")
        .and_then(|symbol| symbol.last_value.as_ref())
        .expect("y value");

    assert!(x_value.ptr_cmp(y_value));

    let (x_mul_lhs, x_mul_rhs) = expect_binary(x_value, BinaryOp::Mul);
    let (y_mul_lhs, y_mul_rhs) = expect_binary(y_value, BinaryOp::Mul);
    assert!(x_mul_lhs.ptr_cmp(&y_mul_lhs));
    assert!(x_mul_rhs.ptr_cmp(&y_mul_rhs));

    let (x_add_lhs, x_add_rhs) = expect_binary(&x_mul_lhs, BinaryOp::Add);
    let (y_add_lhs, y_add_rhs) = expect_binary(&y_mul_lhs, BinaryOp::Add);
    assert!(x_add_lhs.ptr_cmp(&y_add_lhs));
    assert!(x_add_rhs.ptr_cmp(&y_add_rhs));

    expect_literal(&x_mul_rhs, Literal::from(3_u8));
    expect_literal(&x_add_lhs, Literal::from(10_u8));
    expect_literal(&x_add_rhs, Literal::from(2_u8));
}

const IF_EXAMPLE: &str = include_str!("if_example.rx");

#[test]
fn basic_phi_works() {
    let mut tokenizer = tokenizer_for(IF_EXAMPLE);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let i_symbol = graph.symbol("i").expect("i symbol");
    expect_primitive_type(
        &i_symbol.ty,
        Type::Unsigned {
            size: TARGET_PTR_SIZE,
        },
    );
    let i_value = i_symbol.last_value.as_ref().expect("i' last value");

    expect_literal(i_value, Literal::from(0_u8));

    let x_symbol = graph.symbol("x").expect("x symbol");
    expect_primitive_type(&x_symbol.ty, Type::Signed { size: 32 });
    let x_value = x_symbol.last_value.as_ref().expect("x's last value");

    let (condition, when_true, when_false) = expect_phi(&x_value);
    let (lhs, rhs) = expect_binary(&condition, BinaryOp::Smaller);

    expect_literal(&lhs, Literal::from(0_u8));
    expect_literal(&rhs, Literal::from(10_u8));

    expect_literal(&when_true, Literal::from(10_u8));
    expect_literal(&when_false, Literal::from(11_u8));

    let y_symbol = graph.symbol("y").expect("y symbol");
    expect_primitive_type(&y_symbol.ty, Type::Signed { size: 32 });
    let y_value = y_symbol.last_value.as_ref().expect("y's last value");

    let (condition, when_true, when_false) = expect_phi(&y_value);
    let (lhs, rhs) = expect_binary(&condition, BinaryOp::Greater);

    expect_literal(&lhs, Literal::from(0_u8));
    expect_literal(&rhs, Literal::from(10_u8));

    assert!(when_true.ptr_cmp(x_value));
    expect_literal(&when_false, Literal::from(1_u8));
}

fn expect_literal(node: &NodeId<'_>, literal: Literal<'_>) {
    match &node.kind {
        NodeKind::Literal {
            literal: actual_literal,
        } => {
            assert_eq!(actual_literal, &literal);
        }
        other => panic!("expected literal {literal:?}, got {other:?}"),
    }
}

fn expect_quote(node: &NodeId<'_>, requested_quote: &str) {
    match &node.kind {
        NodeKind::Quote { quote } => assert_eq!(quote, requested_quote),
        other => panic!("expected quote {requested_quote}, got {other:?}"),
    }
}

fn expect_primitive_type<'src>(node: &NodeId<'src>, requested_type: Type) {
    match &node.kind {
        NodeKind::PrimitiveType { ty } => {
            assert_eq!(*ty, requested_type)
        }
        other => panic!("expected primitive type {requested_type:?}, got {other:?}"),
    }
}

fn expect_binary<'src>(node: &NodeId<'src>, op: BinaryOp) -> (NodeId<'src>, NodeId<'src>) {
    match &node.kind {
        NodeKind::Binary {
            op: actual_op,
            lhs,
            rhs,
        } => {
            assert_eq!(*actual_op, op);
            (lhs.clone(), rhs.clone())
        }
        other => panic!("expected binary {op:?}, got {other:?}"),
    }
}

// returns: condition - when_true - when_false
fn expect_phi<'src>(node: &NodeId<'src>) -> (NodeId<'src>, NodeId<'src>, NodeId<'src>) {
    match &node.kind {
        NodeKind::Phi {
            condition,
            when_true,
            when_false,
        } => (condition.clone(), when_true.clone(), when_false.clone()),
        other => panic!("expected phi-node, got {other:?}"),
    }
}

fn expect_unknown_ident(node: &NodeId<'_>, requested_name: &str) {
    match &node.kind {
        NodeKind::UnknownIdent { name } => assert_eq!(*name, requested_name),
        other => panic!("expected unknown identifier {requested_name:?}, got {other:?}"),
    }
}
