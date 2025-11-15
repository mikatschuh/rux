use super::{BinaryOp, Graph, NodeId, NodeKind};
use crate::{error::Errors, tokenizing::Tokenizer, utilities::Rc};
use std::{path::Path, ptr};

const EXAMPLE_SOURCE: &str = include_str!("example.rx");

fn tokenizer_for(source: &'static str) -> Tokenizer<'static> {
    let errors = Rc::new(Errors::empty(Path::new("example.rx")));
    Tokenizer::new(source, errors)
}

#[test]
fn builds_graph_for_example_program() {
    let mut tokenizer = tokenizer_for(EXAMPLE_SOURCE);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    assert_eq!(x_symbol.ty, "i32");
    let x_value = expect_assign(&x_symbol.last_value, "x");
    let (mul_lhs, mul_rhs) = expect_binary(&x_value, BinaryOp::Mul);
    expect_constant(&mul_rhs, "20", 20);

    let (add_lhs, add_rhs) = expect_binary(&mul_lhs, BinaryOp::Add);
    expect_constant(&add_rhs, "10", 10);
    let read_source = expect_read(&add_lhs, "a");
    expect_constant(&read_source, "10", 10);

    let a_versions = graph.symbol_versions("a").expect("a versions");
    assert_eq!(a_versions.len(), 2);

    let first_a = &a_versions[0];
    assert_eq!(first_a.version, 0);
    assert_eq!(first_a.ty, "i32");
    let first_initializer = expect_decl(&first_a.decl, "a", "i32").expect("initializer for a");
    expect_constant(&first_initializer, "10", 10);

    let second_a = &a_versions[1];
    assert_eq!(second_a.version, 1);
    assert_eq!(second_a.ty, "u32");
    assert!(expect_decl(&second_a.decl, "a", "u32").is_none());

    let y_symbol = graph.symbol("y").expect("y symbol");
    assert_eq!(y_symbol.ty, "i32");
    let y_initializer = expect_decl(&y_symbol.decl, "y", "i32").expect("initializer for y");
    expect_constant(&y_initializer, "11", 11);
}

#[test]
fn assignment_updates_variable_versions() {
    const PROGRAM: &str = "x i32 = 1\nx = x + 1\n";
    let errors = Rc::new(Errors::empty(Path::new("assign.rx")));
    let mut tokenizer = Tokenizer::new(PROGRAM, errors);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_versions = graph.symbol_versions("x").expect("x versions");
    assert_eq!(x_versions.len(), 1);
    let declaration = &x_versions[0];
    let initializer = expect_decl(&declaration.decl, "x", "i32").expect("initializer for x");
    expect_constant(&initializer, "1", 1);

    let x_symbol = graph.symbol("x").expect("x symbol");
    let assignment = expect_assign(&x_symbol.last_value, "x");
    let (lhs, rhs) = expect_binary(&assignment, BinaryOp::Add);

    let read_source = expect_read(&lhs, "x");
    assert!(
        ptr::eq(&*read_source, &*initializer),
        "reads should use the previous SSA value"
    );
    expect_constant(&rhs, "1", 1);
}

#[test]
fn parses_non_decimal_literals() {
    const PROGRAM: &str = "value i32 = 0x20\n";
    let errors = Rc::new(Errors::empty(Path::new("hex.rx")));
    let mut tokenizer = Tokenizer::new(PROGRAM, errors);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let symbol = graph.symbol("value").expect("value symbol");
    let initializer = expect_decl(&symbol.decl, "value", "i32").expect("initializer for value");
    expect_constant(&initializer, "0x20", 32);
}

fn expect_constant(node: &NodeId<'_>, literal: &str, value: i64) {
    match &node.kind {
        NodeKind::Constant {
            literal: actual_literal,
            value: actual_value,
        } => {
            assert_eq!(actual_literal, &literal);
            assert_eq!(*actual_value, value);
        }
        other => panic!("expected constant {literal}, got {other:?}"),
    }
}

fn expect_decl<'src>(node: &NodeId<'src>, name: &str, ty: &str) -> Option<NodeId<'src>> {
    match &node.kind {
        NodeKind::VariableDecl {
            name: actual_name,
            ty: actual_ty,
            initializer,
        } => {
            assert_eq!(actual_name, &name);
            assert_eq!(actual_ty, &ty);
            initializer.clone()
        }
        other => panic!("expected declaration {name}, got {other:?}"),
    }
}

fn expect_assign<'src>(node: &NodeId<'src>, name: &str) -> NodeId<'src> {
    match &node.kind {
        NodeKind::Assign {
            name: actual_name,
            value,
        } => {
            assert_eq!(actual_name, &name);
            value.clone()
        }
        other => panic!("expected assignment to {name}, got {other:?}"),
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

fn expect_read<'src>(node: &NodeId<'src>, name: &str) -> NodeId<'src> {
    match &node.kind {
        NodeKind::Read {
            name: actual_name,
            source,
        } => {
            assert_eq!(actual_name, &name);
            source.clone()
        }
        other => panic!("expected read of {name}, got {other:?}"),
    }
}
