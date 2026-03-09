use num::{BigUint, FromPrimitive};

use super::{BinaryOp, Graph, GraphError, MemNodeID, NodeID, NodeKind};
use crate::{
    error::Errors,
    tokenizing::{
        num::{Base, Literal},
        ty::Type,
        unary_op::UnaryOp,
        Tokenizer,
    },
    utilities::Rc,
};
use std::path::Path;

const TARGET_PTR_SIZE: usize = 64;

fn tokenizer_for(source: &'static str) -> Tokenizer<'static> {
    let errors = Rc::new(Errors::empty(Path::new("example.rx")));
    Tokenizer::new(source, errors, TARGET_PTR_SIZE)
}

#[test]
fn immutable_symbol_cannot_be_reassigned() {
    const PROGRAM: &str = "x i32 = 1\nx = 2\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer);

    assert!(matches!(
        graph,
        Err(GraphError::AssignmentToImmutableIdent { .. })
    ));
}

#[test]
fn mutable_symbol_assignments_use_memory_nodes() {
    const PROGRAM: &str = "x i32 = -> 1\nx = x + 1\ny i32 = x\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    expect_primitive_type(&x_symbol.ty, Type::Signed { size: 32 });
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);
    expect_literal(&x_addr, Literal::from(1_u8));

    let y_symbol = graph.symbol("y").expect("y symbol");
    let (mem, y_addr) = expect_load(&y_symbol.assignment);
    assert!(x_addr.ptr_cmp(&y_addr));
    expect_store_chain_ends_with_addr(&mem, &x_addr);
}

#[test]
fn parses_non_decimal_literals() {
    const PROGRAM: &str = "value i32 = 0x20\n";
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let symbol = graph.symbol("value").expect("value symbol");
    expect_primitive_type(&symbol.ty, Type::Signed { size: 32 });
    expect_literal(
        &symbol.assignment,
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
    assert!(x_symbol.assignment.ptr_cmp(&y_symbol.assignment));
    expect_literal(&x_symbol.assignment, Literal::from(1_u8));
}

#[test]
fn if_statement_merges_memory_for_mutable_bindings() {
    const PROGRAM: &str = r#"
i ux = 0
x i32 = -> 3000
if i < 10 {
    x = 10
} else {
    x = 11
}
y i32 = if i > 10 x else 1
"#;
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);
    expect_literal(&x_addr, Literal::from(3000_u16));

    let y_symbol = graph.symbol("y").expect("y symbol");
    let (condition, when_true, when_false) = expect_phi(&y_symbol.assignment);
    let (lhs, rhs) = expect_binary(&condition, BinaryOp::Greater);
    expect_literal(&lhs, Literal::from(0_u8));
    expect_literal(&rhs, Literal::from(10_u8));

    let (mem, addr) = expect_load(&when_true);
    assert!(addr.ptr_cmp(&x_addr));
    expect_store_chain_ends_with_addr(&mem, &x_addr);

    expect_literal(&when_false, Literal::from(1_u8));
}

fn expect_literal(node: &NodeID<'_>, literal: Literal<'_>) {
    match &node.kind {
        NodeKind::Literal {
            literal: actual_literal,
        } => {
            assert_eq!(*actual_literal, literal);
        }
        other => panic!("expected literal {literal:?}, got {other:?}"),
    }
}

fn expect_primitive_type(node: &NodeID<'_>, requested_type: Type) {
    match &node.kind {
        NodeKind::PrimitiveType { ty } => {
            assert_eq!(*ty, requested_type)
        }
        other => panic!("expected primitive type {requested_type:?}, got {other:?}"),
    }
}

fn expect_binary<'src>(node: &NodeID<'src>, op: BinaryOp) -> (NodeID<'src>, NodeID<'src>) {
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

fn expect_unary<'src>(node: &NodeID<'src>, op: UnaryOp) -> NodeID<'src> {
    match &node.kind {
        NodeKind::Unary {
            op: actual_op,
            input,
        } => {
            assert_eq!(*actual_op, op);
            input.clone()
        }
        other => panic!("expected unary {op:?}, got {other:?}"),
    }
}

fn expect_phi<'src>(node: &NodeID<'src>) -> (NodeID<'src>, NodeID<'src>, NodeID<'src>) {
    match &node.kind {
        NodeKind::Phi {
            condition,
            when_true,
            when_false,
        } => (condition.clone(), when_true.clone(), when_false.clone()),
        other => panic!("expected phi-node, got {other:?}"),
    }
}

fn expect_load<'src>(node: &NodeID<'src>) -> (MemNodeID<'src>, NodeID<'src>) {
    match &node.kind {
        NodeKind::Load { prev, addr } => (prev.clone(), addr.clone()),
        other => panic!("expected load-node, got {other:?}"),
    }
}

fn expect_store_chain_ends_with_addr(mem: &MemNodeID<'_>, addr: &NodeID<'_>) {
    let mut current = mem.clone();
    loop {
        match &current.kind {
            super::MemNodeKind::ControlFlowStart => {
                panic!("expected at least one store in memory chain")
            }
            super::MemNodeKind::Merge { a, b } => {
                expect_store_chain_ends_with_addr(a, addr);
                expect_store_chain_ends_with_addr(b, addr);
                return;
            }
            super::MemNodeKind::Store {
                prev,
                addr: store_addr,
                ..
            } => {
                if store_addr.ptr_cmp(addr) {
                    return;
                }
                current = prev.clone();
            }
        }
    }
}
