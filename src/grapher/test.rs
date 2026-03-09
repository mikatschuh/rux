use num::{BigUint, FromPrimitive};
use std::collections::HashSet;

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

#[test]
fn loop_statement_supports_block_and_single_statement_forms() {
    const PROGRAM: &str = r#"
x i32 = -> 10
y i32

loop x > 0 {
    x--
}

y = x + 1

a i32 = -> 0
loop a < 10 { a++}
z i32 = a

loop i ux = -> 0 : i < 10 : i++ { a-- }
loop i > 0 : i-- {}

result_i i32 = i
result_a i32 = a
"#;

    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);
    expect_literal(&x_addr, Literal::from(10_u8));

    let y_symbol = graph.symbol("y").expect("y symbol");
    let (lhs, rhs) = expect_binary(&y_symbol.assignment, BinaryOp::Add);
    expect_literal(&rhs, Literal::from(1_u8));
    let (y_mem, y_addr) = expect_load(&lhs);
    assert!(y_addr.ptr_cmp(&x_addr));
    assert!(mem_contains_store_to_addr(&y_mem, &x_addr));

    let z_symbol = graph.symbol("z").expect("z symbol");
    let (z_mem, z_addr) = expect_load(&z_symbol.assignment);

    let a_symbol = graph.symbol("a").expect("a symbol");
    let a_addr = expect_unary(&a_symbol.assignment, UnaryOp::Ptr);
    assert!(z_addr.ptr_cmp(&a_addr));
    assert!(mem_contains_store_to_addr(&z_mem, &a_addr));

    let i_symbol = graph.symbol("i").expect("i symbol");
    let i_addr = expect_unary(&i_symbol.assignment, UnaryOp::Ptr);
    expect_literal(&i_addr, Literal::from(0_u8));

    let result_i_symbol = graph.symbol("result_i").expect("result_i symbol");
    let (result_i_mem, result_i_addr) = expect_load(&result_i_symbol.assignment);
    assert!(result_i_addr.ptr_cmp(&i_addr));
    assert!(mem_contains_store_to_addr(&result_i_mem, &i_addr));
    assert!(mem_has_cycle(&result_i_mem));

    let result_a_symbol = graph.symbol("result_a").expect("result_a symbol");
    let (result_a_mem, result_a_addr) = expect_load(&result_a_symbol.assignment);
    assert!(result_a_addr.ptr_cmp(&a_addr));
    assert!(mem_contains_store_to_addr(&result_a_mem, &a_addr));
    assert!(mem_has_cycle(&result_a_mem));
}

#[test]
fn immutable_increment_in_loop_is_rejected() {
    const PROGRAM: &str = r#"
a i32 = 0
loop a < 10 {a++}
"#;

    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer);

    assert!(matches!(
        graph,
        Err(GraphError::AssignmentToImmutableIdent { .. })
    ));
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
        NodeKind::Load { load, addr } => (
            if let super::MemNodeKind::Load { prev } = &load.kind {
                prev.clone()
            } else {
                panic!("should point to a load!")
            },
            addr.clone(),
        ),
        other => panic!("expected load-node, got {other:?}"),
    }
}

fn expect_store_chain_ends_with_addr(mem: &MemNodeID<'_>, addr: &NodeID<'_>) {
    let mut visited = HashSet::new();
    assert!(
        mem_contains_store_to_addr_with_visited(mem, addr, &mut visited),
        "expected at least one store in memory chain"
    );
}

fn mem_contains_store_to_addr(mem: &MemNodeID<'_>, addr: &NodeID<'_>) -> bool {
    let mut visited = HashSet::new();
    mem_contains_store_to_addr_with_visited(mem, addr, &mut visited)
}

fn mem_contains_store_to_addr_with_visited(
    mem: &MemNodeID<'_>,
    addr: &NodeID<'_>,
    visited: &mut HashSet<usize>,
) -> bool {
    let mem_ptr = std::ptr::from_ref(&**mem) as usize;
    if !visited.insert(mem_ptr) {
        return false;
    }

    match &mem.kind {
        super::MemNodeKind::ControlFlowStart => false,
        super::MemNodeKind::LoopHead { entry, backedge } => {
            mem_contains_store_to_addr_with_visited(entry, addr, visited)
                || backedge.as_ref().is_some_and(|edge| {
                    mem_contains_store_to_addr_with_visited(edge, addr, visited)
                })
        }
        super::MemNodeKind::Merge { condition: _, a, b } => {
            mem_contains_store_to_addr_with_visited(a, addr, visited)
                || mem_contains_store_to_addr_with_visited(b, addr, visited)
        }
        super::MemNodeKind::Store {
            prev,
            addr: store_addr,
            ..
        } => {
            store_addr.ptr_cmp(addr) || mem_contains_store_to_addr_with_visited(prev, addr, visited)
        }
        super::MemNodeKind::Load { prev, .. } => {
            mem_contains_store_to_addr_with_visited(prev, addr, visited)
        }
    }
}

fn mem_has_cycle(mem: &MemNodeID<'_>) -> bool {
    let mut visited = HashSet::new();
    let mut stack = HashSet::new();
    mem_has_cycle_dfs(mem, &mut visited, &mut stack)
}

fn mem_has_cycle_dfs(
    mem: &MemNodeID<'_>,
    visited: &mut HashSet<usize>,
    stack: &mut HashSet<usize>,
) -> bool {
    let mem_ptr = std::ptr::from_ref(&**mem) as usize;
    if stack.contains(&mem_ptr) {
        return true;
    }
    if !visited.insert(mem_ptr) {
        return false;
    }

    stack.insert(mem_ptr);

    let has_cycle = match &mem.kind {
        super::MemNodeKind::ControlFlowStart => false,
        super::MemNodeKind::LoopHead { entry, backedge } => {
            mem_has_cycle_dfs(entry, visited, stack)
                || backedge
                    .as_ref()
                    .is_some_and(|edge| mem_has_cycle_dfs(edge, visited, stack))
        }
        super::MemNodeKind::Merge { condition: _, a, b } => {
            mem_has_cycle_dfs(a, visited, stack) || mem_has_cycle_dfs(b, visited, stack)
        }
        super::MemNodeKind::Store { prev, .. } | super::MemNodeKind::Load { prev, .. } => {
            mem_has_cycle_dfs(prev, visited, stack)
        }
    };

    stack.remove(&mem_ptr);
    has_cycle
}
