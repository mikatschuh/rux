use std::collections::HashSet;

use super::{Graph, GraphError};
use crate::{
    error::Errors,
    grapher::{
        GraphResult,
        graph::{MemID, MemKind, ValueID, ValueKind},
        parser::GraphBuilder,
        symbols::Scopes,
    },
    literal_parsing::{Base, Literal},
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

fn parse(source: &'static str) -> GraphResult<'static, (Graph, Scopes<'static>)> {
    let mut tokenizer = tokenizer_for(source);
    GraphBuilder::new(&mut tokenizer).debug_build()
}

#[cfg(never)]
#[test]
fn immutable_symbol_cannot_be_reassigned() {
    const PROGRAM: &str = "let x i32 = 1\nx = 2\n";
    let (graph, scope) = parse(PROGRAM).expect("graph");

    assert!(matches!(
        graph,
        Err(GraphError::AssignmentToImmutableIdent { .. })
    ));
}

#[cfg(never)]
#[test]
fn mutable_symbol_assignments_use_memory_nodes() {
    const PROGRAM: &str = "x i32 = -> 1\nx = x + 1\ny i32 = x\n";
    let (graph, scope) = parse(PROGRAM).expect("graph");

    let x_symbol = scope.register_symbol("x").expect("x symbol");
    expect_primitive_type(&x_symbol.type_, AtomicType::Signed { size: 32 });
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);
    expect_literal(&x_addr, Literal::from(1_u8));

    let y_symbol = graph.symbol("y").expect("y symbol");
    let (mem, y_addr) = expect_load(&y_symbol.assignment);
    assert!(x_addr.ptr_cmp(&y_addr));
    expect_store_chain_ends_with_addr(&mem, &x_addr);
}

#[cfg(never)]
#[test]
fn parses_non_decimal_literals() {
    const PROGRAM: &str = "value i32 = 0x20\n";
    let (graph, scope) = parse(PROGRAM).expect("graph");

    let symbol = graph.symbol("value").expect("value symbol");
    expect_primitive_type(&symbol.type_, PrimitiveType::Signed { size: 32 });
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

#[cfg(never)]
#[test]
fn deduplicates_types_and_literal_nodes() {
    const PROGRAM: &str = "x i32 = 1\ny i32 = 1\n";
    let (graph, scope) = parse(PROGRAM).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let y_symbol = graph.symbol("y").expect("y symbol");

    assert!(x_symbol.type_.ptr_cmp(&y_symbol.type_));
    assert!(x_symbol.assignment.ptr_cmp(&y_symbol.assignment));
    expect_literal(&x_symbol.assignment, Literal::from(1_u8));
}

#[cfg(never)]
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
    let (graph, scope) = parse(PROGRAM).expect("graph");

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

#[cfg(never)]
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

    let (graph, scope) = parse(PROGRAM).expect("graph");

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

#[cfg(never)]
#[test]
fn continue_skips_remaining_loop_body_but_keeps_backedge() {
    const PROGRAM: &str = r#"
i ux = -> 0
x i32 = -> 0

loop i < 3 : i++ {
    x = 1
    continue
}

result_i i32 = i
result_x i32 = x
"#;

    let (graph, scope) = parse(PROGRAM).expect("graph");

    let i_symbol = graph.symbol("i").expect("i symbol");
    let i_addr = expect_unary(&i_symbol.assignment, UnaryOp::Ptr);

    let x_symbol = graph.symbol("x").expect("x symbol");
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);

    let result_i_symbol = graph.symbol("result_i").expect("result_i symbol");
    let (result_i_mem, result_i_addr) = expect_load(&result_i_symbol.assignment);
    assert!(result_i_addr.ptr_cmp(&i_addr));
    assert!(mem_contains_store_to_addr(&result_i_mem, &i_addr));
    assert!(mem_has_cycle(&result_i_mem));

    let result_x_symbol = graph.symbol("result_x").expect("result_x symbol");
    let (result_x_mem, result_x_addr) = expect_load(&result_x_symbol.assignment);
    assert!(result_x_addr.ptr_cmp(&x_addr));
    assert!(mem_contains_store_to_addr(&result_x_mem, &x_addr));
}

#[cfg(never)]
#[test]
fn break_exits_loop_without_creating_backedge() {
    const PROGRAM: &str = r#"
x i32 = -> 0
loop true {
    x = 1
    break
}
result_x i32 = x
"#;

    let (graph, scope) = parse(PROGRAM).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);

    let result_x_symbol = graph.symbol("result_x").expect("result_x symbol");
    let (result_x_mem, result_x_addr) = expect_load(&result_x_symbol.assignment);
    assert!(result_x_addr.ptr_cmp(&x_addr));
    assert!(mem_contains_store_to_addr(&result_x_mem, &x_addr));
    assert!(!mem_has_cycle(&result_x_mem));
}

#[cfg(never)]
#[test]
fn statements_after_continue_are_rejected() {
    const PROGRAM: &str = r#"
loop true {
    continue
    x i32 = 1
}
"#;

    let result = parse(PROGRAM);

    assert!(matches!(
        result,
        Err(GraphError::UnreachableStatementAfterJump { .. })
    ));
}

#[cfg(never)]
#[test]
fn statements_after_break_are_rejected() {
    const PROGRAM: &str = r#"
loop true {
    break
    x i32 = 1
}
"#;

    let result = parse(PROGRAM);

    assert!(matches!(
        result,
        Err(GraphError::UnreachableStatementAfterJump { .. })
    ));
}

#[cfg(never)]
#[test]
fn repeated_jump_keywords_target_outer_loops() {
    const PROGRAM: &str = r#"
outer ux = -> 0
inner ux = -> 0

loop outer < 3 : outer++ {
    loop inner < 3 : inner++ {
        break break
    }
}

result_outer i32 = outer
result_inner i32 = inner
"#;

    let (graph, scope) = parse(PROGRAM).expect("graph");

    let outer_symbol = graph.symbol("outer").expect("outer symbol");
    let outer_addr = expect_unary(&outer_symbol.assignment, UnaryOp::Ptr);
    let inner_symbol = graph.symbol("inner").expect("inner symbol");
    let inner_addr = expect_unary(&inner_symbol.assignment, UnaryOp::Ptr);

    let result_outer_symbol = graph.symbol("result_outer").expect("result_outer symbol");
    let (result_outer_mem, result_outer_addr) = expect_load(&result_outer_symbol.assignment);
    assert!(result_outer_addr.ptr_cmp(&outer_addr));
    assert!(matches!(
        &result_outer_mem.kind,
        MemKind::LoopHead { .. } | MemKind::Merge { .. }
    ));

    let result_inner_symbol = graph.symbol("result_inner").expect("result_inner symbol");
    let (_result_inner_mem, result_inner_addr) = expect_load(&result_inner_symbol.assignment);
    assert!(result_inner_addr.ptr_cmp(&inner_addr));
}

#[cfg(never)]
#[test]
fn loop_condition_is_kept_on_loop_head_and_uses_loop_memory() {
    const PROGRAM: &str = r#"
i ux = -> 0
loop i < 3 : i++ {}
result_i i32 = i
"#;

    let (graph, scope) = parse(PROGRAM).expect("graph");

    let i_symbol = graph.symbol("i").expect("i symbol");
    let i_addr = expect_unary(&i_symbol.assignment, UnaryOp::Ptr);

    let result_i_symbol = graph.symbol("result_i").expect("result_i symbol");
    let (result_i_mem, result_i_addr) = expect_load(&result_i_symbol.assignment);
    assert!(result_i_addr.ptr_cmp(&i_addr));

    let (condition, ..) = find_first_loop_head(&result_i_mem).expect("loop head");
    let (lhs, rhs) = expect_binary(&condition, BinaryOp::Less);
    let (condition_mem, condition_addr) = expect_load(&lhs);
    assert!(condition_addr.ptr_cmp(&i_addr));
    assert!(matches!(&condition_mem.kind, MemKind::LoopHead { .. }));
    expect_literal(&rhs, Literal::from(3_u8));
}

#[cfg(never)]
#[test]
fn loop_setup_can_start_with_a_block() {
    const PROGRAM: &str = r#"
x i32 = -> 0
loop { x = 1 } : x < 2 : x++ {}
result_x i32 = x
"#;

    let (graph, scope) = parse(PROGRAM).expect("graph");

    let x_symbol = graph.symbol("x").expect("x symbol");
    let x_addr = expect_unary(&x_symbol.assignment, UnaryOp::Ptr);

    let result_x_symbol = graph.symbol("result_x").expect("result_x symbol");
    let (result_x_mem, result_x_addr) = expect_load(&result_x_symbol.assignment);
    assert!(result_x_addr.ptr_cmp(&x_addr));
    assert!(mem_contains_store_to_addr(&result_x_mem, &x_addr));

    let (condition, ..) = find_first_loop_head(&result_x_mem).expect("loop head");
    let (lhs, rhs) = expect_binary(&condition, crate::tokenizing::binary_op::BinaryOp::Less);
    let (condition_mem, condition_addr) = expect_load(&lhs);
    assert!(condition_addr.ptr_cmp(&x_addr));
    assert!(matches!(&condition_mem.kind, MemKind::LoopHead { .. }));
    expect_literal(&rhs, Literal::from(2_u8));
}

#[cfg(never)]
#[test]
fn immutable_increment_in_loop_is_rejected() {
    const PROGRAM: &str = r#"
a i32 = 0
loop a < 10 {a++}
"#;

    let result = parse(PROGRAM);

    assert!(matches!(
        result,
        Err(GraphError::AssignmentToImmutableIdent { .. })
    ));
}

/*
#[cfg(never)]
#[test]
fn dump_includes_loop_backedge_and_conditional_mem_merge() {
    const PROGRAM: &str = r#"
i ux = -> 0
x i32 = -> 3000
if i < 10 {
    x = 10
} else {
    x = 11
}
loop i < 1 { i++ }
y i32 = x
"#;
    let mut tokenizer = tokenizer_for(PROGRAM);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let dump = graph.dump_text();
    assert!(dump.contains("GraphDump {"));
    assert!(dump.contains("symbols:"));
    assert!(dump.contains("nodes:"));
    assert!(dump.contains("memory:"));
    assert!(dump.contains("LoopHead(condition=n"));
    assert!(dump.contains("backedge=m"));
    assert!(dump.contains("Merge(condition=n"));
}
*/

fn expect_literal(node: &ValueID<'_>, literal: Literal<'_>) {
    match &node.kind {
        ValueKind::Literal {
            literal: actual_literal,
        } => {
            assert_eq!(*actual_literal, literal);
        }
        other => panic!("expected literal {literal:?}, got {other:?}"),
    }
}

fn expect_primitive_type(node: &ValueID<'_>, requested_type: AtomicType) {
    match &node.kind {
        ValueKind::AtomicType { ty } => {
            assert_eq!(*ty, requested_type)
        }
        other => panic!("expected primitive type {requested_type:?}, got {other:?}"),
    }
}

fn expect_binary<'src>(node: &ValueID<'src>, op: BinaryOp) -> (ValueID<'src>, ValueID<'src>) {
    match &node.kind {
        ValueKind::Binary {
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

fn expect_unary<'src>(node: &ValueID<'src>, op: UnaryOp) -> ValueID<'src> {
    match &node.kind {
        ValueKind::Unary {
            op: actual_op,
            input,
        } => {
            assert_eq!(*actual_op, op);
            input.clone()
        }
        other => panic!("expected unary {op:?}, got {other:?}"),
    }
}

fn expect_phi<'src>(node: &ValueID<'src>) -> (ValueID<'src>, ValueID<'src>, ValueID<'src>) {
    match &node.kind {
        ValueKind::Phi {
            condition,
            when_true,
            when_false,
        } => (condition.clone(), when_true.clone(), when_false.clone()),
        other => panic!("expected phi-node, got {other:?}"),
    }
}

fn expect_load<'src>(node: &ValueID<'src>) -> (MemID<'src>, ValueID<'src>) {
    match &node.kind {
        ValueKind::Load { mem, addr } => (mem.clone(), addr.clone()),
        other => panic!("expected load-node, got {other:?}"),
    }
}

/*
fn expect_store_chain_ends_with_addr(mem: &MemID<'_>, addr: &ValID<'_>) {
    let mut visited = HashSet::new();
    assert!(
        mem_contains_store_to_addr_with_visited(mem, addr, &mut visited),
        "expected at least one store in memory chain"
    );
}

fn mem_contains_store_to_addr(mem: &MemID<'_>, addr: &ValID<'_>) -> bool {
    let mut visited = HashSet::new();
    mem_contains_store_to_addr_with_visited(mem, addr, &mut visited)
}

fn mem_contains_store_to_addr_with_visited(
    mem: &MemID<'_>,
    addr: &ValID<'_>,
    visited: &mut HashSet<usize>,
) -> bool {
    let mem_ptr = std::ptr::from_ref(&**mem) as usize;
    if !visited.insert(mem_ptr) {
        return false;
    }

    match &mem.kind {
        MemKind::ControlFlowStart => false,
        MemKind::LoopHead {
            condition: _,
            entry,
            backedge,
        } => {
            mem_contains_store_to_addr_with_visited(entry, addr, visited)
                || mem_contains_store_to_addr_with_visited(backedge, addr, visited)
        }
        MemKind::StepClause { ctrl } => {
            mem_contains_store_to_addr_with_visited(ctrl, addr, visited)
        }
        MemKind::PlaceHolder { ctrl } => {
            mem_contains_store_to_addr_with_visited(ctrl, addr, visited)
        }
        MemKind::Merge {
            condition: _,
            when_true: a,
            when_false: b,
        } => {
            mem_contains_store_to_addr_with_visited(a, addr, visited)
                || mem_contains_store_to_addr_with_visited(b, addr, visited)
        }
        MemKind::Store {
            ctrl,
            addr: store_addr,
            ..
        } => {
            store_addr.ptr_cmp(addr) || mem_contains_store_to_addr_with_visited(ctrl, addr, visited)
        }
    }
}

fn mem_has_cycle(mem: &MemID<'_>) -> bool {
    let mut visited = HashSet::new();
    let mut stack = HashSet::new();
    mem_has_cycle_dfs(mem, &mut visited, &mut stack)
}

fn mem_has_cycle_dfs(
    mem: &MemID<'_>,
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
        MemKind::ControlFlowStart => false,
        MemKind::LoopHead {
            condition: _,
            ctrl,
            backedge,
        } => mem_has_cycle_dfs(ctrl, visited, stack) || mem_has_cycle_dfs(backedge, visited, stack),
        MemKind::StepClause { ctrl } => mem_has_cycle_dfs(ctrl, visited, stack),
        MemKind::PlaceHolder { ctrl } => mem_has_cycle_dfs(ctrl, visited, stack),
        MemKind::Merge { branch } => {
            mem_has_cycle_dfs(branch, visited, stack) || mem_has_cycle_dfs(b, visited, stack)
        }
        MemKind::Store { ctrl, .. } => mem_has_cycle_dfs(ctrl, visited, stack),
    };

    stack.remove(&mem_ptr);
    has_cycle
}

fn find_first_loop_head<'src>(
    mem: &MemID<'src>,
) -> Option<(ValID<'src>, MemID<'src>, MemID<'src>)> {
    let mut visited = HashSet::new();
    find_first_loop_head_with_visited(mem, &mut visited)
}

fn find_first_loop_head_with_visited<'src>(
    mem: &MemID<'src>,
    visited: &mut HashSet<usize>,
) -> Option<(ValID<'src>, MemID<'src>, MemID<'src>)> {
    let mem_ptr = std::ptr::from_ref(&**mem) as usize;
    if !visited.insert(mem_ptr) {
        return None;
    }

    match &mem.kind {
        MemKind::ControlFlowStart => None,
        MemKind::LoopHead {
            condition,
            entry,
            backedge,
        } => Some((condition.clone(), entry.clone(), backedge.clone())),
        MemKind::StepClause { ctrl } => find_first_loop_head_with_visited(ctrl, visited),
        MemKind::PlaceHolder { ctrl } => find_first_loop_head_with_visited(ctrl, visited),
        MemKind::Merge {
            condition: _,
            when_true,
            when_false,
        } => find_first_loop_head_with_visited(when_true, visited)
            .or_else(|| find_first_loop_head_with_visited(when_false, visited)),
        MemKind::Store { ctrl, .. } => find_first_loop_head_with_visited(ctrl, visited),
    }
}
*/
