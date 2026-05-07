use std::collections::HashMap;

use super::{Graph, GraphError};
use crate::{
    error::Errors,
    grapher::{
        GraphResult,
        builder::{GraphBuilder, ScopedSymbolTable, Symbol},
        graph::{DataID, DataKind},
    },
    literal_parsing::Literal,
    tokenizing::{TokenStream, Tokenizer, binary_op::BinaryOp, unary_op::UnaryOp},
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
    symbol(&immutables, "first_read", &ty, &first_val);
    symbol(&immutables, "second_read", &ty, &second_val);
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

#[test]
fn builds_real_arithmetic_expression_trees() {
    let (mut graph, immutables, _) = parse_correct!(
        "
let base i32 = (sensor + 10) * 20
let adjusted i32 = -base + 0x10
        "
    );

    let ty = graph.add_type(AtomicType::Signed { size: 32 });
    let ten = graph.add_literal(Literal::from(10));
    let twenty = graph.add_literal(Literal::from(20));
    let hex_sixteen = graph.add_literal(tokenizer_for("0x10").get_literal());

    let base = immutables.get("base").expect("base");
    assert_eq!(base.ty.addr(), ty.addr());

    let DataKind::Binary {
        op: BinaryOp::Mul,
        lhs,
        rhs,
    } = &base.value.kind
    else {
        panic!("base should be a multiplication");
    };
    assert_eq!(rhs.addr(), twenty.addr());

    let DataKind::Binary {
        op: BinaryOp::Add,
        lhs: sensor,
        rhs,
    } = &lhs.kind
    else {
        panic!("base lhs should be an addition");
    };
    assert_eq!(rhs.addr(), ten.addr());
    assert!(matches!(sensor.kind, DataKind::Deferred));

    let adjusted = immutables.get("adjusted").expect("adjusted");
    let DataKind::Binary {
        op: BinaryOp::Add,
        lhs,
        rhs,
    } = &adjusted.value.kind
    else {
        panic!("adjusted should be an addition");
    };
    assert_eq!(rhs.addr(), hex_sixteen.addr());
    assert!(matches!(
        lhs.kind,
        DataKind::Unary {
            op: UnaryOp::Neg,
            ..
        }
    ));
}

#[test]
fn reuses_the_same_unknown_for_repeated_forward_reads() {
    let (_, immutables, _) = parse_correct!(
        "
let predicted i32 = external
let corrected i32 = external
        "
    );

    let predicted_external = &immutables["predicted"].value;
    let corrected_external = &immutables["corrected"].value;

    assert_eq!(predicted_external.addr(), corrected_external.addr());
    assert!(!matches!(predicted_external.kind, DataKind::Never));
}

#[test]
fn parses_compound_assignment_and_increment_as_mutable_state_updates() {
    let (mut graph, immutables, _) = parse_correct!(
        "
var energy i32 = 100
energy -= drain
energy++
let final_energy i32 = energy
        "
    );

    let hundred = graph.add_literal(Literal::from(100));
    let one = graph.add_literal(Literal::from(1));
    let final_energy = immutables.get("final_energy").expect("final_energy");

    let DataKind::Binary {
        op: BinaryOp::Add,
        lhs,
        rhs,
    } = &final_energy.value.kind
    else {
        panic!("increment should become addition");
    };
    assert_eq!(rhs.addr(), one.addr());

    let DataKind::Binary {
        op: BinaryOp::Sub,
        lhs,
        rhs,
    } = &lhs.kind
    else {
        panic!("compound assignment should become subtraction");
    };
    assert_eq!(lhs.addr(), hundred.addr());
    assert!(matches!(rhs.kind, DataKind::Deferred));
}

#[test]
fn if_else_merges_mutable_assignments_with_phi() {
    let (_, immutables, _) = parse_correct!(
        "
var velocity i32 = 10
let acceleration i32 = 2
if input < 0 {
    velocity = -velocity
} else {
    velocity = velocity + acceleration
}
let next_velocity i32 = velocity
        "
    );

    let next_velocity = immutables.get("next_velocity").expect("next_velocity");
    assert!(!matches!(next_velocity.value.kind, DataKind::Deferred));
}

#[test]
fn if_without_else_preserves_pre_branch_state_on_the_false_path() {
    let (mut graph, immutables, _) = parse_correct!(
        "
var retries i32 = 0
if failed {
    retries = retries + 1
}
let recorded_retries i32 = retries
        "
    );

    let zero = graph.add_literal(Literal::from(0));
    let recorded = immutables
        .get("recorded_retries")
        .expect("recorded_retries");

    let DataKind::Phi { phi } = &recorded.value.kind else {
        panic!("if without else should merge original and updated values");
    };

    assert_eq!(phi.merge.branches.len(), 2);
    assert!(
        phi.variants
            .iter()
            .any(|variant| variant.addr() == zero.addr())
    );
    assert!(phi.variants.iter().any(|variant| {
        matches!(
            variant.kind,
            DataKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        )
    }));
}

#[test]
fn loops_merge_backedge_state_and_exit_state() {
    let (_, immutables, _) = parse_correct!(
        "
var x i32 = 3
loop {
    if x == 0 break x
    x--
}
let after_loop i32 = x
        "
    );

    let after_loop = immutables.get("after_loop").expect("after_loop");
    assert!(!matches!(after_loop.value.kind, DataKind::Deferred));
}

#[test]
fn labelled_loop_allows_breaking_outer_loop_from_inner_loop() {
    let (_, immutables, _) = parse_correct!(
        "
var checksum i32 = 0
outer: {
    loop {
        checksum = checksum + 1
        break: outer checksum
    }
}
let result i32 = checksum
        "
    );

    let result = immutables.get("result").expect("result");
    assert!(!matches!(result.value.kind, DataKind::Deferred));
}

#[test]
fn parses_quotes_booleans_and_comparisons_from_configuration_like_code() {
    let (mut graph, immutables, _) = parse_correct!(
        r#"
let feature_name bool = "fast-path"
let enabled bool = true
let should_run bool = enabled && build_number >= 42
        "#
    );

    let quote = graph.add_quote("fast-path".to_string());
    let enabled = graph.add_bool(true);
    let forty_two = graph.add_literal(Literal::from(42));

    assert_eq!(immutables["feature_name"].value.addr(), quote.addr());
    assert_eq!(immutables["enabled"].value.addr(), enabled.addr());

    let should_run = immutables.get("should_run").expect("should_run");
    let DataKind::Binary {
        op: BinaryOp::And,
        lhs,
        rhs,
    } = &should_run.value.kind
    else {
        panic!("should_run should be a boolean and");
    };
    assert_eq!(lhs.addr(), enabled.addr());
    let DataKind::Binary {
        op: BinaryOp::GreaterEq,
        rhs,
        ..
    } = &rhs.kind
    else {
        panic!("rhs should compare build_number and 42");
    };
    assert_eq!(rhs.addr(), forty_two.addr());
}

#[test]
fn reports_assignment_to_immutable_identifier() {
    let err = parse(
        "
let config i32 = 1
config = 2
        ",
    )
    .err()
    .expect("immutable assignment should fail");

    assert!(matches!(
        err,
        GraphError::AssignmentToImmutableIdent { name: "config", .. }
    ));
}

#[test]
fn reports_break_and_continue_outside_loop() {
    let break_err = parse("break")
        .err()
        .expect("break outside loop should fail");
    assert!(matches!(break_err, GraphError::JumpOutsideLoop { .. }));

    let continue_err = parse("continue")
        .err()
        .expect("continue outside loop should fail");
    assert!(matches!(continue_err, GraphError::JumpOutsideLoop { .. }));
}

#[test]
fn reports_unknown_label_for_labelled_jumps() {
    let err = parse(
        "
loop {
    break: missing 1
}
        ",
    )
    .err()
    .expect("unknown label should fail");

    assert!(matches!(err, GraphError::UnknownLabel { .. }));
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
