use std::path::Path;

use crate::{
    error::Errors,
    grapher::{
        builder::GraphBuilder,
        graph::{DataKind, PhiID},
    },
    literal_parsing::Literal,
    tokenizing::Tokenizer,
    type_parsing::AtomicType,
    utilities::Rc,
};

fn builder_for(source: &'static str) -> GraphBuilder<'static, Tokenizer<'static>> {
    let errors = Rc::new(Errors::empty(Path::new("builder-test.rx")));
    let tokenizer = Box::leak(Box::new(Tokenizer::new(source.as_bytes(), errors, 64)));
    GraphBuilder::new(tokenizer)
}

#[test]
fn advance_and_expect_name_move_the_token_stream_forward() {
    let mut builder = builder_for("alpha beta");

    let first = builder.expect_name().expect("first name");
    assert_eq!(first.src, "alpha");
    assert_eq!(builder.peek().src, "beta");

    let second = builder.get_name();
    assert_eq!(second.src, "beta");
    assert_eq!(builder.peek().src, "");
}

#[test]
fn declare_variable_routes_to_immutable_or_mutable_symbol_maps() {
    let mut builder = builder_for("");
    let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
    let immutable_value = builder.graph.add_literal(Literal::from(1));
    let mutable_value = builder.graph.add_literal(Literal::from(2));

    builder.declare_variable("config", ty.clone(), immutable_value.clone(), false);
    builder.declare_variable("counter", ty, mutable_value.clone(), true);

    let dump = builder.symbol_table.all_symbols();
    assert_eq!(
        dump.immutables["config"].value.addr(),
        immutable_value.addr()
    );
    assert_eq!(dump.mutables["counter"].value.addr(), mutable_value.addr());
}

#[test]
fn current_state_and_restore_state_round_trip_mutables() {
    let mut builder = builder_for("");
    let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
    let initial = builder.graph.add_literal(Literal::from(1));
    let overwritten = builder.graph.add_literal(Literal::from(2));

    builder.declare_variable("counter", ty, initial.clone(), true);
    let (snapshot, ctrl) = builder.current_state().expect("state");

    builder
        .symbol_table
        .write_symbol(crate::error::Span::beginning(), "counter", overwritten)
        .expect("write");
    builder.restore_state(snapshot);

    assert_eq!(builder.graph.get_ctrl().expect("ctrl").addr(), ctrl.addr());
    assert_eq!(
        builder
            .symbol_table
            .read_symbol(&mut builder.graph, "counter")
            .addr(),
        initial.addr()
    );
}

#[test]
fn merge_states_keeps_identical_values_without_creating_phi() {
    let mut builder = builder_for("");
    let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
    let value = builder.graph.add_literal(Literal::from(10));
    builder.declare_variable("cached", ty, value.clone(), true);

    let (state_a, ctrl_a) = builder.current_state().expect("state a");
    let (state_b, ctrl_b) = builder.current_state().expect("state b");
    let merge = builder.graph.add_merge(vec![ctrl_a, ctrl_b]);

    builder.merge_states(merge, vec![state_a, state_b]);

    let merged = builder
        .symbol_table
        .read_symbol(&mut builder.graph, "cached");
    assert_eq!(merged.addr(), value.addr());
}

#[test]
fn merge_states_creates_phi_for_different_mutable_values() {
    let mut builder = builder_for("");
    let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
    let first = builder.graph.add_literal(Literal::from(1));
    let second = builder.graph.add_literal(Literal::from(2));
    builder.declare_variable("score", ty, first.clone(), true);

    let (state_a, ctrl_a) = builder.current_state().expect("state a");
    builder
        .symbol_table
        .write_symbol(crate::error::Span::beginning(), "score", second.clone())
        .expect("write");
    let (state_b, ctrl_b) = builder.current_state().expect("state b");
    let merge = builder.graph.add_merge(vec![ctrl_a, ctrl_b]);

    builder.merge_states(merge, vec![state_a, state_b]);

    let merged = builder
        .symbol_table
        .read_symbol(&mut builder.graph, "score");
    let DataKind::Phi { phi } = &merged.kind else {
        panic!("different mutable states should merge through phi");
    };
    assert_eq!(phi.variants.len(), 2);
    assert!(
        phi.variants
            .iter()
            .any(|variant| variant.addr() == first.addr())
    );
    assert!(
        phi.variants
            .iter()
            .any(|variant| variant.addr() == second.addr())
    );
}

#[test]
fn set_up_loop_merge_replaces_mutables_with_open_loop_phis() {
    let mut builder = builder_for("");
    let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
    let initial = builder.graph.add_literal(Literal::from(0));
    builder.declare_variable("index", ty, initial.clone(), true);

    let entry = builder.graph.get_ctrl().expect("entry");
    let (loop_head, loop_phis): (_, Vec<PhiID>) = builder.set_up_loop_merge(entry.clone());

    assert_eq!(loop_head.branches.len(), 1);
    assert_eq!(loop_head.branches[0].addr(), entry.addr());
    assert_eq!(loop_phis.len(), 1);
    assert_eq!(loop_phis[0].variants.len(), 1);
    assert_eq!(loop_phis[0].variants[0].addr(), initial.addr());

    let current = builder
        .symbol_table
        .read_symbol(&mut builder.graph, "index");
    let DataKind::Phi { phi } = &current.kind else {
        panic!("mutable should be replaced by a loop phi data node");
    };
    assert_eq!(phi.addr(), loop_phis[0].addr());
}
