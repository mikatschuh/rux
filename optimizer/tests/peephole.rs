use canonicalizer::{
    BinaryOp, BranchKind, ComBinaryOp, CtrlKind, Data, DataKind, Graph, MergeKind, OrderedOps,
    UnaryOp,
};
use optimizer::optimize_graph;

fn unary(graph: &mut Graph, op: UnaryOp, value: Data) -> Data {
    graph.add(DataKind::Unary { op, value })
}

fn binary(graph: &mut Graph, op: BinaryOp, lhs: Data, rhs: Data) -> Data {
    graph.add(DataKind::Binary {
        op,
        ops: [lhs, rhs],
    })
}

fn commutative(graph: &mut Graph, op: ComBinaryOp, lhs: Data, rhs: Data) -> Data {
    graph.add(DataKind::ComBinary {
        op,
        ops: OrderedOps::sorted([lhs, rhs]),
    })
}

fn merge(graph: &mut Graph, variants: &[Data]) -> Data {
    let parent = graph.add(CtrlKind::Entry);
    let condition = graph.add(DataKind::Boolean(true));
    let branch = graph.add(BranchKind { parent, condition });
    let prev = (0..variants.len())
        .map(|idx| graph.add(CtrlKind::Branch { branch, idx }))
        .collect();
    let merge = graph.add(MergeKind { prev });
    graph.add(DataKind::Merge {
        merge,
        variants: variants.into(),
    })
}

#[test]
fn optimizes_empty_graph() {
    let mut graph = Graph::default();
    optimize_graph(&mut graph);
    assert_eq!(graph.nodes::<DataKind>().count(), 0);
}

#[test]
fn preserves_leaf_values() {
    let mut graph = Graph::default();
    let values = [
        DataKind::Unit,
        DataKind::Boolean(false),
        DataKind::Boolean(true),
        DataKind::Literal(0),
        DataKind::Literal(usize::MAX),
        DataKind::Quote("hello".into()),
    ];
    let handles: Vec<_> = values
        .iter()
        .cloned()
        .map(|value| graph.add(value))
        .collect();

    optimize_graph(&mut graph);

    for (handle, expected) in handles.into_iter().zip(values) {
        assert_eq!(graph[handle], expected);
    }
}

#[test]
fn folds_boolean_not_and_or() {
    for a in [false, true] {
        for b in [false, true] {
            let mut graph = Graph::default();
            let lhs = graph.add(DataKind::Boolean(a));
            let rhs = graph.add(DataKind::Boolean(b));
            let not = unary(&mut graph, UnaryOp::Not, lhs);
            let or = commutative(&mut graph, ComBinaryOp::Or, lhs, rhs);

            optimize_graph(&mut graph);

            assert_eq!(graph[not], DataKind::Boolean(!a), "!{a}");
            assert_eq!(graph[or], DataKind::Boolean(a || b), "{a} or {b}");
        }
    }
}

#[test]
fn folds_bitwise_not_including_high_bits() {
    for value in [0, 1, 42, 1 << (usize::BITS - 1), usize::MAX] {
        let mut graph = Graph::default();
        let input = graph.add(DataKind::Literal(value));
        let result = unary(&mut graph, UnaryOp::BitNot, input);

        optimize_graph(&mut graph);

        assert_eq!(graph[result], DataKind::Literal(!value));
    }
}

#[test]
fn folds_commutative_integer_operations() {
    for (op, a, b, expected) in [
        (ComBinaryOp::Add, 19, 23, 42),
        (ComBinaryOp::Mul, 6, 7, 42),
        (ComBinaryOp::BitOr, 0b1010, 0b0101, 0b1111),
        (ComBinaryOp::BitOr, 0, usize::MAX, usize::MAX),
    ] {
        let mut graph = Graph::default();
        let lhs = graph.add(DataKind::Literal(a));
        let rhs = graph.add(DataKind::Literal(b));
        let result = commutative(&mut graph, op, lhs, rhs);

        optimize_graph(&mut graph);

        assert_eq!(graph[result], DataKind::Literal(expected), "{a} {op:?} {b}");
    }
}

#[test]
fn folds_ordered_integer_operations() {
    for (op, a, b, expected) in [
        (BinaryOp::Lsh, 3, 2, 12),
        (BinaryOp::Rsh, 48, 3, 6),
        (BinaryOp::Mod, 17, 5, 2),
    ] {
        let mut graph = Graph::default();
        let lhs = graph.add(DataKind::Literal(a));
        let rhs = graph.add(DataKind::Literal(b));
        let result = binary(&mut graph, op, lhs, rhs);

        optimize_graph(&mut graph);

        assert_eq!(graph[result], DataKind::Literal(expected), "{a} {op:?} {b}");
    }
}

#[test]
fn folds_less_than_with_correct_operand_order() {
    for (a, b, expected) in [(2, 3, true), (3, 2, false), (3, 3, false)] {
        let mut graph = Graph::default();
        let lhs = graph.add(DataKind::Literal(a));
        let rhs = graph.add(DataKind::Literal(b));
        let result = binary(&mut graph, BinaryOp::Less, lhs, rhs);

        optimize_graph(&mut graph);

        assert_eq!(graph[result], DataKind::Boolean(expected), "{a} < {b}");
    }
}

#[test]
fn eliminates_shifts_by_zero_without_folding_the_value() {
    for op in [BinaryOp::Lsh, BinaryOp::Rsh] {
        let mut graph = Graph::default();
        let literal = graph.add(DataKind::Literal(7));
        // Negation has no constant-folding rule yet, so the identity must apply.
        let input = unary(&mut graph, UnaryOp::Neg, literal);
        let zero = graph.add(DataKind::Literal(0));
        let result = binary(&mut graph, op, input, zero);

        optimize_graph(&mut graph);

        assert_eq!(graph.direct(result), graph.direct(input));
        assert_eq!(
            graph[input],
            DataKind::Unary {
                op: UnaryOp::Neg,
                value: literal
            }
        );
    }
}

#[test]
fn reduces_modulo_one_without_folding_the_value() {
    let mut graph = Graph::default();
    let literal = graph.add(DataKind::Literal(7));
    let input = unary(&mut graph, UnaryOp::Neg, literal);
    let one = graph.add(DataKind::Literal(1));
    let result = binary(&mut graph, BinaryOp::Mod, input, one);

    optimize_graph(&mut graph);

    assert_eq!(graph[result], DataKind::Literal(0));
}

#[test]
fn folds_equality_of_the_same_value() {
    let mut graph = Graph::default();
    let literal = graph.add(DataKind::Literal(7));
    let input = unary(&mut graph, UnaryOp::Neg, literal);
    let result = commutative(&mut graph, ComBinaryOp::Eq, input, input);

    optimize_graph(&mut graph);

    assert_eq!(graph[result], DataKind::Boolean(true));
}

#[test]
fn preserves_operations_without_folding_rules() {
    let mut graph = Graph::default();
    let literal = graph.add(DataKind::Literal(7));
    let neg = unary(&mut graph, UnaryOp::Neg, literal);
    let reciprocal = unary(&mut graph, UnaryOp::Reciprocal, literal);
    let comparison = binary(&mut graph, BinaryOp::Less, neg, literal);
    let expected: Vec<_> = [neg, reciprocal, comparison]
        .into_iter()
        .map(|handle| (handle, graph[handle].clone()))
        .collect();

    optimize_graph(&mut graph);

    for (handle, expected) in expected {
        assert_eq!(graph[handle], expected);
    }
}

#[test]
fn eliminates_single_and_identical_merge_variants() {
    for count in [1, 2] {
        let mut graph = Graph::default();
        let value = graph.add(DataKind::Literal(42));
        let result = merge(&mut graph, &vec![value; count]);

        optimize_graph(&mut graph);

        assert_eq!(graph.direct(result), graph.direct(value));
    }
}

#[test]
fn preserves_distinct_merge_variants_and_control_flow() {
    let mut graph = Graph::default();
    let a = graph.add(DataKind::Literal(1));
    let b = graph.add(DataKind::Literal(2));
    let result = merge(&mut graph, &[a, b]);
    let expected = graph[result].clone();
    let controls: Vec<_> = graph
        .nodes::<CtrlKind>()
        .map(|node| (node, graph[node].clone()))
        .collect();
    let branches: Vec<_> = graph
        .nodes::<BranchKind>()
        .map(|node| (node, graph[node].clone()))
        .collect();

    optimize_graph(&mut graph);

    assert_eq!(graph[result], expected);
    for (node, expected) in controls {
        assert_eq!(graph[node], expected);
    }
    for (node, expected) in branches {
        assert_eq!(graph[node], expected);
    }
}

#[test]
fn propagates_folding_through_shared_dependencies_and_aliases() {
    let mut graph = Graph::default();
    let a = graph.add(DataKind::Literal(2));
    let b = graph.add(DataKind::Literal(3));
    let sum = commutative(&mut graph, ComBinaryOp::Add, a, b);
    let alias = commutative(&mut graph, ComBinaryOp::Add, b, a);
    assert_eq!(graph.direct(sum), graph.direct(alias));
    let product = commutative(&mut graph, ComBinaryOp::Mul, sum, a);
    let shifted = binary(&mut graph, BinaryOp::Lsh, alias, a);
    let result = commutative(&mut graph, ComBinaryOp::Add, product, shifted);

    optimize_graph(&mut graph);

    assert_eq!(graph[sum], DataKind::Literal(5));
    assert_eq!(graph[alias], DataKind::Literal(5));
    assert_eq!(graph[product], DataKind::Literal(10));
    assert_eq!(graph[shifted], DataKind::Literal(20));
    assert_eq!(graph[result], DataKind::Literal(30));
}

#[test]
fn propagates_folded_values_into_branch_conditions() {
    let mut graph = Graph::default();
    let a = graph.add(DataKind::Literal(2));
    let b = graph.add(DataKind::Literal(3));
    let condition = binary(&mut graph, BinaryOp::Less, a, b);
    let parent = graph.add(CtrlKind::Entry);
    let branch = graph.add(BranchKind { parent, condition });

    optimize_graph(&mut graph);

    assert_eq!(graph[graph[branch].condition], DataKind::Boolean(true));
    assert_eq!(graph[branch].parent, parent);
}
