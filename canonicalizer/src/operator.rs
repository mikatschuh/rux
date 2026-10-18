use std::ops::Deref;

use crate::{Data, DataKind, Graph};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    BitNot,
    Neg,
    /// Multiplicative inverse: `1 / value`.
    Reciprocal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Less,

    Lsh,
    Rsh,

    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComBinaryOp {
    Or,
    BitOr,

    Eq,

    Add,
    Mul,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct OrderedOps {
    ops: [Data; 2],
}

impl OrderedOps {
    pub fn sorted(mut ops: [Data; 2]) -> Self {
        ops.sort();
        Self { ops }
    }

    pub fn get(&self) -> &[Data; 2] {
        &self.ops
    }
}

impl Deref for OrderedOps {
    type Target = [Data; 2];
    fn deref(&self) -> &Self::Target {
        &self.ops
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Converted {
    BinaryOp { op: BinaryOp, reverse: bool },
    ComBinaryOp(ComBinaryOp),
    Sub,
    Div,
    LessEq { reverse: bool },
    And { bitwise: bool },
    Xor { bitwise: bool },
}

/// Returns the canonical operator and an optional complement to apply to its result.
fn convert_operator(op: graph_builder::BinaryOp) -> (Converted, Option<UnaryOp>) {
    use graph_builder::BinaryOp as Source;

    match op {
        Source::Or | Source::Nor => (
            Converted::ComBinaryOp(ComBinaryOp::Or),
            (op == Source::Nor).then_some(UnaryOp::Not),
        ),
        Source::Xor | Source::Xnor => (
            Converted::Xor { bitwise: false },
            (op == Source::Xnor).then_some(UnaryOp::Not),
        ),
        Source::And | Source::Nand => (
            Converted::And { bitwise: false },
            (op == Source::Nand).then_some(UnaryOp::Not),
        ),
        Source::Eq => (Converted::ComBinaryOp(ComBinaryOp::Eq), None),
        Source::Ne => (Converted::ComBinaryOp(ComBinaryOp::Eq), Some(UnaryOp::Not)),
        Source::Less | Source::Greater => (
            Converted::BinaryOp {
                op: BinaryOp::Less,
                reverse: op == Source::Greater,
            },
            None,
        ),
        Source::LessEq | Source::GreaterEq => (
            Converted::LessEq {
                reverse: op == Source::GreaterEq,
            },
            None,
        ),
        Source::Lsh => (
            Converted::BinaryOp {
                op: BinaryOp::Lsh,
                reverse: false,
            },
            None,
        ),
        Source::Rsh => (
            Converted::BinaryOp {
                op: BinaryOp::Rsh,
                reverse: false,
            },
            None,
        ),
        Source::BitOr | Source::BitNor => (
            Converted::ComBinaryOp(ComBinaryOp::BitOr),
            (op == Source::BitNor).then_some(UnaryOp::BitNot),
        ),
        Source::BitXor | Source::BitXnor => (
            Converted::Xor { bitwise: true },
            (op == Source::BitXnor).then_some(UnaryOp::BitNot),
        ),
        Source::BitAnd | Source::BitNand => (
            Converted::And { bitwise: true },
            (op == Source::BitNand).then_some(UnaryOp::BitNot),
        ),
        Source::Add => (Converted::ComBinaryOp(ComBinaryOp::Add), None),
        Source::Sub => (Converted::Sub, None),
        Source::Mul => (Converted::ComBinaryOp(ComBinaryOp::Mul), None),
        Source::Div => (Converted::Div, None),
        Source::Mod => (
            Converted::BinaryOp {
                op: BinaryOp::Mod,
                reverse: false,
            },
            None,
        ),
        Source::Dot | Source::Cross | Source::Index | Source::App => {
            unreachable!("{op:?} must be lowered before canonicalization")
        }
    }
}

pub fn process_source_op(
    graph: &mut Graph,
    op: graph_builder::BinaryOp,
    a: Data,
    b: Data,
    result: Data,
) {
    let (op, complement) = convert_operator(op);
    let value = if complement.is_some() {
        graph.nodes.data.allocate()
    } else {
        result
    };
    match op {
        Converted::BinaryOp { op, reverse } => graph.fill_in_data(
            value,
            DataKind::Binary {
                op,
                ops: if reverse { [b, a] } else { [a, b] },
            },
        ),
        Converted::ComBinaryOp(op) => graph.fill_in_data(
            value,
            DataKind::ComBinary {
                op,
                ops: OrderedOps::sorted([a, b]),
            },
        ),
        Converted::Sub | Converted::Div => {
            let (unary, binary) = if op == Converted::Sub {
                (UnaryOp::Neg, ComBinaryOp::Add)
            } else {
                (UnaryOp::Reciprocal, ComBinaryOp::Mul)
            };
            let rhs = unary_node(graph, unary, b);
            graph.fill_in_data(value, commutative_node(binary, a, rhs));
        }
        Converted::LessEq { reverse } => {
            let less = graph.nodes.data.allocate();
            graph.fill_in_data(
                less,
                DataKind::Binary {
                    op: BinaryOp::Less,
                    ops: if reverse { [b, a] } else { [a, b] },
                },
            );
            let equal = commutative(graph, ComBinaryOp::Eq, a, b);
            graph.fill_in_data(value, commutative_node(ComBinaryOp::Or, less, equal));
        }
        Converted::And { bitwise } | Converted::Xor { bitwise } => {
            let (or, not) = if bitwise {
                (ComBinaryOp::BitOr, UnaryOp::BitNot)
            } else {
                (ComBinaryOp::Or, UnaryOp::Not)
            };
            let not_a = unary_node(graph, not.clone(), a);
            let not_b = unary_node(graph, not.clone(), b);
            let either_not = commutative(graph, or, not_a, not_b);
            let operand = if matches!(op, Converted::Xor { .. }) {
                // a XOR b = NOT(NOT(a OR b) OR NOT(NOT(a) OR NOT(b))).
                let either = commutative(graph, or, a, b);
                let neither = unary_node(graph, not.clone(), either);
                let both = unary_node(graph, not.clone(), either_not);
                commutative(graph, or, neither, both)
            } else {
                // a AND b = NOT(NOT(a) OR NOT(b)).
                either_not
            };
            graph.fill_in_data(
                value,
                DataKind::Unary {
                    op: not,
                    value: operand,
                },
            );
        }
    };

    if let Some(op) = complement {
        graph.fill_in_data(result, DataKind::Unary { op, value })
    }
}

fn unary_node(graph: &mut Graph, op: UnaryOp, value: Data) -> Data {
    let result = graph.nodes.data.allocate();
    graph.fill_in_data(result, DataKind::Unary { op, value });
    result
}

fn commutative_node(op: ComBinaryOp, a: Data, b: Data) -> DataKind {
    DataKind::ComBinary {
        op,
        ops: OrderedOps::sorted([a, b]),
    }
}

fn commutative(graph: &mut Graph, op: ComBinaryOp, a: Data, b: Data) -> Data {
    let result = graph.nodes.data.allocate();
    graph.fill_in_data(result, commutative_node(op, a, b));
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use graph_builder::BinaryOp as Source;

    fn input(graph: &mut Graph, value: usize) -> Data {
        let result = graph.nodes.data.allocate();
        graph.fill_in_data(result, DataKind::Literal(value));
        result
    }

    fn evaluate(graph: &Graph, data: Data) -> usize {
        match &graph.nodes.data[data] {
            DataKind::Literal(value) => *value,
            DataKind::Unary { op, value } => {
                let value = evaluate(graph, *value);
                match op {
                    UnaryOp::Not => usize::from(value == 0),
                    UnaryOp::BitNot => !value,
                    _ => panic!("unexpected unary operator: {op:?}"),
                }
            }
            DataKind::ComBinary { op, ops } => {
                assert!(ops.get()[0] <= ops.get()[1]);
                let [a, b] = ops.get().map(|data| evaluate(graph, data));
                match op {
                    ComBinaryOp::Or => usize::from(a != 0 || b != 0),
                    ComBinaryOp::BitOr => a | b,
                    ComBinaryOp::Eq => usize::from(a == b),
                    _ => panic!("unexpected commutative operator: {op:?}"),
                }
            }
            DataKind::Binary {
                op: BinaryOp::Less,
                ops,
            } => usize::from(evaluate(graph, ops[0]) < evaluate(graph, ops[1])),
            node => panic!("unexpected node: {node:?}"),
        }
    }

    #[test]
    fn preserves_logical_truth_tables() {
        for a in 0..=1 {
            for b in 0..=1 {
                for (op, expected) in [
                    (Source::Or, a | b),
                    (Source::Nor, 1 ^ (a | b)),
                    (Source::And, a & b),
                    (Source::Nand, 1 ^ (a & b)),
                    (Source::Xor, a ^ b),
                    (Source::Xnor, 1 ^ (a ^ b)),
                    (Source::Eq, usize::from(a == b)),
                    (Source::Ne, usize::from(a != b)),
                ] {
                    let mut graph = Graph::default();
                    let lhs = input(&mut graph, a);
                    let rhs = input(&mut graph, b);
                    let result = graph.nodes.data.allocate();
                    process_source_op(&mut graph, op, lhs, rhs, result);
                    assert_eq!(evaluate(&graph, result), expected, "{a} {op:?} {b}");
                }
            }
        }
    }

    #[test]
    fn preserves_bitwise_results_including_high_bits() {
        let values = [0, 1, 2, 3, 5, 10, usize::MAX, 1 << (usize::BITS - 1)];
        for a in values {
            for b in values {
                for (op, expected) in [
                    (Source::BitOr, a | b),
                    (Source::BitNor, !(a | b)),
                    (Source::BitAnd, a & b),
                    (Source::BitNand, !(a & b)),
                    (Source::BitXor, a ^ b),
                    (Source::BitXnor, !(a ^ b)),
                ] {
                    let mut graph = Graph::default();
                    let lhs = input(&mut graph, a);
                    let rhs = input(&mut graph, b);
                    let result = graph.nodes.data.allocate();
                    process_source_op(&mut graph, op, lhs, rhs, result);
                    assert_eq!(evaluate(&graph, result), expected, "{a} {op:?} {b}");
                }
            }
        }
    }

    #[test]
    fn preserves_comparison_direction_and_equality() {
        for a in 0..3 {
            for b in 0..3 {
                for (op, expected) in [
                    (Source::Less, a < b),
                    (Source::Greater, a > b),
                    (Source::LessEq, a <= b),
                    (Source::GreaterEq, a >= b),
                ] {
                    let mut graph = Graph::default();
                    let lhs = input(&mut graph, a);
                    let rhs = input(&mut graph, b);
                    let result = graph.nodes.data.allocate();
                    process_source_op(&mut graph, op, lhs, rhs, result);
                    assert_eq!(
                        evaluate(&graph, result),
                        usize::from(expected),
                        "{a} {op:?} {b}"
                    );
                    if matches!(op, Source::LessEq | Source::GreaterEq) {
                        assert!(matches!(
                            graph.nodes.data[result],
                            DataKind::ComBinary {
                                op: ComBinaryOp::Or,
                                ..
                            }
                        ));
                    }
                }
            }
        }
    }

    #[test]
    fn lowers_arithmetic_using_the_original_rhs() {
        for (source, binary, unary) in [
            (Source::Div, ComBinaryOp::Mul, UnaryOp::Reciprocal),
            (Source::Sub, ComBinaryOp::Add, UnaryOp::Neg),
        ] {
            let mut graph = Graph::default();
            let a = input(&mut graph, 6);
            let b = input(&mut graph, 3);
            let result = graph.nodes.data.allocate();
            process_source_op(&mut graph, source, a, b, result);
            let DataKind::ComBinary { op, ops } = &graph.nodes.data[result] else {
                panic!("expected a commutative arithmetic node");
            };
            assert_eq!(*op, binary);
            assert_eq!(ops.get()[0], a);
            assert_eq!(
                graph.nodes.data[ops.get()[1]],
                DataKind::Unary {
                    op: unary,
                    value: b
                }
            );
        }
    }

    #[test]
    fn preserves_remaining_operators() {
        for (source, canonical) in [
            (Source::Lsh, BinaryOp::Lsh),
            (Source::Rsh, BinaryOp::Rsh),
            (Source::Mod, BinaryOp::Mod),
        ] {
            assert_eq!(
                convert_operator(source),
                (
                    Converted::BinaryOp {
                        op: canonical,
                        reverse: false
                    },
                    None
                )
            );
        }
        for (source, canonical) in [
            (Source::Add, ComBinaryOp::Add),
            (Source::Mul, ComBinaryOp::Mul),
        ] {
            assert_eq!(
                convert_operator(source),
                (Converted::ComBinaryOp(canonical), None)
            );
        }
    }

    #[test]
    fn rejects_operators_requiring_prior_lowering() {
        for source in [Source::Dot, Source::Cross, Source::Index, Source::App] {
            assert!(std::panic::catch_unwind(|| convert_operator(source)).is_err());
        }
    }
}
