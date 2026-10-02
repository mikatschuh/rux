use crate::{Data, DataKind, Graph, unary_op::UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Less,
    LessEq,

    Lsh,
    Rsh,

    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComBinaryOp {
    Or,
    Xor,
    And,

    Eq,
    Ne,

    BitOr,
    BitXor,
    BitAnd,

    Add,
    Mul,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct OrderedOps {
    ops: [Data; 2],
}

impl OrderedOps {
    pub fn new_sorted(mut ops: [Data; 2]) -> Self {
        ops.sort();
        Self { ops }
    }

    pub fn get(&self) -> &[Data; 2] {
        &self.ops
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Converted {
    BinaryOp { op: BinaryOp, reverse: bool },
    ComBinaryOp(ComBinaryOp),
    Sub,
}

/// Returns the canonical operator and whether its result must be negated.
fn convert_operator(op: graph_builder::BinaryOp) -> (Converted, bool) {
    use graph_builder::BinaryOp as Source;

    match op {
        Source::Or | Source::Nor => (Converted::ComBinaryOp(ComBinaryOp::Or), op == Source::Nor),
        Source::Xor | Source::Xnor => {
            (Converted::ComBinaryOp(ComBinaryOp::Xor), op == Source::Xnor)
        }
        Source::And | Source::Nand => {
            (Converted::ComBinaryOp(ComBinaryOp::And), op == Source::Nand)
        }
        Source::Eq => (Converted::ComBinaryOp(ComBinaryOp::Eq), false),
        Source::Ne => (Converted::ComBinaryOp(ComBinaryOp::Ne), false),
        Source::Less | Source::Greater => (
            Converted::BinaryOp {
                op: BinaryOp::Less,
                reverse: op == Source::Greater,
            },
            false,
        ),
        Source::LessEq | Source::GreaterEq => (
            Converted::BinaryOp {
                op: BinaryOp::LessEq,
                reverse: op == Source::GreaterEq,
            },
            false,
        ),
        Source::Lsh => (
            Converted::BinaryOp {
                op: BinaryOp::Lsh,
                reverse: false,
            },
            false,
        ),
        Source::Rsh => (
            Converted::BinaryOp {
                op: BinaryOp::Rsh,
                reverse: false,
            },
            false,
        ),
        Source::BitOr | Source::BitNor => (
            Converted::ComBinaryOp(ComBinaryOp::BitOr),
            op == Source::BitNor,
        ),
        Source::BitXor | Source::BitXnor => (
            Converted::ComBinaryOp(ComBinaryOp::BitXor),
            op == Source::BitXnor,
        ),
        Source::BitAnd | Source::BitNand => (
            Converted::ComBinaryOp(ComBinaryOp::BitAnd),
            op == Source::BitNand,
        ),
        Source::Add => (Converted::ComBinaryOp(ComBinaryOp::Add), false),
        Source::Sub => (Converted::Sub, false),
        Source::Mul => (Converted::ComBinaryOp(ComBinaryOp::Mul), false),
        Source::Div => (
            Converted::BinaryOp {
                op: BinaryOp::Div,
                reverse: false,
            },
            false,
        ),
        Source::Mod => (
            Converted::BinaryOp {
                op: BinaryOp::Mod,
                reverse: false,
            },
            false,
        ),
        Source::Dot | Source::Cross | Source::Index | Source::App => {
            unreachable!("{op:?} must be lowered before canonicalization")
        }
    }
}

pub fn process_binary_op(graph: &mut Graph, op: graph_builder::BinaryOp, a: Data, b: Data) -> Data {
    let (op, negate) = convert_operator(op);
    let value = match op {
        Converted::BinaryOp { op, reverse } => graph.add_data_node(DataKind::Binary {
            op,
            ops: if reverse { [b, a] } else { [a, b] },
        }),
        Converted::ComBinaryOp(op) => graph.add_data_node(DataKind::ComBinary {
            op,
            ops: OrderedOps::new_sorted([a, b]),
        }),
        Converted::Sub => {
            // a - b canonicalizes to a + (-b); negate before sorting operands.
            let b = graph.add_data_node(DataKind::Unary {
                op: UnaryOp::Neg,
                value: b,
            });
            graph.add_data_node(DataKind::ComBinary {
                op: ComBinaryOp::Add,
                ops: OrderedOps::new_sorted([a, b]),
            })
        }
    };

    if negate {
        graph.add_data_node(DataKind::Unary {
            op: UnaryOp::Not,
            value,
        })
    } else {
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use graph_builder::BinaryOp as Source;

    #[test]
    fn converts_commutative_operators() {
        for (source, canonical) in [
            (Source::Or, ComBinaryOp::Or),
            (Source::Xor, ComBinaryOp::Xor),
            (Source::And, ComBinaryOp::And),
            (Source::Eq, ComBinaryOp::Eq),
            (Source::Ne, ComBinaryOp::Ne),
            (Source::BitOr, ComBinaryOp::BitOr),
            (Source::BitXor, ComBinaryOp::BitXor),
            (Source::BitAnd, ComBinaryOp::BitAnd),
            (Source::Add, ComBinaryOp::Add),
            (Source::Mul, ComBinaryOp::Mul),
        ] {
            assert_eq!(
                convert_operator(source),
                (Converted::ComBinaryOp(canonical), false),
                "{source:?}"
            );
        }
    }

    #[test]
    fn negates_logical_and_bitwise_complements() {
        for (source, canonical) in [
            (Source::Nor, ComBinaryOp::Or),
            (Source::Xnor, ComBinaryOp::Xor),
            (Source::Nand, ComBinaryOp::And),
            (Source::BitNor, ComBinaryOp::BitOr),
            (Source::BitXnor, ComBinaryOp::BitXor),
            (Source::BitNand, ComBinaryOp::BitAnd),
        ] {
            assert_eq!(
                convert_operator(source),
                (Converted::ComBinaryOp(canonical), true),
                "{source:?}"
            );
        }
    }

    #[test]
    fn reverses_only_greater_comparisons() {
        for (source, op, reverse) in [
            (Source::Less, BinaryOp::Less, false),
            (Source::Greater, BinaryOp::Less, true),
            (Source::LessEq, BinaryOp::LessEq, false),
            (Source::GreaterEq, BinaryOp::LessEq, true),
            (Source::Lsh, BinaryOp::Lsh, false),
            (Source::Rsh, BinaryOp::Rsh, false),
            (Source::Div, BinaryOp::Div, false),
            (Source::Mod, BinaryOp::Mod, false),
        ] {
            assert_eq!(
                convert_operator(source),
                (Converted::BinaryOp { op, reverse }, false),
                "{source:?}"
            );
        }
    }

    #[test]
    fn lowers_subtraction_separately() {
        assert_eq!(convert_operator(Source::Sub), (Converted::Sub, false));
    }

    #[test]
    fn rejects_operators_requiring_prior_lowering() {
        for source in [Source::Dot, Source::Cross, Source::Index, Source::App] {
            assert!(std::panic::catch_unwind(|| convert_operator(source)).is_err());
        }
    }
}
