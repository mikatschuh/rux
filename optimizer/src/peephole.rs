use canonicalizer::{BinaryOp, ComBinaryOp, Data, DataKind, OrderedOps, UnaryOp};

use crate::PeepholePass;

impl PeepholePass {
    fn simplify_data(&mut self, node: Data) -> Option<Data> {
        match &self.graph.nodes.data[node] {
            DataKind::ComBinary {
                op: ComBinaryOp::Eq,
                ops,
            } => {
                if ops[0] == ops[1] {
                    Some(self.graph.add_data(DataKind::Boolean(true)))
                } else {
                    None
                }
            }
            DataKind::ComBinary { op, ops } => self.const_eval_com_binary(*op, ops.clone()),
            DataKind::Binary { op, ops } => {
                let op = *op;
                let ops = *ops;
                self.identity_binary(op, ops)
                    .or_else(|| self.const_eval_binary(op, ops))
            }
            DataKind::Unary { op, value } => self.const_eval_unary(*op, *value),
            DataKind::Merge { variants, .. } if variants.len() == 1 => Some(variants[0]),
            DataKind::Merge { variants, .. } => self.phi_variant_equivalence(variants),
            DataKind::Unit | DataKind::Boolean(_) | DataKind::Literal(_) | DataKind::Quote(_) => {
                None
            }
        }
    }

    fn phi_variant_equivalence(&self, variants: &[Data]) -> Option<Data> {
        assert!(!variants.is_empty());
        if variants.iter().all(|v| *v == variants[0]) {
            Some(variants[0])
        } else {
            None
        }
    }

    fn const_eval_unary(&mut self, op: UnaryOp, value: Data) -> Option<Data> {
        if let DataKind::Boolean(value) = self.graph[value] {
            let result = !value;
            Some(self.graph.add_data(DataKind::Boolean(result)))
        } else if let DataKind::Literal(value) = self.graph[value] {
            let result = match op {
                UnaryOp::Not => unreachable!(),
                UnaryOp::BitNot => !value,
                UnaryOp::Neg | UnaryOp::Reciprocal => return None,
            };
            Some(self.graph.add_data(DataKind::Literal(result)))
        } else {
            None
        }
    }

    fn const_eval_binary(&mut self, op: BinaryOp, ops: [Data; 2]) -> Option<Data> {
        if let DataKind::Literal(a) = self.graph[ops[0]]
            && let DataKind::Literal(b) = self.graph[ops[1]]
        {
            let result = match op {
                BinaryOp::Less => {
                    let result = a < b;
                    return Some(self.graph.add_data(DataKind::Boolean(result)));
                }
                BinaryOp::Lsh => a << b,
                BinaryOp::Rsh => a >> b,
                BinaryOp::Mod => a % b,
            };
            Some(self.graph.add_data(DataKind::Literal(result)))
        } else {
            None
        }
    }

    fn identity_binary(&mut self, op: BinaryOp, ops: [Data; 2]) -> Option<Data> {
        match op {
            BinaryOp::Less => {
                /* if self.graph[ops[0]] == DataKind::Literal(usize::MIN)
                    || self.graph[ops[1]] == DataKind::Literal(usize::MAX)
                {
                    let value = self.graph.add_data(DataKind::ComBinary {
                        op: ComBinaryOp::Eq,
                        ops: OrderedOps::sorted(ops),
                    });
                    return Some(self.graph.add_data(DataKind::Unary {
                        op: UnaryOp::Not,
                        value,
                    }));
                } */
                None
            }
            BinaryOp::Lsh | BinaryOp::Rsh => {
                if self.graph[ops[1]] == DataKind::Literal(0) {
                    Some(ops[0])
                } else {
                    None
                }
            }
            BinaryOp::Mod => {
                if self.graph[ops[1]] == DataKind::Literal(1) {
                    Some(self.graph.add_data(DataKind::Literal(0)))
                } else {
                    None
                }
            }
        }
    }

    fn const_eval_com_binary(&mut self, op: ComBinaryOp, ops: OrderedOps) -> Option<Data> {
        if let DataKind::Boolean(a) = self.graph[ops[0]]
            && let DataKind::Boolean(b) = self.graph[ops[1]]
            && op == ComBinaryOp::Or
        {
            let result = a || b;
            Some(self.graph.add_data(DataKind::Boolean(result)))
        } else if let DataKind::Literal(a) = self.graph[ops[0]]
            && let DataKind::Literal(b) = self.graph[ops[1]]
        {
            let result = match op {
                ComBinaryOp::Or | ComBinaryOp::Eq => unreachable!(),
                ComBinaryOp::BitOr => a | b,
                ComBinaryOp::Add => a + b,
                ComBinaryOp::Mul => a * b,
            };
            Some(self.graph.add_data(DataKind::Literal(result)))
        } else {
            None
        }
    }

    fn identity_com_binary(&mut self, op: ComBinaryOp, ops: OrderedOps) -> Option<Data> {
        match op {
            ComBinaryOp::Eq => None,
            ComBinaryOp::Or => {
                if ops.iter().all(|op| ops[0] == *op) {
                    Some(ops[0])
                } else if let Some(i) = ops
                    .iter()
                    .enumerate()
                    .find(|&(_, op)| self.graph[*op] == DataKind::Boolean(false))
                    .map(|(i, _)| !i & 1)
                {
                    Some(ops[i])
                } else {
                    ops.iter()
                        .copied()
                        .find(|op| self.graph[*op] == DataKind::Boolean(true))
                }
            }
            ComBinaryOp::BitOr => {
                if ops.iter().all(|op| ops[0] == *op) {
                    Some(ops[0])
                } else if let Some(i) = ops
                    .iter()
                    .enumerate()
                    .find(|&(_, op)| self.graph[*op] == DataKind::Literal(0))
                    .map(|(i, _)| !i & 1)
                {
                    Some(ops[i])
                } else {
                    ops.iter()
                        .copied()
                        .find(|op| self.graph[*op] == DataKind::Literal(!0))
                }
            }
            ComBinaryOp::Add => {
                if (0..=1).any(|i| {
                    self.graph[ops[i]]
                        == (DataKind::Unary {
                            op: UnaryOp::Neg,
                            value: ops[!i & 1],
                        })
                }) {
                    Some(self.graph.add_data(DataKind::Literal(0)))
                } else {
                    ops.iter()
                        .enumerate()
                        .find(|&(_, op)| self.graph[*op] == DataKind::Literal(0))
                        .map(|(i, _)| !i & 1)
                        .map(|i| ops[i])
                }
            }
            ComBinaryOp::Mul => {
                if let Some(i) = ops
                    .iter()
                    .enumerate()
                    .find(|&(_, op)| self.graph[*op] == DataKind::Literal(1))
                    .map(|(i, _)| !i & 1)
                {
                    Some(ops[i])
                } else {
                    ops.iter()
                        .copied()
                        .find(|op| self.graph[*op] == DataKind::Literal(0))
                }
            }
        }
    }
}
