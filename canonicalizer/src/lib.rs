//! This is the Global Value Numbering pass module.
//! It's job is to go through all the nodes cleaning up and dedublicating them.
//! At the end a canonicalized graph should be created.

use std::collections::HashMap;

use graph_builder as source;

use crate::operator::process_source_op;

mod canonical;
mod graph;
mod operator;
mod users;

pub use canonical::Unique;
pub use graph::{Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, MergeKind};
pub use operator::{BinaryOp, ComBinaryOp, OrderedOps, UnaryOp};

pub fn process_graph(
    source_graph: &source::Graph,
    start: impl IntoIterator<Item = source::Data>,
) -> (Graph, Box<[Data]>) {
    let mut pass = GvnPass::new(source_graph);
    let start = start
        .into_iter()
        .map(|data| pass.process_data(data))
        .collect::<Box<[Data]>>();

    (pass.canonical_graph, start)
}

#[derive(Default)]
struct Visited {
    data: HashMap<source::Data, Data>,
    ctrl: HashMap<source::Ctrl, Ctrl>,
    branch: HashMap<source::Branch, Branch>,
    merge: HashMap<source::Merge, Merge>,
}

struct GvnPass<'src, 'graph> {
    source_graph: &'graph source::Graph<'src>,
    canonical_graph: Graph,
    visited: Visited,
    /// Maps canonical input positions to source input positions for each source merge.
    merge_variant_permutation_table: HashMap<source::Merge, Box<[usize]>>,
}

impl<'src, 'graph> GvnPass<'src, 'graph> {
    fn new(source_graph: &'graph source::Graph<'src>) -> Self {
        Self {
            source_graph,
            canonical_graph: Graph::default(),
            visited: Visited::default(),
            merge_variant_permutation_table: HashMap::new(),
        }
    }

    fn process_data(&mut self, data: source::Data) -> Data {
        if let Some(prev) = self.visited.data.get(&data) {
            return *prev;
        }

        let canonical_data = self.canonical_graph.allocate();
        self.visited.data.insert(data, canonical_data);

        match self.source_graph[data].clone() {
            source::DataKind::Unit => self.canonical_graph.bind(canonical_data, DataKind::Unit),
            source::DataKind::Literal(_) => todo!("literals shouldn't actually arrive here"),
            source::DataKind::Quote(quote) => self
                .canonical_graph
                .bind(canonical_data, DataKind::Quote(quote)),
            source::DataKind::Boolean(boolean) => self
                .canonical_graph
                .bind(canonical_data, DataKind::Boolean(boolean)),

            source::DataKind::Unary { op, value } => {
                let value = self.process_data(value);
                let op = match op {
                    source::UnaryOp::Not => operator::UnaryOp::Not,
                    source::UnaryOp::Neg => operator::UnaryOp::Neg,
                    source::UnaryOp::Ptr | source::UnaryOp::Deref => {
                        todo!("implement memory")
                    }
                };
                self.canonical_graph
                    .bind(canonical_data, DataKind::Unary { op, value })
            }
            source::DataKind::Binary { op, ops } => {
                let a = self.process_data(ops[0]);
                let b = self.process_data(ops[1]);
                process_source_op(&mut self.canonical_graph, op, a, b, canonical_data)
            }
            source::DataKind::Load { .. } => todo!("implement memory"),

            source::DataKind::Merge { merge, variants } => {
                let canonical_merge = self.process_merge(merge);
                let variants = variants
                    .into_iter()
                    .map(|v| self.process_data(v))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let permutation = &self.merge_variant_permutation_table[&merge];
                assert_eq!(
                    variants.len(),
                    permutation.len(),
                    "data-mergee/merge arity mismatch"
                );
                let variants = permutation.iter().map(|&i| variants[i]).collect();
                self.canonical_graph.bind(
                    canonical_data,
                    DataKind::Merge {
                        merge: canonical_merge,
                        variants,
                    },
                )
            }

            source::DataKind::Type { .. }
            | source::DataKind::Placeholder
            | source::DataKind::Err => unreachable!(),
        };
        canonical_data
    }

    fn process_ctrl(&mut self, ctrl: source::Ctrl) -> Ctrl {
        if let Some(prev) = self.visited.ctrl.get(&ctrl) {
            return *prev;
        }

        let canonical_ctrl = self.canonical_graph.allocate();
        self.visited.ctrl.insert(ctrl, canonical_ctrl);

        match self.source_graph[ctrl].clone() {
            source::CtrlKind::Entry => self.canonical_graph.bind(canonical_ctrl, CtrlKind::Entry),
            source::CtrlKind::Branch { branch, idx } => {
                let branch = self.process_branch(branch);
                self.canonical_graph
                    .bind(canonical_ctrl, CtrlKind::Branch { branch, idx })
            }
            source::CtrlKind::Merge { merge } => {
                let merge = self.process_merge(merge);
                self.canonical_graph
                    .bind(canonical_ctrl, CtrlKind::Merge(merge))
            }
            source::CtrlKind::Placeholder => unreachable!(),
        };

        canonical_ctrl
    }

    fn process_branch(&mut self, branch: source::Branch) -> Branch {
        if let Some(prev) = self.visited.branch.get(&branch) {
            return *prev;
        };

        let canonical_branch = self.canonical_graph.allocate();
        self.visited.branch.insert(branch, canonical_branch);

        let source::BranchKind { parent, condition } = self.source_graph[branch].clone();
        let parent = self.process_ctrl(parent);
        let condition = self.process_data(condition);
        self.canonical_graph
            .bind(canonical_branch, BranchKind { parent, condition });

        canonical_branch
    }

    fn process_merge(&mut self, merge: source::Merge) -> Merge {
        if let Some(prev) = self.visited.merge.get(&merge) {
            return *prev;
        }

        let canonical_merge = self.canonical_graph.allocate();
        self.visited.merge.insert(merge, canonical_merge);

        let source::MergeKind { prev } = self.source_graph[merge].clone();
        let prev = prev
            .into_iter()
            .map(|v| self.process_ctrl(v))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let permutation = input_permutation(&prev);
        let prev = permutation.iter().map(|&i| prev[i]).collect();
        self.canonical_graph
            .bind(canonical_merge, MergeKind { prev });

        self.merge_variant_permutation_table
            .insert(merge, permutation);
        canonical_merge
    }
}

/// Returns source indices in canonical order, preserving equal input occurrences.
fn input_permutation<T: Ord>(inputs: &[T]) -> Box<[usize]> {
    let mut permutation: Vec<_> = (0..inputs.len()).collect();
    permutation.sort_unstable_by(|&a, &b| inputs[a].cmp(&inputs[b]));
    permutation.into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::input_permutation;

    #[test]
    fn preserves_predecessor_value_pairing() {
        // A three-input cycle distinguishes the permutation from its inverse.
        let prev = [30, 10, 20];
        let variants = ["from_30", "from_10", "from_20"];
        let permutation = input_permutation(&prev);
        let incoming: Vec<_> = permutation
            .iter()
            .map(|&i| (prev[i], variants[i]))
            .collect();
        assert_eq!(
            incoming,
            [(10, "from_10"), (20, "from_20"), (30, "from_30")]
        );
    }

    #[test]
    fn preserves_repeated_predecessor_occurrences() {
        assert_eq!(&*input_permutation(&[20, 10, 20, 10]), &[1, 3, 0, 2]);
    }

    #[test]
    fn preserves_canonical_and_empty_input_order() {
        assert_eq!(&*input_permutation(&[10, 20, 30]), &[0, 1, 2]);
        assert!(input_permutation::<usize>(&[]).is_empty());
    }
}
