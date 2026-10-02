//! This is the Global Value Numbering pass module.
//! It's job is to go through all the nodes cleaning up and dedublicating them.
//! At the end a canonicalized graph should be created.

use std::collections::HashMap;

use graph_builder as source;

mod binary_op;
mod canonical;
mod graph;
mod unary_op;
mod users;

use crate::binary_op::process_binary_op;
pub use crate::graph::{
    Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, MergeKind,
};
pub use canonical::Dir;

struct GvnPass<'src, 'graph> {
    source_graph: &'graph source::Graph<'src>,
    canonical_graph: Graph,

    visited_data: HashMap<source::Data, Data>,
    visited_ctrl: HashMap<source::Ctrl, Ctrl>,
    visited_branch: HashMap<source::Branch, Branch>,
    visited_merge: HashMap<source::Merge, Merge>,
}

impl<'src, 'graph> GvnPass<'src, 'graph> {
    fn process_data(&mut self, data: source::Data) -> Data {
        if let Some(prev) = self.visited_data.get(&data) {
            return *prev;
        }

        let canonical_data = match self.source_graph[data].clone() {
            source::DataKind::Unit => self.canonical_graph.add_data_node(DataKind::Unit),
            source::DataKind::Literal(_) => todo!("literals shouldn't actually arrive here"),
            source::DataKind::Quote(quote) => {
                self.canonical_graph.add_data_node(DataKind::Quote(quote))
            }
            source::DataKind::Boolean(boolean) => self
                .canonical_graph
                .add_data_node(DataKind::Boolean(boolean)),

            source::DataKind::Unary { op, value } => {
                let value = self.process_data(value);
                let op = match op {
                    source::UnaryOp::Not => unary_op::UnaryOp::Not,
                    source::UnaryOp::Neg => unary_op::UnaryOp::Neg,
                    source::UnaryOp::Ptr | source::UnaryOp::Deref => {
                        todo!("implement memory")
                    }
                };
                self.canonical_graph
                    .add_data_node(DataKind::Unary { op, value })
            }
            source::DataKind::Binary { op, ops } => {
                let a = self.process_data(ops[0]);
                let b = self.process_data(ops[1]);
                process_binary_op(&mut self.canonical_graph, op, a, b)
            }
            source::DataKind::Load { .. } => todo!("implement memory"),

            source::DataKind::Phi { merge, variants } => {
                let merge = self.process_merge(merge);
                let variants = variants
                    .into_iter()
                    .map(|v| self.process_data(v))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                self.canonical_graph
                    .add_data_node(DataKind::Phi { merge, variants })
            }

            source::DataKind::Type { .. }
            | source::DataKind::Placeholder
            | source::DataKind::Err => unreachable!(),
        };

        self.visited_data.insert(data, canonical_data);
        canonical_data
    }

    fn process_ctrl(&mut self, ctrl: source::Ctrl) -> Ctrl {
        if let Some(prev) = self.visited_ctrl.get(&ctrl) {
            return *prev;
        }

        let canonical_ctrl = match self.source_graph[ctrl].clone() {
            source::CtrlKind::Entry => self.canonical_graph.add_ctrl_node(CtrlKind::Entry),
            source::CtrlKind::Branch { branch, idx } => {
                let branch = self.process_branch(branch);
                self.canonical_graph
                    .add_ctrl_node(CtrlKind::Branch { branch, idx })
            }
            source::CtrlKind::Merge { merge } => {
                let merge = self.process_merge(merge);
                self.canonical_graph.add_ctrl_node(CtrlKind::Merge(merge))
            }
            source::CtrlKind::Placeholder => unreachable!(),
        };

        self.visited_ctrl.insert(ctrl, canonical_ctrl);
        canonical_ctrl
    }

    fn process_branch(&mut self, branch: source::Branch) -> Branch {
        if let Some(prev) = self.visited_branch.get(&branch) {
            return *prev;
        };

        let source::BranchKind { parent, condition } = self.source_graph[branch].clone();
        let parent = self.process_ctrl(parent);
        let condition = self.process_data(condition);
        let canonical_branch = self
            .canonical_graph
            .add_branch_node(BranchKind { parent, condition });

        self.visited_branch.insert(branch, canonical_branch);
        canonical_branch
    }

    fn process_merge(&mut self, merge: source::Merge) -> Merge {
        if let Some(prev) = self.visited_merge.get(&merge) {
            return *prev;
        }

        let source::MergeKind { prev } = self.source_graph[merge].clone();
        let prev = prev
            .into_iter()
            .map(|v| self.process_ctrl(v))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let canonical_merge = self.canonical_graph.add_merge_node(MergeKind { prev });

        self.visited_merge.insert(merge, canonical_merge);
        canonical_merge
    }
}
