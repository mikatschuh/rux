//! This is the Global Value Numbering pass module.
//! It's job is to go through all the nodes cleaning up and dedublicating them.
//! At the end a canonicalized graph should be created.

use std::collections::HashMap;

use graph_builder::{Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, MergeKind};

use crate::graph::{
    Branch as UBranch, BranchKind as UBranchKind, Ctrl as UCtrl, CtrlKind as UCtrlKind,
    Data as UData, DataKind as UDataKind, Graph as UGraph, Merge as UMerge,
    MergeKind as UMergeKind,
};

mod dedup;
mod graph;
mod users;

struct GvnPass<'src, 'graph> {
    graph: &'graph Graph<'src>,
    ugraph: UGraph,

    visited_data: HashMap<Data, UData>,
    visited_ctrl: HashMap<Ctrl, UCtrl>,
    visited_branch: HashMap<Branch, UBranch>,
    visited_merge: HashMap<Merge, UMerge>,
}

impl<'src, 'graph> GvnPass<'src, 'graph> {
    fn process_data(&mut self, data: Data) -> UData {
        if let Some(prev) = self.visited_data.get(&data) {
            return *prev;
        }

        let udata = match self.graph[data].clone() {
            DataKind::Unit => self.ugraph.add_data_node(UDataKind::Unit),
            DataKind::Literal(_) => todo!("literals shouldn't actually arrive here"),
            DataKind::Quote(quote) => self.ugraph.add_data_node(UDataKind::Quote(quote)),
            DataKind::Boolean(boolean) => self.ugraph.add_data_node(UDataKind::Boolean(boolean)),

            DataKind::Unary { op, value } => {
                let value = self.process_data(value);
                self.ugraph.add_data_node(UDataKind::Unary { op, value })
            }
            DataKind::Binary { op, ops } => {
                let ops = [self.process_data(ops[0]), self.process_data(ops[1])];
                self.ugraph.add_data_node(UDataKind::Binary { op, ops })
            }
            DataKind::Load { .. } => todo!("implement memory"),

            DataKind::Phi { merge, variants } => {
                let merge = self.process_merge(merge);
                let variants = variants
                    .into_iter()
                    .map(|v| self.process_data(v))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                self.ugraph
                    .add_data_node(UDataKind::Phi { merge, variants })
            }

            DataKind::Type { .. } | DataKind::Placeholder | DataKind::Err => unreachable!(),
        };

        self.visited_data.insert(data, udata);
        udata
    }

    fn process_ctrl(&mut self, ctrl: Ctrl) -> UCtrl {
        if let Some(prev) = self.visited_ctrl.get(&ctrl) {
            return *prev;
        }

        let uctrl = match self.graph[ctrl].clone() {
            CtrlKind::Entry => self.ugraph.add_ctrl_node(UCtrlKind::Entry),
            CtrlKind::Branch { branch, idx } => {
                let branch = self.process_branch(branch);
                self.ugraph.add_ctrl_node(UCtrlKind::Branch { branch, idx })
            }
            CtrlKind::Merge { merge } => {
                let merge = self.process_merge(merge);
                self.ugraph.add_ctrl_node(UCtrlKind::Merge(merge))
            }
            CtrlKind::Placeholder => unreachable!(),
        };

        self.visited_ctrl.insert(ctrl, uctrl);
        uctrl
    }

    fn process_branch(&mut self, branch: Branch) -> UBranch {
        if let Some(prev) = self.visited_branch.get(&branch) {
            return *prev;
        };

        let BranchKind { parent, condition } = self.graph[branch].clone();
        let parent = self.process_ctrl(parent);
        let condition = self.process_data(condition);
        let ubranch = self
            .ugraph
            .add_branch_node(UBranchKind { parent, condition });

        self.visited_branch.insert(branch, ubranch);
        ubranch
    }

    fn process_merge(&mut self, merge: Merge) -> UMerge {
        if let Some(prev) = self.visited_merge.get(&merge) {
            return *prev;
        }

        let MergeKind { prev } = self.graph[merge].clone();
        let prev = prev
            .into_iter()
            .map(|v| self.process_ctrl(v))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let umerge = self.ugraph.add_merge_node(UMergeKind { prev });

        self.visited_merge.insert(merge, umerge);
        umerge
    }
}
