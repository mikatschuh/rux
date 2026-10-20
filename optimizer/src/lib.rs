use std::collections::HashSet;

use canonicalizer::{BranchKind, CtrlKind, DataKind, Unique, Graph, MergeKind};

mod peephole;

pub fn optimize_graph(graph: &mut Graph) {
    let mut pass = PeepholePass {
        graph,
        worklist: Worklist::default(),
        visited: Visited::default(),
    };

    pass.worklist.data.extend(pass.graph.nodes());
    pass.worklist.ctrl.extend(pass.graph.nodes());
    pass.worklist.branch.extend(pass.graph.nodes());
    pass.worklist.merge.extend(pass.graph.nodes());

    pass.execute_worklist();
}

#[derive(Default)]
struct Worklist {
    data: HashSet<Unique<DataKind>>,
    ctrl: HashSet<Unique<CtrlKind>>,
    branch: HashSet<Unique<BranchKind>>,
    merge: HashSet<Unique<MergeKind>>,
}

#[derive(Default)]
struct Visited {
    data: Vec<Unique<DataKind>>,
    ctrl: Vec<Unique<CtrlKind>>,
    branch: Vec<Unique<BranchKind>>,
    merge: Vec<Unique<MergeKind>>,
}

struct PeepholePass<'graph> {
    graph: &'graph mut Graph,
    worklist: Worklist,
    visited: Visited,
}

impl<'graph> PeepholePass<'graph> {
    fn new(graph: &'graph mut Graph) -> Self {
        Self {
            graph,
            worklist: Worklist::default(),
            visited: Visited::default(),
        }
    }

    fn execute_worklist(&mut self) {
        while let Some(node) = self.worklist.data.iter().copied().next() {
            self.worklist.data.remove(&node);

            if let Some(new) = self.simplify_data(node) {
                self.graph
                    .data_data_users(node)
                    .for_each(|user| _ = self.worklist.data.insert(user));
                // we don't add branch users to the worklist because we can't simplify them right now

                let new = self.graph.direct(new);
                self.graph.remap(node, new);
            }
        }
    }
}
