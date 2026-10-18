use canonicalizer::{BranchKind, CtrlKind, DataKind, Dir, Graph, MergeKind};

mod peephole;

pub struct Worklist {
    data: Vec<Dir<DataKind>>,
    ctrl: Vec<Dir<CtrlKind>>,
    branch: Vec<Dir<BranchKind>>,
    merge: Vec<Dir<MergeKind>>,
}

pub struct PeepholePass {
    graph: Graph,
    worklist: Worklist,
}

impl PeepholePass {}
