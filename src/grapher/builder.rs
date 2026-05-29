use std::collections::HashMap;

use crate::{
    error::{ErrorCode, Errors},
    grapher::{
        Graph,
        binding::VarID,
        graph::{CtrlID, CtrlKind, DataID, DataKind, MergeID, TypeID},
        loops::{LoopBackedges, OpenLoop},
    },
    parser::Expr,
};

/// This describes an **existing** block.
pub type BlockID = usize;

#[derive(Clone, Debug)]
pub struct Placeholder {
    var: VarID,
    data_placeholder: DataID,
    /// this the AST-Node that read out the value of the incomplete phi for the first time
    reference: Expr,
}

#[derive(Clone, Debug)]
pub struct Block {
    definitions: HashMap<VarID, DataID>,
    cfg: CfgNode,
}

#[derive(Clone, Debug)]
enum CfgNode {
    Start,
    Branch {
        predecessor: BlockID,
    },
    Merge {
        merge: MergeID,
        /// pred.len() > 1
        predecessors: Vec<BlockID>,
    },
    IncompleteMerge,
}

pub struct Cfg {
    blocks: Vec<Block>,
    placeholders: Vec<Vec<Placeholder>>,
    ctrl_placeholders: Vec<CtrlID>,
}

impl Cfg {
    pub fn new() -> (Self, BlockID) {
        (
            Self {
                blocks: vec![Block {
                    definitions: HashMap::new(),
                    cfg: CfgNode::Start,
                }],
                placeholders: vec![],
                ctrl_placeholders: vec![],
            },
            0,
        )
    }

    fn push_block(&mut self, block: Block) -> BlockID {
        let id = self.blocks.len();
        self.blocks.push(block);
        id
    }

    fn branch(&mut self, predecessor: BlockID) -> BlockID {
        self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::Branch { predecessor },
        })
    }

    fn merge(&mut self, predecessors: Vec<BlockID>, merge: MergeID) -> BlockID {
        self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::Merge {
                merge,
                predecessors,
            },
        })
    }

    fn add_unsealed(&mut self, graph: &mut Graph) -> BlockID {
        let id = self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::IncompleteMerge,
        });
        self.placeholders.push(vec![]);
        self.ctrl_placeholders.push(graph.add_ctrl_placeholder());
        id
    }

    fn seal_block(
        &mut self,
        block: BlockID,
        predecessors: Vec<BlockID>,
        ctrl_predecessors: Vec<CtrlID>,
        graph: &mut Graph,
        errors: &mut Errors,
    ) {
        let merge = graph.add_merge(ctrl_predecessors);
        let mut ctrl_placeholder = self.ctrl_placeholders.pop().unwrap();
        *ctrl_placeholder = CtrlKind::Merge {
            merge: merge.clone(),
        };

        let unsealed = &mut self.blocks[block];
        unsealed.cfg = CfgNode::Merge {
            merge: merge.clone(),
            predecessors: predecessors.clone(),
        };
        let placeholders = self.placeholders.pop().unwrap(); // caller side guaranties

        // add thoses backedges to the phi nodes of mutable variables declared outside the loop but used inside
        'outer: for Placeholder {
            var,
            data_placeholder: mut placeholder,
            reference,
        } in placeholders
        {
            let mut variants = vec![];
            for block in &predecessors {
                match self.get_definition(*block, var, reference.clone(), graph) {
                    Some(variant) => variants.push(variant),
                    None => {
                        errors.push(reference.span, ErrorCode::ReadUnitializedOrMoved);
                        continue 'outer;
                    }
                }
            }

            let phi = graph.add_phi(merge.clone(), variants);
            placeholder.kind = DataKind::Phi { phi };
        }
    }

    pub fn assign_variable(&mut self, block: BlockID, var: VarID, value: DataID) -> Option<DataID> {
        self.blocks[block].definitions.insert(var, value)
    }

    pub fn read_variable(
        &mut self,
        ty: TypeID,

        block: BlockID,
        var: VarID,
        read: Expr,
        graph: &mut Graph,
    ) -> Option<DataID> {
        self.get_definition(block, var, read, graph)
            .map(|mut data| {
                data.ty = ty;
                data
            })
    }

    fn get_definition(
        &mut self,
        block: BlockID,
        var: VarID,
        read: Expr,
        graph: &mut Graph,
    ) -> Option<DataID> {
        let current_block = &mut self.blocks[block];

        if let Some(current_blocks_definition) = current_block.definitions.get(&var) {
            return Some(current_blocks_definition.clone());
        }

        match &mut current_block.cfg {
            CfgNode::Start => None,
            CfgNode::Branch { predecessor: pred } => {
                let block = *pred;
                match self.get_definition(block, var, read, graph) {
                    Some(state) => {
                        self.blocks[block].definitions.insert(var, state.clone()); // insert for the next lookup
                        Some(state)
                    }
                    None => None,
                }
            }
            CfgNode::Merge {
                merge,
                predecessors: pred,
            } => {
                let merge = merge.clone();

                let mut variants = vec![];
                for pred in pred.clone() {
                    variants.push(self.get_definition(pred, var, read.clone(), graph)?);
                }
                let first = variants.first().unwrap();
                let value = if variants.iter().all(|v| v == first) {
                    variants.pop().unwrap()
                } else {
                    let ty = variants[0].ty.clone();
                    let phi = graph.add_phi(merge, variants);

                    graph.add_data_phi(phi, ty)
                };

                self.blocks[block].definitions.insert(var, value.clone()); // insert for the next lookup

                Some(value)
            }
            CfgNode::IncompleteMerge => {
                // we know that jump_table has to have at least one loop as IncompleteMerge cant be created without that to hold
                let placeholder = graph.add_placeholder();
                self.placeholders.last_mut().unwrap().push(
                    // an incomplete merge can only exist when there are placeholders
                    Placeholder {
                        var,
                        data_placeholder: placeholder.clone(),
                        reference: read,
                    },
                );
                self.blocks[block]
                    .definitions
                    .insert(var, placeholder.clone()); // insert for the next lookup
                Some(placeholder)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct CtrlCursor {
    pub block: BlockID,
    pub ctrl: CtrlID,
}

impl CtrlCursor {
    pub fn with_data(self, data: DataID) -> DataCursor {
        DataCursor {
            block: self.block,
            ctrl: self.ctrl,
            data,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DataCursor {
    pub block: BlockID,
    pub ctrl: CtrlID,
    pub data: DataID,
}

impl DataCursor {
    pub fn without_data(self) -> CtrlCursor {
        CtrlCursor {
            block: self.block,
            ctrl: self.ctrl,
        }
    }

    pub fn split(self) -> (CtrlCursor, DataID) {
        (
            CtrlCursor {
                block: self.block,
                ctrl: self.ctrl,
            },
            self.data,
        )
    }
}

impl Graph {
    pub fn open_loop(&mut self, _: &OpenLoop, cfg: &mut Cfg) -> CtrlCursor {
        // ctrl node structure setup
        let header = cfg.add_unsealed(self);
        CtrlCursor {
            ctrl: cfg.ctrl_placeholders.last().unwrap().clone(),
            block: header,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn close_loop(
        &mut self,
        CtrlCursor {
            block: entry_block,
            ctrl: entry_ctrl,
        }: CtrlCursor,
        body: Option<DataCursor>,

        header: BlockID,

        LoopBackedges {
            continues: mut backedges,
            breaks: mut exits,
        }: LoopBackedges,

        loop_backedge: bool,

        cfg: &mut Cfg,
        errors: &mut Errors,
    ) -> Option<DataCursor> {
        if let Some(cursor) = body {
            if loop_backedge {
                backedges.push(cursor.without_data()); // add the regular backedge, ignoring the data returned by the body
            } else {
                exits.push(cursor);
            }
        }

        let (mut entry_blocks, mut entry_ctrls): (Vec<BlockID>, Vec<CtrlID>) =
            backedges.into_iter().map(|c| (c.block, c.ctrl)).unzip();

        entry_blocks.push(entry_block);
        entry_ctrls.push(entry_ctrl);

        cfg.seal_block(header, entry_blocks, entry_ctrls, self, errors);

        self.merge(exits, cfg)
    }

    /// `(false_branch, true_branch)`
    pub fn branch(&mut self, cursor: DataCursor, cfg: &mut Cfg) -> (CtrlCursor, CtrlCursor) {
        let condition = cursor.data;
        let (false_branch, true_branch) = self.add_branch(cursor.ctrl.clone(), condition.clone());

        (
            CtrlCursor {
                block: cfg.branch(cursor.block),
                ctrl: false_branch,
            },
            CtrlCursor {
                block: cfg.branch(cursor.block),
                ctrl: true_branch,
            },
        )
    }

    pub fn merge(&mut self, cursors: Vec<DataCursor>, cfg: &mut Cfg) -> Option<DataCursor> {
        if cursors.is_empty() {
            return None;
        }
        if cursors.len() == 1 {
            let mut cursors = cursors;
            return Some(cursors.pop().unwrap());
        }

        let (variants, ctrls): (Vec<DataID>, Vec<CtrlID>) = cursors
            .iter()
            .map(|c| -> (DataID, CtrlID) { (c.data.clone(), c.ctrl.clone()) })
            .unzip();
        let cursors: Vec<BlockID> = cursors.into_iter().map(|c| c.block).collect();

        let merge = self.add_merge(ctrls);

        Some(DataCursor {
            ctrl: self.add_ctrl_merge(merge.clone()),
            data: self.data_merge(merge.clone(), variants),
            block: cfg.merge(cursors, merge),
        })
    }

    /// Variants.len() has to be greater 0
    pub fn data_merge(&mut self, merge: MergeID, variants: Vec<DataID>) -> DataID {
        let ty = variants[0].ty.clone();
        let phi = self.add_phi(merge, variants);
        self.add_data_phi(phi, ty)
    }
}

#[cfg(never)]
#[cfg(test)]
mod tests {
    use std::path::Path;

    use bumpalo::Bump;
    use nonempty::NonEmpty;

    use super::CtrlCursor;
    use crate::{
        error::Errors,
        grapher::{
            Graph,
            graph::{CtrlKind, DataKind},
            loops::LoopBackedges,
        },
        literal_parsing::Literal,
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn cursor_starts_at_graph_start_with_empty_state() {
        let graph = graph();
        let cursor = CtrlCursor::new(&graph);

        assert!(cursor.state.is_empty());
        assert!(matches!(*cursor.ctrl, CtrlKind::Start));
    }

    #[test]
    fn branch_clones_state_onto_false_and_true_paths() {
        let mut graph = graph();
        let value = graph.add_literal(Literal::from(10));
        let condition = graph.add_boolean(true);
        let cursor = CtrlCursor {
            state: vec![Some(value.clone())],
            ctrl: graph.start(),
        }
        .with_data(condition.clone());

        let (false_branch, true_branch) = graph.branch(cursor);

        assert_eq!(
            false_branch.state[0].as_ref().expect("false").addr(),
            value.addr()
        );
        assert_eq!(
            true_branch.state[0].as_ref().expect("true").addr(),
            value.addr()
        );
        assert!(matches!(*false_branch.ctrl, CtrlKind::FalseBranch { .. }));
        assert!(matches!(*true_branch.ctrl, CtrlKind::TrueBranch { .. }));
    }

    #[test]
    fn merge_creates_phi_for_values_present_on_every_path() {
        let mut graph = graph();
        let first = graph.add_literal(Literal::from(1));
        let second = graph.add_literal(Literal::from(2));
        let data_a = graph.add_literal(Literal::from(10));
        let data_b = graph.add_literal(Literal::from(20));
        let ctrl_a = graph.start();
        let merge = graph.add_merge(vec![ctrl_a.clone()]);
        let ctrl_b = graph.add_ctrl_merge(merge);

        let merged = graph.merge(NonEmpty {
            head: CtrlCursor {
                state: vec![Some(first.clone())],
                ctrl: ctrl_a,
            }
            .with_data(data_a),
            tail: vec![
                Cursor {
                    state: vec![Some(second.clone())],
                    ctrl: ctrl_b,
                }
                .with_data(data_b),
            ],
        });

        let Some(value) = &merged.state.states[0] else {
            panic!("merged state should contain a phi");
        };
        let DataKind::Phi { phi } = &value.kind else {
            panic!("expected state phi");
        };

        assert_eq!(phi.variants.len(), 2);
        assert!(phi.variants.iter().any(|v| v.addr() == first.addr()));
        assert!(phi.variants.iter().any(|v| v.addr() == second.addr()));
        assert!(matches!(*merged.ctrl, CtrlKind::Merge { .. }));
    }

    #[test]
    fn merge_marks_state_missing_when_any_path_is_missing() {
        let mut graph = graph();
        let first = graph.add_literal(Literal::from(1));
        let data = graph.add_literal(Literal::from(10));
        let ctrl_a = graph.start();
        let merge = graph.add_merge(vec![ctrl_a.clone()]);
        let ctrl_b = graph.add_ctrl_merge(merge);

        let merged = graph.merge(NonEmpty {
            head: CtrlCursor {
                state: vec![Some(first)],
                ctrl: ctrl_a,
            }
            .with_data(data.clone()),
            tail: vec![
                Cursor {
                    state: vec![None],
                    ctrl: ctrl_b,
                }
                .with_data(data),
            ],
        });

        assert!(merged.state[0].is_none());
    }

    #[test]
    fn open_loop_replaces_initialized_state_slots_with_loop_phis() {
        let mut graph = graph();
        let initial = graph.add_literal(Literal::from(0));
        let cursor = CtrlCursor {
            state: vec![Some(initial.clone()), None],
            ctrl: graph.start(),
        };

        let (loop_cursor, incomplete) = graph.open_loop(cursor);

        assert_eq!(incomplete.head.branches.len(), 1);
        assert_eq!(incomplete.phis.len(), 1);
        assert_eq!(incomplete.phis[0].0, 0);
        assert_eq!(incomplete.phis[0].1.variants[0].addr(), initial.addr());
        assert!(matches!(
            loop_cursor.state[0].as_ref().expect("loop phi").kind,
            DataKind::Phi { .. }
        ));
        assert!(loop_cursor.state[1].is_none());
    }

    #[test]
    fn close_loop_returns_none_when_loop_has_no_break_exits() {
        let mut graph = graph();
        let cursor = CtrlCursor {
            state: vec![],
            ctrl: graph.start(),
        };
        let (body_cursor, incomplete) = graph.open_loop(cursor);
        let mut errors = Errors::empty(Path::new("builder-test.rx"));
        let unit = graph.unit();

        let result = graph.close_loop(
            LoopBackedges {
                continue_points: vec![],
                continue_states: vec![],
                breaks: vec![],
            },
            Some(body_cursor.with_data(unit)),
            true,
            incomplete,
            &mut errors,
        );

        assert!(result.is_none());
    }
}
