use parser::{AstBuilder, Expr};

use crate::{
    Diagnostics, Error, Graph,
    binding::BindingID,
    graph::{Ctrl, CtrlKind, CtrlPlaceholder, Data, DataKind, DataPlaceholder, Merge, Type},
};
use std::collections::HashMap;

/// This describes an **existing** block.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct BlockID(usize);

#[derive(Debug)]
pub struct Placeholder {
    var: BindingID,
    placeholder: DataPlaceholder,
    /// this the AST-Node that read out the value of the incomplete phi for the first time
    reference: Expr,
}

#[derive(Clone, Debug)]
pub struct Block {
    definitions: HashMap<BindingID, Data>,
    cfg: CfgNode,
}

#[derive(Clone, Debug)]
enum CfgNode {
    Start,
    Branch {
        predecessor: BlockID,
    },
    Merge {
        merge: Merge,
        /// pred.len() > 1
        predecessors: Vec<BlockID>,
    },
    IncompleteMerge,
}

#[derive(Debug)]
pub struct Cfg {
    blocks: Vec<Block>,
    placeholders: Vec<Vec<Placeholder>>,
    ctrl_placeholders: Vec<CtrlPlaceholder>,
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
            BlockID(0),
        )
    }

    fn push_block(&mut self, block: Block) -> BlockID {
        let id = self.blocks.len();
        self.blocks.push(block);
        BlockID(id)
    }

    fn branch_off(&mut self, predecessor: BlockID) -> BlockID {
        self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::Branch { predecessor },
        })
    }

    fn merge(&mut self, predecessors: Vec<BlockID>, merge: Merge) -> BlockID {
        self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::Merge {
                merge,
                predecessors,
            },
        })
    }

    pub fn add_unsealed(&mut self, graph: &mut Graph<'_>) -> (BlockID, Ctrl) {
        let unsealed_block = self.push_block(Block {
            definitions: HashMap::new(),
            cfg: CfgNode::IncompleteMerge,
        });
        self.placeholders.push(vec![]);
        let placeholder = graph.add_ctrl_placeholder();
        let ctrl = placeholder.ctrl();
        self.ctrl_placeholders.push(placeholder);
        (unsealed_block, ctrl)
    }

    pub fn seal_block(
        &mut self,
        block: BlockID,
        predecessors: CtrlCursors,
        graph: &mut Graph<'_>,
        errors: &mut impl Diagnostics,
        ast: &AstBuilder,
    ) {
        let CtrlCursors { blocks, ctrls } = predecessors;
        let merge = graph.add_merge(ctrls.into_boxed_slice());
        let ctrl_placeholder = self.ctrl_placeholders.pop().unwrap();
        graph[ctrl_placeholder] = CtrlKind::Merge { merge };

        let placeholders = self.placeholders.pop().unwrap();

        // add thoses backedges to the phi nodes of mutable variables declared outside the loop but used inside
        'outer: for Placeholder {
            var,
            placeholder,
            reference,
        } in placeholders
        {
            let mut variants = vec![];
            for block in &blocks {
                match self.get_definition(
                    *block,
                    var,
                    graph.get_type(placeholder.data()),
                    reference,
                    graph,
                ) {
                    Some(variant) => variants.push(variant),
                    None => {
                        errors.add(ast[reference].span, Error::ReadEitherUnitializedOrMoved);
                        continue 'outer;
                    }
                }
            }

            graph[placeholder] = DataKind::Phi {
                merge,
                variants: variants.into_boxed_slice(),
            };
        }

        let unsealed = &mut self.blocks[block.0];
        unsealed.cfg = CfgNode::Merge {
            merge,
            predecessors: blocks,
        };
    }

    pub fn assign_variable(&mut self, block: BlockID, var: BindingID, value: Data) -> Option<Data> {
        self.blocks[block.0].definitions.insert(var, value)
    }

    pub fn get_definition(
        &mut self,
        block: BlockID,
        var: BindingID,
        ty: Type,
        read: Expr,
        graph: &mut Graph<'_>,
    ) -> Option<Data> {
        let current_block = &mut self.blocks[block.0];

        if let Some(current_blocks_definition) = current_block.definitions.get(&var) {
            return Some(*current_blocks_definition);
        }

        match &current_block.cfg {
            CfgNode::Start => None,
            CfgNode::Branch { predecessor: pred } => {
                let block = *pred;
                self.get_definition(block, var, ty, read, graph)
            }
            CfgNode::Merge {
                merge,
                predecessors: pred,
            } => {
                let merge = *merge;

                let mut variants = vec![];
                for pred in pred.clone() {
                    variants.push(self.get_definition(pred, var, ty, read, graph)?);
                }
                let first = variants.first().unwrap();
                let value = if variants.iter().all(|v| v == first) {
                    variants.pop().unwrap()
                } else {
                    let ty = graph.get_type(variants[0]);
                    graph.add_phi(merge, variants.into_boxed_slice(), ty)
                };

                Some(value)
            }
            CfgNode::IncompleteMerge => {
                // we know that jump_table has to have at least one loop as IncompleteMerge cant be created without that to hold
                let placeholder = graph.add_placeholder(ty);
                let data = placeholder.data();
                self.placeholders.last_mut().unwrap().push(
                    // an incomplete merge can only exist when there are placeholders
                    Placeholder {
                        var,
                        placeholder,
                        reference: read,
                    },
                );
                Some(data)
            }
        }
        .inspect(|data| {
            self.blocks[block.0].definitions.insert(var, *data); // insert for the next lookup
        })
    }
}

#[derive(Clone, Debug)]
pub struct CtrlCursor {
    pub block: BlockID,
    pub ctrl: Ctrl,
}

impl CtrlCursor {
    pub fn with_data(self, data: Data) -> DataCursor {
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
    pub ctrl: Ctrl,
    pub data: Data,
}

impl DataCursor {
    pub fn without_data(self) -> CtrlCursor {
        CtrlCursor {
            block: self.block,
            ctrl: self.ctrl,
        }
    }

    pub fn and(self, other: Self) -> DataCursors<true> {
        let mut cursors = DataCursors::<true>::from_cursor(self);
        cursors.push(other);
        cursors
    }

    pub fn split(self) -> (CtrlCursor, Data) {
        (
            CtrlCursor {
                block: self.block,
                ctrl: self.ctrl,
            },
            self.data,
        )
    }

    /// `(false_branch, true_branch)`
    pub fn branch(self, graph: &mut Graph, cfg: &mut Cfg) -> (CtrlCursor, CtrlCursor) {
        let condition = self.data;
        let (false_branch, true_branch) = graph.add_branch(self.ctrl, condition);

        (
            CtrlCursor {
                block: cfg.branch_off(self.block),
                ctrl: false_branch,
            },
            CtrlCursor {
                block: cfg.branch_off(self.block),
                ctrl: true_branch,
            },
        )
    }
}

/// `Vec<CtrlCursor>` but as SoA
#[derive(Debug, Default)]
pub struct CtrlCursors {
    blocks: Vec<BlockID>,
    ctrls: Vec<Ctrl>,
}

impl CtrlCursors {
    pub fn push(&mut self, cursor: CtrlCursor) {
        let CtrlCursor { block, ctrl } = cursor;

        self.blocks.push(block);
        self.ctrls.push(ctrl);
    }
}

/// `Vec<DataCursor>` but as SoA
#[derive(Debug)]
pub struct DataCursors<const NON_EMPTY: bool> {
    blocks: Vec<BlockID>,
    ctrls: Vec<Ctrl>,
    datas: Vec<Data>,
}

impl Default for DataCursors<false> {
    fn default() -> Self {
        Self {
            blocks: vec![],
            ctrls: vec![],
            datas: vec![],
        }
    }
}

impl<const NON_EMPTY: bool> DataCursors<NON_EMPTY> {
    fn from_cursor(cursor: DataCursor) -> DataCursors<true> {
        let DataCursor { block, ctrl, data } = cursor;
        DataCursors {
            blocks: vec![block],
            ctrls: vec![ctrl],
            datas: vec![data],
        }
    }

    pub fn push(&mut self, cursor: DataCursor) {
        let DataCursor { block, ctrl, data } = cursor;

        self.blocks.push(block);
        self.ctrls.push(ctrl);
        self.datas.push(data);
    }

    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn unwrap(mut self) -> DataCursor {
        if let Some(block) = self.blocks.pop()
            && let Some(ctrl) = self.ctrls.pop()
            && let Some(data) = self.datas.pop()
        {
            DataCursor { block, ctrl, data }
        } else {
            panic!("expected atleast one full DataCursor")
        }
    }
}

impl DataCursors<false> {
    pub fn merge(self, graph: &mut Graph, cfg: &mut Cfg) -> Option<DataCursor> {
        if self.len() == 0 {
            return None;
        } else if self.len() == 1 {
            return Some(self.unwrap());
        }

        let DataCursors {
            blocks,
            ctrls,
            datas: variants,
        } = self;

        let merge = graph.add_merge(ctrls.into_boxed_slice());
        Some(DataCursor {
            ctrl: graph.add_ctrl_merge(merge),
            data: graph.merge_data(merge, variants),
            block: cfg.merge(blocks, merge),
        })
    }
}

impl DataCursors<true> {
    pub fn merge(self, graph: &mut Graph, cfg: &mut Cfg) -> DataCursor {
        if self.len() == 1 {
            return self.unwrap();
        }

        let DataCursors {
            blocks,
            ctrls,
            datas: variants,
        } = self;

        let merge = graph.add_merge(ctrls.into_boxed_slice());
        DataCursor {
            ctrl: graph.add_ctrl_merge(merge),
            data: graph.merge_data(merge, variants),
            block: cfg.merge(blocks, merge),
        }
    }
}

impl Graph<'_> {
    /// Variants.len() has to be greater 0
    pub fn merge_data(&mut self, merge: Merge, variants: Vec<Data>) -> Data {
        let ty = self.get_type(variants[0]);
        self.add_phi(merge, variants.into_boxed_slice(), ty)
    }
}

#[cfg(any())]
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
