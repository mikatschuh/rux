use std::collections::{HashMap, HashSet};

use crate::{
    error::{ErrorCode, Errors},
    grapher::{
        Graph,
        binding::VariableID,
        graph::{CtrlID, DataID, DataKind, MergeID, TypeID},
        loops::{ClosedLoop, JumpTableStack, OpenLoop},
    },
    parser::Expr,
};

pub type State = HashMap<VariableID, DataID>;
pub type Incompletes = HashMap<VariableID, Incomplete>;

pub struct Incomplete {
    placeholder: DataID,
    /// this the AST-Node that read out the value of the incomplete phi for the first time
    reference: Expr,
}

#[derive(Clone, Debug)]
pub struct Cursor {
    pub state: State,
    cfg: CfgNode,
}

#[derive(Clone, Debug)]
enum CfgNode {
    Start,
    Branch {
        pred: Box<Cursor>,
    },
    Merge {
        merge: MergeID,
        /// pred.len() > 1
        pred: Vec<Cursor>,
    },
    IncompleteMerge,
}

impl Cursor {
    pub fn new() -> Self {
        Self {
            state: State::new(),
            cfg: CfgNode::Start,
        }
    }

    pub fn with_ctrl(self, ctrl: CtrlID) -> CtrlCursor {
        CtrlCursor { cursor: self, ctrl }
    }

    pub fn get_value(
        &mut self,
        id: VariableID,
        ty: TypeID,
        read: Expr,
        graph: &mut Graph,
        jump_table: &mut JumpTableStack,
    ) -> Option<DataID> {
        let mut visited = HashSet::new();

        self.get_value_recursive(id, read, graph, jump_table, &mut visited)
            .map(|mut data| {
                data.ty = ty;
                data
            })
    }

    pub fn get_value_recursive(
        &mut self,
        id: VariableID,
        read: Expr,
        graph: &mut Graph,
        jump_table: &mut JumpTableStack,
        visited: &mut HashSet<*const CfgNode>, // Just the addresses
    ) -> Option<DataID> {
        if let Some(state) = self.state.get(&id) {
            Some(state.clone())
        } else {
            if !visited.insert(&self.cfg as *const CfgNode) {
                return None;
            }

            match &mut self.cfg {
                CfgNode::Start => None,
                CfgNode::Branch { pred } => {
                    match pred.get_value_recursive(id, read, graph, jump_table, visited) {
                        Some(state) => {
                            self.state.insert(id, state.clone()); // insert for the next lookup
                            Some(state)
                        }
                        None => None,
                    }
                }
                CfgNode::Merge { merge, pred } => {
                    let mut variants = vec![];
                    for pred in pred {
                        variants.push(pred.get_value_recursive(
                            id,
                            read.clone(),
                            graph,
                            jump_table,
                            visited,
                        )?);
                    }
                    let first = variants.first().unwrap();
                    if variants.iter().all(|v| v == first) {
                        let state = variants.pop().unwrap();
                        self.state.insert(id, state.clone()); // insert for the next lookup
                        Some(state)
                    } else {
                        let ty = variants[0].ty.clone();
                        let phi = graph.add_phi(merge.clone(), variants);
                        let phi = graph.add_data_phi(phi, ty);
                        self.state.insert(id, phi.clone()); // insert for the next lookup
                        Some(phi)
                    }
                }
                CfgNode::IncompleteMerge => {
                    // we know that jump_table has to have at least one loop as IncompleteMerge cant be created without that to hold
                    let placeholder = graph.add_placeholder();
                    jump_table.get_block().incomplete_phis.insert(
                        id,
                        Incomplete {
                            placeholder: placeholder.clone(),
                            reference: read,
                        },
                    );
                    self.state.insert(id, placeholder.clone()); // insert for the next lookup
                    Some(placeholder)
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct CtrlCursor {
    pub cursor: Cursor,
    pub ctrl: CtrlID,
}

impl CtrlCursor {
    pub fn with_data(self, data: DataID) -> DataCursor {
        DataCursor {
            cursor: self.cursor,
            ctrl: self.ctrl,
            data,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DataCursor {
    pub cursor: Cursor,
    pub ctrl: CtrlID,
    pub data: DataID,
}

impl DataCursor {
    pub fn without_data(self) -> CtrlCursor {
        CtrlCursor {
            cursor: self.cursor,
            ctrl: self.ctrl,
        }
    }

    pub fn split(self) -> (CtrlCursor, DataID) {
        (
            CtrlCursor {
                cursor: self.cursor,
                ctrl: self.ctrl,
            },
            self.data,
        )
    }
}

impl Graph {
    pub fn open_loop(&mut self, cursor: CtrlCursor, _: &OpenLoop) -> (MergeID, Cursor, CtrlCursor) {
        let CtrlCursor { cursor, ctrl } = cursor;
        // ctrl node structure setup
        let loop_head = self.add_merge(vec![ctrl.clone()]);
        let ctrl_cursor = CtrlCursor {
            ctrl: self.add_ctrl_merge(loop_head.clone()),
            cursor: Cursor {
                state: State::new(),
                cfg: CfgNode::IncompleteMerge,
            },
        };
        (loop_head, cursor, ctrl_cursor)
    }

    pub fn close_loop(
        &mut self,
        mut loop_head: MergeID,
        mut entry: Cursor,

        closed_loop: ClosedLoop,
        jump_table: &mut JumpTableStack,

        body: Option<DataCursor>,
        loop_backedge: bool,

        errors: &mut Errors,
    ) -> Option<DataCursor> {
        let ClosedLoop {
            mut continues,
            mut breaks,
            incomplete_phis: incompletes,
        } = closed_loop;

        if let Some(cursor) = body {
            if loop_backedge {
                continues.push(cursor.without_data()); // add the regular backedge
            } else {
                breaks.push(cursor);
            }
        }

        let (mut continue_states, mut continue_points): (Vec<Cursor>, Vec<CtrlID>) =
            continues.into_iter().map(|c| (c.cursor, c.ctrl)).unzip();

        loop_head.branches.append(&mut continue_points); // add the continuation points as backedges

        // add thoses backedges to the phi nodes of mutable variables declared outside the loo but used inside
        'outer: for (
            id,
            Incomplete {
                mut placeholder,
                reference,
            },
        ) in incompletes
        {
            let mut variants = if let Some(entry) = entry.get_value(
                id,
                placeholder.ty.clone(),
                reference.clone(),
                self,
                jump_table,
            ) {
                vec![entry]
            } else {
                errors.push(reference.span, ErrorCode::ReadUnitializedOrMoved);
                continue 'outer;
            };
            for cursor in &mut continue_states {
                match cursor.get_value(
                    id,
                    placeholder.ty.clone(),
                    reference.clone(),
                    self,
                    jump_table,
                ) {
                    Some(variant) => variants.push(variant),
                    None => {
                        errors.push(reference.span, ErrorCode::ReadUnitializedOrMoved);
                        continue 'outer;
                    }
                }
            }

            let phi = self.add_phi(loop_head.clone(), variants);
            placeholder.kind = DataKind::Phi { phi };
        }

        self.merge(breaks)
    }

    /// `(false_branch, true_branch)`
    pub fn branch(&mut self, cursor: DataCursor) -> (CtrlCursor, CtrlCursor) {
        let condition = cursor.data;
        let (false_branch, true_branch) = self.add_branch(cursor.ctrl.clone(), condition.clone());

        (
            CtrlCursor {
                cursor: Cursor {
                    state: State::new(),
                    cfg: CfgNode::Branch {
                        pred: Box::new(cursor.cursor.clone()),
                    },
                },
                ctrl: false_branch,
            },
            CtrlCursor {
                cursor: Cursor {
                    state: State::new(),
                    cfg: CfgNode::Branch {
                        pred: Box::new(cursor.cursor),
                    },
                },
                ctrl: true_branch,
            },
        )
    }

    pub fn merge(&mut self, cursors: Vec<DataCursor>) -> Option<DataCursor> {
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
        let cursors: Vec<Cursor> = cursors.into_iter().map(|c| c.cursor).collect();

        let merge = self.add_merge(ctrls);

        let data_merge = self.data_merge(merge.clone(), variants);
        Some(DataCursor {
            cursor: Cursor {
                state: State::new(),
                cfg: CfgNode::Merge {
                    merge: merge.clone(),
                    pred: cursors,
                },
            },
            ctrl: self.add_ctrl_merge(merge),
            data: data_merge,
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
            loops::ClosedLoop,
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
            ClosedLoop {
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
