use nonempty::NonEmpty;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        Graph,
        binding::{MutableState, StateID},
        block::Jumps,
        graph::{CtrlID, DataID, MergeID, PhiID},
    },
};

#[derive(Clone, Debug)]
pub struct Cursor {
    pub state: MutableState,
    pub ctrl: CtrlID,
}

impl Cursor {
    pub fn new(graph: &Graph) -> Self {
        Self {
            state: vec![],
            ctrl: graph.start(),
        }
    }
    pub fn with_data(self, data: DataID) -> DataCursor {
        DataCursor {
            state: self.state,
            ctrl: self.ctrl,
            data,
        }
    }
}

#[derive(Clone, Debug)]
pub struct DataCursor {
    pub state: MutableState,
    pub ctrl: CtrlID,
    pub data: DataID,
}

impl DataCursor {
    pub fn cursor(self) -> Cursor {
        Cursor {
            state: self.state,
            ctrl: self.ctrl,
        }
    }

    pub fn split_data(self) -> (Cursor, DataID) {
        let DataCursor { state, ctrl, data } = self;

        (Cursor { state, ctrl }, data)
    }
}

#[derive(Debug)]
pub struct IncompleteNodes {
    head: MergeID,
    phis: Vec<(StateID, PhiID)>,
}

impl Graph {
    pub fn open_loop(&mut self, cursor: Cursor) -> (Cursor, IncompleteNodes) {
        let Cursor { mut state, ctrl } = cursor;
        // ctrl node structure setup
        let loop_head = self.add_merge(vec![ctrl]);

        let mut loop_phis = vec![];

        state
            .iter_mut()
            .enumerate()
            .filter_map(|(i, d)| d.as_mut().map(|d| (i, d)))
            .for_each(|(i, data)| {
                let phi = self.add_phi(loop_head.clone(), vec![data.clone()]);
                *data = self.add_phi_no_dedup(phi.clone(), data.ty.clone());
                loop_phis.push((i, phi))
            }); // replace every variable with a phi and keep track of the phi's

        (
            Cursor {
                state,
                ctrl: self.add_ctrl_merge(loop_head.clone()),
            }, // with this the body has it as a control flow dependency
            IncompleteNodes {
                head: loop_head,
                phis: loop_phis,
            },
        )
    }

    pub fn close_loop(
        &mut self,
        jumps: Jumps,
        body: Option<DataCursor>,
        loop_backedge: bool,
        mut incomplete: IncompleteNodes,
        errors: &mut Errors,
    ) -> Option<DataCursor> {
        let Jumps {
            mut continue_points,
            mut continue_states,
            mut breaks,
        } = jumps;

        if let Some(cursor) = body {
            if loop_backedge {
                continue_points.push(cursor.ctrl); // add the regular backedge
                continue_states.push(cursor.state); // add the regular backedge state
            } else {
                breaks.push(cursor);
            }
        }

        incomplete.head.branches.append(&mut continue_points); // add the continuation points as backedges
        incomplete.phis.iter_mut().for_each(|(i, phi)| {
            let variants = continue_states.iter().map(|s| s[*i].clone());

            if variants.clone().any(|v| v.is_none()) {
                if phi.strong_count() > 1 {
                    errors.push(Span::beginning(), ErrorCode::MovedInLoop);
                }
            } else {
                let mut values = variants.map(|v| v.unwrap()).collect();

                phi.variants.append(&mut values);
            }
        }); // add thoses backedges to the phi nodes of mutable variables outside the loop for every jump

        self.merge_pot_diverge(breaks)
    }

    /// `(false_branch, true_branch)`
    pub fn branch(&mut self, cursor: DataCursor) -> (Cursor, Cursor) {
        let condition = cursor.data;
        let (false_branch, true_branch) = self.add_branch(cursor.ctrl.clone(), condition.clone());

        (
            Cursor {
                state: cursor.state.clone(),
                ctrl: false_branch,
            },
            Cursor {
                state: cursor.state.clone(),
                ctrl: true_branch,
            },
        )
    }

    pub fn merge_pot_diverge(&mut self, cursors: Vec<DataCursor>) -> Option<DataCursor> {
        NonEmpty::from_vec(cursors).map(|cursors| self.merge(cursors))
    }

    pub fn merge(&mut self, cursors: NonEmpty<DataCursor>) -> DataCursor {
        if cursors.len() == 1 {
            return cursors.head;
        }

        let merge = self.add_merge(cursors.iter().map(|c| c.ctrl.clone()).collect());

        let size = cursors[0].state.len();
        debug_assert!(cursors.iter().skip(1).all(|c| c.state.len() == size)); // this is only for making it clear, its actually pretty certified to hold

        let mut state = Vec::with_capacity(size);
        for i in 0..size {
            let mut variants = Vec::with_capacity(cursors.len());
            for variant in cursors.iter().map(|c| c.state[i].clone()) {
                match variant {
                    Some(variant) => variants.push(variant),
                    None => break,
                }
            }
            if variants.len() != cursors.len() {
                state.push(None) // if in any variant the variable is moved or uninitialized it will be in the merged version
            } else {
                let ty = variants[0].ty.clone();
                let phi = self.add_phi(merge.clone(), variants);
                state.push(Some(self.add_data_phi(phi, ty)))
            }
        }

        let ty = cursors[0].data.ty.clone();
        let phi = self.add_phi(merge.clone(), cursors.into_iter().map(|v| v.data).collect());
        DataCursor {
            state,
            ctrl: self.add_ctrl_merge(merge),
            data: self.add_data_phi(phi, ty),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use bumpalo::Bump;
    use nonempty::NonEmpty;

    use super::Cursor;
    use crate::{
        error::Errors,
        grapher::{
            Graph,
            block::Jumps,
            graph::{CtrlKind, DataKind},
        },
        literal_parsing::Literal,
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn cursor_starts_at_graph_start_with_empty_state() {
        let graph = graph();
        let cursor = Cursor::new(&graph);

        assert!(cursor.state.is_empty());
        assert!(matches!(*cursor.ctrl, CtrlKind::Start));
    }

    #[test]
    fn branch_clones_state_onto_false_and_true_paths() {
        let mut graph = graph();
        let value = graph.add_literal(Literal::from(10));
        let condition = graph.add_boolean(true);
        let cursor = Cursor {
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
            head: Cursor {
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

        let Some(value) = &merged.state[0] else {
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
            head: Cursor {
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
        let cursor = Cursor {
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
        let cursor = Cursor {
            state: vec![],
            ctrl: graph.start(),
        };
        let (body_cursor, incomplete) = graph.open_loop(cursor);
        let mut errors = Errors::empty(Path::new("builder-test.rx"));
        let unit = graph.unit();

        let result = graph.close_loop(
            Jumps {
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
