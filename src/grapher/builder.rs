use nonempty::NonEmpty;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        Graph,
        binding::{MutableState, StateID},
        graph::{CtrlID, DataID, MergeID, PhiID},
        jumps::Jumps,
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

pub type IncompleteNodes = (MergeID, Vec<(StateID, PhiID)>);

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
            (loop_head, loop_phis),
        )
    }

    pub fn close_loop(
        &mut self,
        jumps: Jumps,
        body: Option<DataCursor>,
        loop_backedge: bool,
        (mut loop_head, mut loop_phis): IncompleteNodes,
        errors: &mut Errors,
    ) -> Option<DataCursor> {
        let Jumps {
            mut continue_points,
            mut continue_states,
            mut breaks,
        } = jumps;

        if let Some(cursor) = &body {
            if loop_backedge {
                continue_points.push(cursor.ctrl.clone()); // add the regular backedge
                continue_states.push(cursor.state.clone()); // add the regular backedge state
            } else {
                breaks.push(cursor.clone());
            }
        }

        loop_head.branches.append(&mut continue_points); // add the continuation points as backedges
        loop_phis.iter_mut().for_each(|(i, phi)| {
            if continue_states.iter().any(|s| s[*i].is_none()) {
                if phi.strong_count() > 1 {
                    errors.push(Span::beginning(), ErrorCode::MovedInLoop);
                }
            } else {
                let mut values = continue_states
                    .iter()
                    .map(|s| s[*i].clone().unwrap())
                    .collect();

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

#[cfg(never)]
#[cfg(test)]
mod test {
    use std::path::Path;

    use crate::{
        error::Errors,
        grapher::{
            builder::GraphBuilder,
            graph::{DataKind, PhiID},
        },
        literal_parsing::Literal,
        tokenizing::Tokenizer,
        type_parsing::AtomicType,
        utilities::Rc,
    };

    fn builder_for(source: &'static str) -> GraphBuilder<'static, Tokenizer<'static>> {
        let errors = Rc::new(Errors::empty(Path::new("builder-test.rx")));
        let tokenizer = Box::leak(Box::new(Tokenizer::new(source.as_bytes(), errors, 64)));
        GraphBuilder::new(tokenizer)
    }

    #[test]
    fn advance_and_expect_name_move_the_token_stream_forward() {
        let mut builder = builder_for("alpha beta");

        let first = builder.expect_name().expect("first name");
        assert_eq!(first.src, "alpha");
        assert_eq!(builder.peek().src, "beta");

        let second = builder.get_name();
        assert_eq!(second.src, "beta");
        assert_eq!(builder.peek().src, "");
    }

    #[test]
    fn declare_variable_routes_to_immutable_or_mutable_symbol_maps() {
        let mut builder = builder_for("");
        let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
        let immutable_value = builder.graph.add_literal(Literal::from(1));
        let mutable_value = builder.graph.add_literal(Literal::from(2));

        builder.declare_variable("config", ty.clone(), immutable_value.clone(), false);
        builder.declare_variable("counter", ty, mutable_value.clone(), true);

        let dump = builder.symbol_table.all_symbols();
        assert_eq!(
            dump.immutables["config"].value.addr(),
            immutable_value.addr()
        );
        assert_eq!(dump.mutables["counter"].value.addr(), mutable_value.addr());
    }

    #[test]
    fn current_state_and_restore_state_round_trip_mutables() {
        let mut builder = builder_for("");
        let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
        let initial = builder.graph.add_literal(Literal::from(1));
        let overwritten = builder.graph.add_literal(Literal::from(2));

        builder.declare_variable("counter", ty, initial.clone(), true);
        let (snapshot, ctrl) = builder.current_state().expect("state");

        builder
            .symbol_table
            .write_symbol(crate::error::Span::beginning(), "counter", overwritten)
            .expect("write");
        builder.restore_state(snapshot);

        assert_eq!(builder.graph.get_ctrl().expect("ctrl").addr(), ctrl.addr());
        assert_eq!(
            builder
                .symbol_table
                .read_symbol(&mut builder.graph, "counter")
                .addr(),
            initial.addr()
        );
    }

    #[test]
    fn merge_states_keeps_identical_values_without_creating_phi() {
        let mut builder = builder_for("");
        let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
        let value = builder.graph.add_literal(Literal::from(10));
        builder.declare_variable("cached", ty, value.clone(), true);

        let (state_a, ctrl_a) = builder.current_state().expect("state a");
        let (state_b, ctrl_b) = builder.current_state().expect("state b");
        let merge = builder.graph.add_merge(vec![ctrl_a, ctrl_b]);

        builder.merge_states(merge, vec![state_a, state_b]);

        let merged = builder
            .symbol_table
            .read_symbol(&mut builder.graph, "cached");
        assert_eq!(merged.addr(), value.addr());
    }

    #[test]
    fn merge_states_creates_phi_for_different_mutable_values() {
        let mut builder = builder_for("");
        let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
        let first = builder.graph.add_literal(Literal::from(1));
        let second = builder.graph.add_literal(Literal::from(2));
        builder.declare_variable("score", ty, first.clone(), true);

        let (state_a, ctrl_a) = builder.current_state().expect("state a");
        builder
            .symbol_table
            .write_symbol(crate::error::Span::beginning(), "score", second.clone())
            .expect("write");
        let (state_b, ctrl_b) = builder.current_state().expect("state b");
        let merge = builder.graph.add_merge(vec![ctrl_a, ctrl_b]);

        builder.merge_states(merge, vec![state_a, state_b]);

        let merged = builder
            .symbol_table
            .read_symbol(&mut builder.graph, "score");
        let DataKind::Phi { phi } = &merged.kind else {
            panic!("different mutable states should merge through phi");
        };
        assert_eq!(phi.variants.len(), 2);
        assert!(
            phi.variants
                .iter()
                .any(|variant| variant.addr() == first.addr())
        );
        assert!(
            phi.variants
                .iter()
                .any(|variant| variant.addr() == second.addr())
        );
    }

    #[test]
    fn set_up_loop_merge_replaces_mutables_with_open_loop_phis() {
        let mut builder = builder_for("");
        let ty = builder.graph.add_type(AtomicType::Signed { size: 32 });
        let initial = builder.graph.add_literal(Literal::from(0));
        builder.declare_variable("index", ty, initial.clone(), true);

        let entry = builder.graph.get_ctrl().expect("entry");
        let (loop_head, loop_phis): (_, Vec<PhiID>) = builder.set_up_loop_merge(entry.clone());

        assert_eq!(loop_head.branches.len(), 1);
        assert_eq!(loop_head.branches[0].addr(), entry.addr());
        assert_eq!(loop_phis.len(), 1);
        assert_eq!(loop_phis[0].variants.len(), 1);
        assert_eq!(loop_phis[0].variants[0].addr(), initial.addr());

        let current = builder
            .symbol_table
            .read_symbol(&mut builder.graph, "index");
        let DataKind::Phi { phi } = &current.kind else {
            panic!("mutable should be replaced by a loop phi data node");
        };
        assert_eq!(phi.addr(), loop_phis[0].addr());
    }
}
