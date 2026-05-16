pub mod binding;
mod jumps;

pub use binding::ScopedSymbolTable;
use bumpalo::Bump;
use nonempty::NonEmpty;

use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        Graph,
        builder::{
            binding::{MutableState, StateID},
            jumps::{JumpTableStack, Jumps, LoopID, OpenLoop},
        },
        graph::{CtrlID, DataID, MergeID, PhiID},
    },
    parser::Symbol,
};

use std::collections::HashMap;

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

pub struct Builder {
    pub graph: Graph,

    pub symbol_table: ScopedSymbolTable,

    pub labels: HashMap<Symbol, LoopID>,
    pub jump_stack: JumpTableStack,
}

impl Builder {
    pub fn new(arena: Bump) -> Self {
        Self {
            graph: Graph::new(arena),

            symbol_table: ScopedSymbolTable::new(),

            labels: HashMap::new(),
            jump_stack: JumpTableStack::new(),
        }
    }

    pub fn open_loop(
        &mut self,
        cursor: Cursor,
    ) -> (Cursor, (MergeID, Vec<(StateID, PhiID)>), OpenLoop, LoopID) {
        let Cursor { mut state, ctrl } = cursor;
        // ctrl node structure setup
        let loop_head = self.graph.add_merge(vec![ctrl]);

        let mut loop_phis = vec![];

        state
            .iter_mut()
            .enumerate()
            .filter_map(|(i, d)| d.as_mut().map(|d| (i, d)))
            .for_each(|(i, data)| {
                let phi = self.graph.add_phi(loop_head.clone(), vec![data.clone()]);
                *data = self.graph.add_phi_no_dedup(phi.clone(), data.ty.clone());
                loop_phis.push((i, phi))
            }); // replace every variable with a phi and keep track of the phi's

        let (tok, loop_id) = self.jump_stack.open_block(state.len()); // store how many variables belonged to the previous state
        (
            Cursor {
                state,
                ctrl: self.graph.add_ctrl_merge(loop_head.clone()),
            }, // with this the body has it as a control flow dependency
            (loop_head, loop_phis),
            tok,
            loop_id,
        )
    }

    pub fn close_loop(
        &mut self,
        body: Option<DataCursor>,
        tok: OpenLoop,
        loop_backedge: bool,
        (mut loop_head, mut loop_phis): (MergeID, Vec<(StateID, PhiID)>),
        errors: &mut Errors,
    ) -> Option<DataCursor> {
        let Jumps {
            mut continue_points,
            mut continue_states,
            mut breaks,
        } = self.jump_stack.close_block(tok);

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
        let (false_branch, true_branch) = self
            .graph
            .add_branch(cursor.ctrl.clone(), condition.clone());

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
            let mut cursors = cursors;
            let cursor = cursors.pop().unwrap();
            return cursor;
        }

        let merge = self
            .graph
            .add_merge(cursors.iter().map(|c| c.ctrl.clone()).collect());

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
                let phi = self.graph.add_phi(merge.clone(), variants);
                state.push(Some(self.graph.add_data_phi(phi, ty)))
            }
        }

        let ty = cursors[0].data.ty.clone();
        let phi = self
            .graph
            .add_phi(merge.clone(), cursors.into_iter().map(|v| v.data).collect());
        DataCursor {
            state,
            ctrl: self.graph.add_ctrl_merge(merge),
            data: self.graph.add_data_phi(phi, ty),
        }
    }
}
