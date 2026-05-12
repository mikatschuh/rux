pub mod binding;
mod error;
mod jumps;

pub use binding::{Binding, ScopedSymbolTable, SymbolDump};
use bumpalo::Bump;
pub use error::{Error, Result};

use crate::{
    error::Span,
    grapher::{
        Graph,
        builder::{
            binding::MutableState,
            jumps::{JumpTableStack, Jumps, LoopID, OpenLoop},
        },
        graph::{CtrlID, DataID, MergeID, PhiID},
    },
    parser::{Spanned, Symbol},
};

use std::collections::HashMap;

pub struct Builder {
    pub graph: Graph,

    pub symbol_table: ScopedSymbolTable,

    pub labels: HashMap<Symbol, LoopID>,
    jump_stack: JumpTableStack,
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

    pub fn add_continue(&mut self, keyword: Span) -> Result<DataID> {
        if let Some(current_loop) = self.jump_stack.current_loops_scope() {
            self.jump_stack.add_continue(
                self.graph.get_ctrl()?,
                self.symbol_table.snapshot_state_of_scope(current_loop),
            );
            Ok(self.graph.make_unreachable())
        } else {
            Err(Error::ContinueOutsideLoop { keyword })
        }
    }

    pub fn add_continue_to(
        &mut self,
        ctrl: CtrlID,
        keyword: Span,
        label: Spanned<Symbol>,
    ) -> Result<DataID> {
        let Some(branch) = self.labels.get(&label.val).cloned() else {
            return Err(Error::ContinueWithUnknownLabel { keyword, label });
        };
        let scope = self.jump_stack.scope_of(branch);
        self.jump_stack.add_continue_to(
            branch,
            self.graph.get_ctrl()?,
            self.symbol_table.snapshot_state_of_scope(scope),
        );
        Ok(self.graph.make_unreachable())
    }

    pub fn add_break(&mut self, keyword: Span, value: DataID) -> Result<DataID> {
        if let Some(current_loop) = self.jump_stack.current_loops_scope() {
            self.jump_stack.add_break(
                self.graph.get_ctrl()?,
                self.symbol_table.snapshot_state_of_scope(current_loop),
                value,
            );
            Ok(self.graph.make_unreachable())
        } else {
            Err(Error::BreakOutsideLoop { keyword })
        }
    }

    pub fn add_break_to(
        &mut self,
        keyword: Span,
        label: Spanned<Symbol>,
        value: DataID,
    ) -> Result<DataID> {
        let Some(branch) = self.labels.get(&label.val).cloned() else {
            return Err(Error::ContinueWithUnknownLabel { keyword, label });
        };

        let scope = self.jump_stack.scope_of(branch);
        self.jump_stack.add_break_to(
            branch,
            self.graph.get_ctrl()?,
            self.symbol_table.snapshot_state_of_scope(scope),
            value,
        );
        Ok(self.graph.make_unreachable())
    }

    pub fn set_up_loop(&mut self, entry: CtrlID) -> (MergeID, Vec<PhiID>, OpenLoop, LoopID) {
        // ctrl node structure setup
        let loop_head = self.graph.add_merge(vec![entry]);

        let mut loop_phis = vec![];
        self.symbol_table.for_every_mutable(|_, data| {
            let phi = self.graph.add_phi(loop_head.clone(), vec![data.clone()]);
            *data = self.graph.add_phi_no_dedup(phi.clone());
            loop_phis.push(phi);
        }); // replace every variable with a phi and keep track of the phi's

        self.graph.add_merge_to_ctrl(loop_head.clone()); // with this the body has it as a control flow dependency

        let (tok, loop_id) = self
            .jump_stack
            .open_block(self.symbol_table.open_scope_id());
        (loop_head, loop_phis, tok, loop_id)
    }

    pub fn close_loop(
        &mut self,
        tok: OpenLoop,
        loop_backedge: Option<DataID>,
        mut loop_head: MergeID,
        mut loop_phis: Vec<PhiID>,
    ) -> DataID {
        let Jumps {
            mut continue_points,
            mut continue_states,
            mut break_points,
            mut break_states,
            mut break_values,
        } = self.jump_stack.close_block(tok);

        if let Ok(body_end) = self.graph.get_ctrl() {
            let body_end_state = self.symbol_table.snapshot_state(); // the state of every symbol after the hole body
            if let Some(data) = loop_backedge {
                break_points.push(body_end);
                break_values.push(data);
                break_states.push(body_end_state);
            } else {
                continue_points.push(body_end); // add the regular backedge
                continue_states.push(body_end_state); // add the regular backedge state
            }
        }

        loop_head.branches.append(&mut continue_points); // add the continuation points as backedges
        continue_states
            .into_iter()
            .flat_map(|overwrites| overwrites.into_iter().enumerate())
            .for_each(|(i, backedge)| loop_phis[i].variants.push(backedge)); // add thoses backedges to the phi nodes of mutable variables outside the loop for every jump

        if break_points.len() == 1 {
            self.graph.set_ctrl(break_points.pop().unwrap());

            let state = break_states.pop().unwrap();
            self.symbol_table
                .for_every_mutable(|i, value| *value = state[i].clone());

            break_values.pop().unwrap()
        } else {
            let exit_merge = self.graph.add_merge(break_points); // merge them
            self.merge_states(exit_merge.clone(), break_states);
            let phi = self.graph.add_phi(exit_merge.clone(), break_values);

            self.graph.add_merge_to_ctrl(exit_merge); // make it the control flow of the code after that
            self.graph.add_data_phi(phi)
        }
    }

    pub fn snapshot_state(&self) -> MutableState {
        self.symbol_table.snapshot_state()
    }

    pub fn merge_states(&mut self, merge: MergeID, states: Vec<MutableState>) {
        debug_assert_eq!(merge.branches.len(), states.len());

        if states.is_empty() {
            self.symbol_table
                .for_every_mutable(|_, value| *value = self.graph.add_never());
            return;
        }

        self.symbol_table.for_every_mutable(|i, value| {
            let mut variants = states
                .iter()
                .map(|state| state[i].clone())
                .collect::<Vec<_>>();
            let mut iter = variants.iter();
            let first = iter.next();
            *value = if iter.all(|x| Some(x) == first) {
                variants.pop().unwrap()
            } else {
                let phi = self.graph.add_phi(merge.clone(), variants);
                self.graph.add_data_phi(phi)
            };
        });
    }

    pub fn restore_state(&mut self, state: MutableState) {
        let mut state = state.into_iter();
        self.symbol_table
            .for_every_mutable(|_, m| *m = state.next().unwrap());
    }
}
