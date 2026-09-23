//! This module handles jumps statement and lables.
//!
//! `@label { }` and `loop {}` is referred to here as loop block.

use std::collections::HashMap;

use crate::{
    Symbol,
    builder::{CtrlCursors, DataCursors},
};

/// Two SoA datastructures one for storing `continue`-jumps and one for `breaks`-jumps.
#[must_use]
#[derive(Debug, Default)]
pub struct Jumps {
    pub continues: CtrlCursors,
    pub breaks: DataCursors<false>,
}

pub struct LoopBlockStack {
    label_to_loop_block: HashMap<Symbol, usize>,
    loop_blocks: Vec<(Option<Symbol>, Jumps)>,
}

#[must_use]
pub struct LoopIsOpen(());

impl LoopBlockStack {
    pub fn new() -> Self {
        Self {
            label_to_loop_block: HashMap::new(),
            loop_blocks: vec![],
        }
    }

    pub fn open_loop_block(&mut self) -> LoopIsOpen {
        self.loop_blocks.push((None, Jumps::default()));
        LoopIsOpen(())
    }

    pub fn open_loop_block_labeled(&mut self, label: Symbol) -> Option<LoopIsOpen> {
        let id = self.loop_blocks.len();
        self.loop_blocks.push((Some(label), Jumps::default()));

        #[allow(clippy::map_entry)]
        if !self.label_to_loop_block.contains_key(&label) {
            self.label_to_loop_block.insert(label, id);
            Some(LoopIsOpen(()))
        } else {
            None
        }
    }

    pub fn close_loop_block(&mut self, _: LoopIsOpen) -> Jumps {
        let (label, jumps) = self.loop_blocks.pop().unwrap();
        if let Some(label) = label {
            self.label_to_loop_block.remove(&label);
        };
        jumps
    }

    pub fn get(&mut self, label: Option<Symbol>) -> Option<&mut Jumps> {
        match label {
            Some(label) => {
                let id = self.label_to_loop_block.get(&label)?;
                Some(&mut self.loop_blocks[*id].1)
            }
            None => self.loop_blocks.last_mut().map(|(_, jumps)| jumps),
        }
    }
}

#[cfg(any())]
#[cfg(test)]
mod tests {
    use super::LoopBlockStack;
    use crate::{
        grapher::{Graph, builder::CtrlCursor},
        literal_parsing::Literal,
        parser::Interner,
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn empty_stack_has_no_current_or_labelled_block() {
        let mut blocks = LoopBlockStack::new();
        let mut interner = Interner::new();

        assert!(blocks.get(None).is_none());
        assert!(blocks.get(Some(interner.get("missing"))).is_none());
    }

    #[test]
    fn unlabelled_lookup_returns_innermost_block() {
        let mut blocks = LoopBlockStack::new();
        let outer = blocks.open_loop(None, 1);
        let inner = blocks.open_loop(None, 3);

        assert_eq!(blocks.get(None).expect("inner").state_size, 3);

        let inner_jumps = blocks.close_loop(inner);
        assert!(inner_jumps.continue_points.is_empty());
        assert_eq!(blocks.get(None).expect("outer").state_size, 1);

        let outer_jumps = blocks.close_loop(outer);
        assert!(outer_jumps.breaks.is_empty());
        assert!(blocks.get(None).is_none());
    }

    #[test]
    fn labelled_lookup_uses_nearest_matching_label() {
        let mut blocks = LoopBlockStack::new();
        let mut interner = Interner::new();
        let label = interner.get("target");
        let outer = blocks.open_loop(Some(label), 1);
        let _middle = blocks.open_loop(None, 2);
        let inner = blocks.open_loop(Some(label), 4);

        assert_eq!(blocks.get(Some(label)).expect("inner label").state_size, 4);

        let _ = blocks.close_loop(inner);
        assert_eq!(blocks.get(Some(label)).expect("outer label").state_size, 1);

        let _ = blocks.close_loop(_middle);
        let _ = blocks.close_loop(outer);
        assert!(blocks.get(Some(label)).is_none());
    }

    #[test]
    fn close_block_exports_recorded_continue_and_break_cursors() {
        let mut graph = graph();
        let mut blocks = LoopBlockStack::new();
        let tok = blocks.open_loop(None, 1);
        let state_value = graph.add_literal(Literal::from(1));
        let break_value = graph.add_literal(Literal::from(2));
        let ctrl = graph.start();

        let block = blocks.get(None).expect("block");
        block.continue_jumps.push(CtrlCursor {
            state: vec![Some(state_value.clone())],
            ctrl: ctrl.clone(),
        });
        block.break_jumps.push(
            CtrlCursor {
                state: vec![Some(state_value.clone())],
                ctrl: ctrl.clone(),
            }
            .with_data(break_value.clone()),
        );

        let jumps = blocks.close_loop(tok);

        assert_eq!(jumps.continue_points.len(), 1);
        assert_eq!(jumps.continue_points[0].addr(), ctrl.addr());
        assert_eq!(
            jumps.continue_states[0].states[0]
                .as_ref()
                .expect("state")
                .addr(),
            state_value.addr()
        );
        assert_eq!(jumps.breaks[0].data.addr(), break_value.addr());
    }
}
