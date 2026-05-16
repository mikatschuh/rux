use crate::{
    grapher::{
        binding::MutableState,
        builder::{Cursor, DataCursor},
        graph::CtrlID,
    },
    parser::Symbol,
};

#[must_use]
pub struct Jumps {
    pub continue_points: Vec<CtrlID>,
    pub continue_states: Vec<MutableState>,
    pub breaks: Vec<DataCursor>,
}

pub struct Block {
    label: Option<Symbol>,
    pub state_size: usize, // the size of the state the loop owns
    pub continue_jumps: Vec<Cursor>,
    pub break_jumps: Vec<DataCursor>,
}

impl Block {
    fn new(label: Option<Symbol>, state_size: usize) -> Self {
        Self {
            label,
            state_size,
            continue_jumps: vec![],
            break_jumps: vec![],
        }
    }
}

pub struct JumpTableStack {
    blocks: Vec<Block>,
}

#[must_use]
pub struct OpenLoop(());

impl JumpTableStack {
    pub fn new() -> Self {
        Self { blocks: vec![] }
    }

    pub fn open_block(&mut self, label: Option<Symbol>, state: usize) -> OpenLoop {
        self.blocks.push(Block::new(label, state));
        OpenLoop(())
    }

    pub fn close_block(&mut self, _: OpenLoop) -> Jumps {
        let Block {
            continue_jumps,
            break_jumps,
            ..
        } = self.blocks.pop().unwrap(); // This is safe because of the OpenLoop token

        Jumps {
            continue_points: continue_jumps.iter().map(|c| c.ctrl.clone()).collect(),
            continue_states: continue_jumps.into_iter().map(|c| c.state).collect(),
            breaks: break_jumps.into_iter().collect(),
        }
    }

    pub fn get(&mut self, label: Option<Symbol>) -> Option<&mut Block> {
        match label {
            Some(label) => self
                .blocks
                .iter_mut()
                .rev()
                .find(|b| b.label.is_some_and(|l| l == label)),
            None => self.blocks.last_mut(),
        }
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::JumpTableStack;
    use crate::{
        grapher::{Graph, builder::Cursor},
        literal_parsing::Literal,
        parser::Interner,
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    #[test]
    fn empty_stack_has_no_current_or_labelled_block() {
        let mut blocks = JumpTableStack::new();
        let mut interner = Interner::new();

        assert!(blocks.get(None).is_none());
        assert!(blocks.get(Some(interner.get("missing"))).is_none());
    }

    #[test]
    fn unlabelled_lookup_returns_innermost_block() {
        let mut blocks = JumpTableStack::new();
        let outer = blocks.open_block(None, 1);
        let inner = blocks.open_block(None, 3);

        assert_eq!(blocks.get(None).expect("inner").state_size, 3);

        let inner_jumps = blocks.close_block(inner);
        assert!(inner_jumps.continue_points.is_empty());
        assert_eq!(blocks.get(None).expect("outer").state_size, 1);

        let outer_jumps = blocks.close_block(outer);
        assert!(outer_jumps.breaks.is_empty());
        assert!(blocks.get(None).is_none());
    }

    #[test]
    fn labelled_lookup_uses_nearest_matching_label() {
        let mut blocks = JumpTableStack::new();
        let mut interner = Interner::new();
        let label = interner.get("target");
        let outer = blocks.open_block(Some(label), 1);
        let _middle = blocks.open_block(None, 2);
        let inner = blocks.open_block(Some(label), 4);

        assert_eq!(blocks.get(Some(label)).expect("inner label").state_size, 4);

        let _ = blocks.close_block(inner);
        assert_eq!(blocks.get(Some(label)).expect("outer label").state_size, 1);

        let _ = blocks.close_block(_middle);
        let _ = blocks.close_block(outer);
        assert!(blocks.get(Some(label)).is_none());
    }

    #[test]
    fn close_block_exports_recorded_continue_and_break_cursors() {
        let mut graph = graph();
        let mut blocks = JumpTableStack::new();
        let tok = blocks.open_block(None, 1);
        let state_value = graph.add_literal(Literal::from(1));
        let break_value = graph.add_literal(Literal::from(2));
        let ctrl = graph.start();

        let block = blocks.get(None).expect("block");
        block.continue_jumps.push(Cursor {
            state: vec![Some(state_value.clone())],
            ctrl: ctrl.clone(),
        });
        block.break_jumps.push(
            Cursor {
                state: vec![Some(state_value.clone())],
                ctrl: ctrl.clone(),
            }
            .with_data(break_value.clone()),
        );

        let jumps = blocks.close_block(tok);

        assert_eq!(jumps.continue_points.len(), 1);
        assert_eq!(jumps.continue_points[0].addr(), ctrl.addr());
        assert_eq!(
            jumps.continue_states[0][0].as_ref().expect("state").addr(),
            state_value.addr()
        );
        assert_eq!(jumps.breaks[0].data.addr(), break_value.addr());
    }
}
