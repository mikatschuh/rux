use crate::{
    grapher::builder::{CtrlCursor, DataCursor},
    parser::Symbol,
};

#[must_use]
pub struct LoopBackedges {
    pub continues: Vec<CtrlCursor>,
    pub breaks: Vec<DataCursor>,
}

pub struct Loop {
    label: Option<Symbol>,
    pub continue_jumps: Vec<CtrlCursor>,
    pub break_jumps: Vec<DataCursor>,
}

impl Loop {
    fn new(label: Option<Symbol>) -> Self {
        Self {
            label,
            continue_jumps: vec![],
            break_jumps: vec![],
        }
    }
}

pub struct JumpTableStack {
    loops: Vec<Loop>,
}

#[must_use]
pub struct OpenLoop(());

impl JumpTableStack {
    pub fn new() -> Self {
        Self { loops: vec![] }
    }

    pub fn open_loop(&mut self, label: Option<Symbol>) -> OpenLoop {
        self.loops.push(Loop::new(label));
        OpenLoop(())
    }

    pub fn close_loop(&mut self, _: OpenLoop) -> LoopBackedges {
        let Loop {
            continue_jumps,
            break_jumps,
            ..
        } = self.loops.pop().unwrap(); // This is safe because of the OpenLoop token

        LoopBackedges {
            continues: continue_jumps,
            breaks: break_jumps,
        }
    }

    pub fn get(&mut self, label: Option<Symbol>) -> Option<&mut Loop> {
        match label {
            Some(label) => self
                .loops
                .iter_mut()
                .rev()
                .find(|b| b.label.is_some_and(|l| l == label)),
            None => self.loops.last_mut(),
        }
    }

    pub fn get_block(&mut self) -> &mut Loop {
        self.loops.last_mut().unwrap() // caller side proof
    }
}

#[cfg(never)]
#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::JumpTableStack;
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
        let mut blocks = JumpTableStack::new();
        let mut interner = Interner::new();

        assert!(blocks.get(None).is_none());
        assert!(blocks.get(Some(interner.get("missing"))).is_none());
    }

    #[test]
    fn unlabelled_lookup_returns_innermost_block() {
        let mut blocks = JumpTableStack::new();
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
        let mut blocks = JumpTableStack::new();
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
        let mut blocks = JumpTableStack::new();
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
