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

#[cfg(never)]
#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use super::JumpTableStack;
    use crate::{
        error::Span,
        grapher::{
            Graph,
            builder::{ScopedSymbolTable, binding::MutableState},
            graph::DataID,
        },
        literal_parsing::Literal,
        parser::{BuiltinType, Interner},
    };

    fn graph() -> Graph {
        Graph::new(Bump::new())
    }

    fn state_size(index: usize) -> usize {
        let mut table = ScopedSymbolTable::new();
        let mut scopes = Vec::new();
        for _ in 0..index {
            scopes.push(table.open_scope());
        }
        table.state_size()
    }

    fn state_with(graph: &mut Graph, value: DataID) -> MutableState {
        let mut interner = Interner::new();
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_builtin_type(BuiltinType::Signed { size: 32 });
        table.add_symbol(true, Span::beginning(), interner.get("state"), ty, value);
        table.snapshot_state()
    }

    #[test]
    fn starts_without_an_active_branch_scope() {
        let jumps = JumpTableStack::new();
        assert!(jumps.currents_state_size().is_none());
    }

    #[test]
    fn tracks_nested_branch_scopes_and_restores_parent_on_close() {
        let mut jumps = JumpTableStack::new();
        let outer_scope = state_size(3);
        let (outer_tok, outer) = jumps.open_block(outer_scope);
        assert!(jumps.currents_state_size().is_some());

        let inner_scope = state_size(7);
        let (inner_tok, inner) = jumps.open_block(inner_scope);
        assert!(jumps.currents_state_size().is_some());
        let _ = jumps.state_size_of(outer);
        let _ = jumps.state_size_of(inner);

        let inner_jumps = jumps.close_block(inner_tok);
        assert!(inner_jumps.continue_points.is_empty());
        assert!(inner_jumps.breaks.is_empty());
        assert!(jumps.currents_state_size().is_some());

        let outer_jumps = jumps.close_block(outer_tok);
        assert!(outer_jumps.continue_points.is_empty());
        assert!(outer_jumps.breaks.is_empty());
        assert!(jumps.currents_state_size().is_none());
    }

    #[test]
    fn records_continue_and_break_jumps_on_the_current_branch() {
        let mut graph = graph();

        let mut jumps = JumpTableStack::new();
        let (branch, _) = jumps.open_block(state_size(0));

        let continue_ctrl = graph.add_start();
        let break_merge = graph.add_merge(vec![continue_ctrl.clone()]);
        let break_ctrl = graph.add_ctrl_merge(break_merge);
        let state_value = graph.add_literal(Literal::from(5));
        let break_value = graph.add_literal(Literal::from(99));

        jumps.add_continue(
            continue_ctrl.clone(),
            state_with(&mut graph, state_value.clone()),
        );
        jumps.add_break(
            break_ctrl.clone(),
            state_with(&mut graph, break_value.clone()),
            break_value.clone(),
        );

        let closed = jumps.close_block(branch);
        assert_eq!(closed.continue_points.len(), 1);
        assert_eq!(closed.continue_points[0].addr(), continue_ctrl.addr());
        assert_eq!(closed.continue_states.len(), 1);
        assert_eq!(
            closed
                .continue_states
                .into_iter()
                .next()
                .unwrap()
                .into_iter()
                .next()
                .unwrap()
                .addr(),
            state_value.addr()
        );

        assert_eq!(closed.breaks.len(), 1);
        assert_eq!(closed.breaks[0].addr(), break_ctrl.addr());
        assert_eq!(closed.break_values[0].addr(), break_value.addr());
        assert_eq!(
            closed
                .break_states
                .into_iter()
                .next()
                .unwrap()
                .into_iter()
                .next()
                .unwrap()
                .addr(),
            break_value.addr()
        );
    }

    #[test]
    fn can_route_labelled_jumps_to_an_outer_branch_while_inner_branch_is_active() {
        let mut graph = graph();
        let mut jumps = JumpTableStack::new();
        let (outer_tok, outer) = jumps.open_block(state_size(0));
        let (inner_tok, _) = jumps.open_block(state_size(1));

        let ctrl = graph.add_start();
        let continue_value = graph.add_literal(Literal::from(1));
        let break_value = graph.add_literal(Literal::from(2));

        jumps.add_continue_to(
            outer,
            ctrl.clone(),
            state_with(&mut graph, continue_value.clone()),
        );
        jumps.add_break_to(
            outer,
            ctrl.clone(),
            state_with(&mut graph, break_value.clone()),
            break_value.clone(),
        );

        let inner_jumps = jumps.close_block(inner_tok);
        assert!(inner_jumps.continue_points.is_empty());
        assert!(inner_jumps.breaks.is_empty());

        let outer_jumps = jumps.close_block(outer_tok);
        assert_eq!(outer_jumps.continue_points[0].addr(), ctrl.addr());
        assert_eq!(outer_jumps.breaks[0].addr(), ctrl.addr());
        assert_eq!(outer_jumps.break_values[0].addr(), break_value.addr());
    }

    #[test]
    fn preserves_jump_insertion_order_when_closing_a_branch() {
        let mut graph = graph();
        let mut jumps = JumpTableStack::new();
        let (tok, _) = jumps.open_block(state_size(0));

        let ctrl = graph.add_start();
        let first = graph.add_literal(Literal::from(1));
        let second = graph.add_literal(Literal::from(2));

        jumps.add_continue(ctrl.clone(), state_with(&mut graph, first.clone()));
        jumps.add_continue(ctrl, state_with(&mut graph, second.clone()));

        let closed = jumps.close_block(tok);
        let states = closed
            .continue_states
            .into_iter()
            .map(|state| state.into_iter().next().unwrap().addr())
            .collect::<Vec<_>>();
        assert_eq!(states, vec![first.addr(), second.addr()]);
    }
}
