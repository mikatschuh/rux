use crate::grapher::{
    builder::symbols::{MutableState, ScopeID},
    graph::{CtrlID, DataID},
};

pub struct Jumps<'src> {
    pub continue_points: Vec<CtrlID<'src>>,
    pub continue_states: Vec<MutableState<'src>>,
    pub break_points: Vec<CtrlID<'src>>,
    pub break_states: Vec<MutableState<'src>>,
    pub break_values: Vec<DataID<'src>>,
}

pub struct ContinueJump<'src> {
    ctrl: CtrlID<'src>,
    state: MutableState<'src>,
}

pub struct BreakJump<'src> {
    ctrl: CtrlID<'src>,
    state: MutableState<'src>,
    value: DataID<'src>,
}

pub struct Branch<'src> {
    scope: ScopeID,
    continue_jumps: Vec<ContinueJump<'src>>,
    break_jumps: Vec<BreakJump<'src>>,
}

impl<'src> Branch<'src> {
    fn new(scope: ScopeID) -> Self {
        Self {
            scope,
            continue_jumps: vec![],
            break_jumps: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::JumpTableStack;
    use crate::{
        grapher::{
            Graph,
            builder::{
                ScopedSymbolTable, Symbol,
                symbols::{MutableState, ScopeID},
            },
            graph::DataID,
        },
        literal_parsing::Literal,
        type_parsing::AtomicType,
    };

    fn scope_id(index: usize) -> ScopeID {
        let mut table = ScopedSymbolTable::new();
        let mut scopes = Vec::new();
        for _ in 0..index {
            scopes.push(table.open_scope());
        }
        table.open_scope_id()
    }

    fn state_with<'src>(graph: &mut Graph<'src>, value: DataID<'src>) -> MutableState<'src> {
        let mut table = ScopedSymbolTable::new();
        let ty = graph.add_type(AtomicType::Signed { size: 32 });
        table.add_mutable("state", Symbol { ty, value });
        table.snapshot_state()
    }

    #[test]
    fn starts_without_an_active_branch_scope() {
        let jumps = JumpTableStack::new();
        assert!(jumps.scope().is_none());
    }

    #[test]
    fn tracks_nested_branch_scopes_and_restores_parent_on_close() {
        let mut jumps = JumpTableStack::new();
        let outer_scope = scope_id(3);
        let (outer_tok, outer) = jumps.open_branch(outer_scope);
        assert!(jumps.scope().is_some());

        let inner_scope = scope_id(7);
        let (inner_tok, inner) = jumps.open_branch(inner_scope);
        assert!(jumps.scope().is_some());
        let _ = jumps.scope_of(outer);
        let _ = jumps.scope_of(inner);

        let inner_jumps = jumps.close_branch(inner_tok);
        assert!(inner_jumps.continue_points.is_empty());
        assert!(inner_jumps.break_points.is_empty());
        assert!(jumps.scope().is_some());

        let outer_jumps = jumps.close_branch(outer_tok);
        assert!(outer_jumps.continue_points.is_empty());
        assert!(outer_jumps.break_points.is_empty());
        assert!(jumps.scope().is_none());
    }

    #[test]
    fn records_continue_and_break_jumps_on_the_current_branch() {
        let mut graph = Graph::new();
        let mut jumps = JumpTableStack::new();
        let (branch, _) = jumps.open_branch(scope_id(0));

        let continue_ctrl = graph.get_ctrl().expect("ctrl");
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

        let closed = jumps.close_branch(branch);
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

        assert_eq!(closed.break_points.len(), 1);
        assert_eq!(closed.break_points[0].addr(), break_ctrl.addr());
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
        let mut graph = Graph::new();
        let mut jumps = JumpTableStack::new();
        let (outer_tok, outer) = jumps.open_branch(scope_id(0));
        let (inner_tok, _) = jumps.open_branch(scope_id(1));

        let ctrl = graph.get_ctrl().expect("ctrl");
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

        let inner_jumps = jumps.close_branch(inner_tok);
        assert!(inner_jumps.continue_points.is_empty());
        assert!(inner_jumps.break_points.is_empty());

        let outer_jumps = jumps.close_branch(outer_tok);
        assert_eq!(outer_jumps.continue_points[0].addr(), ctrl.addr());
        assert_eq!(outer_jumps.break_points[0].addr(), ctrl.addr());
        assert_eq!(outer_jumps.break_values[0].addr(), break_value.addr());
    }

    #[test]
    fn preserves_jump_insertion_order_when_closing_a_branch() {
        let mut graph = Graph::new();
        let mut jumps = JumpTableStack::new();
        let (tok, _) = jumps.open_branch(scope_id(0));

        let ctrl = graph.get_ctrl().expect("ctrl");
        let first = graph.add_literal(Literal::from(1));
        let second = graph.add_literal(Literal::from(2));

        jumps.add_continue(ctrl.clone(), state_with(&mut graph, first.clone()));
        jumps.add_continue(ctrl, state_with(&mut graph, second.clone()));

        let closed = jumps.close_branch(tok);
        let states = closed
            .continue_states
            .into_iter()
            .map(|state| state.into_iter().next().unwrap().addr())
            .collect::<Vec<_>>();
        assert_eq!(states, vec![first.addr(), second.addr()]);
    }
}

pub struct JumpTableStack<'src> {
    branches: Vec<Branch<'src>>,
}

#[derive(Clone, Copy)]
pub struct BranchID(usize);

#[must_use]
pub struct OpenBranch(());

impl<'src> JumpTableStack<'src> {
    pub fn new() -> Self {
        Self { branches: vec![] }
    }

    pub fn scope(&self) -> Option<ScopeID> {
        self.branches.last().map(|b| b.scope)
    }

    pub fn scope_of(&self, branch: BranchID) -> ScopeID {
        self.branches[branch.0].scope
    }

    pub fn open_branch(&mut self, scope: ScopeID) -> (OpenBranch, BranchID) {
        let branch = self.branches.len();
        self.branches.push(Branch::new(scope));
        (OpenBranch(()), BranchID(branch))
    }

    #[must_use]
    pub fn close_branch(&mut self, _: OpenBranch) -> Jumps<'src> {
        debug_assert!(!self.branches.is_empty());
        let Branch {
            continue_jumps,
            break_jumps,
            ..
        } = self.branches.pop().unwrap();

        Jumps {
            continue_points: continue_jumps.iter().map(|c| c.ctrl.clone()).collect(),
            continue_states: continue_jumps.into_iter().map(|c| c.state).collect(),
            break_points: break_jumps.iter().map(|b| b.ctrl.clone()).collect(),
            break_values: break_jumps.iter().map(|b| b.value.clone()).collect(),
            break_states: break_jumps.into_iter().map(|b| b.state).collect(),
        }
    }

    pub fn add_continue(&mut self, ctrl: CtrlID<'src>, state: MutableState<'src>) {
        self.branches
            .last_mut()
            .unwrap()
            .continue_jumps
            .push(ContinueJump { ctrl, state });
    }

    pub fn add_continue_to(
        &mut self,
        branch: BranchID,
        ctrl: CtrlID<'src>,
        state: MutableState<'src>,
    ) {
        self.branches[branch.0]
            .continue_jumps
            .push(ContinueJump { ctrl, state });
    }

    pub fn add_break(
        &mut self,
        ctrl: CtrlID<'src>,
        state: MutableState<'src>,
        value: DataID<'src>,
    ) {
        self.branches
            .last_mut()
            .unwrap()
            .break_jumps
            .push(BreakJump { ctrl, state, value });
    }

    pub fn add_break_to(
        &mut self,
        branch: BranchID,
        ctrl: CtrlID<'src>,
        state: MutableState<'src>,
        value: DataID<'src>,
    ) {
        self.branches[branch.0]
            .break_jumps
            .push(BreakJump { ctrl, state, value });
    }
}
