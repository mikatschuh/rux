use crate::grapher::{
    graph::{CtrlID, DataID},
    parser::symbols::ScopeIdx,
};

pub struct Jumps<'src> {
    pub continue_points: Vec<CtrlID<'src>>,
    pub continue_states: Vec<Vec<DataID<'src>>>,
    pub break_points: Vec<CtrlID<'src>>,
    pub break_states: Vec<Vec<DataID<'src>>>,
    pub break_values: Vec<DataID<'src>>,
}

pub struct ContinueJump<'src> {
    ctrl: CtrlID<'src>,
    state: Vec<DataID<'src>>,
}

pub struct BreakJump<'src> {
    ctrl: CtrlID<'src>,
    state: Vec<DataID<'src>>,
    value: DataID<'src>,
}

pub struct Branch<'src> {
    scope: ScopeIdx,
    continue_jumps: Vec<ContinueJump<'src>>,
    break_jumps: Vec<BreakJump<'src>>,
}

impl<'src> Branch<'src> {
    fn new(scope: ScopeIdx) -> Self {
        Self {
            scope,
            continue_jumps: vec![],
            break_jumps: vec![],
        }
    }
}

pub struct JumpTableStack<'src> {
    branches: Vec<Branch<'src>>,
}
pub type BranchIdx = usize;

impl<'src> JumpTableStack<'src> {
    pub fn new() -> Self {
        Self { branches: vec![] }
    }

    pub fn open_branch_id(&self) -> BranchIdx {
        self.branches.len() - 1
    }

    pub fn scope(&self) -> Option<ScopeIdx> {
        self.branches.last().map(|b| b.scope)
    }

    pub fn scope_of(&self, branch: BranchIdx) -> ScopeIdx {
        self.branches[branch].scope
    }

    pub fn open_branch(&mut self, scope: ScopeIdx) {
        self.branches.push(Branch::new(scope))
    }

    #[must_use]
    pub fn close_branch(&mut self) -> Jumps<'src> {
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

    pub fn add_continue(&mut self, ctrl: CtrlID<'src>, state: Vec<DataID<'src>>) {
        self.branches
            .last_mut()
            .unwrap()
            .continue_jumps
            .push(ContinueJump { ctrl, state });
    }

    pub fn add_continue_to(
        &mut self,
        branch: BranchIdx,
        ctrl: CtrlID<'src>,
        state: Vec<DataID<'src>>,
    ) {
        self.branches[branch]
            .continue_jumps
            .push(ContinueJump { ctrl, state });
    }

    pub fn add_break(&mut self, ctrl: CtrlID<'src>, state: Vec<DataID<'src>>, value: DataID<'src>) {
        self.branches
            .last_mut()
            .unwrap()
            .break_jumps
            .push(BreakJump { ctrl, state, value });
    }

    pub fn add_break_to(
        &mut self,
        branch: BranchIdx,
        ctrl: CtrlID<'src>,
        state: Vec<DataID<'src>>,
        value: DataID<'src>,
    ) {
        self.branches[branch]
            .break_jumps
            .push(BreakJump { ctrl, state, value });
    }
}
