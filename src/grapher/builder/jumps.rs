use crate::grapher::{
    builder::symbols::ScopeID,
    graph::{CtrlID, DataID},
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

pub struct JumpTableStack<'src> {
    branches: Vec<Branch<'src>>,
}

#[derive(Clone, Copy)]
pub struct BranchID(usize);

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

    pub fn open_branch(&mut self, scope: ScopeID) -> BranchID {
        let branch = self.branches.len();
        self.branches.push(Branch::new(scope));
        BranchID(branch)
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
        branch: BranchID,
        ctrl: CtrlID<'src>,
        state: Vec<DataID<'src>>,
    ) {
        self.branches[branch.0]
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
        branch: BranchID,
        ctrl: CtrlID<'src>,
        state: Vec<DataID<'src>>,
        value: DataID<'src>,
    ) {
        self.branches[branch.0]
            .break_jumps
            .push(BreakJump { ctrl, state, value });
    }
}
