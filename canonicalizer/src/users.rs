use crate::{
    dedup::Idx,
    graph::{Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Merge, MergeKind, Nodes},
};

pub trait Deps {
    fn uses<'a>(&'a self) -> Uses<'a>;
}

pub struct Users {
    data: Vec<DataUsers>,
    ctrl: Vec<CtrlUsers>,
    branch: Vec<BranchUsers>,
    merge: Vec<MergeUsers>,
}

type UsersFrom<T> = Vec<Idx<T>>;
type DataUsers = UsersFrom<DataKind>;

struct CtrlUsers {
    branch: UsersFrom<BranchKind>,
    merge: UsersFrom<MergeKind>,
}

type BranchUsers = UsersFrom<CtrlKind>;

struct MergeUsers {
    data: UsersFrom<DataKind>,
    ctrl: UsersFrom<CtrlKind>,
}

#[derive(Debug, Default)]
pub struct Uses<'a> {
    data: &'a [Data],
    ctrl: &'a [Ctrl],
    branch: &'a [Branch],
    merge: &'a [Merge],
}

impl<'a> Uses<'a> {
    pub fn with_data(self, data: &'a [Data]) -> Self {
        Self { data, ..self }
    }
    pub fn with_ctrl(self, ctrl: &'a [Ctrl]) -> Self {
        Self { ctrl, ..self }
    }
    pub fn with_branch(self, branch: &'a [Branch]) -> Self {
        Self { branch, ..self }
    }
    pub fn with_merge(self, merge: &'a [Merge]) -> Self {
        Self { merge, ..self }
    }

    fn direct(
        self,
        deps_indice: &Nodes,
    ) -> DirectUses<
        impl Iterator<Item = Idx<DataKind>>,
        impl Iterator<Item = Idx<CtrlKind>>,
        impl Iterator<Item = Idx<BranchKind>>,
        impl Iterator<Item = Idx<MergeKind>>,
    > {
        DirectUses {
            data: self.data.iter().map(|dep| deps_indice.data.get_idx(*dep)),
            ctrl: self.ctrl.iter().map(|dep| deps_indice.ctrl.get_idx(*dep)),
            branch: self
                .branch
                .iter()
                .map(|dep| deps_indice.branch.get_idx(*dep)),
            merge: self.merge.iter().map(|dep| deps_indice.merge.get_idx(*dep)),
        }
    }
}

struct DirectUses<D, C, B, M>
where
    D: Iterator<Item = Idx<DataKind>>,
    C: Iterator<Item = Idx<CtrlKind>>,
    B: Iterator<Item = Idx<BranchKind>>,
    M: Iterator<Item = Idx<MergeKind>>,
{
    data: D,
    ctrl: C,
    branch: B,
    merge: M,
}

impl Users {
    pub fn add_data_user(&mut self, deps_indice: &Nodes, user: Idx<DataKind>, uses: Uses) {
        let uses = uses.direct(deps_indice);
        uses.data.for_each(|used| self.data[used].push(user));
        uses.merge.for_each(|used| self.merge[used].data.push(user));
    }

    pub fn add_ctrl_user(&mut self, deps_indice: &Nodes, user: Idx<CtrlKind>, uses: Uses) {
        let uses = uses.direct(deps_indice);
        uses.branch.for_each(|used| self.branch[used].push(user));
        uses.merge.for_each(|used| self.merge[used].ctrl.push(user));
    }

    pub fn add_branch_user(&mut self, deps_indice: &Nodes, user: Idx<BranchKind>, uses: Uses) {
        uses.direct(deps_indice)
            .ctrl
            .for_each(|used| self.ctrl[used].branch.push(user));
    }

    pub fn add_merge_user(&mut self, deps_indice: &Nodes, user: Idx<MergeKind>, uses: Uses) {
        uses.direct(deps_indice)
            .ctrl
            .for_each(|used| self.ctrl[used].merge.push(user));
    }
}
