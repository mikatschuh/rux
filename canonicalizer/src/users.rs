use std::collections::HashSet;

use crate::{
    canonical::Dir,
    graph::{Branch, BranchKind, Ctrl, CtrlKind, Data, DataKind, Merge, MergeKind, Nodes},
};

pub trait ListUses {
    fn uses<'a>(&'a self) -> Uses<'a>;
}

pub trait ReportUses: Sized + ListUses {
    fn add_node(
        &self,
        user_dir: Dir<Self>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        reused: bool,
    );
    fn remove_node(&self, user_dir: Dir<Self>, indirection: &Nodes, user_table: &mut UserTable);
}

pub struct UserTable {
    data: Vec<DataUsers>,
    ctrl: Vec<CtrlUsers>,
    branch: Vec<BranchUsers>,
    merge: Vec<MergeUsers>,
}

type UsersOf<T> = HashSet<Dir<T>>;

#[derive(Default)]
struct DataUsers {
    data: UsersOf<DataKind>,
}

#[derive(Default)]
struct CtrlUsers {
    branch: UsersOf<BranchKind>,
    merge: UsersOf<MergeKind>,
}

#[derive(Default)]
struct BranchUsers {
    ctrl: UsersOf<CtrlKind>,
}

#[derive(Default)]
struct MergeUsers {
    data: UsersOf<DataKind>,
    ctrl: UsersOf<CtrlKind>,
}

#[derive(Debug, Default)]
pub struct Uses<'a> {
    pub data: &'a [Data],
    pub ctrl: &'a [Ctrl],
    pub branch: &'a [Branch],
    pub merge: &'a [Merge],
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
        indirection: &Nodes,
    ) -> DirectUses<
        impl Iterator<Item = Dir<DataKind>>,
        impl Iterator<Item = Dir<CtrlKind>>,
        impl Iterator<Item = Dir<BranchKind>>,
        impl Iterator<Item = Dir<MergeKind>>,
    > {
        DirectUses {
            data: self.data.iter().map(|dep| indirection.data.direct(*dep)),
            ctrl: self.ctrl.iter().map(|dep| indirection.ctrl.direct(*dep)),
            branch: self
                .branch
                .iter()
                .map(|dep| indirection.branch.direct(*dep)),
            merge: self.merge.iter().map(|dep| indirection.merge.direct(*dep)),
        }
    }
}

struct DirectUses<D, C, B, M>
where
    D: Iterator<Item = Dir<DataKind>>,
    C: Iterator<Item = Dir<CtrlKind>>,
    B: Iterator<Item = Dir<BranchKind>>,
    M: Iterator<Item = Dir<MergeKind>>,
{
    data: D,
    ctrl: C,
    branch: B,
    merge: M,
}

impl ReportUses for DataKind {
    fn add_node(
        &self,
        user_dir: Dir<DataKind>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        reused: bool,
    ) {
        if !reused {
            user_table.data.push(DataUsers::default())
        }
        let uses = self.uses().direct(indirection);
        uses.data.for_each(|used| {
            user_table.data[used].data.insert(user_dir);
        });
        uses.merge.for_each(|used| {
            user_table.merge[used].data.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Dir<Self>, indirection: &Nodes, user_table: &mut UserTable) {
        user_table.data[user_dir].data.clear();
        let uses = self.uses().direct(indirection);
        uses.data.for_each(|used| {
            user_table.data[used].data.remove(&user_dir);
        });
        uses.merge.for_each(|used| {
            user_table.merge[used].data.remove(&user_dir);
        });
    }
}

impl ReportUses for CtrlKind {
    fn add_node(
        &self,
        user_dir: Dir<Self>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        reused: bool,
    ) {
        if !reused {
            user_table.ctrl.push(CtrlUsers::default())
        }
        let uses = self.uses().direct(indirection);
        uses.branch.for_each(|used| {
            user_table.branch[used].ctrl.insert(user_dir);
        });
        uses.merge.for_each(|used| {
            user_table.merge[used].ctrl.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Dir<Self>, indirection: &Nodes, user_table: &mut UserTable) {
        user_table.ctrl[user_dir].branch.clear();
        user_table.ctrl[user_dir].merge.clear();
        let uses = self.uses().direct(indirection);
        uses.branch.for_each(|used| {
            user_table.branch[used].ctrl.remove(&user_dir);
        });
        uses.merge.for_each(|used| {
            user_table.merge[used].ctrl.remove(&user_dir);
        });
    }
}

impl ReportUses for BranchKind {
    fn add_node(
        &self,
        user_dir: Dir<Self>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        reused: bool,
    ) {
        if !reused {
            user_table.branch.push(BranchUsers::default())
        }
        let uses = self.uses().direct(indirection);
        uses.ctrl.for_each(|used| {
            user_table.ctrl[used].branch.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Dir<Self>, indirection: &Nodes, user_table: &mut UserTable) {
        user_table.branch[user_dir].ctrl.clear();
        let uses = self.uses().direct(indirection);
        uses.ctrl.for_each(|used| {
            user_table.ctrl[used].branch.remove(&user_dir);
        });
    }
}

impl ReportUses for MergeKind {
    fn add_node(
        &self,
        user_dir: Dir<Self>,
        indirection: &Nodes,
        user_table: &mut UserTable,
        reused: bool,
    ) {
        if !reused {
            user_table.merge.push(MergeUsers::default())
        }
        let uses = self.uses().direct(indirection);
        uses.ctrl.for_each(|used| {
            user_table.ctrl[used].merge.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Dir<Self>, indirection: &Nodes, user_table: &mut UserTable) {
        user_table.merge[user_dir].data.clear();
        user_table.merge[user_dir].ctrl.clear();
        let uses = self.uses().direct(indirection);
        uses.ctrl.for_each(|used| {
            user_table.ctrl[used].merge.remove(&user_dir);
        });
    }
}
