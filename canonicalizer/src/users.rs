use std::collections::HashSet;

use crate::{
    canonical::Unique,
    graph::{BranchKind, CtrlKind, DataKind, ListDeps, MergeKind},
};

pub trait ReportUses: Sized + ListDeps {
    fn add_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable);
    fn remove_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable);

    fn add_dep(user_table: &mut UserTable);
}

#[derive(Default)]
pub struct UserTable {
    pub data: Vec<DataUsers>,
    pub ctrl: Vec<CtrlUsers>,
    pub branch: Vec<BranchUsers>,
    pub merge: Vec<MergeUsers>,
}

type UsersOf<T> = HashSet<Unique<T>>;

#[derive(Default)]
pub struct DataUsers {
    pub data: UsersOf<DataKind>,
    pub branch: UsersOf<BranchKind>,
}

#[derive(Default)]
pub struct CtrlUsers {
    pub branch: UsersOf<BranchKind>,
    pub merge: UsersOf<MergeKind>,
}

#[derive(Default)]
pub struct BranchUsers {
    pub ctrl: UsersOf<CtrlKind>,
}

#[derive(Default)]
pub struct MergeUsers {
    pub data: UsersOf<DataKind>,
    pub ctrl: UsersOf<CtrlKind>,
}

impl ReportUses for DataKind {
    fn add_node(&self, user_dir: Unique<DataKind>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.data.iter().copied().for_each(|used| {
            user_table.data[used].data.insert(user_dir);
        });
        uses.merge.iter().copied().for_each(|used| {
            user_table.merge[used].data.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.data.iter().copied().for_each(|used| {
            user_table.data[used].data.remove(&user_dir);
        });
        uses.merge.iter().copied().for_each(|used| {
            user_table.merge[used].data.remove(&user_dir);
        });
    }

    fn add_dep(user_table: &mut UserTable) {
        user_table.data.push(DataUsers::default());
    }
}

impl ReportUses for CtrlKind {
    fn add_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.branch.iter().copied().for_each(|used| {
            user_table.branch[used].ctrl.insert(user_dir);
        });
        uses.merge.iter().copied().for_each(|used| {
            user_table.merge[used].ctrl.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.branch.iter().copied().for_each(|used| {
            user_table.branch[used].ctrl.remove(&user_dir);
        });
        uses.merge.iter().copied().for_each(|used| {
            user_table.merge[used].ctrl.remove(&user_dir);
        });
    }

    fn add_dep(user_table: &mut UserTable) {
        user_table.ctrl.push(CtrlUsers::default());
    }
}

impl ReportUses for BranchKind {
    fn add_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.ctrl.iter().copied().for_each(|used| {
            user_table.ctrl[used].branch.insert(user_dir);
        });
        uses.data.iter().copied().for_each(|used| {
            user_table.data[used].branch.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.ctrl.iter().copied().for_each(|used| {
            user_table.ctrl[used].branch.remove(&user_dir);
        });
        uses.data.iter().copied().for_each(|used| {
            user_table.data[used].branch.remove(&user_dir);
        });
    }

    fn add_dep(user_table: &mut UserTable) {
        user_table.branch.push(BranchUsers::default());
    }
}

impl ReportUses for MergeKind {
    fn add_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.ctrl.iter().copied().for_each(|used| {
            user_table.ctrl[used].merge.insert(user_dir);
        });
    }

    fn remove_node(&self, user_dir: Unique<Self>, user_table: &mut UserTable) {
        let uses = self.deps();
        uses.ctrl.iter().copied().for_each(|used| {
            user_table.ctrl[used].merge.remove(&user_dir);
        });
    }

    fn add_dep(user_table: &mut UserTable) {
        user_table.merge.push(MergeUsers::default());
    }
}
