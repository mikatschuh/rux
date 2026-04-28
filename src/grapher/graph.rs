use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    literal_parsing::Literal,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    type_parsing::AtomicType,
    utilities::{NoDealloc, Rc},
};

pub type DataID<'src> = Rc<DataNode<'src>, NoDealloc>;
pub type CtrlID<'src> = Rc<CtrlNode<'src>, NoDealloc>;
pub type BranchID<'src> = Rc<Branch<'src>, NoDealloc>;
pub type MergeID<'src> = Rc<Merge<'src>, NoDealloc>;
pub type LoopID<'src> = Rc<Loop<'src>, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DataNode<'src> {
    pub kind: DataKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct CtrlNode<'src> {
    pub kind: CtrlKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch<'src> {
    pub ctrl: CtrlID<'src>,
    pub condition: DataID<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Loop<'src> {
    pub ctrl: CtrlID<'src>,
    pub condition: DataID<'src>,
    pub backedge: CtrlID<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Merge<'src> {
    pub branches: Vec<CtrlID<'src>>, // classically: false - true
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum CtrlKind<'src> {
    Start,
    Merge {
        merge: MergeID<'src>,
    },
    TrueBranch {
        branch: BranchID<'src>,
    },
    FalseBranch {
        branch: BranchID<'src>,
    },

    LoopHead {
        ctrl: CtrlID<'src>,
        condition: DataID<'src>,
        backedge: CtrlID<'src>,
    },
    PlaceHolder {
        ctrl: CtrlID<'src>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum DataKind<'src> {
    AtomicType {
        ty: AtomicType,
    },
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,

    Unary {
        op: UnaryOp,
        input: DataID<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: DataID<'src>,
        rhs: DataID<'src>,
    },
    Load {
        mem: CtrlID<'src>,
        addr: DataID<'src>,
    },

    BranchPhi {
        merge: MergeID<'src>,
        when_true: DataID<'src>,
        when_false: DataID<'src>,
    },
    LoopPhi {
        loop_head: LoopID<'src>,
        entry: DataID<'src>,
        backedge: DataID<'src>,
    },

    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum DataKey<'src> {
    AtomicType {
        ty: AtomicType,
    },
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    Boolean(bool),
    Unit,
    Unary {
        op: UnaryOp,
        input: usize,
    },
    Binary {
        op: BinaryOp,
        lhs: usize,
        rhs: usize,
    },
    Load {
        mem: usize,
        addr: usize,
    },
    BranchPhi {
        merge: usize,
        when_true: usize,
        when_false: usize,
    },
    LoopPhi {
        loop_head: usize,
        entry: usize,
        backedge: usize,
    },
}

#[derive(Debug)]
pub struct Graph<'src> {
    arena: Bump,
    node_cache: HashMap<DataKey<'src>, DataID<'src>>,
    pub current_mem: CtrlID<'src>,
}
impl<'src> Graph<'src> {
    pub fn new() -> Self {
        let arena = Bump::new();

        Self {
            node_cache: HashMap::new(),
            current_mem: Rc::<CtrlNode<'src>, NoDealloc>::new_in_bump(
                CtrlNode {
                    kind: CtrlKind::Start,
                },
                &arena,
            ),
            arena,
        }
    }

    fn push_node(&mut self, kind: DataKind<'src>) -> DataID<'src> {
        let key = kind.node_key();
        if let Some(ref key) = key
            && let Some(existing) = self.node_cache.get(key)
        {
            return existing.clone();
        }

        let node: DataNode<'src> = DataNode { kind };
        let node_id = Rc::<DataNode<'src>, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.node_cache.insert(key, node_id.clone());
        }
        node_id
    }

    fn push_node_no_dedup(&mut self, kind: DataKind<'src>) -> DataID<'src> {
        Rc::<DataNode<'src>, NoDealloc>::new_in_bump(DataNode { kind }, &self.arena)
    }

    fn push_mem_node(&mut self, kind: CtrlKind<'src>) -> CtrlID<'src> {
        let node: CtrlNode<'src> = CtrlNode { kind };
        Rc::<CtrlNode<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    fn push_branch(&mut self, branch: Branch<'src>) -> BranchID<'src> {
        Rc::<Branch<'src>, NoDealloc>::new_in_bump(branch, &self.arena)
    }

    pub fn add_merge(&mut self, branches: Vec<CtrlID<'src>>) -> MergeID<'src> {
        Rc::<Merge<'src>, NoDealloc>::new_in_bump(Merge { branches }, &self.arena)
    }

    pub fn add_ctrl_merge(&mut self, merge: MergeID<'src>) -> CtrlID<'src> {
        self.push_mem_node(CtrlKind::Merge { merge })
    }

    pub fn add_branch(&mut self, condition: DataID<'src>) -> (CtrlID<'src>, CtrlID<'src>) {
        let ctrl = self.current_mem();
        let branch = self.push_branch(Branch { ctrl, condition });
        (
            self.push_mem_node(CtrlKind::FalseBranch {
                branch: branch.clone(),
            }),
            self.push_mem_node(CtrlKind::TrueBranch { branch }),
        )
    }

    pub fn add_loop_head(&mut self, condition: DataID<'src>) -> CtrlID<'src> {
        let ctrl = self.current_mem();
        let place_holder = self.add_placeholder(ctrl.clone());
        self.push_mem_node(CtrlKind::LoopHead {
            condition,
            ctrl,
            backedge: self.current_mem(),
        })
    }

    pub fn add_placeholder(&mut self, ctrl: CtrlID<'src>) -> CtrlID<'src> {
        self.push_mem_node(CtrlKind::PlaceHolder { ctrl })
    }

    pub fn add_load(&mut self, addr: DataID<'src>) -> DataID<'src> {
        self.push_node(DataKind::Load {
            mem: self.current_mem(),
            addr,
        })
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> DataID<'src> {
        self.push_node(DataKind::Literal { literal })
    }

    pub fn add_unary(&mut self, op: UnaryOp, input: DataID<'src>) -> DataID<'src> {
        self.push_node(DataKind::Unary { op, input })
    }

    pub fn add_binary(
        &mut self,
        op: BinaryOp,
        lhs: DataID<'src>,
        rhs: DataID<'src>,
    ) -> DataID<'src> {
        self.push_node(DataKind::Binary { op, lhs, rhs })
    }

    pub fn add_phi(
        &mut self,
        merge: MergeID<'src>,
        when_true: DataID<'src>,
        when_false: DataID<'src>,
    ) -> DataID<'src> {
        self.push_node(DataKind::BranchPhi {
            merge,
            when_true,
            when_false,
        })
    }

    pub fn add_quote(&mut self, quote: String) -> DataID<'src> {
        self.push_node(DataKind::Quote { quote })
    }

    pub fn add_bool(&mut self, boolean: bool) -> DataID<'src> {
        self.push_node(DataKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, type_: AtomicType) -> DataID<'src> {
        self.push_node(DataKind::AtomicType { ty: type_ })
    }

    pub fn add_unknown(&mut self) -> DataID<'src> {
        self.push_node_no_dedup(DataKind::Unknown)
    }

    pub fn add_unit(&mut self) -> DataID<'src> {
        self.push_node(DataKind::Unit)
    }

    pub fn current_mem(&self) -> CtrlID<'src> {
        self.current_mem.clone()
    }
}

impl<'src> DataKind<'src> {
    fn node_key(&self) -> Option<DataKey<'src>> {
        match self {
            DataKind::AtomicType { ty } => Some(DataKey::AtomicType { ty: *ty }),
            DataKind::Literal { literal } => Some(DataKey::Literal {
                literal: literal.clone(),
            }),
            DataKind::Quote { quote } => Some(DataKey::Quote {
                quote: quote.clone(),
            }),
            DataKind::Boolean(boolean) => Some(DataKey::Boolean(*boolean)),
            DataKind::Unit => Some(DataKey::Unit),
            DataKind::Unary { op, input } => Some(DataKey::Unary {
                op: *op,
                input: input.addr(),
            }),
            DataKind::Binary { op, lhs, rhs } => Some(DataKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            }),
            DataKind::Load { mem, addr } => Some(DataKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            }),
            DataKind::BranchPhi {
                merge,
                when_true,
                when_false,
            } => Some(DataKey::BranchPhi {
                merge: merge.addr(),
                when_true: when_true.addr(),
                when_false: when_false.addr(),
            }),
            DataKind::LoopPhi {
                loop_head,
                entry,
                backedge,
            } => Some(DataKey::LoopPhi {
                loop_head: loop_head.addr(),
                entry: entry.addr(),
                backedge: backedge.addr(),
            }),
            DataKind::Unknown => None,
        }
    }
}

impl<'src> CtrlNode<'src> {
    pub fn set_backedge(&mut self, backedge: CtrlID<'src>) {
        match &mut self.kind {
            CtrlKind::LoopHead {
                backedge: placeholder_backedge,
                ..
            } => *placeholder_backedge = backedge,
            _ => panic!("tried to set backedge on non-loop-head node"),
        }
    }

    pub fn set_condition(&mut self, condition: DataID<'src>) {
        match &mut self.kind {
            CtrlKind::LoopHead {
                condition: placeholder_condition,
                ..
            } => *placeholder_condition = condition,
            _ => panic!("attempted to set loop condition on non-loop-head memory node"),
        }
    }

    pub fn set_entry(&mut self, entry: CtrlID<'src>) {
        match &mut self.kind {
            CtrlKind::PlaceHolder { ctrl } => *ctrl = entry,
            CtrlKind::LoopHead { ctrl, .. } => *ctrl = entry,
            _ => panic!("attempted to set step clause prev on non-step-clause memory node"),
        }
    }
}
