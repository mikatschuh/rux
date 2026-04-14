use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    literals::Literal,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    types::AtomicType,
    utilities::{NoDealloc, Rc},
};

pub type ValID<'src> = Rc<ValNode<'src>, NoDealloc>;
pub type MemID<'src> = Rc<MemNode<'src>, NoDealloc>;
pub type BranchID<'src> = Rc<Branch<'src>, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ValNode<'src> {
    pub kind: ValKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct MemNode<'src> {
    pub kind: MemKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch<'src> {
    ctrl: MemKind<'src>,
    condition: ValID<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MemKind<'src> {
    Start,
    TrueBranch {
        branch: BranchID<'src>,
    },
    FalseBranch {
        branch: BranchID<'src>,
    },
    LoopHead {
        ctrl: MemID<'src>,
        condition: ValID<'src>,
        backedge: MemID<'src>,
    },
    StepClause {
        ctrl: MemID<'src>,
    },
    PlaceHolder {
        ctrl: MemID<'src>,
    },
    Merge {
        branch: BranchID<'src>,
    },

    Store {
        mem: MemID<'src>,

        addr: ValID<'src>,
        val: ValID<'src>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ValKind<'src> {
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    PrimitiveType {
        ty: AtomicType,
    },

    Unary {
        op: UnaryOp,
        input: ValID<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: ValID<'src>,
        rhs: ValID<'src>,
    },
    Load {
        mem: MemID<'src>,
        addr: ValID<'src>,
    },

    UnInitialized,

    Phi {
        condition: ValID<'src>,
        when_true: ValID<'src>,
        when_false: ValID<'src>,
    },

    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum NodeKey<'src> {
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    PrimitiveType {
        ty: AtomicType,
    },
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
    UnInitialized,
    Phi {
        condition: usize,
        when_true: usize,
        when_false: usize,
    },
}

#[derive(Debug)]
pub struct Graph<'src> {
    arena: Bump,
    node_cache: HashMap<NodeKey<'src>, ValID<'src>>,
    pub current_mem: MemID<'src>,
}
impl<'src> Graph<'src> {
    pub fn new() -> Self {
        let arena = Bump::new();

        Self {
            node_cache: HashMap::new(),
            current_mem: Rc::<MemNode<'src>, NoDealloc>::new_in_bump(
                MemNode {
                    kind: MemKind::Start,
                },
                &arena,
            ),
            arena,
        }
    }

    fn push_node(&mut self, kind: ValKind<'src>) -> ValID<'src> {
        let key = kind.node_key();
        if let Some(ref key) = key {
            if let Some(existing) = self.node_cache.get(key) {
                return existing.clone();
            }
        }

        let node: ValNode<'src> = ValNode { kind };
        let node_id = Rc::<ValNode<'src>, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.node_cache.insert(key, node_id.clone());
        }
        node_id
    }

    fn push_node_no_dedup(&mut self, kind: ValKind<'src>) -> ValID<'src> {
        Rc::<ValNode<'src>, NoDealloc>::new_in_bump(ValNode { kind }, &self.arena)
    }

    fn push_mem_node(&mut self, kind: MemKind<'src>) -> MemID<'src> {
        let node: MemNode<'src> = MemNode { kind };
        Rc::<MemNode<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    pub fn add_mem_merge(&mut self, branch: BranchID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::Merge { branch })
    }

    pub fn add_loop_head(&mut self, condition: ValID<'src>, ctrl: MemID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::LoopHead {
            condition,
            ctrl,
            backedge: self.latest_mem(),
        })
    }

    pub fn add_step_clause(&mut self, ctrl: MemID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::StepClause { ctrl })
    }

    pub fn add_placeholder(&mut self, ctrl: MemID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::PlaceHolder { ctrl })
    }

    pub fn add_store(&mut self, addr: ValID<'src>, val: ValID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::Store {
            mem: self.latest_mem(),
            addr,
            val,
        })
    }

    pub fn add_load(&mut self, addr: ValID<'src>) -> ValID<'src> {
        self.push_node(ValKind::Load {
            mem: self.latest_mem(),
            addr,
        })
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> ValID<'src> {
        self.push_node(ValKind::Literal { literal })
    }

    pub fn add_unary(&mut self, op: UnaryOp, input: ValID<'src>) -> ValID<'src> {
        self.push_node(ValKind::Unary { op, input })
    }

    pub fn add_binary(&mut self, op: BinaryOp, lhs: ValID<'src>, rhs: ValID<'src>) -> ValID<'src> {
        self.push_node(ValKind::Binary { op, lhs, rhs })
    }

    pub fn add_phi(
        &mut self,
        condition: ValID<'src>,
        when_true: ValID<'src>,
        when_false: ValID<'src>,
    ) -> ValID<'src> {
        self.push_node(ValKind::Phi {
            condition,
            when_true,
            when_false,
        })
    }

    pub fn add_quote(&mut self, quote: String) -> ValID<'src> {
        self.push_node(ValKind::Quote { quote })
    }

    pub fn add_type(&mut self, type_: AtomicType) -> ValID<'src> {
        self.push_node(ValKind::PrimitiveType { ty: type_ })
    }

    pub fn add_unitialized(&mut self) -> ValID<'src> {
        self.push_node(ValKind::UnInitialized)
    }

    pub fn add_unknown(&mut self) -> ValID<'src> {
        self.push_node_no_dedup(ValKind::Unknown)
    }

    pub fn latest_mem(&self) -> MemID<'src> {
        self.current_mem.clone()
    }
}

impl<'src> ValKind<'src> {
    fn node_key(&self) -> Option<NodeKey<'src>> {
        match self {
            ValKind::Literal { literal } => Some(NodeKey::Literal {
                literal: literal.clone(),
            }),
            ValKind::Quote { quote } => Some(NodeKey::Quote {
                quote: quote.clone(),
            }),
            ValKind::PrimitiveType { ty } => Some(NodeKey::PrimitiveType { ty: *ty }),
            ValKind::Unary { op, input } => Some(NodeKey::Unary {
                op: *op,
                input: input.addr(),
            }),
            ValKind::Binary { op, lhs, rhs } => Some(NodeKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            }),
            ValKind::Load { mem, addr } => Some(NodeKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            }),
            ValKind::UnInitialized => Some(NodeKey::UnInitialized),
            ValKind::Phi {
                condition,
                when_true,
                when_false,
            } => Some(NodeKey::Phi {
                condition: condition.addr(),
                when_true: when_true.addr(),
                when_false: when_false.addr(),
            }),
            ValKind::Unknown => None,
        }
    }
}

impl<'src> MemNode<'src> {
    pub fn set_backedge(&mut self, backedge: MemID<'src>) {
        match &mut self.kind {
            MemKind::LoopHead {
                backedge: placeholder_backedge,
                ..
            } => *placeholder_backedge = backedge,
            _ => panic!("tried to set backedge on non-loop-head node"),
        }
    }

    pub fn set_condition(&mut self, condition: ValID<'src>) {
        match &mut self.kind {
            MemKind::LoopHead {
                condition: placeholder_condition,
                ..
            } => *placeholder_condition = condition,
            _ => panic!("attempted to set loop condition on non-loop-head memory node"),
        }
    }

    pub fn set_entry(&mut self, entry: MemID<'src>) {
        match &mut self.kind {
            MemKind::StepClause { ctrl, .. } => *ctrl = entry,
            MemKind::PlaceHolder { ctrl } => *ctrl = entry,
            _ => panic!("attempted to set step clause prev on non-step-clause memory node"),
        }
    }
}
