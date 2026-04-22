use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    literal_parsing::Literal,
    tokenizing::{binary_op::BinaryOp, unary_op::UnaryOp},
    type_parsing::AtomicType,
    utilities::{NoDealloc, Rc},
};

pub type ValueID<'src> = Rc<ValueNode<'src>, NoDealloc>;
pub type MemID<'src> = Rc<MemNode<'src>, NoDealloc>;
pub type BranchID<'src> = Rc<Branch<'src>, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ValueNode<'src> {
    pub kind: ValueKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct MemNode<'src> {
    pub kind: MemKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Branch<'src> {
    ctrl: MemKind<'src>,
    condition: ValueID<'src>,
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
        condition: ValueID<'src>,
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

        addr: ValueID<'src>,
        val: ValueID<'src>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ValueKind<'src> {
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
        input: ValueID<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: ValueID<'src>,
        rhs: ValueID<'src>,
    },
    Load {
        mem: MemID<'src>,
        addr: ValueID<'src>,
    },

    Phi {
        condition: ValueID<'src>,
        when_true: ValueID<'src>,
        when_false: ValueID<'src>,
    },

    Unknown,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum ValueKey<'src> {
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
    Phi {
        condition: usize,
        when_true: usize,
        when_false: usize,
    },
}

const UNIT: ValueNode<'static> = ValueNode {
    kind: ValueKind::Unit,
};

#[derive(Debug)]
pub struct Graph<'src> {
    arena: Bump,
    node_cache: HashMap<ValueKey<'src>, ValueID<'src>>,
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

    fn push_node(&mut self, kind: ValueKind<'src>) -> ValueID<'src> {
        let key = kind.node_key();
        if let Some(ref key) = key
            && let Some(existing) = self.node_cache.get(key)
        {
            return existing.clone();
        }

        let node: ValueNode<'src> = ValueNode { kind };
        let node_id = Rc::<ValueNode<'src>, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.node_cache.insert(key, node_id.clone());
        }
        node_id
    }

    fn push_node_no_dedup(&mut self, kind: ValueKind<'src>) -> ValueID<'src> {
        Rc::<ValueNode<'src>, NoDealloc>::new_in_bump(ValueNode { kind }, &self.arena)
    }

    fn push_mem_node(&mut self, kind: MemKind<'src>) -> MemID<'src> {
        let node: MemNode<'src> = MemNode { kind };
        Rc::<MemNode<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    pub fn add_mem_merge(&mut self, branch: BranchID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::Merge { branch })
    }

    pub fn add_loop_head(&mut self, condition: ValueID<'src>, ctrl: MemID<'src>) -> MemID<'src> {
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

    pub fn add_store(&mut self, addr: ValueID<'src>, val: ValueID<'src>) -> MemID<'src> {
        self.push_mem_node(MemKind::Store {
            mem: self.latest_mem(),
            addr,
            val,
        })
    }

    pub fn add_load(&mut self, addr: ValueID<'src>) -> ValueID<'src> {
        self.push_node(ValueKind::Load {
            mem: self.latest_mem(),
            addr,
        })
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> ValueID<'src> {
        self.push_node(ValueKind::Literal { literal })
    }

    pub fn add_unary(&mut self, op: UnaryOp, input: ValueID<'src>) -> ValueID<'src> {
        self.push_node(ValueKind::Unary { op, input })
    }

    pub fn add_binary(
        &mut self,
        op: BinaryOp,
        lhs: ValueID<'src>,
        rhs: ValueID<'src>,
    ) -> ValueID<'src> {
        self.push_node(ValueKind::Binary { op, lhs, rhs })
    }

    pub fn add_phi(
        &mut self,
        condition: ValueID<'src>,
        when_true: ValueID<'src>,
        when_false: ValueID<'src>,
    ) -> ValueID<'src> {
        self.push_node(ValueKind::Phi {
            condition,
            when_true,
            when_false,
        })
    }

    pub fn add_quote(&mut self, quote: String) -> ValueID<'src> {
        self.push_node(ValueKind::Quote { quote })
    }

    pub fn add_bool(&mut self, boolean: bool) -> ValueID<'src> {
        self.push_node(ValueKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, type_: AtomicType) -> ValueID<'src> {
        self.push_node(ValueKind::AtomicType { ty: type_ })
    }

    pub fn add_unknown(&mut self) -> ValueID<'src> {
        self.push_node_no_dedup(ValueKind::Unknown)
    }

    pub fn add_unit(&mut self) -> ValueID<'src> {
        self.push_node(ValueKind::Unit)
    }

    pub fn latest_mem(&self) -> MemID<'src> {
        self.current_mem.clone()
    }
}

impl<'src> ValueKind<'src> {
    fn node_key(&self) -> Option<ValueKey<'src>> {
        match self {
            ValueKind::AtomicType { ty } => Some(ValueKey::AtomicType { ty: *ty }),
            ValueKind::Literal { literal } => Some(ValueKey::Literal {
                literal: literal.clone(),
            }),
            ValueKind::Quote { quote } => Some(ValueKey::Quote {
                quote: quote.clone(),
            }),
            ValueKind::Boolean(boolean) => Some(ValueKey::Boolean(*boolean)),
            ValueKind::Unit => Some(ValueKey::Unit),
            ValueKind::Unary { op, input } => Some(ValueKey::Unary {
                op: *op,
                input: input.addr(),
            }),
            ValueKind::Binary { op, lhs, rhs } => Some(ValueKey::Binary {
                op: *op,
                lhs: lhs.addr(),
                rhs: rhs.addr(),
            }),
            ValueKind::Load { mem, addr } => Some(ValueKey::Load {
                mem: mem.addr(),
                addr: addr.addr(),
            }),
            ValueKind::Phi {
                condition,
                when_true,
                when_false,
            } => Some(ValueKey::Phi {
                condition: condition.addr(),
                when_true: when_true.addr(),
                when_false: when_false.addr(),
            }),
            ValueKind::Unknown => None,
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

    pub fn set_condition(&mut self, condition: ValueID<'src>) {
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
