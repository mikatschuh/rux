use std::collections::HashMap;

use bumpalo::Bump;

use crate::{
    grapher::{GraphError, GraphResult, parser::GraphBuilder},
    literals::Literal,
    tokenizing::{TokenStream, binary_op::BinaryOp, token::Token, unary_op::UnaryOp},
    types::PrimitiveType,
    utilities::{NoDealloc, Rc},
};

pub type NodeID<'src> = Rc<Node<'src>, NoDealloc>;
pub type MemNodeID<'src> = Rc<MemNode<'src>, NoDealloc>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Node<'src> {
    pub kind: NodeKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct MemNode<'src> {
    pub kind: MemNodeKind<'src>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum MemNodeKind<'src> {
    ControlFlowStart,
    LoopHead {
        condition: NodeID<'src>,
        entry: MemNodeID<'src>,
        backedge: MemNodeID<'src>,
    },
    StepClause {
        prev: MemNodeID<'src>,
    },
    PlaceHolder {
        prev: MemNodeID<'src>,
    },
    Merge {
        condition: NodeID<'src>,
        when_true: MemNodeID<'src>,
        when_false: MemNodeID<'src>,
    },

    Store {
        prev: MemNodeID<'src>,
        addr: NodeID<'src>,
        val: NodeID<'src>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum NodeKind<'src> {
    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    PrimitiveType {
        ty: PrimitiveType,
    },

    Unary {
        op: UnaryOp,
        input: NodeID<'src>,
    },
    Binary {
        op: BinaryOp,
        lhs: NodeID<'src>,
        rhs: NodeID<'src>,
    },
    Load {
        mem: MemNodeID<'src>,
        addr: NodeID<'src>,
    },

    UnInitialized,

    Phi {
        condition: NodeID<'src>,
        when_true: NodeID<'src>,
        when_false: NodeID<'src>,
    },

    UnknownIdent {
        name: &'src str,
    },
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
        ty: PrimitiveType,
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
    UnknownIdent {
        name: &'src str,
    },
}

#[derive(Debug, Clone)]
pub struct Symbol<'src> {
    pub ty: NodeID<'src>,
    pub assignment: NodeID<'src>,
}

#[derive(Debug)]
pub struct Graph<'src> {
    arena: Bump,
    pub symbols: HashMap<&'src str, Symbol<'src>>,
    node_cache: HashMap<NodeKey<'src>, NodeID<'src>>,
    pub latest_mem: MemNodeID<'src>,
}

impl<'src> Graph<'src> {
    pub fn node_ptr_id(node: &NodeID<'src>) -> usize {
        std::ptr::from_ref(&**node) as usize
    }
    pub fn mem_ptr_id(mem: &MemNodeID<'src>) -> usize {
        std::ptr::from_ref(&**mem) as usize
    }

    fn node_key(kind: &NodeKind<'src>) -> Option<NodeKey<'src>> {
        match kind {
            NodeKind::Literal { literal } => Some(NodeKey::Literal {
                literal: literal.clone(),
            }),
            NodeKind::Quote { quote } => Some(NodeKey::Quote {
                quote: quote.clone(),
            }),
            NodeKind::PrimitiveType { ty } => Some(NodeKey::PrimitiveType { ty: *ty }),
            NodeKind::Unary { op, input } => Some(NodeKey::Unary {
                op: *op,
                input: Self::node_ptr_id(input),
            }),
            NodeKind::Binary { op, lhs, rhs } => Some(NodeKey::Binary {
                op: *op,
                lhs: Self::node_ptr_id(lhs),
                rhs: Self::node_ptr_id(rhs),
            }),
            NodeKind::Load { mem, addr } => Some(NodeKey::Load {
                mem: Self::mem_ptr_id(mem),
                addr: Self::node_ptr_id(addr),
            }),
            NodeKind::UnInitialized => Some(NodeKey::UnInitialized),
            NodeKind::Phi {
                condition,
                when_true,
                when_false,
            } => Some(NodeKey::Phi {
                condition: Self::node_ptr_id(condition),
                when_true: Self::node_ptr_id(when_true),
                when_false: Self::node_ptr_id(when_false),
            }),
            NodeKind::UnknownIdent { name } => Some(NodeKey::UnknownIdent { name }),
        }
    }

    pub fn new() -> Self {
        let arena = Bump::new();
        let current_mem = Rc::<MemNode<'src>, NoDealloc>::new_in_bump(
            MemNode {
                kind: MemNodeKind::ControlFlowStart,
            },
            &arena,
        );

        Self {
            arena,
            symbols: HashMap::new(),
            node_cache: HashMap::new(),
            latest_mem: current_mem,
        }
    }

    pub fn from_stream<'tokens>(
        tokens: &'tokens mut impl TokenStream<'src>,
    ) -> GraphResult<'src, Graph<'src>> {
        GraphBuilder::new(tokens).build()
    }

    pub fn symbol(&self, name: &str) -> Option<&Symbol<'src>> {
        self.symbols.get(name)
    }

    fn push_node(&mut self, kind: NodeKind<'src>) -> NodeID<'src> {
        let key = Self::node_key(&kind);
        if let Some(ref key) = key {
            if let Some(existing) = self.node_cache.get(key) {
                return existing.clone();
            }
        }

        let node: Node<'src> = Node { kind };
        let node_id = Rc::<Node<'src>, NoDealloc>::new_in_bump(node, &self.arena);
        if let Some(key) = key {
            self.node_cache.insert(key, node_id.clone());
        }
        node_id
    }

    fn push_mem_node(&mut self, kind: MemNodeKind<'src>) -> MemNodeID<'src> {
        let node: MemNode<'src> = MemNode { kind };
        Rc::<MemNode<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    pub fn add_mem_merge(
        &mut self,
        condition: NodeID<'src>,
        a: MemNodeID<'src>,
        b: MemNodeID<'src>,
    ) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::Merge {
            condition,
            when_true: a,
            when_false: b,
        })
    }

    pub fn add_loop_head(
        &mut self,
        condition: NodeID<'src>,
        entry: MemNodeID<'src>,
    ) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::LoopHead {
            condition,
            entry,
            backedge: self.latest_mem(),
        })
    }

    pub fn add_step_clause(&mut self, entry: MemNodeID<'src>) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::StepClause { prev: entry })
    }

    pub fn add_placeholder(&mut self, entry: MemNodeID<'src>) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::PlaceHolder { prev: entry })
    }

    fn add_store(&mut self, addr: NodeID<'src>, val: NodeID<'src>) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::Store {
            prev: self.latest_mem(),
            addr,
            val,
        })
    }

    fn add_load(&mut self, addr: NodeID<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Load {
            mem: self.latest_mem(),
            addr,
        })
    }

    pub fn add_literal(&mut self, literal: Literal<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Literal { literal })
    }

    pub fn add_unary(&mut self, op: UnaryOp, input: NodeID<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Unary { op, input })
    }

    pub fn add_binary(
        &mut self,
        op: BinaryOp,
        lhs: NodeID<'src>,
        rhs: NodeID<'src>,
    ) -> NodeID<'src> {
        self.push_node(NodeKind::Binary { op, lhs, rhs })
    }

    pub fn add_phi(
        &mut self,
        condition: NodeID<'src>,
        when_true: NodeID<'src>,
        when_false: NodeID<'src>,
    ) -> NodeID<'src> {
        self.push_node(NodeKind::Phi {
            condition,
            when_true,
            when_false,
        })
    }

    pub fn add_quote(&mut self, quote: String) -> NodeID<'src> {
        self.push_node(NodeKind::Quote { quote })
    }

    pub fn add_type(&mut self, ty: PrimitiveType) -> NodeID<'src> {
        self.push_node(NodeKind::PrimitiveType { ty })
    }

    pub fn declare_variable(&mut self, name: Token<'src>, ty: NodeID<'src>) {
        let not_assigned = self.push_node(NodeKind::UnInitialized);

        self.symbols.insert(
            name.src,
            Symbol {
                ty,
                assignment: not_assigned,
            },
        );
    }

    pub fn assign_variable(
        &mut self,
        name: Token<'src>,
        value: NodeID<'src>,
    ) -> GraphResult<'src, ()> {
        let Some(existing) = self.symbols.get(name.src) else {
            return Err(GraphError::AssignmentToUnknownIdent { ident: name });
        };
        let existing_assignment = existing.assignment.clone();

        if existing_assignment.kind == NodeKind::UnInitialized {
            if let Some(symbol) = self.symbols.get_mut(name.src) {
                symbol.assignment = value;
            }
            return Ok(());
        }

        let NodeKind::Unary {
            op: UnaryOp::Ptr,
            input,
        } = &existing_assignment.kind
        else {
            return Err(GraphError::AssignmentToImmutableIdent { ident: name });
        };

        let new_mem = self.add_store(input.clone(), value);
        self.latest_mem = new_mem;
        Ok(())
    }

    pub fn read_variable(&mut self, ident: Token<'src>) -> GraphResult<'src, NodeID<'src>> {
        let Some(symbol) = self.symbols.get(ident.src) else {
            let unknown_id = self.push_node(NodeKind::UnknownIdent { name: ident.src });
            return Ok(unknown_id);
        };

        if symbol.assignment.kind == NodeKind::UnInitialized {
            Err(GraphError::IdentWithoutAssignment { ident })
        } else if let NodeKind::Unary {
            op: UnaryOp::Ptr,
            input,
        } = &symbol.assignment.kind
        {
            Ok(self.add_load(input.clone()))
        } else {
            Ok(symbol.assignment.clone())
        }
    }

    pub fn snapshot_symbols(&self) -> HashMap<&'src str, Symbol<'src>> {
        self.symbols.clone()
    }

    pub fn replace_symbols(&mut self, snapshot: HashMap<&'src str, Symbol<'src>>) {
        self.symbols = snapshot;
    }

    pub fn latest_mem(&self) -> MemNodeID<'src> {
        self.latest_mem.clone()
    }
}

impl<'src> MemNode<'src> {
    pub fn set_backedge(&mut self, backedge: MemNodeID<'src>) {
        match &mut self.kind {
            MemNodeKind::LoopHead {
                backedge: placeholder_backedge,
                ..
            } => *placeholder_backedge = backedge,
            _ => panic!("tried to set backedge on non-loop-head node"),
        }
    }

    pub fn set_condition(&mut self, condition: NodeID<'src>) {
        match &mut self.kind {
            MemNodeKind::LoopHead {
                condition: placeholder_condition,
                ..
            } => *placeholder_condition = condition,
            _ => panic!("attempted to set loop condition on non-loop-head memory node"),
        }
    }

    pub fn set_entry(&mut self, entry: MemNodeID<'src>) {
        match &mut self.kind {
            MemNodeKind::StepClause { prev, .. } => *prev = entry,
            MemNodeKind::PlaceHolder { prev } => *prev = entry,
            _ => panic!("attempted to set step clause prev on non-step-clause memory node"),
        }
    }
}
