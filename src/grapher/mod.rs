use bumpalo::Bump;

use crate::{
    grapher::parser::{GraphBuilder, Parser},
    tokenizing::{
        binary_op::BinaryOp, num::Literal, token::Token, ty::Type, unary_op::UnaryOp, TokenStream,
    },
    utilities::{NoDealloc, Rc},
};
use std::{collections::HashMap, fmt};

mod parser;
#[cfg(test)]
mod test;

pub type GraphResult<'src, T> = Result<T, GraphError<'src>>;

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
        entry: MemNodeID<'src>,
        backedge: Option<MemNodeID<'src>>,
    },
    Merge {
        condition: NodeID<'src>,
        a: MemNodeID<'src>,
        b: MemNodeID<'src>,
    },

    Store {
        prev: MemNodeID<'src>,
        addr: NodeID<'src>,
        val: NodeID<'src>,
    },
    Load {
        prev: MemNodeID<'src>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum NodeKind<'src> {
    Load {
        load: MemNodeID<'src>,
        addr: NodeID<'src>,
    },

    Literal {
        literal: Literal<'src>,
    },
    Quote {
        quote: String,
    },
    PrimitiveType {
        ty: Type,
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
        ty: Type,
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
    symbols: HashMap<&'src str, Symbol<'src>>,
    node_cache: HashMap<NodeKey<'src>, NodeID<'src>>,
    current_mem: MemNodeID<'src>,
}

impl<'src> Graph<'src> {
    fn node_ptr_id(node: &NodeID<'src>) -> usize {
        std::ptr::from_ref(&**node) as usize
    }

    fn node_key(kind: &NodeKind<'src>) -> Option<NodeKey<'src>> {
        match kind {
            NodeKind::Load { .. } => None, // effectful, never interned
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

    fn new() -> Self {
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
            current_mem,
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

    fn current_mem_id(&mut self) -> MemNodeID<'src> {
        self.current_mem.clone()
    }

    fn add_mem_merge(
        &mut self,
        condition: NodeID<'src>,
        a: MemNodeID<'src>,
        b: MemNodeID<'src>,
    ) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::Merge { condition, a, b })
    }

    fn add_loop_head(&mut self, entry: MemNodeID<'src>) -> MemNodeID<'src> {
        self.push_mem_node(MemNodeKind::LoopHead {
            entry,
            backedge: None,
        })
    }

    fn set_loop_backedge(&mut self, mut loop_head: MemNodeID<'src>, backedge: MemNodeID<'src>) {
        match &mut loop_head.kind {
            MemNodeKind::LoopHead {
                backedge: loop_backedge,
                ..
            } => {
                *loop_backedge = Some(backedge);
            }
            _ => panic!("attempted to set loop backedge on non-loop-head memory node"),
        }
    }

    fn add_store(&mut self, addr: NodeID<'src>, val: NodeID<'src>) -> MemNodeID<'src> {
        let prev = self.current_mem_id();
        self.push_mem_node(MemNodeKind::Store { prev, addr, val })
    }

    fn add_load(&mut self, addr: NodeID<'src>) -> NodeID<'src> {
        let prev = self.current_mem_id();
        let mem_node = self.push_mem_node(MemNodeKind::Load { prev });
        let node = Node {
            kind: NodeKind::Load {
                load: mem_node,
                addr,
            },
        };
        Rc::<Node<'src>, NoDealloc>::new_in_bump(node, &self.arena)
    }

    fn add_literal(&mut self, literal: Literal<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Literal { literal })
    }

    fn add_unary(&mut self, op: UnaryOp, input: NodeID<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Unary { op, input })
    }

    fn add_binary(&mut self, op: BinaryOp, lhs: NodeID<'src>, rhs: NodeID<'src>) -> NodeID<'src> {
        self.push_node(NodeKind::Binary { op, lhs, rhs })
    }

    fn add_phi(
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

    fn add_quote(&mut self, quote: String) -> NodeID<'src> {
        self.push_node(NodeKind::Quote { quote })
    }

    fn add_type(&mut self, ty: Type) -> NodeID<'src> {
        self.push_node(NodeKind::PrimitiveType { ty })
    }

    fn declare_variable(&mut self, name: Token<'src>, ty: NodeID<'src>) {
        let not_assigned = self.push_node(NodeKind::UnInitialized);

        self.symbols.insert(
            name.src,
            Symbol {
                ty,
                assignment: not_assigned,
            },
        );
    }

    fn assign_variable(&mut self, name: Token<'src>, value: NodeID<'src>) -> GraphResult<'src, ()> {
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
        self.current_mem = new_mem;
        Ok(())
    }

    fn read_variable(&mut self, ident: Token<'src>) -> GraphResult<'src, NodeID<'src>> {
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

    fn snapshot_symbols(&self) -> HashMap<&'src str, Symbol<'src>> {
        self.symbols.clone()
    }

    fn replace_symbols(&mut self, snapshot: HashMap<&'src str, Symbol<'src>>) {
        self.symbols = snapshot;
    }

    fn snapshot_mem(&self) -> MemNodeID<'src> {
        self.current_mem.clone()
    }

    fn replace_mem(&mut self, snapshot: MemNodeID<'src>) {
        self.current_mem = snapshot;
    }
}

#[derive(Debug)]
pub enum GraphError<'src> {
    UnexpectedToken {
        expected: &'static str,
        found: Token<'src>,
    },
    AssignmentToUnknownIdent {
        ident: Token<'src>,
    },
    AssignmentToImmutableIdent {
        ident: Token<'src>,
    },
    IdentWithoutAssignment {
        ident: Token<'src>,
    },
    InvalidLiteral {
        token: Token<'src>,
    },
    ExpectedExpression {
        found: Token<'src>,
    },
    MismatchedBracket {
        opener: Token<'src>,
        closer: Token<'src>,
    },
}

impl fmt::Display for GraphError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use GraphError::*;
        match self {
            UnexpectedToken { expected, found } => write!(
                f,
                "expected {}, found '{}' at {:?}",
                expected, found.src, found.span
            ),
            AssignmentToUnknownIdent { ident } => write!(
                f,
                "assignment to unknown identifier '{}' at {:?}",
                ident.src, ident.span
            ),
            AssignmentToImmutableIdent { ident } => write!(
                f,
                "assignment to immutable identifier '{}' at {:?}",
                ident.src, ident.span
            ),
            IdentWithoutAssignment { ident } => {
                write!(
                    f,
                    "identifier '{}' read without being ever assigned at {:?}",
                    ident.src, ident.span
                )
            }
            InvalidLiteral { token } => {
                write!(f, "invalid literal '{}' at {:?}", token.src, token.span)
            }
            ExpectedExpression { found } => write!(f, "expected expression near {:?}", found.span),
            MismatchedBracket { opener, closer } => write!(
                f,
                "mismatched brackets: opened at {:?}, closed with '{}' at {:?}",
                opener.span, closer.src, closer.span
            ),
        }
    }
}

impl std::error::Error for GraphError<'_> {}
