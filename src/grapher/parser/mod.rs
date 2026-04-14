use std::collections::HashMap;

use crate::{
    error::{Position, Span},
    grapher::{
        Graph, GraphError, GraphResult,
        graph::{MemID, ValueID, ValueKind},
        scope::{Scopes, Symbol},
    },
    literals::Literal,
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
        unary_op::UnaryOp,
    },
    types::AtomicType,
};

mod parsing;

#[derive(Clone)]
pub struct LoopContext<'src> {
    pub loop_head: MemID<'src>,
    pub break_mem: Option<MemID<'src>>,
    pub continue_mem: Option<MemID<'src>>,
}

#[derive(Clone)]
pub struct ParserState<'src> {
    pub symbols: HashMap<&'src str, Symbol<'src>>,
    pub mem: MemID<'src>,
    pub loops: Vec<LoopContext<'src>>,
    pub reachable: bool,
    pub last_jump: Option<Token<'src>>,
}

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    pub graph: Graph<'src>,
    symbols: Scopes<'src>,
    // pub loops: Vec<LoopContext<'src>>,
    // pub reachable: bool,
    // pub last_jump: Option<Token<'src>>,
}

#[allow(dead_code)]
impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::new(),
            symbols: Scopes::new(),
            // loops: Vec::new(),
            // reachable: true,
            // last_jump: None,
        }
    }

    pub fn build(mut self) -> GraphResult<'src, Graph<'src>> {
        self.parse_file()?;
        Ok(self.graph)
    }

    pub fn debug_build(mut self) -> GraphResult<'src, (Graph<'src>, Scopes<'src>)> {
        self.parse_file()?;
        Ok((self.graph, self.symbols))
    }

    pub fn advance(&mut self) -> Token<'src> {
        let tok = self.tokens.peek();
        self.tokens.consume();
        tok
    }

    pub fn peek(&self) -> Token<'src> {
        self.tokens.peek()
    }

    pub fn expect_name(&mut self) -> GraphResult<'src, &'src str> {
        if self.peek().kind == TokenKind::Name {
            Ok(self.advance().src)
        } else {
            Err(GraphError::ExpectedItem { found: self.peek() })
        }
    }

    pub fn last_pos(&self) -> Position {
        self.tokens.last_pos()
    }

    pub fn get_literal(&mut self) -> Literal<'src> {
        self.tokens.get_literal()
    }

    pub fn get_quote(&mut self) -> String {
        self.tokens.get_quote()
    }

    pub fn get_type(&mut self) -> AtomicType {
        self.tokens.get_type()
    }

    pub fn declare_variable(
        &mut self,
        decl: Span,
        name: &'src str,
        type_: ValueID<'src>,
        value: Option<ValueID<'src>>,
    ) -> GraphResult<'src, ()> {
        let assignment = value.unwrap_or_else(|| self.graph.add_unitialized());

        self.symbols
            .add_variable(decl, name, Symbol { type_, assignment })
    }

    pub fn declare_mutable(
        &mut self,
        decl: Span,
        name: &'src str,

        type_: ValueID<'src>,
        value: Option<ValueID<'src>>,
    ) -> GraphResult<'src, ()> {
        let assignment = value.unwrap_or_else(|| self.graph.add_unitialized());

        self.symbols
            .add_mutable(decl, name, Symbol { type_, assignment })
    }

    pub fn declare_const(
        &mut self,
        decl: Span,
        name: &'src str,
        type_: ValueID<'src>,
        value: ValueID<'src>,
    ) -> GraphResult<'src, ()> {
        self.symbols.add_constant(
            decl,
            name,
            Symbol {
                type_,
                assignment: value,
            },
        )
    }

    /*
    pub fn assign_variable(
        &mut self,
        name: Token<'src>,
        value: ValID<'src>,
    ) -> GraphResult<'src, ()> {
        let Some(existing) = self.symbols.mutable(name.src) else {
            return Err(GraphError::AssignmentToUnknownVar { ident: name });
        };
        let existing_assignment = existing.assignment.clone();

        if existing_assignment.kind == ValKind::UnInitialized {
            if let Some(symbol) = self.symbols.get_mut(name.src) {
                symbol.assignment = value;
            }
            return Ok(());
        }

        let ValKind::Unary {
            op: UnaryOp::Ptr,
            input,
        } = &existing_assignment.kind
        else {
            return Err(GraphError::AssignmentToImmutableIdent { ident: name });
        };

        let new_mem = self.add_store(input.clone(), value);
        self.latest_mem = new_mem;
        Ok(())
    }*/

    pub fn read_variable(&mut self, name: Token<'src>) -> GraphResult<'src, ValueID<'src>> {
        match self.symbols.register_symbol(&mut self.graph, name.src) {
            Ok(symbol) => {
                if symbol.assignment.kind == ValueKind::UnInitialized {
                    Err(GraphError::TriedToReadUnitialized { ident: name })
                } else if let ValueKind::Unary {
                    op: UnaryOp::Ptr,
                    input,
                } = &symbol.assignment.kind
                {
                    Ok(self.graph.add_load(input.clone()))
                } else {
                    Ok(symbol.assignment.clone())
                }
            }
            Err(node) => Ok(node),
        }
    }

    /*
    pub fn snapshot_symbols(&self) -> HashMap<&'src str, Symbol<'src>> {
        self.scope.symbols.clone()
    }

    pub fn replace_symbols(&mut self, snapshot: HashMap<&'src str, Symbol<'src>>) {
        self.scope.symbols = snapshot;
    }*/
}
