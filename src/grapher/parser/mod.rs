use std::collections::HashMap;

use crate::{
    error::Span,
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{MemID, ValueID, ValueKind},
        parser::overwrites::{Branches, Overwrites},
    },
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
    },
};

mod overwrites;
mod parsing;
mod symbols;

pub use symbols::{Scopes, Symbol, SymbolDump};

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
    overwrites: Branches<'src>,
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
            overwrites: Branches::new(),
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

    pub fn get_name(&mut self) -> IdentToken<'src> {
        let tok = self.advance();
        IdentToken {
            src: tok.src,
            span: tok.span,
        }
    }

    pub fn expect_name(&mut self) -> GraphResult<'src, IdentToken<'src>> {
        if self.peek().kind == TokenKind::Name {
            let name = self.advance();
            Ok(IdentToken {
                src: name.src,
                span: name.span,
            })
        } else {
            Err(GraphError::ExpectedItem { found: self.peek() })
        }
    }

    pub fn open_scope(&mut self) {
        self.overwrites.open_scope();
        self.symbols.open_scope();
    }

    pub fn close_scope(&mut self) {
        self.overwrites.close_scope();
        self.symbols.close_scope();
    }

    pub fn declare_variable(
        &mut self,
        name: &'src str,
        type_: ValueID<'src>,
        value: ValueID<'src>,
        mutable: bool,
    ) {
        self.symbols.add_symbol(
            name,
            Symbol {
                mutable,
                type_,
                assignment: value,
            },
        )
    }

    /*pub fn declare_const(
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
        }*/

    pub fn write_variable(&mut self, assignment: Span, name: &'src str, value: ValueID<'src>) {
        if let Some(symbol) = self
            .symbols
            .get_mutable_in_this_branch(name, self.overwrites.scopes_within_branch())
        {
            symbol.assignment = value;
        } else {
            self.overwrites.add(assignment, name, value);
        }
    }

    pub fn read_variable(&mut self, name: IdentToken<'src>) -> GraphResult<'src, ValueID<'src>> {
        match self.symbols.use_symbol(&mut self.graph, name.src) {
            Ok(symbol) if symbol.assignment.kind == ValueKind::UnInitialized => {
                Err(GraphError::TriedToReadUnitialized { name })
            }
            Ok(symbol) => Ok(symbol.assignment.clone()),
            Err(node) => Ok(node),
        }
    }

    fn merge_overwrites(
        &mut self,
        condition: ValueID<'src>,
        overwrites_when_true: Overwrites<'src>,
        mut overwrites_when_false: Overwrites<'src>,
    ) -> GraphResult<'src, ()> {
        for (name, (when_true, assignments)) in overwrites_when_true {
            let span = *assignments.last();
            let symbol = self.symbols.get_symbol_to_assign(span, name)?;

            symbol.assignment = if let Some((when_false, _)) = overwrites_when_false.remove(name) {
                self.graph.add_phi(condition.clone(), when_true, when_false)
            } else {
                self.graph
                    .add_phi(condition.clone(), when_true, symbol.assignment.clone())
            };
        }

        for (name, (when_false, assignments)) in overwrites_when_false {
            let span = *assignments.last();
            let symbol = self.symbols.get_symbol_to_assign(span, name)?;

            symbol.assignment =
                self.graph
                    .add_phi(condition.clone(), symbol.assignment.clone(), when_false);
        }

        Ok(())
    }
}
