use std::collections::HashMap;

use crate::{
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{MemID, ValueID},
        parser::symbols::Overwrites,
    },
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
    },
};

mod parsing;
mod symbols;

pub use symbols::{ScopedSymbolTable, Symbol, SymbolDump};

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
    pub symbols: ScopedSymbolTable<'src>,
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
            symbols: ScopedSymbolTable::new(),
            // loops: Vec::new(),
            // reachable: true,
            // last_jump: None,
        }
    }

    pub fn build(mut self) -> GraphResult<'src, Graph<'src>> {
        self.parse_file()?;
        Ok(self.graph)
    }

    pub fn debug_build(mut self) -> GraphResult<'src, (Graph<'src>, ScopedSymbolTable<'src>)> {
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

    pub fn declare_variable(
        &mut self,
        name: &'src str,
        type_: ValueID<'src>,
        value: ValueID<'src>,
        mutable: bool,
    ) {
        if mutable {
            self.symbols.add_mutable(name, Symbol { type_, value });
        } else {
            self.symbols.add_immutable(name, Symbol { type_, value })
        }
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

    pub fn read_variable(&mut self, name: IdentToken<'src>) -> ValueID<'src> {
        self.symbols.use_symbol(&mut self.graph, name.src)
    }

    fn merge_overwrites(
        &mut self,
        condition: ValueID<'src>,
        overwrites_when_true: Overwrites<'src>,
        mut overwrites_when_false: Overwrites<'src>,
    ) -> GraphResult<'src, ()> {
        unsafe {
            for (symbol, when_true) in overwrites_when_true {
                *symbol = if let Some(when_false) = overwrites_when_false.remove(&symbol) {
                    self.graph.add_phi(condition.clone(), when_true, when_false)
                } else {
                    self.graph
                        .add_phi(condition.clone(), when_true, (*symbol).clone())
                }
            }

            for (symbol, when_false) in overwrites_when_false {
                *symbol = self
                    .graph
                    .add_phi(condition.clone(), (*symbol).clone(), when_false);
            }
        }

        Ok(())
    }
}
