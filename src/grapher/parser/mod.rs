use crate::{
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{DataID, MergeID},
        parser::symbols::Overwrites,
    },
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
    },
};

mod alias;
mod parsing;
mod symbols;

pub use symbols::{ScopedSymbolTable, Symbol, SymbolDump};

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    pub graph: Graph<'src>,
    pub symbols: ScopedSymbolTable<'src>,
}

#[allow(dead_code)]
impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::new(),
            symbols: ScopedSymbolTable::new(),
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
        type_: DataID<'src>,
        value: DataID<'src>,
        mutable: bool,
    ) {
        if mutable {
            self.symbols.add_mutable(name, Symbol { type_, value });
        } else {
            self.symbols.add_immutable(name, Symbol { type_, value })
        }
    }

    pub fn read_variable(&mut self, name: IdentToken<'src>) -> DataID<'src> {
        self.symbols.read_symbol(&mut self.graph, name.src)
    }

    fn merge_overwrites(
        &mut self,
        merge: MergeID<'src>,
        overwrites_when_true: Overwrites<'src>,
        mut overwrites_when_false: Overwrites<'src>,
    ) -> GraphResult<'src, ()> {
        for (symbol, when_true) in overwrites_when_true {
            let new_value = if let Some(when_false) = overwrites_when_false.remove(&symbol) {
                self.graph.add_phi(merge.clone(), when_true, when_false)
            } else {
                self.graph.add_phi(
                    merge.clone(),
                    when_true,
                    symbol.read_current_value().clone(),
                )
            };

            symbol.over_write(new_value)
        }

        for (symbol, when_false) in overwrites_when_false {
            let new_value = self.graph.add_phi(
                merge.clone(),
                symbol.read_current_value().clone(),
                when_false,
            );

            symbol.over_write(new_value);
        }

        Ok(())
    }
}
