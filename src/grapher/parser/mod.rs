use std::collections::HashSet;

use crate::{
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{DataID, MergeID},
        parser::{alias::Alias, jumps::JumpTableStack, symbols::Overwrites},
    },
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
    },
};

mod alias;
mod jumps;
mod parsing;
mod symbols;

pub use symbols::{ScopedSymbolTable, Symbol, SymbolDump};

pub struct BreakJump<'src> {
    overwrites: Overwrites<'src>,
    value: DataID<'src>,
}

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    pub graph: Graph<'src>,
    pub symbols: ScopedSymbolTable<'src>,

    pub loops: Vec<usize>,
    pub continue_jumps: JumpTableStack<'src, Overwrites<'src>>,
    pub break_jumps: JumpTableStack<'src, BreakJump<'src>>,
}

#[allow(dead_code)]
impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::new(),
            symbols: ScopedSymbolTable::new(),

            loops: vec![],
            continue_jumps: JumpTableStack::new(),
            break_jumps: JumpTableStack::new(),
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
        mut branches_overwrites: Vec<Overwrites<'src>>,
    ) {
        debug_assert_eq!(merge.branches.len(), branches_overwrites.len());
        if merge.branches.len() == 1 {
            for (alias, new_value) in branches_overwrites.pop().unwrap().into_iter() {
                alias.over_write(new_value);
            }
            return;
        }

        let mut phis = HashSet::<Alias<'src>>::new();
        for (alias, _) in branches_overwrites.iter().flat_map(|o| o.iter()) {
            phis.insert(alias.clone());
        }

        for alias in phis.into_iter() {
            let variants = branches_overwrites
                .iter_mut()
                .map(|o| {
                    o.remove(&alias)
                        .unwrap_or_else(|| alias.read_current_value().clone())
                })
                .collect();

            let new_value = {
                let phi = self.graph.add_phi(merge.clone(), variants);
                self.graph.add_data_phi(phi)
            };
            alias.over_write(new_value);
        }
    }
}
