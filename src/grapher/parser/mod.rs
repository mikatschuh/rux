use crate::{
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{DataID, MergeID},
        parser::jumps::JumpTableStack,
    },
    tokenizing::{
        TokenStream,
        token::{Token, TokenKind},
    },
};

mod jumps;
mod parsing;
mod symbols;

pub use symbols::{ScopedSymbolTable, Symbol, SymbolDump};

pub struct BreakJump<'src> {
    state: Vec<DataID<'src>>,
    value: DataID<'src>,
}

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    pub graph: Graph<'src>,

    symbols: ScopedSymbolTable<'src>,

    loops: Vec<usize>, // the scope-idxs where loops are located
    continue_jumps: JumpTableStack<'src, Vec<DataID<'src>>>,
    break_jumps: JumpTableStack<'src, BreakJump<'src>>,
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

    fn merge_states(&mut self, merge: MergeID<'src>, states: Vec<Vec<DataID<'src>>>) {
        debug_assert_eq!(merge.branches.len(), states.len());

        if states.is_empty() {
            self.symbols
                .for_every_mutable(|_, value| *value = self.graph.add_never());
            return;
        }

        self.symbols.for_every_mutable(|i, value| {
            let mut variants = states
                .iter()
                .map(|state| state[i].clone())
                .collect::<Vec<_>>();
            let mut iter = variants.iter();
            let first = iter.next();
            *value = if iter.all(|x| Some(x) == first) {
                variants.pop().unwrap()
            } else {
                let phi = self.graph.add_phi(merge.clone(), variants);
                self.graph.add_data_phi(phi)
            };
        });
    }
}
