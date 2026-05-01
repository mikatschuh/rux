use std::collections::HashMap;

use crate::{
    grapher::{
        Graph, GraphError, GraphResult, IdentToken,
        graph::{CtrlID, DataID, MergeID, PhiID},
        parser::jumps::{BranchIdx, JumpTableStack, Jumps},
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

pub struct GraphBuilder<'tokens, 'src, T: TokenStream<'src>> {
    tokens: &'tokens mut T,
    pub graph: Graph<'src>,

    symbols: ScopedSymbolTable<'src>,

    labels: HashMap<&'src str, BranchIdx>,
    jumps: JumpTableStack<'src>,
}

#[allow(dead_code)]
impl<'tokens, 'src, T: TokenStream<'src>> GraphBuilder<'tokens, 'src, T> {
    pub fn new(tokens: &'tokens mut T) -> Self {
        Self {
            tokens,
            graph: Graph::new(),

            symbols: ScopedSymbolTable::new(),

            labels: HashMap::new(),
            jumps: JumpTableStack::new(),
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

    fn advance(&mut self) -> Token<'src> {
        let tok = self.tokens.peek();
        self.tokens.consume();
        tok
    }

    fn peek(&self) -> Token<'src> {
        self.tokens.peek()
    }

    fn get_name(&mut self) -> IdentToken<'src> {
        let tok = self.advance();
        IdentToken {
            src: tok.src,
            span: tok.span,
        }
    }

    fn expect_name(&mut self) -> GraphResult<'src, IdentToken<'src>> {
        if self.peek().kind == TokenKind::Ident {
            let name = self.advance();
            Ok(IdentToken {
                src: name.src,
                span: name.span,
            })
        } else {
            Err(GraphError::ExpectedIdent { found: self.peek() })
        }
    }

    fn declare_variable(
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

    fn read_variable(&mut self, name: IdentToken<'src>) -> DataID<'src> {
        self.symbols.read_symbol(&mut self.graph, name.src)
    }

    fn set_up_loop_merge(&mut self, entry: CtrlID<'src>) -> (MergeID<'src>, Vec<PhiID<'src>>) {
        // ctrl node structure setup
        let loop_head = self.graph.add_merge(vec![entry]);

        let mut loop_phis = vec![];
        self.symbols.for_every_mutable(|_, data| {
            let phi = self.graph.add_phi(loop_head.clone(), vec![data.clone()]);
            *data = self.graph.add_phi_no_dedup(phi.clone());
            loop_phis.push(phi);
        }); // replace every variable with a phi and keep track of the phi's

        self.graph.add_merge_to_ctrl(loop_head.clone()); // with this the body has it as a control flow dependency
        (loop_head, loop_phis)
    }

    fn close_loop(
        &mut self,
        loop_backedge: Option<DataID<'src>>,
        mut loop_head: MergeID<'src>,
        mut loop_phis: Vec<PhiID<'src>>,
    ) -> GraphResult<'src, DataID<'src>> {
        let Jumps {
            mut continue_points,
            mut continue_states,
            mut break_points,
            mut break_states,
            mut break_values,
        } = self.jumps.close_branch();

        if !self.graph.is_unreachable() {
            let body_end = self.graph.get_ctrl()?;
            let body_end_state = self.symbols.snapshot_state(); // the state of every symbol after the hole body
            if let Some(data) = loop_backedge {
                break_points.push(body_end);
                break_values.push(data);
                break_states.push(body_end_state);
            } else {
                continue_points.push(body_end); // add the regular backedge
                continue_states.push(body_end_state); // add the regular backedge state
            }
        }

        loop_head.branches.append(&mut continue_points); // add the continuation points as backedges
        continue_states
            .into_iter()
            .flat_map(|overwrites| overwrites.into_iter().enumerate())
            .for_each(|(i, backedge)| loop_phis[i].variants.push(backedge)); // add thoses backedges to the phi nodes of mutable variables outside the loop for every jump

        if break_points.len() == 1 {
            self.graph.set_ctrl(break_points.pop().unwrap());

            let state = break_states.pop().unwrap();
            self.symbols
                .for_every_mutable(|i, value| *value = state[i].clone());

            Ok(break_values.pop().unwrap())
        } else {
            let exit_merge = self.graph.add_merge(break_points); // merge them
            self.graph.add_merge_to_ctrl(exit_merge.clone()); // make it the control flow of the code after that

            self.merge_states(exit_merge.clone(), break_states);

            let phi = self.graph.add_phi(exit_merge, break_values);
            Ok(self.graph.add_data_phi(phi))
        }
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

    fn go_back_to_state(&mut self, state: Vec<DataID<'src>>) {
        let mut state = state.into_iter();
        self.symbols
            .for_every_mutable(|_, m| *m = state.next().unwrap());
    }
}
