use std::{collections::VecDeque, iter::FusedIterator};

use crate::{
    error::Position,
    parser::tokenizing::{num::Literal, token::Token, TokenStream},
};

#[derive(Debug)]
pub struct TokenBuffer<'src> {
    pub(super) last_outputted_pos: Position,
    pub(super) numbers: VecDeque<Literal>,
    pub(super) tokens: VecDeque<Token<'src>>,
}

impl<'src> Iterator for TokenBuffer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.len() == 0 {
            return None;
        }
        let token = self.tokens.pop_front().map(|tok| {
            self.last_outputted_pos = tok.span.end;
            tok
        });
        token
    }
}
impl<'src, 'a> FusedIterator for TokenBuffer<'src> {}

impl<'src> TokenStream<'src> for TokenBuffer<'src> {
    fn peek(&mut self) -> Option<&Token<'src>> {
        self.tokens.front()
    }

    #[inline]
    fn current_pos(&mut self) -> Position {
        if let Some(Token { span, .. }) = self.peek() {
            span.start
        } else {
            self.last_outputted_pos
        }
    }

    fn pop_literal(&mut self) -> Literal {
        self.numbers
            .pop_front()
            .expect("This shouldnt be called without the literal token")
    }

    /// Method for tokensing a token. If the tokens is full (or this is called twice in a row)
    /// a panic is invocated.
    fn buffer(&mut self, token: Token<'src>) {
        self.last_outputted_pos = token.span.start;
        self.tokens.push_front(token);
    }
}
