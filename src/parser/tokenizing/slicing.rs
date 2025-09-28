use std::{collections::VecDeque, iter::FusedIterator};

use crate::{
    error::Position,
    parser::tokenizing::{num::Literal, token::Token, TokenStream},
};

pub struct TokenSlice<'src, 'a> {
    pub(super) last_outputted_pos: Position,
    pub(super) numbers: &'a mut VecDeque<Literal>,
    pub(super) tokens: &'a mut VecDeque<Token<'src>>,
    pub(super) tokens_len: usize,
}

impl<'src, 'a> Iterator for TokenSlice<'src, 'a> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.len() == 0 {
            return None;
        }
        let token = self.tokens.pop_front().map(|tok| {
            self.last_outputted_pos = tok.span.end;
            tok
        });
        self.tokens_len -= 1;
        token
    }
}
impl<'src, 'a> FusedIterator for TokenSlice<'src, 'a> {}

impl<'src, 'a> TokenStream<'src> for TokenSlice<'src, 'a> {
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
        self.tokens_len += 1;
    }

    fn slice_start(
        &mut self,
        mut regular_end: impl FnMut(&Token<'src>) -> Option<bool>,
    ) -> (TokenSlice<'src, '_>, bool) {
        let mut i = 0;
        loop {
            if i == self.tokens_len {
                return (
                    TokenSlice {
                        numbers: self.numbers,
                        tokens_len: i,
                        tokens: self.tokens,
                        ..*self
                    },
                    false,
                );
            }
            let tok = self.tokens[i];
            if let Some(x) = regular_end(&tok) {
                return (
                    TokenSlice {
                        numbers: self.numbers,
                        tokens_len: i,
                        tokens: self.tokens,
                        ..*self
                    },
                    x,
                );
            }

            i += 1;
        }
    }
}
