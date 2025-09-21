use std::{collections::VecDeque, iter::FusedIterator, vec::IntoIter};

use crate::parser::tokenizing::{num::Literal, token::Token, TokenStream};

pub struct TokenSlice<'src> {
    pub numbers: &'src mut VecDeque<Literal>,
    tokens: &'src mut VecDeque<Token<'src>>,
    tokens_len: usize,
}

impl<'src> TokenSlice<'src> {
    #[inline]
    fn pop_front(&mut self) -> Option<Token<'src>> {
        if self.tokens.len() == 0 {
            return None;
        }
        let token = self.tokens.pop_front();
        self.tokens_len -= 1;
        token
    }
}

impl<'src> Iterator for TokenSlice<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        self.pop_front()
    }
}
impl<'src> FusedIterator for TokenSlice<'src> {}

impl<'src> TokenStream<'src> for TokenSlice<'src> {
    fn peek(&mut self) -> Option<&Token<'src>> {
        self.tokens.front()
    }

    fn next_if(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> Option<Token<'src>> {
        if let Some(tok) = self.tokens.front() {
            if predicate(tok) {
                return self.pop_front();
            }
        }
        None
    }

    fn next_is(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> bool {
        if let Some(tok) = self.tokens.front() {
            predicate(tok)
        } else {
            false
        }
    }

    fn get_literal(&mut self) -> Literal {
        self.numbers
            .pop_front()
            .expect("This shouldnt be called without the literal token")
    }

    /// Method for tokensing a token. If the tokens is full (or this is called twice in a row)
    /// a panic is invocated.
    fn buffer(&mut self, token: Token<'src>) {
        self.tokens.push_front(token);
        self.tokens_len += 1;
    }

    fn consume_while(&mut self, mut predicate: impl FnMut(&Token) -> bool) -> IntoIter<Token> {
        let mut tokens = Vec::new();
        while self.peek().is_some_and(&mut predicate) {
            tokens.push(unsafe { self.next().unwrap_unchecked() });
        }
        tokens.into_iter()
    }
}
