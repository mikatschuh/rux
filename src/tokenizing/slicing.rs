use super::{token::Token, TokenStream};
use crate::{
    error::Position,
    tokenizing::{num::Literal, token::TokenKind::*},
};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenBuffer<'src> {
    pub last_outputted_pos: Position,
    pub tokens: VecDeque<Token<'src>>,
    pub literals: VecDeque<Literal<'src>>,
    pub quotes: VecDeque<String>,
}

impl<'src> TokenStream<'src> for TokenBuffer<'src> {
    #[inline]
    fn current_pos(&self) -> Position {
        self.last_outputted_pos
    }

    fn peek(&mut self) -> Token<'src> {
        self.tokens.front().copied().unwrap_or(Token {
            span: self.last_outputted_pos.into(),
            src: "",
            kind: EOF,
        })
    }
    fn get_literal(&mut self) -> Literal<'src> {
        unsafe { self.literals.pop_front().unwrap_unchecked() }
    }
    fn get_quote(&mut self) -> String {
        unsafe { self.quotes.pop_front().unwrap_unchecked() }
    }

    fn consume(&mut self) {
        if let Some(Token { span, .. }) = self.tokens.pop_front() {
            self.last_outputted_pos = span.end
        }
    }
}
