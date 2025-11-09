use super::{token::Token, TokenStream};
use crate::{error::Position, tokenizing::token::TokenKind::*};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenBuffer<'src> {
    pub last_outputted_pos: Position,
    pub tokens: VecDeque<Token<'src>>,
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
    fn consume(&mut self) {
        if let Some(Token { span, .. }) = self.tokens.pop_front() {
            self.last_outputted_pos = span.end
        }
    }
}
