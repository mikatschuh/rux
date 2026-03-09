use super::{token::Token, TokenStream};
use crate::{
    error::Position,
    tokenizing::{num::Literal, token::TokenKind::*, ty::Type},
};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenBuffer<'src> {
    last_outputted_pos: Position,
    pub(super) tokens: VecDeque<Token<'src>>,
    pub(super) literals: VecDeque<Literal<'src>>,
    pub(super) quotes: VecDeque<String>,
    pub(super) types: VecDeque<Type>,
}

impl<'src> TokenBuffer<'src> {
    pub fn new(starting_pos: Position) -> Self {
        Self {
            last_outputted_pos: starting_pos,
            tokens: VecDeque::new(),
            literals: VecDeque::new(),
            quotes: VecDeque::new(),
            types: VecDeque::new(),
        }
    }
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
    fn get_type(&mut self) -> Type {
        unsafe { self.types.pop_front().unwrap_unchecked() }
    }

    fn consume(&mut self) {
        if let Some(Token { span, .. }) = self.tokens.pop_front() {
            self.last_outputted_pos = if let Some(Token { span, .. }) = self.tokens.front() {
                span.start
            } else {
                span.end
            }
        }
    }
}
