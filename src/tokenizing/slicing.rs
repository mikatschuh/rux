use super::{TokenStream, token::Token};
use crate::{
    error::Position, literals::Literal, tokenizing::token::TokenKind::*, types::AtomicType,
};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct TokenBuffer<'src> {
    last_outputted_pos: Position,
    pub(super) tokens: VecDeque<Token<'src>>,
    pub(super) literals: VecDeque<Literal<'src>>,
    pub(super) quotes: VecDeque<String>,
    pub(super) types: VecDeque<AtomicType>,
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
    fn peek(&self) -> Token<'src> {
        self.tokens.front().copied().unwrap_or(Token {
            span: self.last_outputted_pos.into(),
            src: "",
            kind: Eof,
        })
    }
    fn get_literal(&mut self) -> Literal<'src> {
        unsafe { self.literals.pop_front().unwrap_unchecked() }
    }
    fn get_quote(&mut self) -> String {
        unsafe { self.quotes.pop_front().unwrap_unchecked() }
    }
    fn get_type(&mut self) -> AtomicType {
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
