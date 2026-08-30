use crate::{
    byte_parsing::whitespace_at_start_or_empty,
    error::Errors,
    literal_parsing::Literal,
    ref_count::Rc,
    tokenizing::{
        parse_tok::parse_token,
        quote::QuoteEmbeddingState,
        span::{Position, Span},
        token::{Token, TokenKind},
    },
    type_parsing::{IntegerType, TypeSize},
};
use std::mem::{self};

pub mod binary_op;
pub mod binding_pow;
pub mod parse_tok;
mod quote;
pub mod span;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
pub mod token;
pub mod unary_op;

pub use quote::with_written_out_escape_sequences;

pub trait TokenStream {
    fn peek(&self) -> Option<Token>; // has to be free
    fn pos(&self) -> Span;

    fn get_literal(&mut self) -> Literal;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> IntegerType;
    fn consume(&mut self);
}

pub struct Tokenizer<'errors> {
    text: &'static [u8],

    tok: Result<Token, Position>,
    data: Option<Data>,

    quote_embedding_state: QuoteEmbeddingState,

    errors: Rc<Errors<'errors>>,
    target_ptr_size: TypeSize, // necessary for type parsing
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Data {
    Lit(Literal),
    Quote(String),
    Type(IntegerType),
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'static str, errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
        let quote_embedding_state = QuoteEmbeddingState::default();
        let text = text.as_bytes();
        let pos = Position::beginning();

        let mut tokenizer = Self {
            text,
            tok: Err(pos),
            data: None,
            quote_embedding_state,
            errors,
            target_ptr_size,
        };
        tokenizer.advance(pos);
        tokenizer
    }

    pub fn advance(&mut self, pos: Position) {
        self.data = None;
        self.tok = parse_token(
            &mut self.text,
            pos,
            &mut self.data,
            &mut self.quote_embedding_state,
            &mut self.errors,
            self.target_ptr_size,
        )
        .ok_or(pos);
    }
}

impl<'src> TokenStream for Tokenizer<'src> {
    fn peek(&self) -> Option<Token> {
        self.tok.ok()
    }

    fn pos(&self) -> Span {
        self.tok.map_or_else(|pos| pos.into(), |tok| tok.span)
    }

    fn get_literal(&mut self) -> Literal {
        match mem::take(&mut self.data) {
            Some(Data::Lit(lit)) => lit,
            _ => unreachable!(),
        }
    }

    fn get_quote(&mut self) -> String {
        match mem::take(&mut self.data) {
            Some(Data::Quote(quote)) => quote,
            _ => unreachable!(),
        }
    }

    fn get_type(&mut self) -> IntegerType {
        match mem::take(&mut self.data) {
            Some(Data::Type(ty)) => ty,
            _ => unreachable!(),
        }
    }

    fn consume(&mut self) {
        let Ok(tok) = self.tok else {
            return;
        };

        self.advance(tok.span.end);
    }
}
