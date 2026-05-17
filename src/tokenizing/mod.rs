use crate::{
    byte_parsing::whitespace_at_start_or_empty,
    error::{Errors, Position},
    literal_parsing::Literal,
    ref_count::Rc,
    tokenizing::{
        parse_tok::parse_token,
        quote::EmbeddingSyntax,
        token::{
            Token,
            TokenKind::{self, *},
        },
    },
    type_parsing::{IntegerType, TypeSize},
};
use std::mem::{self};

#[allow(unused)]
pub mod binary_op;
#[allow(unused)]
pub mod binding_pow;
pub mod parse_tok;
mod quote;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
#[allow(dead_code)]
pub mod token;
#[allow(unused)]
pub mod unary_op;

pub use quote::with_written_out_escape_sequences;

pub trait TokenStream {
    fn peek(&self) -> Token; // has to be free

    fn get_literal(&mut self) -> Literal;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> IntegerType;
    fn consume(&mut self);
}

pub struct Tokenizer<'errors> {
    text: &'static [u8],

    tok: Token,
    data: Option<Data>,

    state: EmbeddingSyntax,

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
    pub fn new(text: &'static str, mut errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
        let mut state = EmbeddingSyntax::default();
        let mut text = text.as_bytes();

        let (tok, data) = parse_token(
            &mut text,
            Position::beginning(),
            &mut state,
            &mut errors,
            target_ptr_size,
        );

        Self {
            text,
            tok,
            data,
            state,
            errors,
            target_ptr_size,
        }
    }
}

impl<'src> TokenStream for Tokenizer<'src> {
    fn peek(&self) -> Token {
        self.tok
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
        if self.tok.kind == Eof {
            return;
        }

        let (tok, data) = parse_token(
            &mut self.text,
            self.tok.span.end,
            &mut self.state,
            &mut self.errors,
            self.target_ptr_size,
        );
        self.tok = tok;
        self.data = data;
    }
}
