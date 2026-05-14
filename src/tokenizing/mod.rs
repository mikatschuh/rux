use crate::{
    byte_parsing::whitespace_at_start_or_empty,
    error::{Errors, Position},
    literal_parsing::Literal,
    tokenizing::{
        parse_tok::parse_token,
        quote::EmbeddingSyntax,
        token::{
            Token,
            TokenKind::{self, *},
        },
    },
    type_parsing::{IntegerType, TypeSize},
    utilities::Rc,
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

    /*
    fn buffer_if(&mut self, mut predicate: impl FnMut(Token) -> bool) -> TokenBuffer<'src> {
        let mut buffer = TokenBuffer::new(self.peek().span.start);
        loop {
            let tok = self.peek();
            if !predicate(tok) {
                break;
            }

            match tok.kind {
                Literal => buffer.literals.push_back(self.get_literal()),
                Quote { .. } => buffer.quotes.push_back(self.get_quote()),
                Type => buffer.types.push_back(self.get_type()),
                _ => {}
            }

            buffer.tokens.push_back(tok);
            self.consume();
        }
        buffer
    }*/
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
    pub fn new(
        mut text: &'static [u8],
        mut errors: Rc<Errors<'src>>,
        target_ptr_size: u128,
    ) -> Self {
        let mut state = EmbeddingSyntax::default();

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
