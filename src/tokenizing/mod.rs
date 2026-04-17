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
    type_parsing::{AtomicType, TypeSize},
    utilities::Rc,
};
use std::mem::{self};

pub mod binary_op;
pub mod binding_pow;
pub mod parse_tok;
mod quote;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
#[allow(dead_code)]
pub mod token;
pub mod unary_op;

pub use quote::with_written_out_escape_sequences;

pub trait TokenStream<'src> {
    fn peek(&self) -> Token<'src>; // has to be free
    fn last_pos(&self) -> Position; // has to be free

    fn get_literal(&mut self) -> Literal<'src>;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> AtomicType;
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

pub struct Tokenizer<'src> {
    text: &'src [u8],

    last_pos: Position,
    tok: Token<'src>,
    data: Option<Data<'src>>,

    state: EmbeddingSyntax,

    errors: Rc<Errors<'src>>,
    target_ptr_size: TypeSize, // necessary for type parsing
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Data<'src> {
    Lit(Literal<'src>),
    Quote(String),
    Type(AtomicType),
}

impl<'src> Tokenizer<'src> {
    pub fn new(mut text: &'src [u8], mut errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
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
            last_pos: Position::beginning(),
            tok,
            data,
            state,
            errors,
            target_ptr_size,
        }
    }
}

impl<'src> TokenStream<'src> for Tokenizer<'src> {
    fn peek(&self) -> Token<'src> {
        self.tok
    }
    fn last_pos(&self) -> Position {
        self.last_pos
    }

    fn get_literal(&mut self) -> Literal<'src> {
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

    fn get_type(&mut self) -> AtomicType {
        match mem::take(&mut self.data) {
            Some(Data::Type(ty)) => ty,
            _ => unreachable!(),
        }
    }

    fn consume(&mut self) {
        if self.tok.kind == Eof {
            return;
        }

        self.last_pos = self.tok.span.end;

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
