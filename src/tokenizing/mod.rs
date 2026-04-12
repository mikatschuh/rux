use crate::{
    byte_parsing::whitespace_at_start_or_empty,
    error::{Errors, Position, Span},
    literals::Literal,
    tokenizing::{
        embedding::EmbeddingSyntax,
        parse_tok::parse_token,
        slicing::TokenBuffer,
        token::{
            Token,
            TokenKind::{self, *},
        },
    },
    types::{AtomicType, TypeSize},
    utilities::Rc,
};
use std::mem::{self};

pub mod binary_op;
pub mod binding_pow;
mod embedding;
pub mod parse_tok;
#[allow(dead_code)]
pub mod slicing;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
#[allow(dead_code)]
pub mod token;
pub mod unary_op;

pub trait TokenStream<'src> {
    fn peek(&mut self) -> Token<'src>; // has to be free
    fn get_literal(&mut self) -> Literal<'src>;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> AtomicType;
    fn consume(&mut self);

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
    }
}

pub struct Tokenizer<'src> {
    text: &'src [u8], // valid UTF-8

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
    pub fn new(text: &'src str, mut errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
        let mut text = text.as_bytes();
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

impl<'src> TokenStream<'src> for Tokenizer<'src> {
    fn peek(&mut self) -> Token<'src> {
        self.tok
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSequenceConfusion {
    pos: Span,
    sequence: String,
}

pub fn with_written_out_escape_sequences(string: &str) -> String {
    let mut output_string = String::new();
    for c in string.chars() {
        match c {
            '\n' => output_string += "\\n",
            '\t' => output_string += "\\t",
            '\u{0008}' => output_string += "\\b",
            '\u{000C}' => output_string += "\\f",
            '\\' => output_string += "\\\\",
            '"' => output_string += "\\\"",
            _ => output_string.push(c),
        }
    }
    output_string
}
