use crate::{
    byte_parsing::whitespace_at_start_or_empty, literal_parsing::Error as LiteralParsingError,
    parse_tok::parse_token, quote::QuoteEmbeddingState, type_parsing::Error as TypeParsingError,
};
use std::mem::{self};

mod byte_parsing;
mod error;
mod interner;
mod literal_parsing;
mod literal_types;
mod parse_tok;
mod quote;
mod span;
mod token;
mod type_parsing;

pub use error::Error;
pub use interner::{Interner, Symbol};
pub use literal_types::{Base, Literal};
pub use span::{Position, Span};
pub use token::{Bracket, FloatPrecision, Token, TokenKind};
pub use type_parsing::{IntegerType, TypeSize};

pub trait Diagnostics {
    fn add(&mut self, span: Span, err: Error);
}

pub trait TokenStream {
    type DiagnosticsStack: Diagnostics;

    fn peek(&self) -> Option<Token>; // has to be free
    fn pos(&self) -> Span;

    fn get_literal(&mut self) -> Literal;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> IntegerType;
    fn consume(&mut self);

    fn into_parts(self) -> (Interner, Self::DiagnosticsStack);
}

pub struct Tokenizer<D: Diagnostics> {
    text: &'static [u8],

    tok: Result<Token, Position>,
    data: Option<Data>,

    quote_embedding_state: QuoteEmbeddingState,

    interner: Interner,
    errors: D,
    target_ptr_size: TypeSize, // necessary for type parsing
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Data {
    Lit(Literal),
    Quote(String),
    Type(IntegerType),
}

impl<D: Diagnostics> Tokenizer<D> {
    pub fn new(text: &'static str, errors: D, target_ptr_size: TypeSize) -> Self {
        let pos = Position::beginning();

        let mut tokenizer = Self {
            text: text.as_bytes(),
            tok: Err(pos),
            data: None,
            quote_embedding_state: QuoteEmbeddingState::default(),
            interner: Interner::new(),
            errors,
            target_ptr_size,
        };
        tokenizer.advance(pos);
        tokenizer
    }

    fn advance(&mut self, pos: Position) {
        self.data = None;
        self.tok = parse_token(
            &mut self.text,
            pos,
            &mut self.data,
            &mut self.quote_embedding_state,
            &mut self.interner,
            &mut self.errors,
            self.target_ptr_size,
        )
        .ok_or(pos);
    }
}

impl<D: Diagnostics> TokenStream for Tokenizer<D> {
    type DiagnosticsStack = D;

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

    fn into_parts(self) -> (Interner, D) {
        (self.interner, self.errors)
    }
}

pub fn with_written_out_escape_sequences(quote: &str) -> String {
    let mut output_string = String::new();
    for c in quote.bytes() {
        output_string += match c {
            0x0 => "\\0", // null byte

            0x7 => "\\a", // alert / bell
            0x8 => "\\b", // backspace
            0x9 => "\\t", // horizontal tab
            0xA => "\\n", // newline
            0xB => "\\v", // vertical tab
            0xC => "\\f", // form feed
            0xD => "\\r", // carriage return

            0x1B => "\\e", // escape

            b'\\' => "\\",  // backslash
            b'"' => "\\\"", // quote
            b'{' => "\\{",  // open brace
            _ => {
                output_string.push(c as char);
                continue;
            }
        }
    }
    output_string
}
