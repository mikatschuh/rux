use crate::{
    byte_parsing::whitespace_at_start_or_empty, literal_parsing::Error as LiteralParsingError,
    parse_tok::parse_token, quote::QuoteEmbeddingState, span::Position,
    type_parsing::Error as TypeParsingError,
};

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
pub use span::Span;
pub use token::{Bracket, FloatPrecision, Quote, Token};
pub use type_parsing::{IntegerType, TypeSize};

pub trait Diagnostics {
    fn add(&mut self, span: Span, err: Error);
}

/// Tokens and interned names borrow the source, independently of the stream borrow.
pub trait TokenStream<'src>: Iterator<Item = Token<'src>> {
    type DiagnosticsStack: Diagnostics;

    fn peek(&self) -> Option<&Token<'src>>;
    fn pos(&self) -> Span;
    fn into_parts(self) -> (Interner<'src>, Self::DiagnosticsStack);

    fn try_get(&mut self, kind: &Token<'src>) -> Option<Span> {
        self.next_if(|tok| tok == kind).map(|(_, span)| span)
    }
    fn consume_while_matching(&mut self, tok: &Token<'src>) {
        while self.try_get(tok).is_some() {}
    }
    fn next_if(
        &mut self,
        predicate: impl FnOnce(&Token<'src>) -> bool,
    ) -> Option<(Token<'src>, Span)> {
        if predicate(self.peek()?) {
            let span = self.pos();
            self.next().map(|tok| (tok, span))
        } else {
            None
        }
    }
    fn advance(&mut self) -> Span {
        let span = self.pos();
        _ = self.next();
        span
    }
    fn get_literal(&mut self) -> Option<(Literal<'src>, Span)> {
        let span = self.pos();
        if let Some(Token::Literal(_)) = self.peek()
            && let Some(Token::Literal(literal)) = self.next()
        {
            Some((literal, span))
        } else {
            None
        }
    }
    fn get_quote(&mut self) -> Option<(Quote, Span)> {
        let span = self.pos();
        if let Some(Token::Quote(_)) = self.peek()
            && let Some(Token::Quote(quote)) = self.next()
        {
            Some((quote, span))
        } else {
            None
        }
    }
}

pub struct Tokenizer<'src, D: Diagnostics> {
    text: &'src [u8],
    span: Span,
    tok: Option<Token<'src>>,
    quote_embedding_state: QuoteEmbeddingState,

    interner: Interner<'src>,
    errors: D,
    target_ptr_size: TypeSize, // necessary for type parsing
}

impl<'src, D: Diagnostics> Tokenizer<'src, D> {
    pub fn new(text: &'src str, errors: D, target_ptr_size: TypeSize) -> Self {
        let pos = Position::beginning();

        let mut tokenizer = Self {
            text: text.as_bytes(),
            span: pos.into(),
            tok: None,
            quote_embedding_state: QuoteEmbeddingState::default(),
            interner: Interner::new(),
            errors,
            target_ptr_size,
        };
        tokenizer.advance();
        tokenizer
    }

    fn advance(&mut self) {
        self.span.start = self.span.end;
        self.tok = parse_token(
            &mut self.text,
            &mut self.span,
            &mut self.quote_embedding_state,
            &mut self.interner,
            &mut self.errors,
            self.target_ptr_size,
        );
    }
}

impl<'src, D: Diagnostics> Iterator for Tokenizer<'src, D> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.tok.take() {
            self.advance();
            Some(tok)
        } else {
            None
        }
    }
}

impl<'src, D: Diagnostics> TokenStream<'src> for Tokenizer<'src, D> {
    type DiagnosticsStack = D;

    fn peek(&self) -> Option<&Token<'src>> {
        self.tok.as_ref()
    }

    fn pos(&self) -> Span {
        self.span
    }

    fn into_parts(self) -> (Interner<'src>, D) {
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
