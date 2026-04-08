use crate::{
    error::{ErrorCode, Errors, Position, Span},
    literals::{self, Error as LiteralError, Literal},
    tokenizing::{
        slicing::TokenBuffer,
        token::{
            Keyword, Token,
            TokenKind::{self, *},
        },
    },
    types::{self, Error as PrimitiveTypeParsingError, PrimitiveType},
    utilities::Rc,
};
use std::{
    mem::{self, MaybeUninit},
    slice,
};

pub mod binary_op;
pub mod binding_pow;
pub mod slicing;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
#[allow(dead_code)]
pub mod token;
pub mod unary_op;

pub trait TokenStream<'src> {
    fn current_pos(&self) -> Position;

    fn peek(&mut self) -> Token<'src>;
    fn get_literal(&mut self) -> Literal<'src>;
    fn get_quote(&mut self) -> String;
    fn get_type(&mut self) -> PrimitiveType;
    fn consume(&mut self);

    fn consume_if(&mut self, mut predicate: impl FnMut(Token) -> bool) -> TokenBuffer<'src> {
        let mut buffer = TokenBuffer::new(self.current_pos());
        loop {
            let tok = self.peek();
            if !predicate(tok) {
                break;
            }

            match tok.kind {
                Literal => buffer.literals.push_back(self.get_literal()),
                Quote => buffer.quotes.push_back(self.get_quote()),
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
    pos: Position,

    already_processed: bool,
    token: MaybeUninit<Token<'src>>, // cached token
    data: Data<'src>,

    // get only used when already_processed == false:
    next_text: &'src [u8],
    next_pos: Position,

    errors: Rc<Errors<'src>>,
    target_ptr_size: u128,
}

#[derive(Default)]
enum Data<'src> {
    #[default]
    None,
    Lit(Literal<'src>),
    Quote(String),
    Type(PrimitiveType),
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
        Self {
            text: text.as_bytes(),
            pos: Position::beginning(),

            already_processed: false,
            token: MaybeUninit::uninit(),
            data: Data::None,

            next_text: text.as_bytes(),
            next_pos: Position::beginning(),

            errors,
            target_ptr_size,
        }
    }
}

impl<'src> TokenStream<'src> for Tokenizer<'src> {
    fn current_pos(&self) -> Position {
        self.pos
    }

    fn peek(&mut self) -> Token<'src> {
        if self.already_processed {
            return unsafe { self.token.assume_init_read() };
        }

        if self.is_empty_after_spaces_consumed() {
            return self.cache_tok(Token {
                span: self.next_pos.into(),
                src: "",
                kind: Eof,
            });
        }

        match self.next_text[0] {
            // quotes:
            b'\"' => self.parse_quote(),

            // identifiers
            _ => {
                let ptr = self.next_text.as_ptr();
                let mut len: usize;
                let mut span: Span = self.next_pos.into();

                macro_rules! consume_bytes {
                    ($used_bytes:expr) => {{
                        len = $used_bytes;
                        self.next_text = &self.next_text[$used_bytes..];
                        span.end += $used_bytes;
                        self.next_pos += $used_bytes;
                    }};
                }

                match literals::parse_literal(self.next_text) {
                    Ok((used_bytes, literal)) => {
                        self.data = Data::Lit(literal);

                        consume_bytes!(used_bytes);

                        return self.cache_tok(Token {
                            span,
                            src: unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                            },
                            kind: Literal,
                        });
                    }
                    Err((used_bytes, LiteralError::NoDigitsAtBeginning)) => {
                        consume_bytes!(used_bytes);
                    }

                    Err((used_bytes, err)) => {
                        consume_bytes!(used_bytes);

                        self.errors
                            .push(self.pos.into(), ErrorCode::LiteralParsingError(err));
                    }
                }

                if let Some(mut state) = TokenKind::new(self.next_text[0]) {
                    len = 1;
                    self.next_text = &self.next_text[1..];
                    span.end += 1;

                    loop {
                        if self.next_text.is_empty() {
                            self.next_pos = span.end;
                            return self.cache_tok(Token {
                                span,
                                src: unsafe {
                                    str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                                },
                                kind: state,
                            });
                        }

                        if let Some(next_state) = state.add(self.next_text[0]) {
                            len += 1;
                            if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                                span.end += 1
                            }
                            self.next_text = &self.next_text[1..];

                            state = next_state;
                        } else {
                            self.next_pos = span.end;
                            return self.cache_tok(Token {
                                span,
                                src: unsafe {
                                    str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                                },
                                kind: state,
                            });
                        }
                    }
                }

                loop {
                    if whitespace_at_start_or_empty(self.next_text)
                        || TokenKind::new(self.next_text[0]).is_some()
                    {
                        self.next_pos = span.end;
                        let src =
                            unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) };

                        match types::parse_type(src.as_bytes(), self.target_ptr_size) {
                            Ok(ty) => {
                                self.data = Data::Type(ty);

                                return self.cache_tok(Token {
                                    span,
                                    src,
                                    kind: Type,
                                });
                            }
                            Err(PrimitiveTypeParsingError::NotAType) => {
                                return self.cache_tok(Token {
                                    span,
                                    src,
                                    kind: match src {
                                        _ if src.trim_start_matches('_').is_empty() => {
                                            TokenKind::Underscore
                                        }
                                        _ => Keyword::from_str(src)
                                            .map(TokenKind::Keyword)
                                            .unwrap_or(TokenKind::Ident),
                                    },
                                });
                            }
                            Err(err) => todo!(),
                        }
                    }

                    len += 1;

                    if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                        span.end += 1;
                    }
                    self.next_text = &self.next_text[1..];
                }
            }
        }
    }

    fn get_literal(&mut self) -> Literal<'src> {
        match mem::take(&mut self.data) {
            Data::Lit(lit) => lit,
            _ => unreachable!(),
        }
    }

    fn get_quote(&mut self) -> String {
        match mem::take(&mut self.data) {
            Data::Quote(quote) => quote,
            _ => unreachable!(),
        }
    }

    fn get_type(&mut self) -> PrimitiveType {
        match mem::take(&mut self.data) {
            Data::Type(ty) => ty,
            _ => unreachable!(),
        }
    }

    fn consume(&mut self) {
        self.text = self.next_text;
        self.pos = self.next_pos;
        self.data = Data::None;
        self.already_processed = false;
    }
}

impl<'src> Tokenizer<'src> {
    fn cache_tok(&mut self, tok: Token<'src>) -> Token<'src> {
        self.token = MaybeUninit::new(tok);
        self.already_processed = true;

        tok
    }

    #[inline]
    fn is_empty_after_spaces_consumed(&mut self) -> bool {
        'outer: loop {
            if self.next_text.is_empty() {
                return true;
            }

            match self.next_text[0] {
                b'\n' => {
                    self.next_text = &self.next_text[1..];
                    self.next_pos.next_line();
                    continue; // skip incrementation of collum
                }
                0x09..=0x0D | 0x20 => self.next_text = &self.next_text[1..],

                0xC2 => {
                    if self.next_text.len() < 2 {
                        self.errors
                            .push(self.next_pos.into(), ErrorCode::InvalidUTF8);
                        return true;
                    }
                    match self.next_text[1] {
                        0x85 | 0xA0 => {
                            self.next_text = &self.next_text[2..];
                        }
                        _ => return false,
                    }
                }
                0xE1 => {
                    if self.next_text.len() < 3 {
                        self.errors
                            .push(self.next_pos.into(), ErrorCode::InvalidUTF8);
                        return true;
                    }
                    if self.next_text[1] == 0x9A && self.next_text[2] == 0x80 {
                        self.next_text = &self.next_text[2..];
                    } else {
                        return false;
                    }
                }
                0xE2 => {
                    if self.next_text.len() < 3 {
                        self.errors
                            .push(self.next_pos.into(), ErrorCode::InvalidUTF8);
                        return true;
                    }
                    if (self.next_text[1] == 0x80
                        && matches!(self.next_text[2], 0x80..=0x8A | 0xA8 | 0xA9 | 0xAF))
                        || (self.next_text[1] == 0x81 && self.next_text[2] == 0x9F)
                    {
                        self.next_text = &self.next_text[2..];
                    } else {
                        return false;
                    }
                }
                0xE3 => {
                    if self.next_text.len() < 3 {
                        self.errors
                            .push(self.next_pos.into(), ErrorCode::InvalidUTF8);
                        return true;
                    }
                    if self.next_text[1] == 0x80 && self.next_text[2] == 0x80 {
                        self.next_text = &self.next_text[2..];
                    } else {
                        return false;
                    }
                }
                b'/' => {
                    if self.next_text.len() < 2 || self.next_text[1] != b'/' {
                        return false;
                    }
                    self.next_text = &self.next_text[2..];
                    self.next_pos += 2;

                    loop {
                        if self.next_text.is_empty() {
                            return true;
                        }
                        if self.next_text[0] == b'\n' {
                            self.next_text = &self.next_text[1..];
                            self.next_pos.next_line();

                            continue 'outer;
                        }
                        if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                            self.next_pos += 1;
                        }
                        self.next_text = &self.next_text[1..];
                    }
                }
                _ => return false,
            }
            self.next_pos += 1;
        }
    }

    fn parse_quote(&mut self) -> Token<'src> {
        let mut quote = String::new();
        let quote_ptr = unsafe { quote.as_mut_vec() };

        let ptr = self.next_text.as_ptr();
        let mut len = 1; // beginning quote

        let mut span: Span = self.next_pos.into();
        span.end += 1; // add the quote

        self.next_text = &self.next_text[1..];

        loop {
            if self.next_text.is_empty() {
                self.errors.push(span, ErrorCode::NoClosingQuotes);

                self.next_pos = span.end;
                self.data = Data::Quote(quote);
                return self.cache_tok(Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) },
                    kind: Quote,
                });
            }

            if self.next_text[0] == b'\\' {
                self.next_text = &self.next_text[1..];
                span.end += 1;
                len += 1;

                let c = match self.next_text[0] {
                    b'0' => 0,
                    b'n' => b'\n',
                    b't' => b'\t',

                    _ => todo!("add error message"),
                };
                quote_ptr.push(c);

                self.next_text = &self.next_text[1..];
                span.end += 1;
                len += 1;

                continue;
            } else if self.next_text[0] == b'\"' {
                self.next_text = &self.next_text[1..];
                len += 1;
                span.end += 1; // add the closing quotes

                self.next_pos = span.end;
                self.data = Data::Quote(quote);
                return self.cache_tok(Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) },
                    kind: Quote,
                });
            } else if self.next_text[0] == b'\n' {
                span.end.next_line();
            } else if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                span.end += 1;
            }
            quote_ptr.push(self.next_text[0]);

            self.next_text = &self.next_text[1..];
            len += 1;
        }
    }
}

pub fn whitespace_at_start_or_empty(slice: &[u8]) -> bool {
    if slice.is_empty() {
        return true;
    }

    match slice[0] {
        0x09..=0x0D | 0x20 => true,

        0xC2 => {
            if slice.len() < 2 {
                return true;
            }
            matches!(slice[1], 0x85 | 0xA0)
        }
        0xE1 => {
            if slice.len() < 3 {
                return true;
            }
            slice[1] == 0x9A && slice[2] == 0x80
        }
        0xE2 => {
            if slice.len() < 3 {
                return true;
            }
            (slice[1] == 0x80 && matches!(slice[2], 0x80..=0x8A | 0xA8 | 0xA9 | 0xAF))
                || (slice[1] == 0x81 && slice[2] == 0x9F)
        }
        0xE3 => {
            if slice.len() < 3 {
                return true;
            }
            slice[1] == 0x80 && slice[2] == 0x80
        }
        _ => false,
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
