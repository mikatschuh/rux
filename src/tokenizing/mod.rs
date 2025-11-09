pub mod num;
pub mod slicing;
#[cfg(test)]
#[allow(dead_code)]
pub mod test;
#[allow(dead_code)]
pub mod token;

use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::{keyword::Keyword, typing::TypeParser},
    tokenizing::token::{
        Token,
        TokenKind::{self, *},
    },
    utilities::Rc,
};
use std::{mem::MaybeUninit, slice, vec::IntoIter};

pub trait TokenStream<'src> {
    fn current_pos(&self) -> Position;

    fn peek(&mut self) -> Token<'src>;
    fn consume(&mut self);

    /// Consumes a token if it matches a predicate. If a token got consumed true is outputted.
    fn match_and_consume(&mut self, mut predicate: impl FnMut(Token) -> bool) -> bool {
        let tok = self.peek();
        if predicate(tok) {
            self.consume();
            true
        } else {
            false
        }
    }

    fn consume_while(&mut self, mut predicate: impl FnMut(Token) -> bool) -> IntoIter<Token<'src>> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.peek();
            if !predicate(tok) {
                break;
            }
            self.consume();
            tokens.push(tok);
        }
        tokens.into_iter()
    }
}

pub struct Tokenizer<'src> {
    text: &'src [u8], // valid UTF-8
    pos: Position,

    already_processed: bool,
    token: MaybeUninit<Token<'src>>, // cached token

    // get only used when already_processed == false:
    next_text: &'src [u8],
    next_pos: Position,

    errors: Rc<Errors<'src>>,
    type_parser: TypeParser,
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, errors: Rc<Errors<'src>>, type_parser: TypeParser) -> Self {
        Self {
            text: text.as_bytes(),
            pos: Position::beginning(),

            already_processed: false,
            token: MaybeUninit::uninit(),
            next_text: text.as_bytes(),
            next_pos: Position::beginning(),

            errors,
            type_parser,
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
                kind: EOF,
            });
        }

        match self.next_text[0] {
            // quotes:
            b'\"' => {
                let ptr = self.next_text.as_ptr();
                let mut len = 1;
                let mut span: Span = self.next_pos.into();
                span.end += 1; // add the quote

                loop {
                    self.next_text = &self.next_text[1..];
                    len += 1;

                    if self.next_text.is_empty() {
                        self.errors.push(span, ErrorCode::NoClosingQuotes);

                        self.next_pos = span.end;
                        return self.cache_tok(Token {
                            span,
                            src: unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                            },
                            kind: Quote,
                        });
                    }

                    if self.next_text[0] & 0b1100_0000 == 0b1000_0000 {
                        continue;
                    }

                    if self.next_text[0] == b'\n' {
                        span.end.next_line();
                        continue;
                    }
                    span.end += 1;

                    if self.next_text[0] == b'\"' {
                        self.next_text = &self.next_text[1..]; // remove the closing quotes

                        self.next_pos = span.end;
                        return self.cache_tok(Token {
                            span,
                            src: unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                            },
                            kind: Quote,
                        });
                    }
                }
            }

            // identifiers
            _ => {
                let ptr = self.next_text.as_ptr();
                let mut len = 1;
                let mut span: Span = self.next_pos.into();
                span.end += 1; // add the first character

                if let Some(mut state) = TokenKind::new(self.next_text[0]) {
                    loop {
                        self.next_text = &self.next_text[1..];

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
                            state = next_state;

                            len += 1;
                            if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                                span.end += 1
                            }
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
                    self.next_text = &self.next_text[1..];

                    if self.whitespace_at_pos_or_empty()
                        || TokenKind::new(self.next_text[0]).is_some()
                    {
                        self.next_pos = span.end;
                        let src =
                            unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) };
                        return self.cache_tok(Token {
                            span,
                            src: unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(ptr, len))
                            },
                            kind: match src {
                                "." => TokenKind::Dot,
                                _ if src.trim_start_matches('_').is_empty() => {
                                    TokenKind::Placeholder
                                }
                                _ => Keyword::from_str(src)
                                    .map(TokenKind::Keyword)
                                    .unwrap_or(TokenKind::Ident),
                            },
                        });
                    }

                    len += 1;

                    if self.next_text[0] & 0b1100_0000 != 0b1000_0000 {
                        span.end += 1;
                    }
                }
            }
        }
    }
    fn consume(&mut self) {
        self.text = self.next_text;
        self.pos = self.next_pos;
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
    fn whitespace_at_pos_or_empty(&self) -> bool {
        if self.next_text.is_empty() {
            return true;
        }

        match self.next_text[0] {
            0x09..=0x0D | 0x20 => return true,

            0xC2 => {
                if self.next_text.len() < 2 {
                    return true;
                }
                match self.next_text[1] {
                    0x85 | 0xA0 => true,
                    _ => false,
                }
            }
            0xE1 => {
                if self.next_text.len() < 3 {
                    return true;
                }
                if self.next_text[1] == 0x9A && self.next_text[2] == 0x80 {
                    true
                } else {
                    false
                }
            }
            0xE2 => {
                if self.next_text.len() < 3 {
                    return true;
                }
                if (self.next_text[1] == 0x80
                    && matches!(self.next_text[2], 0x80..=0x8A | 0xA8 | 0xA9 | 0xAF))
                    || (self.next_text[1] == 0x81 && self.next_text[2] == 0x9F)
                {
                    true
                } else {
                    false
                }
            }
            0xE3 => {
                if self.next_text.len() < 3 {
                    return true;
                }
                if self.next_text[1] == 0x80 && self.next_text[2] == 0x80 {
                    true
                } else {
                    false
                }
            }
            _ => return false,
        }
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSequenceConfusion {
    pos: Span,
    sequence: String,
}

pub fn resolve_escape_sequences(quote: &str) -> (String, Vec<EscapeSequenceConfusion>) {
    #[derive(PartialEq)]
    enum ParsingEscapeSequence {
        False,
        True,
        Whitespace,
    }
    let mut output_string = String::new();
    let mut escape_sequence = ParsingEscapeSequence::False;
    let mut confusions: Vec<EscapeSequenceConfusion> = vec![];
    let mut relative_position = Position::at(1, 0);
    for c in quote.chars().skip(1) {
        use ParsingEscapeSequence::*;
        if let True = escape_sequence {
            match c {
                'n' => output_string.push('\n'),
                't' => output_string.push('\t'),
                'b' => output_string.push('\u{0008}'),
                'f' => output_string.push('\u{000C}'),
                '\\' => output_string.push('\\'),
                '"' => output_string.push('"'),
                '\n' => {
                    escape_sequence = Whitespace;
                    output_string.push('\n');
                    continue;
                }
                _ => {
                    output_string += "\\";
                    output_string.push(c);
                    confusions.push(EscapeSequenceConfusion {
                        pos: relative_position - 1 - relative_position,
                        sequence: format!("\\{c}"),
                    })
                }
            }
            escape_sequence = False; // setting the escape_sequence to false if it was true previously
        } else {
            if escape_sequence == Whitespace {
                if c == '\t' || c == ' ' {
                    continue;
                } else {
                    escape_sequence = False;
                }
            }
            match c {
                '\\' => escape_sequence = True,
                '\n' => {
                    output_string.push('\n');
                    relative_position.next_line();
                }
                _ => {
                    output_string.push(c);
                    relative_position += 1
                }
            }
        }
    }
    output_string.pop();
    (output_string, confusions)
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
