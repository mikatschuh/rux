pub mod num;
pub mod slicing;
#[cfg(test)]
pub mod test;
#[allow(dead_code)]
pub mod token;

use std::{collections::VecDeque, iter::FusedIterator, vec::IntoIter};
use token::Token;

use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        keyword::Keyword,
        tokenizing::{num::Literal, slicing::TokenSlice, token::TokenKind},
    },
    typing::TypeParser,
    utilities::Rc,
};

pub trait TokenStream<'src>: Iterator<Item = Token<'src>> + FusedIterator {
    fn peek(&mut self) -> Option<&Token<'src>>;
    fn current_pos(&mut self) -> Position;
    fn next_is(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> bool {
        if let Some(tok) = self.peek() {
            predicate(tok)
        } else {
            false
        }
    }
    fn next_if(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> Option<Token<'src>> {
        if let Some(tok) = self.peek() {
            if predicate(tok) {
                return self.next();
            }
        }
        None
    }
    fn pop_literal(&mut self) -> Literal;
    fn buffer(&mut self, token: Token<'src>);
    fn consume_while(
        &mut self,
        mut predicate: impl FnMut(&Token) -> bool,
    ) -> IntoIter<Token<'src>> {
        let mut tokens = Vec::new();
        while self.peek().is_some_and(&mut predicate) {
            tokens.push(unsafe { self.next().unwrap_unchecked() });
        }
        tokens.into_iter()
    }
    fn slice_start(
        &mut self,
        regular_end: impl FnMut(&Token) -> Option<bool>,
    ) -> (TokenSlice<'src, '_>, bool);
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'src> {
    last_outputted_pos: Position,
    span: Span,

    state: State,

    text: &'src [u8],

    start_i: usize,
    i: usize,
    next_i: usize,

    buffer: VecDeque<Token<'src>>,
    numbers: VecDeque<Literal>,

    errors: Rc<Errors<'src>>,
    type_parser: TypeParser,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum State {
    Op(TokenKind),
    Id,
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSequenceConfusion {
    pos: Span,
    sequence: String,
}

impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }

        self.buffer.pop_front().map(|tok| {
            self.last_outputted_pos = tok.span.end;
            tok
        })
    }
}
impl<'src> FusedIterator for Tokenizer<'src> {}

impl<'src> TokenStream<'src> for Tokenizer<'src> {
    fn peek(&mut self) -> Option<&Token<'src>> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }

        self.buffer.front()
    }

    #[inline]
    fn current_pos(&mut self) -> Position {
        if let Some(Token { span, .. }) = self.peek() {
            span.start
        } else {
            self.last_outputted_pos
        }
    }

    fn pop_literal(&mut self) -> Literal {
        self.numbers
            .pop_front()
            .expect("This shouldnt be called without the literal token")
    }

    /// Method for buffering a token. If the buffer is full (or this is called twice in a row)
    /// a panic is invocated.
    fn buffer(&mut self, token: Token<'src>) {
        self.last_outputted_pos = token.span.start;
        self.buffer.push_front(token)
    }

    fn slice_start(
        &mut self,
        mut regular_end: impl FnMut(&Token<'src>) -> Option<bool>,
    ) -> (TokenSlice<'src, '_>, bool) {
        let mut i = 1;
        loop {
            if i >= self.buffer.len() {
                self.restock_tokens();
            }
            if i >= self.buffer.len() {
                return (
                    TokenSlice {
                        last_outputted_pos: self.last_outputted_pos,
                        numbers: &mut self.numbers,
                        tokens_len: i,
                        tokens: &mut self.buffer,
                    },
                    false,
                );
            }
            let tok = self.buffer[i];
            if let Some(x) = regular_end(&tok) {
                return (
                    TokenSlice {
                        last_outputted_pos: self.last_outputted_pos,
                        numbers: &mut self.numbers,
                        tokens_len: i,
                        tokens: &mut self.buffer,
                    },
                    x,
                );
            }

            i += 1;
        }
    }
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, errors: Rc<Errors<'src>>, type_parser: TypeParser) -> Self {
        Tokenizer {
            last_outputted_pos: Position::beginning(),
            span: Span::beginning(),

            state: State::Nothing,

            text: text.as_bytes(),

            start_i: 0,
            i: 0,
            next_i: 0,

            buffer: VecDeque::new(),
            numbers: VecDeque::new(),

            errors,
            type_parser,
        }
    }
}

impl<'src> Tokenizer<'src> {
    fn consume_one_char(&mut self) -> Option<char> {
        if self.text.is_empty() || self.next_i >= self.text.len() {
            return None;
        }
        self.i = self.next_i;
        if self.i != 0 {
            if self.text[self.i - 1] == b'\n' {
                self.span.end.next_line();
            } else {
                self.span.end += 1;
            }
        } // stepping all important variables

        let first = self.text[self.i];

        let (len, mut codepoint): (usize, u32) = if first & 0b1000_0000 == 0b0000_0000 {
            // 1 Byte ASCII: 0xxxxxxx
            (1, first as u32)
        } else if first & 0b1110_0000 == 0b1100_0000 {
            // 2 Byte: 110xxxxx 10xxxxxx
            (2, (first & 0x1F) as u32)
        } else if first & 0b1111_0000 == 0b1110_0000 {
            // 3 Byte: 1110xxxx 10xxxxxx 10xxxxxx
            (3, (first & 0x0F) as u32)
        } else if first & 0b1111_1000 == 0b1111_0000 {
            // 4 Byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
            (4, (first & 0x07) as u32)
        } else {
            self.errors.push(self.span, ErrorCode::InvalidUTF8); // ungültiges UTF-8
            self.text = &[];
            return None;
        };

        if self.text.len() < len {
            self.errors.push(self.span, ErrorCode::InvalidUTF8); // ungültiges UTF-8
            self.text = &[];
            return None;
        }

        // Folgebytes: 10xxxxxx
        for &b in &self.text[self.i + 1..self.i + len] {
            if b & 0b1100_0000 != 0b1000_0000 {
                self.errors.push(self.span, ErrorCode::InvalidUTF8); // zu kurz
                self.text = &[];
                return None;
            }
            codepoint = (codepoint << 6) | ((b & 0x3F) as u32);
        }

        // In char verwandeln
        let c = char::from_u32(codepoint)?;

        self.next_i += len;
        return Some(c);
    }

    fn restock_tokens(&mut self) {
        while let Some(c) = self.consume_one_char() {
            if c.is_whitespace() {
                self.submit_current(self.span - 1, self.i); // -1 to ignore the whitespace
            } else if c == '"' {
                self.submit_current(self.span - 1, self.i); // -1 to ignore the quotation mark
                self.quote(self.i);
                return;
            } else {
                if let State::Op(ref mut token) = self.state {
                    if *token == TokenKind::Slash && c == '/' {
                        self.comment();

                        self.state = State::Nothing;
                        continue;
                    } else if let Some(new_token) = token.add(c) {
                        *token = new_token;
                        continue;
                    } else {
                        self.submit_current(self.span - 1, self.i);
                    }
                }
                if let Some(new_token) = TokenKind::new(c) {
                    self.submit_current(self.span - 1, self.i); // incase the previous one was an identifier
                    self.span.start = self.span.end;
                    self.start_i = self.i;
                    self.state = State::Op(new_token)
                } else if let State::Nothing = self.state {
                    self.span.start = self.span.end;
                    self.start_i = self.i;
                    let (num_of_chars_used, literal) =
                        self.try_to_parse_literal(&self.text[self.start_i..], self.type_parser);
                    if num_of_chars_used > 0 {
                        self.i += num_of_chars_used - 1;
                        self.next_i += num_of_chars_used - 1;
                        self.span.end += num_of_chars_used - 1;
                    }
                    match literal {
                        Some(literal) => {
                            self.numbers.push_back(literal);
                            self.buffer.push_back(Token {
                                span: self.span.start - self.span.end + 1,
                                kind: TokenKind::Literal,
                                src: to_str(self.text, self.start_i, self.i + 1),
                            });
                            self.state = State::Nothing
                        }
                        None => self.state = State::Id,
                    }
                }
            }
            if self.buffer.len() >= 10 {
                return;
            }
        }
        self.submit_current(self.span, self.next_i);
    }

    fn comment(&mut self) {
        while !matches!(self.consume_one_char(), Some('\n')) {}
    }

    fn quote(&mut self, start_i: usize) {
        self.span.start = self.span.end;
        while let Some(c) = self.consume_one_char() {
            if c == '"' {
                self.buffer.push_back(Token {
                    span: self.span.start - self.span.end + 1,
                    src: to_str(self.text, start_i, self.next_i),
                    kind: TokenKind::Quote,
                });
                return;
            } // quote ends
        }
        self.errors.push(
            self.span,
            ErrorCode::NoClosingQuotes {
                quote: to_str(self.text, start_i, self.next_i),
            },
        );
        self.buffer.push_back(Token {
            span: self.span,
            src: to_str(self.text, start_i, self.next_i),
            kind: TokenKind::Quote,
        })
    }

    fn submit_current(&mut self, span: Span, end_i: usize) {
        match self.state {
            State::Op(token) => self.buffer.push_back(Token {
                span: span.start - span.end + 1,
                src: to_str(self.text, self.start_i, end_i),
                kind: token,
            }),
            State::Id => {
                let src = to_str(self.text, self.start_i, end_i);
                self.buffer.push_back(Token {
                    span: span.start - span.end + 1,
                    kind: match src {
                        "." => TokenKind::Dot,
                        _ if src.trim_start_matches('_').is_empty() => TokenKind::Placeholder,
                        _ => Keyword::from_str(src)
                            .map(TokenKind::Keyword)
                            .unwrap_or(TokenKind::Ident),
                    },
                    src,
                })
            }
            State::Nothing => return, // skip the reassignment
        }
        self.state = State::Nothing
    }
}

fn to_str(bytes: &[u8], start: usize, end: usize) -> &str {
    unsafe { str::from_utf8_unchecked(&bytes[start..end]) }
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
