use crate::{
    byte_parsing::whitespace_at_start_or_empty,
    error::{ErrorCode, Errors, Position, Span},
    literals::Literal,
    tokenizing::{
        slicing::TokenBuffer,
        token::{
            Token,
            TokenKind::{self, *},
        },
    },
    types::PrimitiveType,
    utilities::Rc,
};
use std::{
    mem::{self},
    slice,
};

pub mod binary_op;
pub mod binding_pow;
pub mod parse_tok;
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

    fn buffer_if(&mut self, mut predicate: impl FnMut(Token) -> bool) -> TokenBuffer<'src> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
enum StateMachine<'src> {
    NotYetProcessed,
    AlreadyProcessed { tok: Token<'src>, last: bool },
}

pub struct Tokenizer<'src> {
    text: &'src [u8], // valid UTF-8
    pos: Position,
    next_pos: Position,

    state: StateMachine<'src>,
    data: Option<Data<'src>>,

    errors: Rc<Errors<'src>>,
    target_ptr_size: u128, // necessary for type parsing
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Data<'src> {
    Lit(Literal<'src>),
    Quote(String),
    Type(PrimitiveType),
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, mut errors: Rc<Errors<'src>>, target_ptr_size: u128) -> Self {
        let mut text = text.as_bytes();
        let mut pos = Position::beginning();

        Self {
            state: generate_new_state(&mut text, &mut pos, &mut errors),
            data: None,

            text,
            pos,
            next_pos: pos,

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
        if let StateMachine::AlreadyProcessed { tok, .. } = self.state {
            return tok;
        }

        loop {
            let tok = match parse_tok::parse_token(self.pos, self.text, self.target_ptr_size) {
                Ok((tok, data)) => {
                    self.pos = tok.span.start;
                    self.next_pos = tok.span.end;

                    self.data = data;
                    tok
                }
                Err(err) => {
                    self.pos = err.span.end;

                    self.errors.push_err(err);

                    self.state =
                        generate_new_state(&mut self.text, &mut self.pos, &mut self.errors);
                    continue;
                }
            };

            self.state = generate_new_state(&mut self.text, &mut self.next_pos, &mut self.errors);
            self.state = StateMachine::AlreadyProcessed {
                tok,
                last: self.state != StateMachine::NotYetProcessed,
            };

            return tok;
        }
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

    fn get_type(&mut self) -> PrimitiveType {
        match mem::take(&mut self.data) {
            Some(Data::Type(ty)) => ty,
            _ => unreachable!(),
        }
    }

    fn consume(&mut self) {
        self.pos = self.next_pos;
        self.data = None;

        match self.state {
            StateMachine::AlreadyProcessed { tok, last } => {
                if tok.kind == Eof {
                    return;
                }

                if last {
                    self.state = StateMachine::AlreadyProcessed {
                        tok: Token {
                            span: self.next_pos.into(),
                            src: "",
                            kind: Eof,
                        },
                        last: true,
                    };
                } else {
                    self.state = StateMachine::NotYetProcessed;
                }
            }
            _ => unreachable!("`consume` should never be called without peek being called before"),
        }
    }
}

fn generate_new_state<'src>(
    text: &mut &'src [u8],
    pos: &mut Position,
    errors: &mut Errors,
) -> StateMachine<'src> {
    let text_state = is_empty_after_spaces_consumed(text, pos);

    match text_state {
        TextState::TokenReached => StateMachine::NotYetProcessed,
        _ => {
            let span = (*pos).into();
            if text_state == TextState::UncontinuedUTF8 {
                errors.push(span, ErrorCode::InvalidUTF8);
            }

            let tok = Token {
                span,
                src: "",
                kind: Eof,
            };
            StateMachine::AlreadyProcessed { tok, last: true }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TextState {
    TokenReached,
    UncontinuedUTF8,
    Empty,
}
fn is_empty_after_spaces_consumed(text: &mut &[u8], pos: &mut Position) -> TextState {
    use TextState::*;
    'outer: loop {
        if text.is_empty() {
            return Empty;
        }

        match text[0] {
            b'\n' => {
                *text = &text[1..];
                pos.next_line();
                continue; // skip incrementation of collum
            }
            0x09..=0x0D | 0x20 => *text = &text[1..],

            0xC2 => {
                if text.len() < 2 {
                    return UncontinuedUTF8;
                }
                match text[1] {
                    0x85 | 0xA0 => {
                        *text = &text[2..];
                    }
                    _ => return TokenReached,
                }
            }
            0xE1 => {
                if text.len() < 3 {
                    return UncontinuedUTF8;
                }
                if text[1] == 0x9A && text[2] == 0x80 {
                    *text = &text[2..];
                } else {
                    return TokenReached;
                }
            }
            0xE2 => {
                if text.len() < 3 {
                    return UncontinuedUTF8;
                }
                if (text[1] == 0x80 && matches!(text[2], 0x80..=0x8A | 0xA8 | 0xA9 | 0xAF))
                    || (text[1] == 0x81 && text[2] == 0x9F)
                {
                    *text = &text[2..];
                } else {
                    return TokenReached;
                }
            }
            0xE3 => {
                if text.len() < 3 {
                    return UncontinuedUTF8;
                }
                if text[1] == 0x80 && text[2] == 0x80 {
                    *text = &text[2..];
                } else {
                    return TokenReached;
                }
            }
            b'/' if text.len() > 1 && text[1] == b'/' => {
                *text = &text[2..];
                *pos += 2;

                loop {
                    if text.is_empty() {
                        return Empty;
                    }
                    if text[0] == b'\n' {
                        *text = &text[1..];
                        pos.next_line();

                        continue 'outer;
                    }
                    if text[0] & 0b1100_0000 != 0b1000_0000 {
                        *pos += 1;
                    }
                    *text = &text[1..];
                }
            }
            _ => return TokenReached,
        }
        *pos += 1;
    }
}

impl<'src> Tokenizer<'src> {
    fn parse_quote(&mut self) -> Token<'src> {
        let mut quote = String::new();
        let quote_ptr = unsafe { quote.as_mut_vec() };

        let ptr = self.text.as_ptr();
        let mut len = 1; // beginning quote

        let mut span: Span = self.pos.into();
        span.end += 1; // add the quote

        self.text = &self.text[1..];

        loop {
            if self.text.is_empty() {
                self.errors.push(span, ErrorCode::NoClosingQuotes);

                self.pos = span.end;
                self.data = Some(Data::Quote(quote));
                return Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) },
                    kind: Quote,
                };
            }

            if self.text[0] == b'\\' {
                self.text = &self.text[1..];
                span.end += 1;
                len += 1;

                let c = match self.text[0] {
                    b'0' => 0,
                    b'n' => b'\n',
                    b't' => b'\t',

                    _ => todo!("add error message"),
                };
                quote_ptr.push(c);

                self.text = &self.text[1..];
                span.end += 1;
                len += 1;

                continue;
            } else if self.text[0] == b'\"' {
                self.text = &self.text[1..];
                len += 1;
                span.end += 1; // add the closing quotes

                self.next_pos = span.end;
                self.data = Some(Data::Quote(quote));
                return Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr, len)) },
                    kind: Quote,
                };
            } else if self.text[0] == b'\n' {
                span.end.next_line();
            } else if self.text[0] & 0b1100_0000 != 0b1000_0000 {
                span.end += 1;
            }
            quote_ptr.push(self.text[0]);

            self.text = &self.text[1..];
            len += 1;
        }
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
