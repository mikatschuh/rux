use crate::{
    byte_parsing::{
        TextState, TokenSlice, is_empty_after_spaces_consumed, is_unicode_payload_byte,
        whitespace_at_start_or_empty,
    },
    error::{ErrorCode, Errors, Position, Span},
    literals::Literal,
    tokenizing::{
        slicing::TokenBuffer,
        token::{
            Bracket, Token,
            TokenKind::{self, *},
        },
    },
    types::PrimitiveType,
    utilities::Rc,
};
use std::mem::{self};

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

#[derive(Clone, Debug, PartialEq, Eq)]
enum StateMachine<'src> {
    NotYetProcessed,
    AlreadyProcessed {
        tok: Token<'src>,
        data: Option<Data<'src>>,
        last: bool,
    },
}

pub struct Tokenizer<'src> {
    text: &'src [u8], // valid UTF-8
    pos: Position,
    next_pos: Position,

    state: StateMachine<'src>,
    open_braces_after_formatting_quote: Option<usize>,

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

            text,
            pos,
            next_pos: pos,

            open_braces_after_formatting_quote: None,

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
        // right now self.text can't be empty as this is an invalid state
        let (tok, data) = match self.text[0] {
            b'"' => self.parse_quote(false),
            b'}' if self.open_braces_after_formatting_quote == Some(0) => {
                self.open_braces_after_formatting_quote = None;
                self.parse_quote(true)
            }
            _ => {
                let (tok, data) = parse_tok::parse_token(
                    &mut self.text,
                    self.pos,
                    &mut self.errors,
                    self.target_ptr_size,
                );
                self.pos = tok.span.start;
                self.next_pos = tok.span.end;

                if let Some(open_braces_after_formatting_quote) =
                    &mut self.open_braces_after_formatting_quote
                {
                    if tok.kind == Open(Bracket::Curly) {
                        *open_braces_after_formatting_quote += 1;
                    } else if tok.kind == Closed(Bracket::Curly) {
                        *open_braces_after_formatting_quote -= 1;
                    }
                }

                (tok, data)
            }
        };

        self.state = generate_new_state(&mut self.text, &mut self.next_pos, &mut self.errors);
        self.state = StateMachine::AlreadyProcessed {
            tok,
            data,
            last: self.state != StateMachine::NotYetProcessed,
        };

        return tok;
    }

    fn get_literal(&mut self) -> Literal<'src> {
        match &mut self.state {
            StateMachine::NotYetProcessed => unreachable!(),
            StateMachine::AlreadyProcessed { data, .. } => match mem::take(data) {
                Some(Data::Lit(lit)) => lit,
                _ => unreachable!(),
            },
        }
    }

    fn get_quote(&mut self) -> String {
        match &mut self.state {
            StateMachine::NotYetProcessed => unreachable!(),
            StateMachine::AlreadyProcessed { data, .. } => match mem::take(data) {
                Some(Data::Quote(quote)) => quote,
                _ => unreachable!(),
            },
        }
    }

    fn get_type(&mut self) -> PrimitiveType {
        match &mut self.state {
            StateMachine::NotYetProcessed => unreachable!(),
            StateMachine::AlreadyProcessed { data, .. } => match mem::take(data) {
                Some(Data::Type(ty)) => ty,
                _ => unreachable!(),
            },
        }
    }

    fn consume(&mut self) {
        self.pos = self.next_pos;

        match &self.state {
            StateMachine::AlreadyProcessed { tok, last, .. } => {
                if tok.kind == Eof {
                    return;
                }

                if *last {
                    self.state = StateMachine::AlreadyProcessed {
                        tok: Token {
                            span: self.next_pos.into(),
                            src: "",
                            kind: Eof,
                        },
                        data: None,
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

impl<'src> Tokenizer<'src> {
    fn parse_quote(&mut self, closing_scope: bool) -> (Token<'src>, Option<Data<'src>>) {
        let (tok, quote) = parse_quote(&mut self.text, self.pos, closing_scope, &mut self.errors);
        self.pos = tok.span.start;
        self.next_pos = tok.span.end;
        self.open_braces_after_formatting_quote = if let Quote {
            opening_scope: true,
            ..
        } = tok.kind
        {
            Some(0)
        } else {
            None
        };
        (tok, Some(Data::Quote(quote)))
    }
}

fn generate_new_state<'src>(
    text: &mut &'src [u8],
    pos: &mut Position,
    errors: &mut Errors,
) -> StateMachine<'src> {
    let text_state = is_empty_after_spaces_consumed(text, pos);

    match text_state {
        TextState::SpacesConsumed => StateMachine::NotYetProcessed,
        _ => {
            let span = (*pos).into();
            if text_state == TextState::UncontinuedUTF8 {
                errors.push(span, ErrorCode::InvalidUTF8);
            }

            StateMachine::AlreadyProcessed {
                tok: Token {
                    span,
                    src: "",
                    kind: Eof,
                },
                data: None,
                last: true,
            }
        }
    }
}

fn parse_quote<'src>(
    text: &mut &'src [u8],
    pos: Position,
    closing_scope: bool,
    errors: &mut Errors,
) -> (Token<'src>, String) {
    let mut quote = String::new();
    let quote_ptr = unsafe { quote.as_mut_vec() };

    let mut slice = TokenSlice::new(&text[0..0]);
    slice.push_byte_over(text); // add the opening quote

    let mut span: Span = pos.into();
    span.end += 1; // add the opening quote

    loop {
        if text.is_empty() {
            errors.push(span, ErrorCode::NoClosingQuotes);

            return (
                Token {
                    span,
                    src: slice.to_str(),
                    kind: Quote {
                        closing_scope,
                        opening_scope: false,
                    },
                },
                quote,
            );
        }

        if text[0] == b'\\' {
            slice.push_byte_over(text);
            span.end += 1;

            let c = match *text {
                b"0" => 0,
                b"n" => b'\n',
                b"t" => b'\t',

                b"b" => 8,
                b"v" => 0xB,
                b"f" => 0xC,
                b"r" => 0xD,
                b"e" => 0x1B,

                b"\\" => b'\\',
                b"\"" => b'\"',
                b"{" => b'{',

                _ => todo!("add error message"),
            };
            quote_ptr.push(c);

            slice.push_byte_over(text);
            span.end += 1;

            continue;
        } else if text[0] == b'{' {
            slice.push_byte_over(text);
            span.end += 1; // add the closing quotes

            return (
                Token {
                    span,
                    src: slice.to_str(),
                    kind: Quote {
                        closing_scope,
                        opening_scope: true,
                    },
                },
                quote,
            );
        } else if text[0] == b'\"' {
            slice.push_byte_over(text);
            span.end += 1; // add the closing quotes

            return (
                Token {
                    span,
                    src: slice.to_str(),
                    kind: Quote {
                        closing_scope,
                        opening_scope: false,
                    },
                },
                quote,
            );
        } else if text[0] == b'\n' {
            span.end.next_line();
        } else if !is_unicode_payload_byte(text[0]) {
            span.end += 1;
        }
        quote_ptr.push(text[0]);
        slice.push_byte_over(text);
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
