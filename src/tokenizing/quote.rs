use crate::{
    byte_parsing::{TokenSlice, is_unicode_payload_byte},
    error::{ErrorCode, Errors, Position, Span},
    tokenizing::token::{Token, TokenKind},
};

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct EmbeddingSyntax {
    open_braces_after_embedding_quote: Vec<usize>,
}

impl EmbeddingSyntax {
    pub fn embedded_scope_opening(&mut self) {
        self.open_braces_after_embedding_quote.push(0)
    }

    pub fn opening_curly_brace(&mut self) {
        if let Some(open_braces) = self.open_braces_after_embedding_quote.last_mut() {
            *open_braces += 1
        }
    }

    pub fn closing_curly_brace(
        &mut self,
        text: &mut &'static [u8],
        pos: Position,
        errors: &mut Errors,
    ) -> Option<(Token, String)> {
        if let Some(open_braces_after_embedding_quote) =
            self.open_braces_after_embedding_quote.last_mut()
        {
            if *open_braces_after_embedding_quote == 0 {
                self.open_braces_after_embedding_quote.pop();

                return Some(parse_quote(text, pos, self, true, errors));
            }
            *open_braces_after_embedding_quote -= 1;
        }
        None
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

pub fn parse_quote(
    text: &mut &'static [u8],
    pos: Position,
    state: &mut EmbeddingSyntax,
    closing_scope: bool,

    errors: &mut Errors,
) -> (Token, String) {
    let mut quote = String::new();
    let quote_ptr = unsafe { quote.as_mut_vec() };

    let mut slice = TokenSlice::new(text, 0);
    slice.push_byte_over(); // add the opening quote

    let mut span: Span = pos.into();
    span.end += 1; // add the opening quote

    loop {
        if slice.no_bytes_left() {
            break;
        }

        if slice.current_byte() == b'\n' {
            span.end.next_line();
            quote_ptr.push(slice.current_byte());
            slice.push_byte_over();
            continue;
        } else if is_unicode_payload_byte(slice.current_byte()) {
            quote_ptr.push(slice.current_byte());
            slice.push_byte_over();
            continue;
        } else {
            span.end += 1;
        }

        if slice.current_byte() == b'{' {
            slice.push_byte_over();

            state.embedded_scope_opening();
            return (
                Token {
                    span,
                    src: slice.to_str(),
                    kind: TokenKind::Quote {
                        closing_scope,
                        opening_scope: true,
                    },
                },
                quote,
            );
        } else if slice.current_byte() == b'\"' {
            slice.push_byte_over();

            return (
                Token {
                    span,
                    src: slice.to_str(),
                    kind: TokenKind::Quote {
                        closing_scope,
                        opening_scope: false,
                    },
                },
                quote,
            );
        }

        if slice.current_byte() == b'\\' {
            slice.push_byte_over();

            if slice.no_bytes_left() {
                quote_ptr.push(b'\\');
                break;
            }

            let c = match slice.current_byte() {
                b'0' => 0x0, // null byte

                b'a' => 0x7, // alert / bell
                b'b' => 0x8, // backspace
                b't' => 0x9, // horizontal tab
                b'n' => 0xA, // newline
                b'v' => 0xB, // vertical tab
                b'f' => 0xC, // form feed
                b'r' => 0xD, // carriage return

                b'e' => 0x1B, // escape

                b'\\' => b'\\', // backslash
                b'"' => b'\"',  // quote
                b'{' => b'{',   // open brace

                _ => {
                    errors.push(
                        (span.end - 1) - (span.end + 1),
                        ErrorCode::UnknownEscapeSequence {
                            given: format!("\\{}", slice.current_byte() as char),
                        },
                    );

                    quote_ptr.push(b'\\');
                    continue;
                }
            };
            quote_ptr.push(c);

            slice.push_byte_over();
            span.end += 1;

            continue;
        }

        quote_ptr.push(slice.current_byte());
        slice.push_byte_over();
    }

    errors.push(span, ErrorCode::NoClosingQuotes);

    (
        Token {
            span,
            src: slice.to_str(),
            kind: TokenKind::Quote {
                closing_scope,
                opening_scope: false,
            },
        },
        quote,
    )
}
