use crate::{
    byte_parsing::{
        TextState, TokenSlice, is_empty_after_spaces_consumed, is_unicode_payload_byte,
    },
    error::{ErrorCode, Errors, Position, Span},
    literal_parsing,
    tokenizing::{
        Data,
        TokenKind::*,
        embedding::EmbeddingSyntax,
        token::{Bracket, Keyword, Token, TokenKind},
        whitespace_at_start_or_empty,
    },
    type_parsing::{self, TypeSize},
};

pub fn starts_with_none_identifier_char(text: &[u8]) -> bool {
    text.is_empty()
        || text[0] == b'\"'
        || whitespace_at_start_or_empty(text)
        || TokenKind::new(text[0]).is_some()
}

pub fn push_over_until_none_identifier_char<'a, 'src>(
    text: &'a mut &'src [u8],
    span: &mut Span,
) -> TokenSlice<'a, 'src> {
    let mut slice = TokenSlice::new(text, 0);
    loop {
        if starts_with_none_identifier_char(slice.larger_slice()) {
            break;
        }
        if !is_unicode_payload_byte(slice.current_byte()) {
            span.end += 1
        }

        slice.push_byte_over();
    }

    slice
}

pub(super) fn parse_token<'src>(
    text: &mut &'src [u8],
    mut pos: Position,
    embedding_syntax_state: &mut EmbeddingSyntax,

    errors: &mut Errors,
    target_ptr_size: TypeSize,
) -> (Token<'src>, Option<Data<'src>>) {
    if consume_spaces(text, &mut pos, errors) {
        return (
            Token {
                span: pos.into(),
                src: "",
                kind: Eof,
            },
            None,
        );
    }

    // right now self.text can't be empty as that would be an invalid state
    if text[0] == b'}' {
        if let Some((tok, quote)) = embedding_syntax_state.closing_curly_brace(text, pos, errors) {
            return (tok, Some(Data::Quote(quote)));
        }
    }

    if text[0] == b'"' {
        let (tok, quote) = parse_quote(text, pos, embedding_syntax_state, false, errors);
        // we give it the state - it will call the embedding syntax state machine automatically
        return (tok, Some(Data::Quote(quote)));
    }

    let mut span: Span = pos.into();

    // we have to parse literals first because they can include a dot
    // they wouln't be parsed as identifiers and their dot would be identified as TokenKind::Dot
    let text_before = *text;
    match literal_parsing::parse_literal(text, &mut span, errors) {
        Some(literal) => {
            return (
                Token {
                    span,
                    src: unsafe {
                        str::from_utf8_unchecked(&text_before[0..text_before.len() - text.len()])
                    },
                    kind: Literal,
                },
                Some(Data::Lit(literal)),
            );
        }
        None => {
            *text = text_before;
        } // anything that we can parse as something else
    }

    if let Some(tok_kind) = TokenKind::new(text[0]) {
        return (
            parse_operator(text, &mut span, embedding_syntax_state, tok_kind),
            None,
        );
    }

    // assumes that the next token is not a whitespace
    let slice = push_over_until_none_identifier_char(text, &mut span);
    let src = slice.to_str();

    // possibly reinterpret the identifier
    match type_parsing::parse_type(src.as_bytes(), span, errors, target_ptr_size) {
        Some(ty) => {
            return (
                Token {
                    span,
                    src,
                    kind: Type,
                },
                Some(Data::Type(ty)),
            );
        }
        None => {}
    }
    (
        Token {
            span,
            src,
            kind: match src {
                _ if src.trim_start_matches('_').is_empty() => TokenKind::Underscore,
                _ => Keyword::from_str(src)
                    .map(TokenKind::Keyword)
                    .unwrap_or(TokenKind::Name),
            },
        },
        None,
    )
}

/// - empty => `true`
/// - spaces consumed until next token => `false`
fn consume_spaces<'src>(text: &mut &'src [u8], pos: &mut Position, errors: &mut Errors) -> bool {
    let text_state = is_empty_after_spaces_consumed(text, pos);

    match text_state {
        TextState::SpacesConsumed => false,
        _ => {
            let span = (*pos).into();
            if text_state == TextState::UncontinuedUTF8 {
                errors.push(span, ErrorCode::InvalidUTF8);
            }

            true
        }
    }
}

pub fn parse_quote<'src>(
    text: &mut &'src [u8],
    pos: Position,
    state: &mut EmbeddingSyntax,
    closing_scope: bool,

    errors: &mut Errors,
) -> (Token<'src>, String) {
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
                    kind: Quote {
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
                    kind: Quote {
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
                b'\'' => b'\'', // apostrophe
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

fn parse_operator<'src>(
    text: &mut &'src [u8],
    span: &mut Span,
    embedding_syntax_state: &mut EmbeddingSyntax,

    mut tok_kind: TokenKind,
) -> Token<'src> {
    let mut slice = TokenSlice::new(text, 0);
    slice.push_byte_over();
    span.end += 1;

    loop {
        let next_state: Option<TokenKind>;
        if slice.no_bytes_left() || {
            next_state = tok_kind.add(slice.current_byte());
            next_state.is_none()
        } {
            if tok_kind == Open(Bracket::Curly) {
                embedding_syntax_state.opening_curly_brace();
            }
            return Token {
                span: *span,
                src: slice.to_str(),
                kind: tok_kind,
            };
        }
        let next_state = next_state.unwrap();

        if !is_unicode_payload_byte(slice.current_byte()) {
            span.end += 1
        }
        slice.push_byte_over();

        tok_kind = next_state;
    }
}
