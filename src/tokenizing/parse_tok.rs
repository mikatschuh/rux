use crate::{
    byte_parsing::{TokenSlice, is_unicode_payload_byte},
    error::{Errors, Position, Span},
    literals,
    tokenizing::{
        Data,
        TokenKind::*,
        token::{Keyword, Token, TokenKind},
        whitespace_at_start_or_empty,
    },
    types,
};

pub fn starts_with_none_identifier_char(text: &[u8]) -> bool {
    text.is_empty()
        || text[0] == b'\"'
        || whitespace_at_start_or_empty(text)
        || TokenKind::new(text[0]).is_some()
}

pub fn push_over_until_none_identifier_char<'src>(
    text: &mut &'src [u8],
    span: &mut Span,
) -> TokenSlice<'src> {
    let mut slice = TokenSlice::new(&text[0..0]);
    loop {
        if starts_with_none_identifier_char(text) {
            break;
        }
        if !is_unicode_payload_byte(text[0]) {
            span.end += 1
        }

        slice.push_byte_over(text);
    }

    slice
}

pub(super) fn parse_token<'src>(
    text: &mut &'src [u8],
    starting_pos: Position,
    errors: &mut Errors,

    target_ptr_size: u128,
) -> (Token<'src>, Option<Data<'src>>) {
    let mut span: Span = starting_pos.into();

    // we have to parse literals first because they can include a dot
    // they wouln't be parsed as identifiers and their dot would be identified as TokenKind::Dot
    let text_before = *text;
    match literals::parse_literal(text, &mut span, errors) {
        Some(literal) => {
            return (
                Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(&text_before[0..text_before.len() - text.len()]) },
                    kind: Literal,
                },
                Some(Data::Lit(literal)),
            );
        }
        None => {
            *text = text_before;
        } // anything that we can parse as something else
    }

    if let Some(mut state) = TokenKind::new(text[0]) {
        let mut slice = TokenSlice::new(&text[0..0]);
        slice.push_byte_over(text);
        span.end += 1;

        loop {
            let next_state: Option<TokenKind>;
            if text.is_empty() || {
                next_state = state.add(text[0]);
                next_state.is_none()
            } {
                return (
                    Token {
                        span,
                        src: slice.to_str(),
                        kind: state,
                    },
                    None,
                );
            }

            let next_state = next_state.unwrap();
            if !is_unicode_payload_byte(text[0]) {
                span.end += 1
            }
            slice.push_byte_over(text);

            state = next_state;
        }
    }

    // assumes that the next token is not a whitespace
    let slice = push_over_until_none_identifier_char(text, &mut span);
    let src = slice.to_str();

    // possibly reinterpret the identifier
    match types::parse_type(src.as_bytes(), span, errors, target_ptr_size) {
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
                    .unwrap_or(TokenKind::Ident),
            },
        },
        None,
    )
}
