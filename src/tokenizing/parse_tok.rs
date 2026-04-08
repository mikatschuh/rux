use crate::{
    byte_parsing::{TokenSlice, is_unicode_payload_byte},
    error::{Error, ErrorCode, Position, Span},
    literals::{self, Error as LiteralParsingError},
    tokenizing::{
        Data,
        TokenKind::*,
        token::{Keyword, Token, TokenKind},
        whitespace_at_start_or_empty,
    },
    types::{self, Error as PrimitiveTypeParsingError},
};

pub fn is_empty_or_starts_with_space_operator_or_quote(text: &[u8]) -> bool {
    text.is_empty()
        || text[0] == b'\"'
        || whitespace_at_start_or_empty(text)
        || TokenKind::new(text[0]).is_some()
}

// pub fn parse_token(text ^[_]u8, errors -> Errors) Token
pub(super) fn parse_token<'src>(
    starting_pos: Position,
    mut text: &'src [u8],
    target_ptr_size: u128,
) -> Result<(Token<'src>, Option<Data<'src>>), Error> {
    match text[0] {
        // quotes:
        b'\"' => todo!(),

        // identifiers
        _ => {
            let mut span: Span = starting_pos.into();

            // we have to parse literals first because they can include a dot
            // they wouln't be parsed as identifiers and their dot would be identified as TokenKind::Dot
            match literals::parse_literal(text) {
                Ok((used_bytes, literal)) => {
                    span.end += used_bytes; // used_bytes equals the visible characters in that case
                    return Ok((
                        Token {
                            span,
                            src: unsafe { str::from_utf8_unchecked(&text[0..used_bytes]) },
                            kind: Literal,
                        },
                        Some(Data::Lit(literal)),
                    ));
                }
                Err((_, LiteralParsingError::NotALiteral)) => {} // anything that we can parse as something else
                Err((used_bytes, err)) => {
                    span.end += used_bytes;

                    return Err(Error::new(span, ErrorCode::LiteralParsingError(err))); // syntax errors in a otherwise correct literal
                }
            }

            if let Some(mut state) = TokenKind::new(text[0]) {
                let mut slice = TokenSlice::new(&text[0..0]);
                slice.push_byte_over(&mut text);
                span.end += 1;

                loop {
                    let next_state: Option<TokenKind>;
                    if text.is_empty() || {
                        next_state = state.add(text[0]);
                        next_state.is_none()
                    } {
                        return Ok((
                            Token {
                                span,
                                src: slice.to_str(),
                                kind: state,
                            },
                            None,
                        ));
                    }

                    let next_state = next_state.unwrap();
                    if !is_unicode_payload_byte(text[0]) {
                        span.end += 1
                    }
                    slice.push_byte_over(&mut text);

                    state = next_state;
                }
            }

            let mut slice = TokenSlice::new(&text[0..0]);

            // assumes that the next token is not a whitespace
            loop {
                if !is_unicode_payload_byte(text[0]) {
                    span.end += 1;
                }
                slice.push_byte_over(&mut text);

                if is_empty_or_starts_with_space_operator_or_quote(text) {
                    let src = slice.to_str();

                    // possibly reinterpret the identifier
                    match types::parse_type(src.as_bytes(), target_ptr_size) {
                        Ok(ty) => {
                            return Ok((
                                Token {
                                    span,
                                    src,
                                    kind: Type,
                                },
                                Some(Data::Type(ty)),
                            ));
                        }
                        Err(PrimitiveTypeParsingError::NotAType) => {
                            return Ok((
                                Token {
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
                                },
                                None,
                            ));
                        }
                        Err(err) => {
                            return Err(Error::new(span, ErrorCode::TypeParsingError(err)));
                        }
                    }
                }
            }
        }
    }
}
