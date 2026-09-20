use crate::{
    Diagnostics, Error,
    byte_parsing::{
        TextState, TokenSlice, is_empty_after_spaces_consumed, is_unicode_payload_byte,
    },
    interner::Interner,
    literal_parsing,
    quote::{QuoteEmbeddingState, parse_quote},
    span::{Position, Span},
    token::{Bracket, Token, as_keyword},
    type_parsing::{self, TypeSize},
    whitespace_at_start_or_empty,
};

pub fn starts_with_none_identifier_char(text: &[u8]) -> bool {
    text.is_empty()
        || text[0] == b'\"'
        || whitespace_at_start_or_empty(text)
        || Token::new(text[0]).is_some()
}

pub fn push_over_until_none_identifier_char<'a>(
    text: &'a mut &'static [u8],
    span: &mut Span,
) -> TokenSlice<'a, 'static> {
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

pub(super) fn parse_token(
    text: &mut &'static [u8],
    span: &mut Span,
    embedding_syntax_state: &mut QuoteEmbeddingState,
    interner: &mut Interner,
    errors: &mut impl Diagnostics,
    target_ptr_size: TypeSize,
) -> Option<Token> {
    let empty = consumed_spaces_and_empty(text, &mut span.start, errors);
    span.end = span.start;
    if empty {
        return None;
    }

    if text[0] == b'}'
        && let Some(quote) = embedding_syntax_state.closing_brace(text, span, errors)
    {
        return Some(Token::Quote(quote));
    } else if text[0] == b'{' {
        *text = &text[1..];
        span.end += 1;
        embedding_syntax_state.open_brace();
        return Some(Token::Open(Bracket::Curly));
    }

    if text[0] == b'"' {
        let quote = parse_quote(text, span, embedding_syntax_state, false, errors);
        return Some(Token::Quote(quote));
    }

    let text_before = *text;
    let literal = literal_parsing::parse_literal(text, span);
    if let Err(Some((_, e))) = &literal {
        errors.add(*span, Error::Literal(e.clone()));
    }
    match literal {
        Ok(literal) | Err(Some((literal, _))) => {
            return Some(Token::Literal(literal));
        }
        Err(None) => {
            *text = text_before;
        } // anything that we can parse as something else
    }

    if let Some(tok_kind) = Token::new(text[0]) {
        return Some(parse_operator(text, span, tok_kind));
    }

    // assumes that the next token is not a whitespace
    let slice = push_over_until_none_identifier_char(text, span);
    let src = slice.to_str();

    // possibly reinterpret the identifier
    let integer_type = type_parsing::parse_type(src.as_bytes(), target_ptr_size);
    if let Err(Some((_, e))) = &integer_type {
        errors.add(*span, Error::Type(e.clone()));
    }
    if let Ok(integer_type) | Err(Some((integer_type, _))) = integer_type {
        return Some(Token::IntegerType(integer_type));
    }
    Some(match src {
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        _ if src.trim_start_matches('_').is_empty() => Token::Underscore,
        _ => as_keyword(src).unwrap_or(Token::Ident(interner.get(src))),
    })
}

/// - empty => `true`
/// - spaces consumed until next token => `false`
fn consumed_spaces_and_empty(
    text: &mut &[u8],
    pos: &mut Position,
    errors: &mut impl Diagnostics,
) -> bool {
    let text_state = is_empty_after_spaces_consumed(text, pos);

    if text_state == TextState::SpacesConsumed {
        false
    } else {
        let span = (*pos).into();
        if text_state == TextState::UncontinuedUTF8 {
            errors.add(span, Error::InvalidUTF8);
        }

        true
    }
}

fn parse_operator(text: &mut &'static [u8], span: &mut Span, mut tok: Token) -> Token {
    let mut slice = TokenSlice::new(text, 0);
    slice.push_byte_over();
    span.end += 1;

    loop {
        let next_state: Option<Token>;
        if slice.no_bytes_left() || {
            next_state = tok.add(slice.current_byte());
            next_state.is_none()
        } {
            return tok;
        }
        let next_state = next_state.unwrap();

        if !is_unicode_payload_byte(slice.current_byte()) {
            span.end += 1
        }
        slice.push_byte_over();

        tok = next_state;
    }
}
