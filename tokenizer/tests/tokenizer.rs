use std::sync::{Arc, Mutex};

use num::BigUint;

use tokenizer::{
    Base, Diagnostics, Error, Interner, Literal, Quote, Span, Token, TokenStream, Tokenizer,
};

#[derive(Clone, Default)]
struct MockDiagnostics {
    errors: Arc<Mutex<Vec<(Span, Error)>>>,
}

impl Diagnostics for MockDiagnostics {
    fn add(&mut self, span: Span, err: Error) {
        self.errors.lock().unwrap().push((span, err));
    }
}

type TokenCollection<'src> = (
    Vec<(Token<'src>, (usize, usize, usize, usize))>,
    Interner<'src>,
    MockDiagnostics,
);
fn collect_tokens(input: &str) -> TokenCollection<'_> {
    let mut tokenizer = Tokenizer::new(input, MockDiagnostics::default(), 64);
    let mut tokens = vec![];
    while tokenizer.peek().is_some() {
        let span = tokenizer.pos().get();
        tokens.push((tokenizer.next().unwrap(), span));
    }
    let (interner, errors) = tokenizer.into_parts();
    (tokens, interner, errors)
}

fn quote<'src>(content: &str, closing_scope: bool, opening_scope: bool) -> Token<'src> {
    Token::Quote(Quote {
        content: content.to_owned(),
        closing_scope,
        opening_scope,
    })
}

#[test]
fn tokenizes_basic_sequences() {
    let (tokens, _, errors) = collect_tokens("");
    assert!(tokens.is_empty());
    assert!(errors.errors.lock().unwrap().is_empty());

    let (tokens, mut interner, errors) = collect_tokens("\n a \"Hallo\n\"+");
    assert_eq!(
        tokens,
        vec![
            (Token::Ident(interner.get("a")), (2, 2, 3, 2)),
            (quote("Hallo\n", false, false), (4, 2, 2, 3)),
            (Token::Plus, (2, 3, 3, 3)),
        ]
    );
    assert!(errors.errors.lock().unwrap().is_empty());

    let (tokens, mut interner, errors) = collect_tokens("a// b + a\nb//");
    assert_eq!(
        tokens,
        vec![
            (Token::Ident(interner.get("a")), (1, 1, 2, 1)),
            (Token::Ident(interner.get("b")), (1, 2, 2, 2)),
        ]
    );
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn tokenizes_literal_sequences() {
    let errors = MockDiagnostics::default();
    let mut tokenizer = Tokenizer::new("-1.3 + 0x345", errors.clone(), 64);

    assert_eq!(tokenizer.peek(), Some(&Token::Dash));
    assert_eq!(tokenizer.pos().get(), (1, 1, 2, 1));
    // A mismatched getter leaves the current token untouched.
    assert_eq!(tokenizer.get_literal(), None);
    assert_eq!(tokenizer.next(), Some(Token::Dash));

    let decimal = Literal {
        base: Base::Decimal,
        digits: BigUint::from(13_u8),
        num_digits_after_dot: Some(1),
        exponent: None,
        suffix: "",
    };
    assert_eq!(tokenizer.peek(), Some(&Token::Literal(decimal.clone())));
    assert_eq!(tokenizer.pos().get(), (2, 1, 5, 1));
    assert_eq!(
        tokenizer
            .get_literal()
            .map(|(literal, span)| (literal, span.get())),
        Some((decimal, (2, 1, 5, 1)))
    );

    // get_literal() consumes the literal, so the next token is already visible.
    assert_eq!(tokenizer.peek(), Some(&Token::Plus));
    assert_eq!(tokenizer.pos().get(), (6, 1, 7, 1));
    assert_eq!(tokenizer.get_literal(), None);
    assert_eq!(tokenizer.next(), Some(Token::Plus));

    let hexadecimal = Literal {
        base: Base::Hexadecimal,
        digits: BigUint::from(0x345_u32),
        num_digits_after_dot: None,
        exponent: None,
        suffix: "",
    };
    assert_eq!(tokenizer.peek(), Some(&Token::Literal(hexadecimal.clone())));
    assert_eq!(tokenizer.pos().get(), (8, 1, 13, 1));
    assert_eq!(
        tokenizer
            .get_literal()
            .map(|(literal, span)| (literal, span.get())),
        Some((hexadecimal, (8, 1, 13, 1)))
    );
    assert_eq!(tokenizer.peek(), None);
    assert_eq!(tokenizer.get_literal(), None);
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn decodes_quote_escape_sequences() {
    let input = concat!(
        "\"", "\\0", "\\a", "\\b", "\\t", "\\n", "\\v", "\\f", "\\r", "\\e", "\""
    );
    let (tokens, _, errors) = collect_tokens(input);
    assert_eq!(
        tokens,
        vec![(
            quote("\0\x07\x08\t\n\x0b\x0c\r\x1b", false, false),
            (1, 1, input.len() + 1, 1),
        )]
    );
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn decodes_escaped_structural_quote_characters() {
    let input = concat!("\"", "x", "\\\\", "y", "\\\"", "z", "'", "w", "\\{", "\"");
    let (tokens, _, errors) = collect_tokens(input);
    assert_eq!(
        tokens,
        vec![(
            quote("x\\y\"z'w{", false, false),
            (1, 1, input.len() + 1, 1),
        )]
    );
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn tokenizes_embedded_quotes_across_scopes() {
    let mut tokenizer = Tokenizer::new("\"a{b}c\"", MockDiagnostics::default(), 64);
    assert_eq!(
        tokenizer
            .get_quote()
            .map(|(quote, span)| (quote, span.get())),
        Some((
            Quote {
                content: "a".to_owned(),
                closing_scope: false,
                opening_scope: true,
            },
            (1, 1, 4, 1),
        ))
    );
    assert_eq!(tokenizer.get_quote(), None);
    assert_eq!(tokenizer.pos().get(), (4, 1, 5, 1));
    let Some(Token::Ident(symbol)) = tokenizer.next() else {
        panic!("expected the embedded identifier");
    };
    assert_eq!(
        tokenizer
            .get_quote()
            .map(|(quote, span)| (quote, span.get())),
        Some((
            Quote {
                content: "c".to_owned(),
                closing_scope: true,
                opening_scope: false,
            },
            (5, 1, 8, 1),
        ))
    );
    assert_eq!(tokenizer.peek(), None);
    assert_eq!(tokenizer.get_quote(), None);
    let (interner, errors) = tokenizer.into_parts();
    assert_eq!(interner.resolve(symbol), "b");
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn reports_unknown_escape_sequences() {
    let (tokens, _, errors) = collect_tokens("\"\\q\"");
    assert_eq!(tokens, vec![(quote("\\q", false, false), (1, 1, 5, 1))]);
    let errors = errors.errors.lock().unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0.get(), (2, 1, 4, 1));
    assert!(matches!(
        &errors[0].1,
        Error::UnknownEscapeSequence(given) if given == "\\q"
    ));
}

#[test]
fn reports_unterminated_quotes_and_keeps_trailing_backslash() {
    let (tokens, _, errors) = collect_tokens("\"abc\\");
    assert_eq!(tokens, vec![(quote("abc\\", false, false), (1, 1, 6, 1))]);
    let errors = errors.errors.lock().unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0.get(), (1, 1, 6, 1));
    assert!(matches!(&errors[0].1, Error::NoClosingQuotes));
}

#[test]
fn tokens_and_interner_borrow_owned_source_after_tokenizer_is_consumed() {
    let source = String::from("123suffix name");
    let mut tokenizer = Tokenizer::new(&source, MockDiagnostics::default(), 64);
    let (literal, _) = tokenizer.get_literal().unwrap();
    let Some(Token::Ident(symbol)) = tokenizer.next() else {
        panic!("expected identifier");
    };
    let (interner, errors) = tokenizer.into_parts();

    assert!(errors.errors.lock().unwrap().is_empty());
    assert_eq!(literal.suffix, "suffix");
    assert_eq!(literal.suffix.as_ptr(), source[3..9].as_ptr());
    assert_eq!(interner.resolve(symbol), "name");
    assert_eq!(interner.resolve(symbol).as_ptr(), source[10..].as_ptr());
}
