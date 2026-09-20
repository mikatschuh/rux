use std::sync::{Arc, Mutex};

use num::BigUint;

use tokenizer::{
    Base, Diagnostics, Error, Literal as LiteralValue, Span, Token2, Token::*, TokenStream,
    Tokenizer,
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

fn collect_tokens_and_quotes(input: &'static str) -> (Vec<Token2>, Vec<String>, MockDiagnostics) {
    let errors = MockDiagnostics::default();
    let mut tokenizer = Tokenizer::new(input, errors.clone(), 64);

    let mut tokens = vec![];
    let mut quotes = vec![];
    while let Some(tok) = tokenizer.peek() {
        if matches!(tok.kind, Quote { .. }) {
            quotes.push(tokenizer.get_quote());
        }

        tokens.push(tok);
        tokenizer.consume();
    }

    (tokens, quotes, errors)
}

#[test]
fn tokenizes_basic_sequences() {
    let sequences = vec![
        ("", vec![]),
        /*(
            "++a a+· wdwwkd\n iw22i-*==",
            vec![
                Token {
                    span: Span::at(1, 1, 3, 1),
                    src: "++",
                    kind: PlusPlus,
                },
                Token {
                    span: Span::at(3, 1, 4, 1),
                    src: "a",
                    kind: Ident,
                },
                Token {
                    span: Span::at(5, 1, 6, 1),
                    src: "a",
                    kind: Ident,
                },
                Token {
                    span: Span::at(6, 1, 7, 1),
                    src: "+",
                    kind: Plus,
                },
                Token {
                    span: Span::at(7, 1, 8, 1),
                    src: "·",
                    kind: CenterDot,
                },
                Token {
                    span: Span::at(9, 1, 15, 1),
                    src: "wdwwkd",
                    kind: Ident,
                },
                Token {
                    span: Span::at(2, 2, 7, 2),
                    src: "iw22i",
                    kind: Ident,
                },
                Token {
                    span: Span::at(7, 2, 8, 2),
                    src: "-",
                    kind: Dash,
                },
                Token {
                    span: Span::at(8, 2, 9, 2),
                    src: "*",
                    kind: Star,
                },
                Token {
                    span: Span::at(9, 2, 11, 2),
                    src: "==",
                    kind: EqualEqual,
                },
                Token {
                    span: Span::at(11, 2, 11, 2),
                    src: "",
                    kind: Eof,
                },
            ],
        ),*/
        (
            "\n a \"Hallo\n\"+",
            vec![
                Token2 {
                    span: Span::at(2, 2, 3, 2),
                    src: "a",
                    kind: Ident,
                },
                Token2 {
                    span: Span::at(4, 2, 2, 3),
                    src: "\"Hallo\n\"",
                    kind: Quote {
                        closing_scope: false,
                        opening_scope: false,
                    },
                },
                Token2 {
                    span: Span::at(2, 3, 3, 3),
                    src: "+",
                    kind: Plus,
                },
            ],
        ),
        (
            "a// b + a\nb//",
            vec![
                Token2 {
                    span: Span::at(1, 1, 2, 1),
                    src: "a",
                    kind: Ident,
                },
                Token2 {
                    span: Span::at(1, 2, 2, 2),
                    src: "b",
                    kind: Ident,
                },
            ],
        ),
    ];

    for seq in sequences {
        let (tokens, _, errors) = collect_tokens_and_quotes(seq.0);
        assert_eq!(tokens, seq.1);
        assert!(errors.errors.lock().unwrap().is_empty());
    }
}

#[test]
fn tokenizes_literal_sequences() {
    // testing literal behavior:

    let errors = MockDiagnostics::default();
    let mut tokenizer = Tokenizer::new("-1.3 + 0x345", errors.clone(), 64);

    assert_eq!(
        tokenizer.peek(),
        Some(Token2 {
            span: Span::at(1, 1, 2, 1),
            src: "-",
            kind: Dash
        })
    );
    tokenizer.consume();

    assert_eq!(
        tokenizer.peek(),
        Some(Token2 {
            span: Span::at(2, 1, 5, 1),
            src: "1.3",
            kind: Literal
        })
    );
    assert_eq!(
        tokenizer.get_literal(),
        LiteralValue {
            base: Base::Decimal,
            digits: BigUint::from(13_u8),
            num_digits_after_dot: Some(1),
            exponent: None,
            suffix: "",
        }
    );
    tokenizer.consume();

    // get_literal() panics unless the current token holds an unread literal.

    assert_eq!(
        tokenizer.peek(),
        Some(Token2 {
            span: Span::at(6, 1, 7, 1),
            src: "+",
            kind: Plus
        })
    );
    tokenizer.consume();

    assert_eq!(
        tokenizer.peek(),
        Some(Token2 {
            span: Span::at(8, 1, 13, 1),
            src: "0x345",
            kind: Literal
        })
    );
    assert_eq!(
        tokenizer.get_literal(),
        LiteralValue {
            base: Base::Hexadecimal,
            digits: BigUint::from(0x345_u32),
            num_digits_after_dot: None,
            exponent: None,
            suffix: ""
        }
    );
    tokenizer.consume();

    assert_eq!(tokenizer.peek(), None);
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn decodes_quote_escape_sequences() {
    let input = concat!(
        "\"", "\\0", "\\a", "\\b", "\\t", "\\n", "\\v", "\\f", "\\r", "\\e", "\""
    );
    let (tokens, quotes, errors) = collect_tokens_and_quotes(input);

    assert_eq!(
        tokens,
        vec![Token2 {
            span: Span::at(1, 1, input.len() + 1, 1),
            src: input,
            kind: Quote {
                closing_scope: false,
                opening_scope: false,
            },
        },]
    );
    assert_eq!(quotes.len(), 1);
    assert_eq!(
        quotes[0].as_bytes(),
        &[0x0, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0x1B]
    );
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn decodes_escaped_structural_quote_characters() {
    let input = concat!("\"", "x", "\\\\", "y", "\\\"", "z", "'", "w", "\\{", "\"");
    let (tokens, quotes, errors) = collect_tokens_and_quotes(input);

    assert_eq!(
        tokens,
        vec![Token2 {
            span: Span::at(1, 1, input.len() + 1, 1),
            src: input,
            kind: Quote {
                closing_scope: false,
                opening_scope: false,
            },
        },]
    );
    assert_eq!(quotes, vec!["x\\y\"z'w{".to_owned()]);
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn tokenizes_embedded_quotes_across_scopes() {
    let (tokens, quotes, errors) = collect_tokens_and_quotes("\"a{b}c\"");

    assert_eq!(
        tokens,
        vec![
            Token2 {
                span: Span::at(1, 1, 4, 1),
                src: "\"a{",
                kind: Quote {
                    closing_scope: false,
                    opening_scope: true,
                },
            },
            Token2 {
                span: Span::at(4, 1, 5, 1),
                src: "b",
                kind: Ident,
            },
            Token2 {
                span: Span::at(5, 1, 8, 1),
                src: "}c\"",
                kind: Quote {
                    closing_scope: true,
                    opening_scope: false,
                },
            },
        ]
    );
    assert_eq!(quotes, vec!["a".to_owned(), "c".to_owned()]);
    assert!(errors.errors.lock().unwrap().is_empty());
}

#[test]
fn reports_unknown_escape_sequences() {
    let (tokens, quotes, errors) = collect_tokens_and_quotes("\"\\q\"");

    assert_eq!(
        tokens,
        vec![Token2 {
            span: Span::at(1, 1, 5, 1),
            src: "\"\\q\"",
            kind: Quote {
                closing_scope: false,
                opening_scope: false,
            },
        },]
    );
    assert_eq!(quotes, vec!["\\q".to_owned()]);

    let errors = errors.errors.lock().unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0, Span::at(2, 1, 4, 1));
    assert!(matches!(
        &errors[0].1,
        Error::UnknownEscapeSequence(given) if given == "\\q"
    ));
}

#[test]
fn reports_unterminated_quotes_and_keeps_trailing_backslash() {
    let (tokens, quotes, errors) = collect_tokens_and_quotes("\"abc\\");

    assert_eq!(
        tokens,
        vec![Token2 {
            span: Span::at(1, 1, 6, 1),
            src: "\"abc\\",
            kind: Quote {
                closing_scope: false,
                opening_scope: false,
            },
        },]
    );
    assert_eq!(quotes, vec!["abc\\".to_owned()]);

    let errors = errors.errors.lock().unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0, Span::at(1, 1, 6, 1));
    assert!(matches!(&errors[0].1, Error::NoClosingQuotes));

    /*let tokenizer = Tokenizer::new(EXAMPLE, errors);
    let tokens = tokenizer.clone().count() as f64;
    let mut count = 0;
    println!(
        "tok/s = {}",
        tokens
            / bench(|| {
                count += tokenizer.clone().count();
            })
    );
    println!("total number of processed tokens: {count}");*/
}
#[allow(unused)]
const EXAMPLE: &str = "time.now() Instant
time.since(Instant) f64
sin(f32) f32
cos(f32) f32

Vec2 { .x f32, .y f32 }
Vec3 { .x f32, .y f32, .z f32 }

main() {
    start = time.now()
    loop () std.out.display(width=100, height=100).fill(Cap[uv Vec3][std.char][
        f = 1
        epsilon = 0.01
        r = 15.0
        mut p Vec3 := 0

        d = norm(uv, f)
        loop {
            sdf(p Vec3) f32 {
                angle = time.since(start)
                tilt = -45.0 * PI / 180.0
                p = (cos(tilt), sin(tilt)(1, 0, 0))
                    * (cos(angle), sin(angle)(0, 1, 0))
                    * (p - (0, 0, f + r))

                t = vec2 { 4, 2.2 }
                q = vec2 { len(p.(x, z)) - t.x, p.y }
                len(q) - t.y
            }
            dst = sdf(p)
            if dst < epsilon returny {
                dx = vec3 { epsilon, 0, 0 }
                dy = vec3 { 0, epsilon, 0 }
                dz = vec3 { 0, 0, epsilon }

                brightness = -norm(
                    sdf(p + dx) - sdf(p - dx),
                    sdf(p + dy) - sdf(p - dy),
                    sdf(p + dz) - sdf(p - dz),
                ) · (1, -1, 1) + 1

                \"·-+*oag3&8\"[brightness * brightness]

            } else if dst > 100 return \" \"
            else p += dst * d
        }
    ])
}
";
