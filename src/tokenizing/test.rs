use std::path::Path;

use crate::{
    error::{Errors, Span},
    tokenizing::{
        token::{Token, TokenKind::*},
        TokenStream, Tokenizer,
    },
    utilities::Rc,
};

#[test]
fn test() {
    let sequences = vec![
        (
            "",
            vec![Token {
                span: Span::at(1, 1, 1, 1),
                src: "",
                kind: EOF,
            }],
        ),
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
                    kind: EOF,
                },
            ],
        ),*/
        (
            "\n a \"Hallo\n\"+",
            vec![
                Token {
                    span: Span::at(2, 2, 3, 2),
                    src: "a",
                    kind: Ident,
                },
                Token {
                    span: Span::at(4, 2, 2, 3),
                    src: "\"Hallo\n\"",
                    kind: Quote,
                },
                Token {
                    span: Span::at(2, 3, 3, 3),
                    src: "+",
                    kind: Plus,
                },
                Token {
                    span: Span::at(3, 3, 3, 3),
                    src: "",
                    kind: EOF,
                },
            ],
        ),
        (
            "a// b + a\nb//",
            vec![
                Token {
                    span: Span::at(1, 1, 2, 1),
                    src: "a",
                    kind: Ident,
                },
                Token {
                    span: Span::at(1, 2, 2, 2),
                    src: "b",
                    kind: Ident,
                },
                Token {
                    span: Span::at(4, 2, 4, 2),
                    src: "",
                    kind: EOF,
                },
            ],
        ),
    ];

    for seq in sequences {
        let errors = Rc::new(Errors::empty(Path::new("example.rx")));
        let mut tokenizer = Tokenizer::new(seq.0, errors);

        let mut tokens = vec![];
        loop {
            let tok = tokenizer.peek();
            tokens.push(tok);
            tokenizer.consume();

            if tok.kind == EOF {
                break;
            }
        }

        assert_eq!(tokens, seq.1)
    }

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
