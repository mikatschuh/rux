use crate::typing::TypeParser;

#[test]
fn test() {
    use super::token::TokenKind::*;

    use crate::{
        error::{Errors, Span},
        parser::tokenizing::{token::Token, Tokenizer},
        utilities::Rc,
    };
    use std::path::Path;

    let errors = Rc::new(Errors::empty(Path::new("example.flou")));
    assert_eq!(
        Tokenizer::new(
            "+++*===!>|\nx\"some string\"+1 v",
            errors.clone(),
            TypeParser::new()
        )
        .collect::<Vec<_>>(),
        vec![
            Token::new(Span::at(1, 1, 2, 1), "++", PlusPlus),
            Token::new(Span::at(3, 1, 3, 1), "+", Plus),
            Token::new(Span::at(4, 1, 5, 1), "*=", StarEqual),
            Token::new(Span::at(6, 1, 7, 1), "==", EqualEqual),
            Token::new(Span::at(8, 1, 10, 1), "!>|", NotRightPipe),
            Token::new(Span::at(1, 2, 1, 2), "x", Ident),
            Token::new(Span::at(2, 2, 14, 2), "\"some string\"", Quote),
            Token::new(Span::at(15, 2, 15, 2), "+", Plus),
            Token::new(Span::at(16, 2, 16, 2), "1", Ident),
            Token::new(Span::at(18, 2, 18, 2), "v", Ident)
        ]
    );
    dbg!(1);
    assert_eq!(
        Tokenizer::new("a + b //!\n c", errors.clone(), TypeParser::new()).collect::<Vec<_>>(),
        vec![
            Token::new(Span::at(1, 1, 1, 1), "a", Ident),
            Token::new(Span::at(3, 1, 3, 1), "+", Plus),
            Token::new(Span::at(5, 1, 5, 1), "b", Ident),
            Token::new(Span::at(2, 2, 2, 2), "c", Ident),
        ]
    );
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
