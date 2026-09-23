use crate::{IntegerType, Literal, Symbol};

#[derive(Debug, PartialEq, Eq)]
pub enum Token<'src> {
    Not, // !

    Dot,   // .
    Equal, // =

    EqualEqual, // ==
    NotEqual,   // !=

    Left,          // <
    LeftLeft,      // <<
    LeftLeftEqual, // <<=
    NotLeft,       // !<
    LeftEqual,     // <=
    NotLeftEqual,  // !<=
    LeftArrow,     // <-

    Right,           // >
    RightRight,      // >>
    RightRightEqual, // >>=
    NotRight,        // !>
    RightEqual,      // >=
    NotRightEqual,   // !>=
    RightArrow,      // ->

    Plus,      // +
    PlusPlus,  // ++
    PlusEqual, // +=
    Dash,      // -
    DashDash,  // --
    DashEqual, // -=

    Star,         // *
    StarEqual,    // *=
    Slash,        // /
    SlashEqual,   // /=
    Percent,      // %
    PercentEqual, // %=

    Cross,      // ><
    CrossEqual, // ><=

    Pipe,             // |
    PipePipe,         // ||
    NotPipe,          // !|
    NotPipePipe,      // !||
    PipeEqual,        // |=
    NotPipeEqual,     // !|=
    PipePipeEqual,    // ||=
    NotPipePipeEqual, // !||=

    RightPipe,             // >|
    RightPipePipe,         // >||
    NotRightPipe,          // !>|
    NotRightPipePipe,      // !>||
    RightPipeEqual,        // >|=
    NotRightPipeEqual,     // !>|=
    RightPipePipeEqual,    // >||=
    NotRightPipePipeEqual, // !>||=

    And,            // &
    AndAnd,         // &&
    NotAnd,         // !&
    NotAndAnd,      // !&&
    AndEqual,       // &=
    NotAndEqual,    // !&=
    AndAndEqual,    // &&=
    NotAndAndEqual, // !&&=

    Colon,      // :
    ColonColon, // ::
    AtSign,     // @
    Semicolon,  // ;

    Comma, // ,

    Open(Bracket),   // ( / [ / {
    Closed(Bracket), // ) / ] / }

    Boolean(bool),
    Ident(Symbol), // x
    Underscore,    // _

    // keywords:
    Fn,
    Enum,
    Struct,

    Let,
    Var,

    If,
    Else,
    Loop,
    In,
    Continue,
    Break,
    Return,
    Unreachable,

    TypesType,
    StructType,
    EnumType,
    UnitType,
    NeverType,

    BoolType,
    FloatType(FloatPrecision),
    // =========
    IntegerType(IntegerType), // u8, i8, i1, u0, u128, i32, u11818
    Literal(Literal<'src>),   // 1001010101
    Quote(Quote),             // "..." / }..." / "...{ / }...{
}

#[derive(Debug, PartialEq, Eq)]
pub struct Quote {
    pub content: String,
    pub closing_scope: bool,
    pub opening_scope: bool,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum FloatPrecision {
    Half = 16,
    Full = 32,
    Double = 64,
    DoubleDouble = 128,
}

use Token::*;

pub fn as_keyword<'src>(string: &str) -> Option<Token<'src>> {
    Some(match string {
        "fn" => Fn,
        "enum" => Enum,
        "struct" => Struct,

        "let" => Let,
        "var" => Var,

        "if" => If,
        "else" => Else,
        "loop" => Loop,
        "in" => In,
        "continue" => Continue,
        "break" => Break,
        "return" => Return,
        "unreachable" => Unreachable,

        "void" => UnitType,
        "never" => NeverType,

        "type" => TypesType,
        "structtype" => StructType,
        "enumtype" => EnumType,
        "bool" => BoolType,
        "f16" => FloatType(FloatPrecision::Half),
        "f32" => FloatType(FloatPrecision::Full),
        "f64" => FloatType(FloatPrecision::Double),
        "f128" => FloatType(FloatPrecision::DoubleDouble),
        _ => return None,
    })
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Bracket {
    Round,
    Squared,
    Curly,
}
impl Bracket {
    pub fn display_open(self) -> &'static str {
        match self {
            Bracket::Round => "(",
            Bracket::Squared => "[",
            Bracket::Curly => "{",
        }
    }
    pub fn display_closed(self) -> &'static str {
        match self {
            Bracket::Round => ")",
            Bracket::Squared => "]",
            Bracket::Curly => "}",
        }
    }
}

impl<'src> Token<'src> {
    pub const fn new(c: u8) -> Option<Token<'src>> {
        Some(match c {
            b'!' => Not,
            b'.' => Dot,
            b'=' => Equal,
            b'+' => Plus,
            b'-' => Dash,
            b'*' => Star,
            b'/' => Slash,
            b'%' => Percent,
            b'|' => Pipe,
            b'&' => And,
            b'<' => Left,
            b'>' => Right,
            b':' => Colon,
            b'@' => AtSign,
            b';' => Semicolon,
            b',' => Comma,
            b'(' => Open(Bracket::Round),
            b')' => Closed(Bracket::Round),
            b'[' => Open(Bracket::Squared),
            b']' => Closed(Bracket::Squared),
            b'{' => Open(Bracket::Curly),
            b'}' => Closed(Bracket::Curly),
            _ => return None,
        })
    }
    pub fn add(&self, c: u8) -> Option<Token<'src>> {
        // transformation table to make tokens out of their char components
        Some(match self {
            Not if c == b'=' => NotEqual,
            NotEqual if c == b'=' => NotEqual,

            Not if c == b'|' => NotPipe,
            NotPipe if c == b'|' => NotPipePipe,
            NotPipePipe if c == b'=' => NotPipePipeEqual,
            NotPipe if c == b'=' => NotPipeEqual,

            Not if c == b'&' => NotAnd,
            NotAnd if c == b'&' => NotAndAnd,
            NotAndAnd if c == b'=' => NotAndAndEqual,
            NotAnd if c == b'=' => NotAndEqual,

            Not if c == b'<' => NotLeft,
            NotLeft if c == b'=' => NotLeftEqual,
            NotLeftEqual if c == b'=' => NotLeftEqual,

            Not if c == b'>' => NotRight,
            NotRight if c == b'|' => NotRightPipe,
            NotRightPipe if c == b'|' => NotRightPipePipe,
            NotRightPipePipe if c == b'=' => NotRightPipePipeEqual,
            NotRightPipe if c == b'=' => NotRightPipeEqual,
            NotRight if c == b'=' => NotRightEqual,
            NotRightEqual if c == b'=' => NotRightEqual,

            Equal if c == b'=' => EqualEqual,
            EqualEqual if c == b'=' => EqualEqual,

            Left if c == b'<' => LeftLeft,
            LeftLeft if c == b'=' => LeftLeftEqual,
            Left if c == b'=' => LeftEqual,
            LeftEqual if c == b'=' => LeftEqual,
            Left if c == b'-' => LeftArrow,

            Right if c == b'>' => RightRight,
            RightRight if c == b'=' => RightRightEqual,
            Right if c == b'=' => RightEqual,
            Right if c == b'|' => RightPipe,
            RightPipe if c == b'|' => RightPipePipe,
            RightPipePipe if c == b'=' => RightPipePipeEqual,
            RightPipe if c == b'=' => RightPipeEqual,
            Right if c == b'<' => Cross,
            RightEqual if c == b'=' => RightEqual,

            Plus if c == b'+' => PlusPlus,
            Plus if c == b'=' => PlusEqual,

            Dash if c == b'-' => DashDash,
            Dash if c == b'=' => DashEqual,
            Dash if c == b'>' => RightArrow,

            Star if c == b'=' => StarEqual,

            Slash if c == b'=' => SlashEqual,

            Percent if c == b'=' => PercentEqual,

            Cross if c == b'=' => CrossEqual,

            Pipe if c == b'|' => PipePipe,
            PipePipe if c == b'=' => PipePipeEqual,
            Pipe if c == b'=' => PipeEqual,

            And if c == b'&' => AndAnd,
            AndAnd if c == b'=' => AndAndEqual,
            And if c == b'=' => AndEqual,

            Colon if c == b':' => ColonColon,

            _ => return None,
        })
    }
}
