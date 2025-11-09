use crate::{
    error::Span,
    parser::{binary_op::BinaryOp, keyword::Keyword, tree::Bracket, unary_op::UnaryOp},
};
use colored::{ColoredString, Colorize};
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub struct Token<'src> {
    pub span: Span,
    pub src: &'src str,
    pub kind: TokenKind,
}

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Not, // !

    Tick,  // '
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

    HalfCenterDot, // ·
    CenterDot,     // ·
    DotEqual,      // ·=
    Cross,         // ><
    CrossEqual,    // ><=
    Up,            // (^)+
    UpEqual,       // (^)+=

    Pipe,         // |
    PipePipe,     // ||
    NotPipe,      // !|
    NotPipePipe,  // !||
    PipeEqual,    // |=
    NotPipeEqual, // !|=

    RightPipe,         // >|
    RightPipePipe,     // >||
    NotRightPipe,      // !>|
    NotRightPipePipe,  // !>||
    RightPipeEqual,    // >|=
    NotRightPipeEqual, // !>|=

    And,         // &
    AndAnd,      // &&
    NotAnd,      // !&
    NotAndAnd,   // !&&
    AndEqual,    // &=
    NotAndEqual, // !&=

    Colon,      // :
    ColonColon, // ::

    Semicolon, // ;

    Comma, // ,

    Ident,            // x
    Placeholder,      // _
    Literal,          // 1001010101
    Quote,            // "_"
    Keyword(Keyword), // if / loop / ..

    Open(Bracket),   // ( / [ / {
    Closed(Bracket), // ) / ] / }

    EOF,
}
use Bracket::*;
use TokenKind::*;

const FIRST_CENTER_DOT_CHARACTER: u8 = "·".as_bytes()[0];
const SECOND_CENTER_DOT_CHARACTER: u8 = "·".as_bytes()[1];

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.src)
    }
}

impl<'a> Token<'a> {
    #[inline]
    pub const fn new(span: Span, src: &'a str, kind: TokenKind) -> Self {
        Self { span, src, kind }
    }
    #[inline]
    pub fn bold(&self) -> ColoredString {
        self.to_string().bold()
    }
}

impl TokenKind {
    pub const fn new(c: u8) -> Option<TokenKind> {
        Some(match c {
            b'!' => Not,
            b'\'' => Tick,
            b'.' => Dot,
            b'=' => Equal,
            b'+' => Plus,
            b'-' => Dash,
            b'*' => Star,
            b'/' => Slash,
            b'%' => Percent,
            FIRST_CENTER_DOT_CHARACTER => CenterDot,
            b'^' => Up,
            b'|' => Pipe,
            b'&' => And,
            b'<' => Left,
            b'>' => Right,
            b':' => Colon,
            b';' => Semicolon,
            b',' => Comma,
            b'(' => Open(Round),
            b'[' => Open(Squared),
            b'{' => Open(Curly),
            b')' => Closed(Round),
            b']' => Closed(Squared),
            b'}' => Closed(Curly),
            _ => return None,
        })
    }
    pub const fn add(self, c: u8) -> Option<TokenKind> {
        // transformation table to make tokens out of their char components
        Some(match self {
            Not if c == b'=' => NotEqual,
            NotEqual if c == b'=' => NotEqual,

            Not if c == b'|' => NotPipe,
            NotPipe if c == b'|' => NotPipePipe,
            NotPipe if c == b'=' => NotPipeEqual,

            Not if c == b'&' => NotAnd,
            NotAnd if c == b'&' => NotAndAnd,
            NotAnd if c == b'=' => NotAndEqual,

            Not if c == b'<' => NotLeft,
            NotLeft if c == b'=' => NotLeftEqual,
            NotLeftEqual if c == b'=' => NotLeftEqual,

            Not if c == b'>' => NotRight,
            NotRight if c == b'|' => NotRightPipe,
            NotRightPipe if c == b'|' => NotRightPipePipe,
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

            HalfCenterDot if c == SECOND_CENTER_DOT_CHARACTER => CenterDot,
            CenterDot if c == b'=' => DotEqual,

            Cross if c == b'=' => CrossEqual,

            Up if c == b'^' => Up,
            Up if c == b'=' => UpEqual,

            Pipe if c == b'|' => PipePipe,
            Pipe if c == b'=' => PipeEqual,

            And if c == b'&' => AndAnd,
            And if c == b'=' => AndEqual,

            Colon if c == b':' => ColonColon,

            _ => return None,
        })
    }
    pub fn ends_with(self, c: char) -> bool {
        match c {
            '!' => matches!(self, Not),
            '\'' => matches!(self, Tick),
            '.' => matches!(self, Dot),
            '=' => matches!(
                self,
                Equal
                    | PlusEqual
                    | DashEqual
                    | StarEqual
                    | SlashEqual
                    | PercentEqual
                    | DotEqual
                    | CrossEqual
                    | UpEqual
                    | PipeEqual
                    | NotPipeEqual
                    | RightPipeEqual
                    | NotRightPipeEqual
                    | AndAnd
                    | NotAndEqual
                    | EqualEqual
                    | NotEqual
                    | LeftEqual
                    | NotLeftEqual
                    | RightEqual
                    | NotRightEqual
            ),
            '>' => matches!(self, RightArrow | Right | RightRight | NotRight),
            '+' => matches!(self, Plus | PlusPlus),
            '-' => matches!(self, Dash | DashDash | LeftArrow),
            '*' => self == Star,
            '/' => self == Slash,
            '%' => self == Percent,
            '·' => self == CenterDot,
            '<' => matches!(self, Cross | Left | LeftLeft | NotLeft),
            '^' => self == Up,
            '|' => matches!(
                self,
                Pipe | NotPipe
                    | PipePipe
                    | NotPipePipe
                    | RightPipe
                    | NotRightPipe
                    | RightPipePipe
                    | NotRightPipePipe
            ),
            '&' => matches!(self, And | NotAnd | AndAnd | NotAndAnd),
            ':' => matches!(self, Colon | ColonColon),
            ';' => self == Semicolon,
            ',' => self == Comma,
            '(' => self == Open(Round),
            '[' => self == Open(Squared),
            '{' => self == Open(Curly),
            ')' => self == Closed(Round),
            ']' => self == Closed(Squared),
            '}' => self == Closed(Curly),
            _ => false,
        }
    }

    pub const fn is_terminator(self) -> bool {
        matches!(self, Closed(..) | Comma | Semicolon)
    }
}
impl<'src> Token<'src> {
    pub fn as_prefix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        Some(match self.kind {
            Dash => Neg,
            TokenKind::Not => Not,
            RightArrow => Ptr,
            _ => return None,
        })
    }

    pub fn as_infix(self) -> Option<BinaryOp> {
        use BinaryOp::*;
        Some(match self.kind {
            TokenKind::Dot => FieldAccess,
            Equal => Write,
            EqualEqual => Eq,
            NotEqual => Ne,

            Left => Smaller,
            LeftLeft => Lsh,
            LeftLeftEqual => LshAssign,
            NotLeft => GreaterEq,
            LeftEqual => SmallerEq,
            NotLeftEqual => Greater,

            Right => Greater,
            RightRight => Rsh,
            RightRightEqual => RshAssign,
            NotRight => SmallerEq,
            RightEqual => GreaterEq,
            NotRightEqual => Smaller,

            Plus => Add,
            PlusEqual => AddAssign,
            Dash => Sub,
            DashEqual => SubAssign,

            Star => Mul,
            StarEqual => MulAssign,
            Slash => Div,
            SlashEqual => DivAssign,
            Percent => Mod,
            PercentEqual => ModAssign,

            TokenKind::CenterDot => Dot,
            DotEqual => DotAssign,
            TokenKind::Cross => Cross,
            CrossEqual => CrossAssign,
            Up => Pow {
                grade: if self.src.len() < 5 {
                    self.src.len() as u8 - 1
                } else {
                    return None;
                },
            },
            UpEqual => PowAssign {
                grade: if self.src.len() < 6 {
                    self.src.len() as u8 - 2
                } else {
                    return None;
                },
            }, // -2 to account for the equal sign

            Pipe => BitOr,
            PipePipe => Or,
            NotPipe => BitNor,
            NotPipePipe => Nor,
            PipeEqual => OrAssign,
            NotPipeEqual => NorAssign,

            RightPipe => BitXor,
            RightPipePipe => Xor,
            NotRightPipe => BitXnor,
            NotRightPipePipe => Xnor,
            RightPipeEqual => XorAssign,
            NotRightPipeEqual => XnorAssign,

            TokenKind::And => BitAnd,
            AndAnd => And,
            NotAnd => BitNand,
            NotAndAnd => Nand,
            AndEqual => AndAssign,
            NotAndEqual => NandAssign,

            _ => return None,
        })
    }

    pub fn as_postfix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        Some(match self.kind {
            PlusPlus => Inc,
            DashDash => Dec,
            TokenKind::Not => Fac,
            LeftArrow => Ptr,
            _ => return None,
        })
    }
}
