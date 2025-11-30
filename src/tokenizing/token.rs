use super::binary_op::BinaryOp;
use crate::{error::Span, tokenizing::unary_op::UnaryOp};
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

    Semicolon, // ;

    Comma, // ,

    Ident,            // x
    Placeholder,      // _
    Literal,          // 1001010101
    Quote,            // "_"
    Keyword(Keyword), // if / loop / ..
    Type,             // i32

    Open(Bracket),   // ( / [ / {
    Closed(Bracket), // ) / ] / }

    EOF,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Proc,
    Loop,
    If,
    Else,
    Continue,
    Break,
    Return,
}

impl Keyword {
    pub fn display(&self) -> &'static str {
        match self {
            Proc => "proc",
            Loop => "loop",
            If => "if",
            Else => "else",
            Continue => "continue",
            Break => "break",
            Return => "return",
        }
    }
    pub fn from_str(string: &str) -> Option<Self> {
        Some(match string {
            "proc" => Proc,
            "prozedur" => Proc,
            "loop" => Loop,
            "wiederhole" => Loop,
            "if" => If,
            "wenn" => If,
            "else" => Else,
            "sonst" => Else,
            "continue" => Continue,
            "nächste" => Continue,
            "break" => Break,
            "verlasse" => Break,
            "return" => Return,
            "zurückgeben" => Return,
            _ => return None,
        })
    }
}

use super::token::Keyword::*;
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

            HalfCenterDot if c == SECOND_CENTER_DOT_CHARACTER => CenterDot,
            CenterDot if c == b'=' => DotEqual,

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
            EqualEqual => Eq,
            NotEqual => Ne,

            Left => Smaller,
            LeftLeft => Lsh,
            NotLeft => GreaterEq,
            LeftEqual => SmallerEq,
            NotLeftEqual => Greater,

            Right => Greater,
            RightRight => Rsh,
            NotRight => SmallerEq,
            RightEqual => GreaterEq,
            NotRightEqual => Smaller,

            Plus => Add,
            Dash => Sub,

            Star => Mul,
            Slash => Div,
            Percent => Mod,

            TokenKind::CenterDot => Dot,
            TokenKind::Cross => Cross,

            Pipe => BitOr,
            PipePipe => Or,
            NotPipe => BitNor,
            NotPipePipe => Nor,

            RightPipe => BitXor,
            RightPipePipe => Xor,
            NotRightPipe => BitXnor,
            NotRightPipePipe => Xnor,

            TokenKind::And => BitAnd,
            AndAnd => And,
            NotAnd => BitNand,
            NotAndAnd => Nand,

            _ => return None,
        })
    }

    pub fn as_assign(self) -> Option<BinaryOp> {
        use BinaryOp::*;
        Some(match self.kind {
            PipePipeEqual => Or,
            NotPipePipeEqual => Nor,
            RightPipePipeEqual => Xor,
            NotRightPipePipeEqual => Xnor,
            AndAndEqual => And,
            NotAndAndEqual => Nand,

            LeftLeftEqual => Lsh,
            RightRightEqual => Rsh,

            PipeEqual => BitOr,
            NotPipeEqual => BitNor,
            RightPipeEqual => BitXor,
            NotRightPipeEqual => BitXnor,
            AndEqual => BitAnd,
            NotAndEqual => BitNand,

            PlusEqual => Add,
            DashEqual => Sub,

            StarEqual => Mul,
            SlashEqual => Div,
            PercentEqual => Mod,

            DotEqual => Dot,
            CrossEqual => Cross,

            _ => return None,
        })
    }

    pub fn as_inc_or_dec(self) -> Option<BinaryOp> {
        use BinaryOp::*;
        Some(match self.kind {
            PlusPlus => Add,
            DashDash => Sub,
            _ => return None,
        })
    }

    pub fn as_postfix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        Some(match self.kind {
            LeftArrow => Ptr,
            _ => return None,
        })
    }
}
