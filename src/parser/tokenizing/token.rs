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

    CenterDot,  // ·
    DotEqual,   // ·=
    Cross,      // ><
    CrossEqual, // ><=
    Up,         // (^)+
    UpEqual,    // (^)+=

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

    Comma, // ,

    Ident,            // x
    Placeholder,      // _
    Literal,          // 1001010101
    Quote,            // "_"
    Keyword(Keyword), // if / loop / ..

    Open(Bracket),   // ( / [ / {
    Closed(Bracket), // ) / ] / }
}
use Bracket::*;
use TokenKind::*;

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
    pub const fn new(c: char) -> Option<TokenKind> {
        Some(match c {
            '!' => Not,
            '\'' => Tick,
            '.' => Dot,
            '=' => Equal,
            '+' => Plus,
            '-' => Dash,
            '*' => Star,
            '/' => Slash,
            '%' => Percent,
            '·' => CenterDot,
            '^' => Up,
            '|' => Pipe,
            '&' => And,
            '<' => Left,
            '>' => Right,
            ':' => Colon,
            ',' => Comma,
            '(' => Open(Round),
            '[' => Open(Squared),
            '{' => Open(Curly),
            ')' => Closed(Round),
            ']' => Closed(Squared),
            '}' => Closed(Curly),
            _ => return None,
        })
    }
    pub const fn add(self, c: char) -> Option<TokenKind> {
        // transformation table to make tokens out of their char components
        Some(match self {
            Not if c == '=' => NotEqual,
            NotEqual if c == '=' => NotEqual,

            Not if c == '|' => NotPipe,
            NotPipe if c == '|' => NotPipePipe,
            NotPipe if c == '=' => NotPipeEqual,

            Not if c == '&' => NotAnd,
            NotAnd if c == '&' => NotAndAnd,
            NotAnd if c == '=' => NotAndEqual,

            Not if c == '<' => NotLeft,
            NotLeft if c == '=' => NotLeftEqual,
            NotLeftEqual if c == '=' => NotLeftEqual,

            Not if c == '>' => NotRight,
            NotRight if c == '|' => NotRightPipe,
            NotRightPipe if c == '|' => NotRightPipePipe,
            NotRightPipe if c == '=' => NotRightPipeEqual,
            NotRight if c == '=' => NotRightEqual,
            NotRightEqual if c == '=' => NotRightEqual,

            Equal if c == '=' => EqualEqual,
            EqualEqual if c == '=' => EqualEqual,

            Left if c == '<' => LeftLeft,
            LeftLeft if c == '=' => LeftLeftEqual,
            Left if c == '=' => LeftEqual,
            LeftEqual if c == '=' => LeftEqual,

            Right if c == '>' => RightRight,
            RightRight if c == '=' => RightRightEqual,
            Right if c == '=' => RightEqual,
            Right if c == '|' => RightPipe,
            RightPipe if c == '|' => RightPipePipe,
            RightPipe if c == '=' => RightPipeEqual,
            Right if c == '<' => Cross,
            RightEqual if c == '=' => RightEqual,

            Plus if c == '+' => PlusPlus,
            Plus if c == '=' => PlusEqual,

            Dash if c == '-' => DashDash,
            Dash if c == '=' => DashEqual,
            Dash if c == '>' => RightArrow,

            Star if c == '=' => StarEqual,

            Slash if c == '=' => SlashEqual,

            Percent if c == '=' => PercentEqual,

            CenterDot if c == '=' => DotEqual,

            Cross if c == '=' => CrossEqual,

            Up if c == '^' => Up,
            Up if c == '=' => UpEqual,

            Pipe if c == '|' => PipePipe,
            Pipe if c == '=' => PipeEqual,

            And if c == '&' => AndAnd,
            And if c == '=' => AndEqual,

            Colon if c == ':' => ColonColon,

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
            '-' => matches!(self, Dash | DashDash),
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
            ':' => matches!(self, ColonColon),
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
        matches!(self, Closed(..) | Comma)
    }

    pub const fn has_right_side(self) -> bool {
        !matches!(
            self,
            Ident | Placeholder | Dot | Literal | Quote | Closed(..)
        )
    }
}
impl<'src> Token<'src> {
    pub fn as_prefix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        Some(match self.kind {
            Dash => Neg,
            TokenKind::Not => Not,
            And => Ref,
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
            _ => return None,
        })
    }
}
