use super::{binary_op::BinaryOp, unary_op::UnaryOp};
use crate::tokenizing::token::{Bracket, Token, TokenKind};

use TokenKind::*;

pub const LABEL: u8 = 15; // allows tick

pub const BOOLEAN: u8 = 20;
pub const BOOLEAN_RIGHT: u8 = 21;

pub const COMPARISON: u8 = 50;
pub const COMPARISON_RIGHT: u8 = 51;

pub const BITWISE: u8 = 60;
pub const BITWISE_RIGHT: u8 = 61;

pub const ADDITIVE: u8 = 100;
pub const ADDITIVE_RIGHT: u8 = 101;

pub const MULTIPLICATIVE: u8 = 110;
pub const MULTIPLICATIVE_RIGHT: u8 = 111;

pub const UNARY_PREFIX: u8 = 130;

pub const APPLICATION: u8 = 140;

pub const CALL_CONVENTION: u8 = 150;
pub const ACCESSOR: u8 = 150;
pub const ACCESSOR_RIGHT: u8 = 151;

impl<'src> Token<'src> {
    pub const fn binding_pow(self) -> u8 {
        match self.kind {
            Comma
            | Closed(..)
            | Eof
            | HalfCenterDot
            | RightArrow
            | Underscore
            | DashDashDash
            | Ident
            | Not
            | Literal
            | Quote { .. }
            // keywords:
            | Fn
            | Enum
            | Struct
            | Let
            | Var
            | If
            | Else
            | Loop
            | In
            | Continue
            | Break
            | Return
            | Unreachable
            // =========
            | Type
            | Open(Bracket::Curly)
            | Semicolon
            | Colon
            | ColonColon
            | Equal
            | LeftLeftEqual
            | Boolean(..)
            | RightRightEqual
            | PipeEqual
            | PipePipeEqual
            | NotPipeEqual
            | NotPipePipeEqual
            | RightPipeEqual
            | RightPipePipeEqual
            | NotRightPipeEqual
            | NotRightPipePipeEqual
            | AndEqual
            | AndAndEqual
            | NotAndEqual
            | NotAndAndEqual
            | PlusEqual
            | DashEqual
            | StarEqual
            | SlashEqual
            | PercentEqual
            | DotEqual
            | CrossEqual
            | PlusPlus
            | DashDash => 0,

            Tick => LABEL,

            PipePipe | NotPipePipe | RightPipePipe | NotRightPipePipe | AndAnd | NotAndAnd => {
                BOOLEAN
            }

            EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right
            | NotRight | RightEqual | NotRightEqual => COMPARISON,

            LeftLeft
            | RightRight
            | Pipe
            | NotPipe
            | RightPipe
            | NotRightPipe
            | TokenKind::And
            | NotAnd => BITWISE,

            Plus | Dash => ADDITIVE,

            Star | Slash | Percent | CenterDot | Cross => MULTIPLICATIVE,

            Open(Bracket::Squared | Bracket::Round) | LeftArrow => APPLICATION,

            Dot => ACCESSOR,
        }
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Deref => panic!("These don't have right binding power!"),

            Neg => UNARY_PREFIX,
            Not => UNARY_PREFIX,

            Ptr => UNARY_PREFIX,
        }
    }
}

impl BinaryOp {
    pub const fn binding_pow(self) -> u8 {
        use BinaryOp::*;

        match self {
            Index | App => panic!("These dont have right binding power!"),

            BinaryOp::Or | Nor | BinaryOp::Xor | Xnor | BinaryOp::And | Nand => BOOLEAN_RIGHT,

            Eq | Ne | Less | LessEq | Greater | GreaterEq => COMPARISON_RIGHT,

            Lsh
            | Rsh
            | BinaryOp::BitOr
            | BitNor
            | BinaryOp::BitXor
            | BitXnor
            | BinaryOp::BitAnd
            | BitNand => BITWISE_RIGHT,

            Add | Sub => ADDITIVE_RIGHT,

            Mul | Div | Mod | Dot | Cross => MULTIPLICATIVE_RIGHT,

            FieldAccess => ACCESSOR_RIGHT,
        }
    }
}
