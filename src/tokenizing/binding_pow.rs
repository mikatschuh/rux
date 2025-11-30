use super::{binary_op::BinaryOp, unary_op::UnaryOp};
use crate::tokenizing::token::{Bracket, Token, TokenKind};

use TokenKind::*;

pub const PATH: u8 = 2;
pub const COLON: u8 = 8;

pub const WRITE: u8 = 10;
pub const WRITE_RIGHT: u8 = 11;

pub const LABEL: u8 = 15; // allows tick

pub const OR: u8 = 20;
pub const OR_RIGHT: u8 = 21;
pub const XOR: u8 = 30;
pub const XOR_RIGHT: u8 = 31;
pub const AND: u8 = 40;
pub const AND_RIGHT: u8 = 41;

pub const COMPARISON: u8 = 50;
pub const COMPARISON_RIGHT: u8 = 51;

pub const BIT_SHIFT: u8 = 60;
pub const BIT_SHIFT_RIGHT: u8 = 61;

pub const BIT_OR: u8 = 70;
pub const BIT_OR_RIGHT: u8 = 71;
pub const BIT_XOR: u8 = 80;
pub const BIT_XOR_RIGHT: u8 = 81;
pub const BIT_AND: u8 = 90;
pub const BIT_AND_RIGHT: u8 = 91;

pub const ADDITIVE: u8 = 100;
pub const ADDITIVE_RIGHT: u8 = 101;

pub const MULTIPLICATIVE: u8 = 110;
pub const MULTIPLICATIVE_RIGHT: u8 = 111;

pub const NEGATION_RIGHT: u8 = 114;

pub const PREFIX: u8 = 130;
pub const APPLICATION: u8 = 140;

pub const CALL_CONVENTION: u8 = 150;
pub const ACCESSOR: u8 = 150;
pub const ACCESSOR_RIGHT: u8 = 151;

impl<'src> Token<'src> {
    pub const fn binding_pow(self) -> u8 {
        match self.kind {
            Comma | Closed(..) | EOF | HalfCenterDot | RightArrow | Placeholder | Ident | Not
            | Literal | Quote | Keyword(..) | Type | Open(Bracket::Curly) | Semicolon => 0,

            TokenKind::Colon => COLON,

            ColonColon
            | Equal
            | LeftLeftEqual
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
            | DashDash => WRITE,

            Tick => LABEL,

            PipePipe | NotPipePipe => OR,
            RightPipePipe | NotRightPipePipe => XOR,
            AndAnd | NotAndAnd => AND,

            EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right
            | NotRight | RightEqual | NotRightEqual => COMPARISON,

            LeftLeft | RightRight => BIT_SHIFT,

            Pipe | NotPipe => BIT_OR,
            RightPipe | NotRightPipe => BIT_XOR,
            TokenKind::And | NotAnd => BIT_AND,

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

            Neg => NEGATION_RIGHT,
            Not => PREFIX,

            Ptr => PREFIX,
        }
    }
}

impl BinaryOp {
    pub const fn binding_pow(self) -> u8 {
        use BinaryOp::*;

        match self {
            Index | App => panic!("These dont have right binding power!"),

            BinaryOp::Or | Nor => OR_RIGHT,
            BinaryOp::Xor | Xnor => XOR_RIGHT,
            BinaryOp::And | Nand => AND_RIGHT,

            Eq | Ne | Smaller | SmallerEq | Greater | GreaterEq => COMPARISON_RIGHT,

            Lsh | Rsh => BIT_SHIFT_RIGHT,

            BinaryOp::BitOr | BitNor => BIT_OR_RIGHT,
            BinaryOp::BitXor | BitXnor => BIT_XOR_RIGHT,
            BinaryOp::BitAnd | BitNand => BIT_AND_RIGHT,

            Add | Sub => ADDITIVE_RIGHT,

            Mul | Div | Mod | Dot | Cross => MULTIPLICATIVE_RIGHT,

            FieldAccess => ACCESSOR_RIGHT,
        }
    }
}
