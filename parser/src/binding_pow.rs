use tokenizer::{
    Bracket,
    Token::{self, *},
};

use crate::{BinaryOp, UnaryOp};

pub const LOGICAL: u8 = 20;
pub const LOGICAL_RIGHT: u8 = 21;

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

pub const fn binding_pow(tok: &Token) -> u8 {
    match tok {
        PipePipe | NotPipePipe | RightPipePipe | NotRightPipePipe | AndAnd | NotAndAnd => LOGICAL,

        EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right | NotRight
        | RightEqual | NotRightEqual => COMPARISON,

        LeftLeft | RightRight | Pipe | NotPipe | RightPipe | NotRightPipe | Token::And | NotAnd => {
            BITWISE
        }

        Plus | Dash => ADDITIVE,

        Star | Slash | Percent | Cross => MULTIPLICATIVE,

        Open(Bracket::Squared | Bracket::Round) | LeftArrow | Dot => APPLICATION,

        _ => 0,
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> u8 {
        match self {
            UnaryOp::Deref => panic!("These don't have right binding power!"),

            UnaryOp::Neg => UNARY_PREFIX,
            UnaryOp::Not => UNARY_PREFIX,

            UnaryOp::Ptr => UNARY_PREFIX,
        }
    }
}

impl BinaryOp {
    pub const fn binding_pow(self) -> u8 {
        match self {
            BinaryOp::Index | BinaryOp::App => panic!("These dont have right binding power!"),

            BinaryOp::Or
            | BinaryOp::Nor
            | BinaryOp::Xor
            | BinaryOp::Xnor
            | BinaryOp::And
            | BinaryOp::Nand => LOGICAL_RIGHT,

            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Less
            | BinaryOp::LessEq
            | BinaryOp::Greater
            | BinaryOp::GreaterEq => COMPARISON_RIGHT,

            BinaryOp::Lsh
            | BinaryOp::Rsh
            | BinaryOp::BitOr
            | BinaryOp::BitNor
            | BinaryOp::BitXor
            | BinaryOp::BitXnor
            | BinaryOp::BitAnd
            | BinaryOp::BitNand => BITWISE_RIGHT,

            BinaryOp::Add | BinaryOp::Sub => ADDITIVE_RIGHT,

            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::Dot | BinaryOp::Cross => {
                MULTIPLICATIVE_RIGHT
            }
        }
    }
}
