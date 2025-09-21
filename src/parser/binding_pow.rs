use crate::parser::{
    binary_op::BinaryOp,
    tokenizing::token::{Token, TokenKind},
    tree::Bracket,
    unary_op::UnaryOp,
};

use TokenKind::*;

pub(super) const STATEMENT: u8 = 1;
pub(super) const PATH: u8 = 2;
pub(super) const COLON: u8 = 8;

pub(super) const WRITE: u8 = 10;
pub(super) const WRITE_RIGHT: u8 = 11;

pub(super) const LABEL: u8 = 15; // allows tick

pub(super) const OR: u8 = 20;
pub(super) const OR_RIGHT: u8 = 21;
pub(super) const XOR: u8 = 30;
pub(super) const XOR_RIGHT: u8 = 31;
pub(super) const AND: u8 = 40;
pub(super) const AND_RIGHT: u8 = 41;

pub(super) const COMPARISON: u8 = 50;
pub(super) const COMPARISON_RIGHT: u8 = 51;

pub(super) const BIT_SHIFT: u8 = 60;
pub(super) const BIT_SHIFT_RIGHT: u8 = 61;

pub(super) const BIT_OR: u8 = 70;
pub(super) const BIT_OR_RIGHT: u8 = 71;
pub(super) const BIT_XOR: u8 = 80;
pub(super) const BIT_XOR_RIGHT: u8 = 81;
pub(super) const BIT_AND: u8 = 90;
pub(super) const BIT_AND_RIGHT: u8 = 91;

pub(super) const ADDITIVE: u8 = 100;
pub(super) const ADDITIVE_RIGHT: u8 = 101;

pub(super) const MULTIPLICATIVE: u8 = 110;
pub(super) const MULTIPLICATIVE_RIGHT: u8 = 111;

pub(super) const NEGATION_RIGHT: u8 = 114;

pub(super) const FACTORIAL: u8 = 115;

pub(super) const POTENTIATION_RIGHT: u8 = 120;
pub(super) const POTENTIATION: u8 = 121;

pub(super) const PREFIX: u8 = 130;
pub(super) const APPLICATION: u8 = 140;

impl<'src> Token<'src> {
    pub const fn binding_pow(self) -> Option<u8> {
        Some(match self.kind {
            Closed(..) | Comma => return None,

            RightArrow | Ident | Literal | Quote | Keyword(..) | Open(Bracket::Curly) => 0,

            TokenKind::Colon => COLON,

            ColonColon | Equal | EqualPipe | LeftLeftEqual | RightRightEqual | PipeEqual
            | NotPipeEqual | RightPipeEqual | NotRightPipeEqual | AndEqual | NotAndEqual
            | PlusEqual | DashEqual | StarEqual | SlashEqual | PercentEqual | DotEqual
            | CrossEqual | UpEqual | SwapSign | PlusPlus | DashDash => WRITE,

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

            Star | Slash | Percent | Dot | Cross => MULTIPLICATIVE,

            Not => FACTORIAL,

            Up => POTENTIATION + ((self.src.len() - 1) << 1) as u8,

            Open(Bracket::Squared | Bracket::Round) => APPLICATION,
        })
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Inc | Dec | Fac => panic!("These don't have right binding power!"),

            Neg => NEGATION_RIGHT,
            Not => PREFIX,

            Ptr => PREFIX,
            Ref => PREFIX,
        }
    }
}

impl BinaryOp {
    pub const fn binding_pow(self) -> u8 {
        use BinaryOp::*;

        match self {
            Index | App => panic!("These dont have right binding power!"),

            BinaryOp::Write
            | LshAssign
            | RshAssign
            | OrAssign
            | NorAssign
            | XorAssign
            | XnorAssign
            | AndAssign
            | NandAssign
            | AddAssign
            | SubAssign
            | MulAssign
            | DivAssign
            | ModAssign
            | DotAssign
            | CrossAssign
            | PowAssign { .. }
            | Swap => WRITE_RIGHT,

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

            Pow { grade } => POTENTIATION_RIGHT + grade << 1,
        }
    }
}
