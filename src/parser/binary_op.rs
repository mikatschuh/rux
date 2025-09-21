#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum BinaryOp {
    Write, // a := b

    LshAssign, // a <<= b
    RshAssign, // a >>= b

    OrAssign,   // a |= b
    NorAssign,  // a !|= b
    XorAssign,  // a >|= b
    XnorAssign, // a !>|= b
    AndAssign,  // a &= b
    NandAssign, // a !&= b

    AddAssign, // a += b
    SubAssign, // a -= b

    MulAssign, // a *= b
    DivAssign, // a /= b
    ModAssign, // a %= b

    DotAssign,   // a ·= b
    CrossAssign, // a ><= b

    PowAssign { grade: u8 }, // a ^= b

    Swap, // a =|= b

    Or,   // a || b
    Nor,  // a !|| b
    Xor,  // a >|| b
    Xnor, // a !>|| b
    And,  // a && b
    Nand, // a !&& b

    Eq,        // a == b
    Ne,        // a != b
    Smaller,   // a < b
    GreaterEq, // a >= b
    Greater,   // a > b
    SmallerEq, // a <= b

    Lsh, // a << b
    Rsh, // a >> b

    BitOr,   // a | b
    BitNor,  // a !| b
    BitXor,  // a >| b
    BitXnor, // a !>| b
    BitAnd,  // a & b
    BitNand, // a !& b

    Add, // a + b
    Sub, // a - b

    Mul, // a * b
    Div, // a / b
    Mod, // a % b

    Dot,   // a · b
    Cross, // a >< b

    Pow { grade: u8 }, // a (^)+ b

    Index, // a[b]
    App,   // a(b)
}
use std::fmt::{self, Display};

use BinaryOp::*;
impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Write => "=",

                LshAssign => "<<=",
                RshAssign => ">>=",

                OrAssign => "|=",
                NorAssign => "!|=",
                XorAssign => ">|=",
                XnorAssign => "!>|=",
                AndAssign => "&=",
                NandAssign => "!&=",

                AddAssign => "+=",
                SubAssign => "-=",

                MulAssign => "*=",
                DivAssign => "/=",
                ModAssign => "%=",

                DotAssign => "·=",
                CrossAssign => "><=",
                PowAssign { grade } =>
                    return write!(f, "^{}", (0..*grade).map(|_| "^").collect::<String>()),

                Swap => "=|=",

                Or => "||",
                Nor => "!||",
                Xor => ">||",
                Xnor => "!>||",
                And => "&&",
                Nand => "!&&",

                Eq => "==",
                Ne => "!=",
                Smaller => "<",
                GreaterEq => ">=",
                Greater => ">",
                SmallerEq => "<=",

                Lsh => "<<",
                Rsh => ">>",

                BitOr => "|",
                BitNor => "!|",
                BitXor => ">|",
                BitXnor => "!>|",
                BitAnd => "&",
                BitNand => "!&",

                Add => "+",
                Sub => "-",

                Mul => "*",
                Div => "/",
                Mod => "%",

                Dot => "·",
                Cross => "><",
                Pow { grade } =>
                    return write!(f, "^{}", (0..*grade).map(|_| "^").collect::<String>()),

                Index => "[",
                App => "(",
            }
        )
    }
}

impl BinaryOp {
    pub const fn is_chained(self) -> bool {
        matches!(self, Eq | Ne | Smaller | SmallerEq | Greater | GreaterEq,)
    }
}
