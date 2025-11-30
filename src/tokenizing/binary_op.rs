#[derive(Clone, PartialEq, Eq, Debug, Copy, Hash)]
pub enum BinaryOp {
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

    Index, // a[b]
    App,   // a(b)

    FieldAccess, // a.b
}
use std::fmt::{self, Display};

use BinaryOp::*;
impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
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

                Index => "[",
                App => "(",

                FieldAccess => ".",
            }
        )
    }
}
