use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Not, // !a

    Neg, // -a

    Ptr,   // -> a
    Deref, // a.
}

use UnaryOp::*;

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Not => "!",

            Neg => "-",

            Ptr => "->",
            Deref => ".",
        };
        write!(f, "{string}")
    }
}
