use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Not, // !a

    Neg, // -a

    // after one argument // unary - postfix - ops
    Inc, // a++
    Dec, // b--

    Fac, // a!

    Ptr,   // -> a
    Deref, // a.
}

use UnaryOp::*;

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Not => "!",

            Neg => "-",

            Inc => "++",
            Dec => "--",

            Fac => "_!",

            Ptr => "->",
            Deref => ".",
        };
        write!(f, "{string}")
    }
}
