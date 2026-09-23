use std::fmt;

use tokenizer::Token;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Not, // !a

    Neg, // -a

    Ptr,   // -> a
    Deref, // a.
}

impl UnaryOp {
    pub fn from_prefix(tok: &Token<'_>) -> Option<UnaryOp> {
        Some(match tok {
            Token::Dash => UnaryOp::Neg,
            Token::Not => UnaryOp::Not,
            Token::RightArrow => UnaryOp::Ptr,
            _ => return None,
        })
    }

    pub fn from_postfix(tok: &Token<'_>) -> Option<UnaryOp> {
        Some(match tok {
            Token::LeftArrow => UnaryOp::Ptr,
            _ => return None,
        })
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            UnaryOp::Not => "!",

            UnaryOp::Neg => "-",

            UnaryOp::Ptr => "->",
            UnaryOp::Deref => ".",
        };
        write!(f, "{string}")
    }
}
