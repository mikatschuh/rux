use std::fmt;

use tokenizer::TokenKind;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Not, // !a

    Neg, // -a

    Ptr,   // -> a
    Deref, // a.
}

impl UnaryOp {
    pub fn from_prefix(tok: TokenKind) -> Option<UnaryOp> {
        Some(match tok {
            TokenKind::Dash => UnaryOp::Neg,
            TokenKind::Not => UnaryOp::Not,
            TokenKind::RightArrow => UnaryOp::Ptr,
            _ => return None,
        })
    }

    pub fn from_postfix(tok: TokenKind) -> Option<UnaryOp> {
        Some(match tok {
            TokenKind::LeftArrow => UnaryOp::Ptr,
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
