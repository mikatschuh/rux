use std::fmt;

use tokenizer::TokenKind;

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
    Less,      // a < b
    GreaterEq, // a >= b
    Greater,   // a > b
    LessEq,    // a <= b

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
}

impl BinaryOp {
    pub fn from_infix(tok: TokenKind) -> Option<BinaryOp> {
        Some(match tok {
            TokenKind::EqualEqual => BinaryOp::Eq,
            TokenKind::NotEqual => BinaryOp::Ne,

            TokenKind::Left => BinaryOp::Less,
            TokenKind::LeftLeft => BinaryOp::Lsh,
            TokenKind::NotLeft => BinaryOp::GreaterEq,
            TokenKind::LeftEqual => BinaryOp::LessEq,
            TokenKind::NotLeftEqual => BinaryOp::Greater,

            TokenKind::Right => BinaryOp::Greater,
            TokenKind::RightRight => BinaryOp::Rsh,
            TokenKind::NotRight => BinaryOp::LessEq,
            TokenKind::RightEqual => BinaryOp::GreaterEq,
            TokenKind::NotRightEqual => BinaryOp::Less,

            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Dash => BinaryOp::Sub,

            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Mod,

            TokenKind::Cross => BinaryOp::Cross,

            TokenKind::Pipe => BinaryOp::BitOr,
            TokenKind::PipePipe => BinaryOp::Or,
            TokenKind::NotPipe => BinaryOp::BitNor,
            TokenKind::NotPipePipe => BinaryOp::Nor,

            TokenKind::RightPipe => BinaryOp::BitXor,
            TokenKind::RightPipePipe => BinaryOp::Xor,
            TokenKind::NotRightPipe => BinaryOp::BitXnor,
            TokenKind::NotRightPipePipe => BinaryOp::Xnor,

            TokenKind::And => BinaryOp::BitAnd,
            TokenKind::AndAnd => BinaryOp::And,
            TokenKind::NotAnd => BinaryOp::BitNand,
            TokenKind::NotAndAnd => BinaryOp::Nand,

            _ => return None,
        })
    }

    pub fn from_assign(tok: TokenKind) -> Option<BinaryOp> {
        Some(match tok {
            TokenKind::PipePipeEqual => BinaryOp::Or,
            TokenKind::NotPipePipeEqual => BinaryOp::Nor,
            TokenKind::RightPipePipeEqual => BinaryOp::Xor,
            TokenKind::NotRightPipePipeEqual => BinaryOp::Xnor,
            TokenKind::AndAndEqual => BinaryOp::And,
            TokenKind::NotAndAndEqual => BinaryOp::Nand,

            TokenKind::LeftLeftEqual => BinaryOp::Lsh,
            TokenKind::RightRightEqual => BinaryOp::Rsh,

            TokenKind::PipeEqual => BinaryOp::BitOr,
            TokenKind::NotPipeEqual => BinaryOp::BitNor,
            TokenKind::RightPipeEqual => BinaryOp::BitXor,
            TokenKind::NotRightPipeEqual => BinaryOp::BitXnor,
            TokenKind::AndEqual => BinaryOp::BitAnd,
            TokenKind::NotAndEqual => BinaryOp::BitNand,

            TokenKind::PlusEqual => BinaryOp::Add,
            TokenKind::DashEqual => BinaryOp::Sub,

            TokenKind::StarEqual => BinaryOp::Mul,
            TokenKind::SlashEqual => BinaryOp::Div,
            TokenKind::PercentEqual => BinaryOp::Mod,

            TokenKind::CrossEqual => BinaryOp::Cross,

            _ => return None,
        })
    }

    pub fn from_inc_or_dec(tok: TokenKind) -> Option<BinaryOp> {
        Some(match tok {
            TokenKind::PlusPlus => BinaryOp::Add,
            TokenKind::DashDash => BinaryOp::Sub,
            _ => return None,
        })
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Or => "||",
                BinaryOp::Nor => "!||",
                BinaryOp::Xor => ">||",
                BinaryOp::Xnor => "!>||",
                BinaryOp::And => "&&",
                BinaryOp::Nand => "!&&",

                BinaryOp::Eq => "==",
                BinaryOp::Ne => "!=",
                BinaryOp::Less => "<",
                BinaryOp::GreaterEq => ">=",
                BinaryOp::Greater => ">",
                BinaryOp::LessEq => "<=",

                BinaryOp::Lsh => "<<",
                BinaryOp::Rsh => ">>",

                BinaryOp::BitOr => "|",
                BinaryOp::BitNor => "!|",
                BinaryOp::BitXor => ">|",
                BinaryOp::BitXnor => "!>|",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitNand => "!&",

                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",

                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Mod => "%",

                BinaryOp::Dot => "·",
                BinaryOp::Cross => "><",

                BinaryOp::Index => "[",
                BinaryOp::App => "(",
            }
        )
    }
}
