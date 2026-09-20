use std::fmt;

use tokenizer::Token;

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
    pub fn from_infix(tok: &Token) -> Option<BinaryOp> {
        Some(match tok {
            Token::EqualEqual => BinaryOp::Eq,
            Token::NotEqual => BinaryOp::Ne,

            Token::Left => BinaryOp::Less,
            Token::LeftLeft => BinaryOp::Lsh,
            Token::NotLeft => BinaryOp::GreaterEq,
            Token::LeftEqual => BinaryOp::LessEq,
            Token::NotLeftEqual => BinaryOp::Greater,

            Token::Right => BinaryOp::Greater,
            Token::RightRight => BinaryOp::Rsh,
            Token::NotRight => BinaryOp::LessEq,
            Token::RightEqual => BinaryOp::GreaterEq,
            Token::NotRightEqual => BinaryOp::Less,

            Token::Plus => BinaryOp::Add,
            Token::Dash => BinaryOp::Sub,

            Token::Star => BinaryOp::Mul,
            Token::Slash => BinaryOp::Div,
            Token::Percent => BinaryOp::Mod,

            Token::Cross => BinaryOp::Cross,

            Token::Pipe => BinaryOp::BitOr,
            Token::PipePipe => BinaryOp::Or,
            Token::NotPipe => BinaryOp::BitNor,
            Token::NotPipePipe => BinaryOp::Nor,

            Token::RightPipe => BinaryOp::BitXor,
            Token::RightPipePipe => BinaryOp::Xor,
            Token::NotRightPipe => BinaryOp::BitXnor,
            Token::NotRightPipePipe => BinaryOp::Xnor,

            Token::And => BinaryOp::BitAnd,
            Token::AndAnd => BinaryOp::And,
            Token::NotAnd => BinaryOp::BitNand,
            Token::NotAndAnd => BinaryOp::Nand,

            _ => return None,
        })
    }

    pub fn from_assign(tok: &Token) -> Option<BinaryOp> {
        Some(match tok {
            Token::PipePipeEqual => BinaryOp::Or,
            Token::NotPipePipeEqual => BinaryOp::Nor,
            Token::RightPipePipeEqual => BinaryOp::Xor,
            Token::NotRightPipePipeEqual => BinaryOp::Xnor,
            Token::AndAndEqual => BinaryOp::And,
            Token::NotAndAndEqual => BinaryOp::Nand,

            Token::LeftLeftEqual => BinaryOp::Lsh,
            Token::RightRightEqual => BinaryOp::Rsh,

            Token::PipeEqual => BinaryOp::BitOr,
            Token::NotPipeEqual => BinaryOp::BitNor,
            Token::RightPipeEqual => BinaryOp::BitXor,
            Token::NotRightPipeEqual => BinaryOp::BitXnor,
            Token::AndEqual => BinaryOp::BitAnd,
            Token::NotAndEqual => BinaryOp::BitNand,

            Token::PlusEqual => BinaryOp::Add,
            Token::DashEqual => BinaryOp::Sub,

            Token::StarEqual => BinaryOp::Mul,
            Token::SlashEqual => BinaryOp::Div,
            Token::PercentEqual => BinaryOp::Mod,

            Token::CrossEqual => BinaryOp::Cross,

            _ => return None,
        })
    }

    pub fn from_inc_or_dec(tok: &Token) -> Option<BinaryOp> {
        Some(match tok {
            Token::PlusPlus => BinaryOp::Add,
            Token::DashDash => BinaryOp::Sub,
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
