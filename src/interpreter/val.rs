use crossbeam::epoch::CompareAndSetOrdering;
use num::{traits::ConstOne, BigRational};

use crate::typing::Type;

pub struct Value<'src> {
    pub val: ConcretValue,
    pub ty: Type<'src>,
}

pub enum ConcretValue {
    Number(Number),
    String(String),
}

impl ConcretValue {
    pub fn to_rational(self) -> BigRational {
        if let ConcretValue::Number(number) = self {
            if let Number::BigRational(rational) = number {
                rational
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }
}

pub enum Number {
    Generator,
    BigRational(BigRational),
}
