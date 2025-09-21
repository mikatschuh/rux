use num::{BigRational, BigUint};

use crate::{
    interpreter::val::{ConcretValue, Number, Value},
    parser::{
        binary_op::BinaryOp,
        tokenizing::num::Base,
        tree::{Node, NodeBox},
    },
    typing::{NumberKind, NumberType},
};

pub mod val;

fn divisor_equation(base: Base, digits_after_dot: usize) -> BigUint {
    (base as u32).pow(digits_after_dot as u32).into()
}

impl<'src> NodeBox<'src> {
    fn eval(&self) -> val::Value<'src> {
        assert!(self.node.is_some());

        use BinaryOp::*;
        use Node::*;
        match self.node.unwrap() {
            Ident { sym, literal } => {
                let Some(literal) = literal else { todo!() };
                Value {
                    val: ConcretValue::Number(Number::BigRational(BigRational::new(
                        literal.digits.into(),
                        divisor_equation(literal.base, literal.digits_after_dot).into(),
                    ))),
                    ty: NumberType {
                        kind: NumberKind::Arbitrary,
                        size: None,
                    }
                    .into(),
                }
            }
            Binary { op, lhs, rhs } => match op {
                Mul => Value {
                    val: lhs.eval().val.to_rational() * rhs.eval().val.to_rational(),
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}
