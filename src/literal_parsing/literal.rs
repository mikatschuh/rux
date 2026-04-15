use std::fmt;

use num::{BigInt, BigUint, FromPrimitive, Integer, ToPrimitive, Zero};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Base {
    Binary = 2,
    Seximal = 6,
    Octal = 8,
    Decimal = 10,
    Dozenal = 12,
    Hexadecimal = 16,
}

impl Base {
    fn prefix(self) -> &'static str {
        use Base::*;
        match self {
            Binary => "0b",
            Seximal => "0s",
            Octal => "0o",
            Decimal => "",
            Dozenal => "0d",
            Hexadecimal => "0x",
        }
    }

    fn fmt_in_base(self, mut digits: BigUint) -> String {
        let base = self as u8;
        if digits.is_zero() {
            return "0".to_string();
        }

        let base = BigUint::from(base);
        let mut out = Vec::new();

        while !digits.is_zero() {
            let (quotient, remainder) = digits.div_rem(&base);
            let remainder = remainder
                .to_u8()
                .expect("remainder must fit in a single digit");

            let digit = match remainder {
                0..=9 => (b'0' + remainder) as char,
                10..=35 => (b'a' + (remainder - 10)) as char,
                _ => unreachable!("unsupported radix digit"),
            };
            out.push(digit);
            digits = quotient;
        }

        out.into_iter().rev().collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal<'src> {
    pub base: Base,
    pub digits: BigUint,
    pub num_digits_after_dot: Option<usize>,
    pub exponent: Option<BigInt>,
    pub suffix: &'src str,
}

impl<'src, N: Into<u128>> From<N> for Literal<'src> {
    fn from(num: N) -> Self {
        Self {
            base: Base::Decimal,
            digits: BigUint::from_u128(num.into()).expect("BigUint"),
            num_digits_after_dot: None,
            exponent: None,
            suffix: "",
        }
    }
}

impl<'src> fmt::Display for Literal<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = self.base.prefix().to_string();
        let digits = self.base.fmt_in_base(self.digits.clone());

        match self.num_digits_after_dot {
            None => out.push_str(&digits),
            Some(num_digits_after_dot) => {
                if num_digits_after_dot >= digits.len() {
                    out.push('0');
                    out.push('.');
                    out.extend(std::iter::repeat_n(
                        '0',
                        num_digits_after_dot - digits.len(),
                    ));
                    out.push_str(&digits);
                } else {
                    let split = digits.len() - num_digits_after_dot;
                    out.push_str(&digits[..split]);
                    out.push('.');
                    out.push_str(&digits[split..]);
                }
            }
        }

        if let Some(exponent) = &self.exponent {
            out.push(match self.base {
                Base::Hexadecimal => 'p',
                _ => 'e',
            });
            out.push_str(&exponent.to_string());
        }

        out.push_str(self.suffix);

        write!(f, "{out}")
    }
}
