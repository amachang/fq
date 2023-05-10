#![deny(warnings, clippy::all, clippy::pedantic)]

use std::{
    convert::From,
    cmp::Ordering,
    ops::{
        Neg,
        Add,
        Sub,
        Div,
        Rem,
        Mul,
    },
};

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

impl Number {
    pub fn is_nan(self) -> bool {
        match self {
            Self::Integer(_) => false,
            Self::Float(v) => v.is_nan(),
        }
    }
}

impl From<i64> for Number {
    fn from(v: i64) -> Self {
        Self::Integer(v)
    }
}

impl From<f64> for Number {
    fn from(f: f64) -> Self {
        if f == f.floor() as i64 as f64 {
            Number::Integer(f as i64)
        } else {
            Number::Float(f as f64)
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, number: &Self) -> Option<Ordering> {
        match (self, number) {
            (Self::Integer(lv), Self::Integer(rv)) => Some(lv.cmp(rv)),
            (Self::Integer(lv), Self::Float(rv)) => (*lv as f64).partial_cmp(rv),
            (Self::Float(lv), Self::Integer(rv)) => lv.partial_cmp(&(*rv as f64)),
            (Self::Float(lv), Self::Float(rv)) => lv.partial_cmp(rv),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, number: &Self) -> bool {
        self.partial_cmp(number) == Some(Ordering::Equal)
    }
}

impl ToString for Number {
    fn to_string(&self) -> String {
        match self {
            Self::Integer(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
        }
    }
}

impl Neg for Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(v) => {
                match v.checked_neg() {
                    Some(r) => Self::Integer(r),
                    None => Self::Float(-(v as f64)),
                }
            }
            Self::Float(v) => Self::Float(-v),
        }
    }
}

impl Neg for &Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        -(*self)
    }
}

macro_rules! number_binary_operator_overload {
    ( $op_trait:ident, $op_fn:ident, $checked_fn:ident, $op:tt) => (
        impl $op_trait<Number> for Number {
            type Output = Number;

            fn $op_fn(self, number: Number) -> Self::Output {
                match (self, number) {
                    (Self::Integer(lv), Self::Integer(rv)) => {
                        match lv.$checked_fn(rv) {
                            Some(r) => Self::Integer(r),
                            None => Self::Float(lv as f64 $op rv as f64),
                        }
                    },
                    (Self::Integer(lv), Self::Float(rv)) => Self::Float(lv as f64 $op rv),
                    (Self::Float(lv), Self::Integer(rv)) => Self::Float(lv $op rv as f64),
                    (Self::Float(lv), Self::Float(rv)) => Self::Float(lv $op rv),
                }
            }
        }

        impl $op_trait<&Number> for Number {
            type Output = Number;

            fn $op_fn(self, number: &Number) -> Self::Output {
                self + *number
            }
        }

        impl $op_trait<Number> for &Number {
            type Output = Number;

            fn $op_fn(self, number: Number) -> Self::Output {
                *self + number
            }
        }

        impl $op_trait<&Number> for &Number {
            type Output = Number;

            fn $op_fn(self, number: &Number) -> Self::Output {
                *self + *number
            }
        }

    );
}

number_binary_operator_overload!(Add, add, checked_add, +);
number_binary_operator_overload!(Sub, sub, checked_sub, -);
number_binary_operator_overload!(Div, div, checked_div, /);
number_binary_operator_overload!(Rem, rem, checked_rem, %);
number_binary_operator_overload!(Mul, mul, checked_mul, *);

