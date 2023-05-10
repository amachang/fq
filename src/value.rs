#![deny(warnings, clippy::all, clippy::pedantic)]

use super::primitive::Number;

use std::{
    convert::{
        From,
        Into,
    },
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

#[derive(Debug, PartialEq)]
pub enum Value {
    Number(Number),
    Boolean(bool),
}

impl Value {
    pub fn is_nan(&self) -> bool {
        let v: Number = self.into();
        v.is_nan()
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Boolean(v)
    }
}

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match self {
            Value::Number(number) => {
                match number {
                    Number::Float(f) => !f.is_nan() && *f != 0.0,
                    Number::Integer(i) => *i != 0,
                }
            },
            Value::Boolean(primitive_boolean) => *primitive_boolean,
        }
    }
}

impl From<Number> for Value {
    fn from(v: Number) -> Self {
        Self::Number(v)
    }
}

impl Into<Number> for &Value {
    fn into(self) -> Number {
        match self {
            Value::Number(number) => *number,
            Value::Boolean(primitive_boolean) => if *primitive_boolean { Number::Integer(1) } else { Number::Integer(0) },
        }
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Self::Number(Number::from(v))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Number(Number::from(v))
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, number: &Self) -> Option<Ordering> {
        let rv: Number = self.into();
        let lv: Number = number.into();
        return rv.partial_cmp(&lv)
    }
}

macro_rules! value_unary_operator {
    ( $op_trait:ident, $op_fn:ident, $operand_type:ty, $op:tt) => (
        impl $op_trait for &Value {
            type Output = Value;

            fn $op_fn(self) -> Self::Output {
                let v: $operand_type = self.into();
                Value::from($op v)
            }
        }
    );
}

value_unary_operator!(Neg, neg, Number, -);

macro_rules! value_binary_operator {
    ( $op_trait:ident, $op_fn:ident, $left_type:ty, $right_type:ty, $op:tt) => (
        impl $op_trait for &Value {
            type Output = Value;

            fn $op_fn(self, value: &Value) -> Self::Output {
                let lv: $left_type = self.into();
                let rv: $right_type = value.into();
                Value::from(lv $op rv)
            }
        }
    );
}

value_binary_operator!(Add, add, Number, Number, +);
value_binary_operator!(Sub, sub, Number, Number, -);
value_binary_operator!(Div, div, Number, Number, /);
value_binary_operator!(Rem, rem, Number, Number, %);
value_binary_operator!(Mul, mul, Number, Number, *);

