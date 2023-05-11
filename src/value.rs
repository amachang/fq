#![deny(warnings, clippy::all, clippy::pedantic)]

use super::primitive::{
    Number,
    RealNumber,
};

use std::{
    convert::{
        From,
        Into,
    },
    cmp::Ordering,
    collections::BTreeSet,
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
    String(String),
    Set(BTreeSet<RealValue>),
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum RealValue {
    Number(RealNumber),
    Boolean(bool),
    String(String),
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

impl Into<bool> for Value {
    fn into(self) -> bool {
        (&self).into()
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
            Value::Boolean(boolean) => *boolean,
            Value::String(string) => 0 < string.len(),
            Value::Set(set) => 0 < set.len(),
        }
    }
}

impl From<Number> for Value {
    fn from(v: Number) -> Self {
        Self::Number(v)
    }
}

impl Into<Number> for Value {
    fn into(self) -> Number {
        (&self).into()
    }
}

impl Into<Number> for &Value {
    fn into(self) -> Number {
        match self {
            Value::Number(number) => *number,
            Value::Boolean(boolean) => if *boolean { Number::from(1) } else { Number::from(0) },
            Value::String(string) => {
                match string.parse::<f64>() {
                    Ok(number) => Number::from(number),
                    Err(_) => Number::from(f64::NAN),
                }
            },
            Value::Set(set) => {
                match set.iter().cloned().next() {
                    Some(real_value) => {
                        let value: Value = real_value.into();
                        let number: Number = value.into();
                        number
                    },
                    None => Number::from(f64::NAN),
                }
            }
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

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::from(v.to_string())
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        (&self).into()
    }
}

impl Into<String> for &Value {
    fn into(self) -> String {
        match self {
            Value::Number(number) => number.to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::String(string) => string.clone(),
            Value::Set(values) => {
                match values.iter().cloned().next() {
                    Some(real_value) => real_value.to_string(),
                    None => String::from(""),
                }
            },
        }
    }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(vs: [Value; N]) -> Self {
        let mut values = BTreeSet::new();
        for v in vs {
            match RealValue::new(v) {
                Some(real_value) => {
                    values.insert(real_value);
                },
                None => (),
            };
        };
        return Value::Set(values)
    }
}

impl<const N: usize> From<[i64; N]> for Value {
    fn from(is: [i64; N]) -> Self {
        let mut values = BTreeSet::new();
        for i in is {
            match RealNumber::new(Number::from(i)) {
                Some(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                None => (),
            };
        };
        return Value::Set(values)
    }
}

impl<const N: usize> From<[f64; N]> for Value {
    fn from(fs: [f64; N]) -> Self {
        let mut values = BTreeSet::new();
        for f in fs {
            match RealNumber::new(Number::from(f)) {
                Some(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                None => (),
            };
        };
        return Value::Set(values)
    }
}

impl<const N: usize> From<[bool; N]> for Value {
    fn from(bs: [bool; N]) -> Self {
        let mut values = BTreeSet::new();
        for b in bs {
            values.insert(RealValue::Boolean(b));
        };
        return Value::Set(values)
    }
}

impl<const N: usize> From<[&str; N]> for Value {
    fn from(ss: [&str; N]) -> Self {
        let mut values = BTreeSet::new();
        for s in ss {
            values.insert(RealValue::String(s.to_string()));
        };
        return Value::Set(values)
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

impl RealValue {
    pub fn new(v: Value) -> Option<Self> {
        match v {
            Value::Number(number) => {
                match RealNumber::new(number) {
                    Some(real_number) => Some(Self::Number(real_number)),
                    None => None,
                }
            }
            Value::Boolean(boolean) => Some(Self::Boolean(boolean)),
            Value::String(string) => Some(Self::String(string)),
            Value::Set(set) => {
                set.iter().cloned().next()
            },
        }
    }
}

impl PartialOrd for RealValue {
    fn partial_cmp(&self, v: &Self) -> Option<Ordering> {
        Some(self.cmp(v))
    }
}

impl Ord for RealValue {
    fn cmp(&self, v: &Self) -> Ordering {
        let r_str: String = self.into();
        let l_str: String = v.into();
        r_str.cmp(&l_str)
    }
}

impl Into<Value> for RealValue {
    fn into(self) -> Value {
        (&self).into()
    }
}

impl Into<Value> for &RealValue {
    fn into(self) -> Value {
        match self {
            RealValue::Number(real_number) => {
                let number: Number = real_number.into();
                Value::from(number)
            }
            RealValue::Boolean(boolean) => Value::from(*boolean),
            RealValue::String(string) => Value::from(string.clone()),
        }
    }
}

impl Into<String> for RealValue {
    fn into(self) -> String {
        (&self).into()
    }
}

impl Into<String> for &RealValue {
    fn into(self) -> String {
        match self {
            RealValue::Number(real_number) => real_number.to_string(),
            RealValue::Boolean(boolean) => boolean.to_string(),
            RealValue::String(string) => string.clone(),
        }
    }
}

impl ToString for RealValue {
    fn to_string(&self) -> String {
        let str: String = self.into();
        str
    }
}
