#![deny(warnings, clippy::all, clippy::pedantic)]

use super::primitive::{
    Number,
    RealNumber,
    RealNumberError,
};

use std::{
    convert::{
        From,
        Into,
    },
    cmp::Ordering,
    collections::{
        BTreeSet,
        btree_set,
    },
    ops::{
        Neg,
        Add,
        Sub,
        Div,
        Rem,
        Mul,
    },
    path::PathBuf,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Number),
    Boolean(bool),
    String(String),
    Path(PathBuf),
    Set(BTreeSet<RealValue>),
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum RealValue {
    Number(RealNumber),
    Boolean(bool),
    Path(PathBuf),
    String(String),
}

pub enum RealValueError {
    InvalidFloatNumber,
    EmptySet,
}

impl Value {
    pub fn is_nan(&self) -> bool {
        let v: Number = self.into();
        v.is_nan()
    }
}

impl Value {
    pub fn as_string(self) -> Self {
        match self {
            Value::String(_) => self,
            _ => {
                let v: String = self.into();
                Value::from(v)
            },
        }
    }

    pub fn as_number(self) -> Self {
        match self {
            Value::Number(_) => self,
            _ => {
                let v: Number = self.into();
                Value::from(v)
            },
        }
    }

    pub fn as_boolean(self) -> Self {
        match self {
            Value::Boolean(_) => self,
            _ => {
                let v: bool = self.into();
                Value::from(v)
            },
        }
    }

    pub fn as_path(self) -> Self {
        match self {
            Value::Path(_) => self,
            _ => {
                let v: PathBuf = self.into();
                Value::from(v)
            },
        }
    }

    pub fn as_set(self) -> Self {
        match self {
            Value::Set(_) => self,
            _ => Value::from([self.clone()]),
        }
    }

    pub fn join_path(dir_path_value: &Value, path_component_value: &Value) -> Value {
        let dir_path: PathBuf = dir_path_value.into();
        let path_component: PathBuf = path_component_value.into();
        Value::from(dir_path.join(path_component))
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
            Value::Path(_) => true,
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
            Value::String(string) => match string.parse::<f64>() {
                Ok(number) => Number::from(number),
                Err(_) => Number::from(f64::NAN),
            },
            Value::Path(path) => {
                let string: String = path.to_string_lossy().into_owned();
                match string.parse::<f64>() {
                    Ok(number) => Number::from(number),
                    Err(_) => Number::from(f64::NAN),
                }
            },
            Value::Set(set) => match set.iter().cloned().next() {
                Some(real_value) => {
                    let value: Value = real_value.into();
                    let number: Number = value.into();
                    number
                },
                None => Number::from(f64::NAN),
            },
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
            Value::Path(path) => path.to_string_lossy().into_owned(), // TODO String should be
                                                                      // contains OsString
            Value::Set(values) => {
                match values.iter().cloned().next() {
                    Some(real_value) => {
                        let value: Value = real_value.into();
                        let string: String = value.into();
                        string
                    },
                    None => String::from(""),
                }
            },
        }
    }
}

impl From<PathBuf> for Value {
    fn from(path: PathBuf) -> Self {
        Self::Path(path)
    }
}

impl From<Vec<Value>> for Value {
    fn from(vs: Vec<Value>) -> Self {
        let real_values = vs.iter().flat_map(|v| {
            match v {
                Value::Set(new_real_values) => new_real_values.clone(),
                _ => {
                    match RealValue::try_from(v) {
                        Ok(real_value) => {
                            let mut set = BTreeSet::new();
                            set.insert(real_value);
                            set
                        },
                        Err(RealValueError::InvalidFloatNumber) => BTreeSet::new(),
                        Err(RealValueError::EmptySet) => unreachable!(),
                    }
                },
            }
        }).collect();
        return Value::Set(real_values)
    }
}

impl Into<PathBuf> for Value {
    fn into(self) -> PathBuf {
        (&self).into()
    }
}

impl Into<PathBuf> for &Value {
    fn into(self) -> PathBuf {
        match self {
            Value::Number(number) => PathBuf::from(number.to_string()),
            Value::Boolean(boolean) => PathBuf::from(boolean.to_string()),
            Value::String(string) => PathBuf::from(string),
            Value::Path(path) => path.clone(),
            Value::Set(values) => {
                match values.iter().cloned().next() {
                    Some(real_value) => {
                        let value: Value = real_value.into();
                        let path: PathBuf = value.into();
                        path
                    },
                    None => PathBuf::from("."),
                }
            },
        }
    }
}

impl From<&[Value]> for Value {
    fn from(vs: &[Value]) -> Self {
        Value::from(vs.to_vec())
    }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(vs: [Value; N]) -> Self {
        let vs: &[Value] = &vs;
        Value::from(vs)
    }
}

impl<const N: usize> From<[i64; N]> for Value {
    fn from(is: [i64; N]) -> Self {
        let mut values = BTreeSet::new();
        for i in is {
            match RealNumber::try_from(Number::from(i)) {
                Ok(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                Err(_) => (),
            };
        };
        return Value::Set(values)
    }
}

impl<const N: usize> From<[f64; N]> for Value {
    fn from(fs: [f64; N]) -> Self {
        let mut values = BTreeSet::new();
        for f in fs {
            match RealNumber::try_from(Number::from(f)) {
                Ok(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                Err(_) => (),
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

impl<const N: usize> From<[PathBuf; N]> for Value {
    fn from(ps: [PathBuf; N]) -> Self {
        let mut values = BTreeSet::new();
        for p in ps {
            values.insert(RealValue::Path(p));
        };
        return Value::Set(values)
    }
}

pub enum ValueIterator<'a> {
    SetIterator(btree_set::Iter<'a, RealValue>),
    SingleIterator(&'a Value, bool),
}

impl<'a> Iterator for ValueIterator<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {

        match self {
            Self::SetIterator(iter) => {
                let real_value = iter.next()?;
                let value: Value = real_value.into();
                Some(value)
            },
            Self::SingleIterator(value, false) => {
                let result_value = (*value).clone();
                *self = Self::SingleIterator(*value, true);
                Some(result_value)
            },
            Self::SingleIterator(_, true) => None,
        }
    }
}

impl<'a> IntoIterator for &'a Value {
    type Item = Value;
    type IntoIter = ValueIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Value::Set(set) => ValueIterator::SetIterator(set.iter()),
            _ => return ValueIterator::SingleIterator(self, false),
        }
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

impl PartialOrd for RealValue {
    fn partial_cmp(&self, v: &Self) -> Option<Ordering> {
        Some(self.cmp(v))
    }
}

impl Ord for RealValue {
    fn cmp(&self, v: &Self) -> Ordering {
        let rv: Value = self.into();
        let r_str: String = rv.into();
        let lv: Value = v.into();
        let l_str: String = lv.into();
        r_str.cmp(&l_str)
    }
}

impl TryFrom<&Value> for RealValue {
    type Error = RealValueError;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        match v {
            Value::Number(number) => {
                match RealNumber::try_from(*number) {
                    Ok(real_number) => Ok(Self::Number(real_number)),
                    Err(RealNumberError::InvalidFloat) => Err(Self::Error::InvalidFloatNumber),
                }
            }
            Value::Boolean(boolean) => Ok(Self::Boolean(*boolean)),
            Value::String(string) => Ok(Self::String(string.clone())),
            Value::Path(path) => Ok(Self::Path(path.clone())),
            Value::Set(set) => {
                match set.iter().cloned().next() {
                    Some(real_value) => Ok(real_value),
                    None => Err(Self::Error::EmptySet),
                }
            },
        }
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
            RealValue::Path(path) => Value::from(path.clone()),
            RealValue::Boolean(boolean) => Value::from(*boolean),
            RealValue::String(string) => Value::from(string.clone()),
        }
    }
}

