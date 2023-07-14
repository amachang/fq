use super::primitive::{
    Number,
    RealNumber,
    RealNumberError,
    PathExistence,
    PathInfo,
    Pattern,
};

use std::{
    convert::{
        From,
        Into,
    },
    cmp::Ordering,
    collections::{
        HashSet,
        hash_set,
    },
    ops::{
        Neg,
        Add,
        Sub,
        Div,
        Rem,
        Mul,
    },
    borrow::Cow,
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub enum Value {
    Number(Number),
    Boolean(bool),
    String(String),
    Path(PathInfo),
    Pattern(Pattern),
    Set(HashSet<RealValue>, PathExistence),
}

impl PartialEq for Value {
    fn eq(&self, value: &Self) -> bool {
        use Value::*;
        match (self, value) {
            (Number(r), Number(l)) => r == l,
            (Boolean(r), Boolean(l)) => r == l,
            (String(r), String(l)) => r == l,
            (Set(r, _), Set(l, _)) => r == l,
            (Path(r), Path(l)) => r == l,
            (_, Number(_)) => match self.as_number() {
                Cow::Borrowed(number_value) => number_value == value,
                Cow::Owned(number_value) => &number_value == value,
            },
            (Number(_), _) => match value.as_number() {
                Cow::Borrowed(number_value) => self == number_value,
                Cow::Owned(number_value) => self == &number_value,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
pub enum RealValue {
    Number(RealNumber),
    Boolean(bool),
    Path(PathInfo),
    String(String),
    Pattern(Pattern),
}

pub enum RealValueError {
    InvalidFloatNumber,
    EmptySet,
}


impl Value {
    pub fn empty_set() -> Value {
        return Value::Set(HashSet::new(), PathExistence::NotChecked);
    }

    pub fn is_nan(&self) -> bool {
        let v: Number = self.into();
        v.is_nan()
    }

    pub fn as_string(&self) -> Cow<Self> {
        match self {
            Value::String(_) => Cow::Borrowed(self),
            _ => {
                let v: String = self.into();
                Cow::Owned(Value::from(v))
            },
        }
    }

    pub fn as_number(&self) -> Cow<Self> {
        match self {
            Value::Number(_) => Cow::Borrowed(self),
            _ => {
                let v: Number = self.into();
                Cow::Owned(Value::from(v))
            },
        }
    }

    pub fn as_boolean(&self) -> Cow<Self> {
        match self {
            Value::Boolean(_) => Cow::Borrowed(self),
            _ => {
                let v: bool = self.into();
                Cow::Owned(Value::from(v))
            },
        }
    }

    pub fn as_path(&self) -> Cow<Self> {
        match self {
            Value::Path(_) => Cow::Borrowed(self),
            _ => {
                let v: PathInfo = self.into();
                Cow::Owned(Value::from(v))
            },
        }
    }

    pub fn as_set(&self) -> Cow<Self> {
        match self {
            Value::Set(_, _) => Cow::Borrowed(self),
            _ => Cow::Owned(Value::from([self.clone()])),
        }
    }

    pub fn path_existence(&self) -> PathExistence {
        match self {
            Value::Set(_, existence) => *existence,
            _ => PathExistence::NotChecked,
        }
    }

    pub fn likes_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            Value::Boolean(_) => true,
            Value::String(_) => false,
            Value::Pattern(_) => false,
            Value::Path(path) => {
                let string = convert_path_to_string(path);
                match string.parse::<f64>() {
                    Ok(_) => true,
                    Err(_) => false,
                }
            },
            Value::Set(set, _) => convert_set_to_single_value(set).map_or(false, |v| v.likes_number()),
        }
    }

    pub fn is_set(&self) -> bool {
        match self {
            Value::Number(_) => false,
            Value::Boolean(_) => false,
            Value::String(_) => false,
            Value::Path(_) => false,
            Value::Pattern(_) => false,
            Value::Set(_, _) => true,
        }
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
            Value::Number(number) => convert_number_to_boolean(*number),
            Value::Boolean(boolean) => *boolean,
            Value::String(string) => 0 < string.len(),
            Value::Pattern(pattern) => 0 < pattern.to_string().len(),
            Value::Path(path) => {
                let string = convert_path_to_string(path);
                let number = convert_string_to_number(&string);
                convert_number_to_boolean(number)
            },
            Value::Set(set, _) => 0 < set.len(),
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
            Value::String(string) => convert_string_to_number(string),
            Value::Pattern(pattern) => convert_string_to_number(pattern.into()),
            Value::Path(path) => {
                let string = convert_path_to_string(path);
                convert_string_to_number(&string)
            },
            Value::Set(set, _) => convert_set_to_single_value(set).map_or(Number::from(f64::NAN), |v| v.into()),
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

impl From<usize> for Value {
    fn from(v: usize) -> Self {
        Self::Number(Number::from(v))
    }
}

impl Into<usize> for &Value {
    fn into(self) -> usize {
        let number: Number = self.into();
        let i: usize = number.into();
        i
    }
}

impl Into<usize> for Value {
    fn into(self) -> usize {
        (&self).into()
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
            Value::Pattern(pattern) => pattern.to_string(),
            Value::Path(path) => path.to_string(),
            Value::Set(set, _) => convert_set_to_single_value(set).map_or(String::from(""), |v| v.into()),
        }
    }
}

impl From<PathInfo> for Value {
    fn from(path: PathInfo) -> Self {
        Self::Path(path)
    }
}

impl From<Vec<Value>> for Value {
    fn from(vs: Vec<Value>) -> Self {
        let (real_values, existence) = vs.iter().fold((HashSet::new(), PathExistence::Checked), |(mut all_real_values, all_existence), v| {
            match v {
                Value::Set(new_real_values, existence) => {
                    all_real_values.extend(new_real_values.clone());
                    (all_real_values, match all_existence { PathExistence::Checked => *existence, _ => PathExistence::NotChecked })
                },
                _ => {
                    match RealValue::try_from(v) {
                        Ok(real_value) => {
                            all_real_values.insert(real_value);
                            (all_real_values, PathExistence::NotChecked)
                        },
                        Err(RealValueError::InvalidFloatNumber) => (all_real_values, PathExistence::NotChecked),
                        Err(RealValueError::EmptySet) => unreachable!(),
                    }
                },
            }
        });
        return Value::Set(real_values, existence)
    }
}

impl From<Value> for PathInfo {
    fn from(v: Value) -> Self {
        (&v).into()
    }
}

impl From<&Value> for PathInfo {
    fn from(v: &Value) -> PathInfo {
        match v {
            Value::Number(number) => Self::from(number.to_string()),
            Value::Boolean(boolean) => Self::from(boolean.to_string()),
            Value::String(string) => Self::from(string),
            Value::Pattern(pattern) => {
                match pattern {
                    Pattern::Name(name) => Self::from(name),
                    Pattern::Regex(_) => Self::from(""),
                }
            },
            Value::Path(path) => path.clone(),
            Value::Set(set, _) => convert_set_to_single_value(set).map_or(Self::from(""), |v| v.into()),
        }
    }
}

impl From<Value> for PathBuf {
    fn from(v: Value) -> Self {
        (&v).into()
    }
}

impl From<&Value> for PathBuf {
    fn from(v: &Value) -> PathBuf {
        let path: PathInfo = v.into();
        path.into()
    }
}

impl From<Pattern> for Value {
    fn from(v: Pattern) -> Self {
        Self::Pattern(v)
    }
}

impl Into<Pattern> for Value {
    fn into(self) -> Pattern {
        match self {
            Value::Number(number) => Pattern::Name(number.to_string()),
            Value::Boolean(boolean) => Pattern::Name(boolean.to_string()),
            Value::String(string) => Pattern::Name(string),
            Value::Path(path) => Pattern::Name(convert_path_to_string(&path)),
            Value::Pattern(pattern) => pattern,
            Value::Set(set, _) => {
                let patterns: Vec<Pattern> = set.into_iter().map(|real_value| {
                    match real_value {
                        RealValue::Pattern(pattern) => pattern,
                        _ => {
                            let value: Value = real_value.into();
                            value.into()
                        },
                    }
                }).collect();
                Pattern::join(&patterns)
            },
        }
    }
}

impl Into<Pattern> for &Value {
    fn into(self) -> Pattern {
        self.clone().into()
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
        let mut values = HashSet::new();
        for i in is {
            match RealNumber::try_from(Number::from(i)) {
                Ok(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                Err(_) => (),
            };
        };
        return Value::Set(values, PathExistence::NotChecked)
    }
}

impl<const N: usize> From<[f64; N]> for Value {
    fn from(fs: [f64; N]) -> Self {
        let mut values = HashSet::new();
        for f in fs {
            match RealNumber::try_from(Number::from(f)) {
                Ok(real_number) => {
                    values.insert(RealValue::Number(real_number));
                },
                Err(_) => (),
            };
        };
        return Value::Set(values, PathExistence::NotChecked)
    }
}

impl<const N: usize> From<[bool; N]> for Value {
    fn from(bs: [bool; N]) -> Self {
        let mut values = HashSet::new();
        for b in bs {
            values.insert(RealValue::Boolean(b));
        };
        return Value::Set(values, PathExistence::NotChecked)
    }
}

impl<const N: usize> From<[&str; N]> for Value {
    fn from(ss: [&str; N]) -> Self {
        let mut values = HashSet::new();
        for s in ss {
            values.insert(RealValue::String(s.to_string()));
        };
        return Value::Set(values, PathExistence::NotChecked)
    }
}

impl<const N: usize> From<[PathInfo; N]> for Value {
    fn from(ps: [PathInfo; N]) -> Self {
        let mut values = HashSet::new();
        for p in ps {
            values.insert(RealValue::Path(p));
        };
        return Value::Set(values, PathExistence::NotChecked)
    }
}

pub enum ValueIterator<'a> {
    SetIterator(hash_set::Iter<'a, RealValue>),
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
            Value::Set(set, _) => ValueIterator::SetIterator(set.iter()),
            _ => return ValueIterator::SingleIterator(self, false),
        }
    }
}

impl Into<HashSet<RealValue>> for Value {
    fn into(self) -> HashSet<RealValue> {
        (&self).into()
    }
}

impl Into<HashSet<RealValue>> for &Value {
    fn into(self) -> HashSet<RealValue> {
        match self {
            Value::Set(set, _) => set.clone(),
            _ => self.as_set().into_owned().into(),
        }
    }
}

fn convert_set_to_single_value(set: &HashSet<RealValue>) -> Option<Value> {
    Some(set.iter().cloned().next()?.into())
}

fn convert_path_to_string(path: &PathInfo) -> String {
    path.to_string()
}

fn convert_string_to_number(string: &String) -> Number {
    match string.parse::<f64>() {
        Ok(number) => Number::from(number),
        Err(_) => Number::from(f64::NAN),
    }
}

fn convert_number_to_boolean(number: Number) -> bool {
    match number {
        Number::Float(f) => !f.is_nan() && f != 0.0,
        Number::Integer(i) => i != 0,
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
            Value::Pattern(pattern) => Ok(Self::Pattern(pattern.clone())),
            Value::Path(path) => Ok(Self::Path(path.clone())),
            Value::Set(set, _) => {
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
        match self {
            RealValue::Number(real_number) => {
                let number: Number = real_number.into();
                Value::from(number)
            }
            RealValue::Path(path) => Value::from(path),
            RealValue::Boolean(boolean) => Value::from(boolean),
            RealValue::String(string) => Value::from(string),
            RealValue::Pattern(pattern) => Value::from(pattern),
        }
    }
}

impl Into<Value> for &RealValue {
    fn into(self) -> Value {
        self.clone().into()
    }
}

