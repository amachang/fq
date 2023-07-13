use std::{
    convert::From,
    cmp::Ordering,
    hash::{
        Hash,
        Hasher,
    },
    ops::{
        Neg,
        Add,
        Sub,
        Div,
        Rem,
        Mul,
    },
};

use regex::{
    self,
    Regex,
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

impl From<usize> for Number {
    fn from(v: usize) -> Self {
        Self::Integer(v as i64)
    }
}

impl From<i32> for Number {
    fn from(v: i32) -> Self {
        Self::Integer(v as i64)
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

impl Into<usize> for Number {
    fn into(self) -> usize {
        let i = match self {
            Self::Integer(i) => i,
            Self::Float(f) => f.round() as i64,
        };
        i as usize
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RealNumber {
    Integer(i64),
    Float(f64),
}

pub enum RealNumberError {
    InvalidFloat,
}

impl PartialOrd for RealNumber {
    fn partial_cmp(&self, number: &Self) -> Option<Ordering> {
        Some(self.cmp(number))
    }
}

impl Ord for RealNumber {
    fn cmp(&self, number: &Self) -> Ordering {
        let opt_order = match (self, number) {
            (Self::Integer(lv), Self::Integer(rv)) => Some(lv.cmp(rv)),
            (Self::Integer(lv), Self::Float(rv)) => {
                assert!(!rv.is_nan());
                (*lv as f64).partial_cmp(rv)
            }
            (Self::Float(lv), Self::Integer(rv)) => {
                assert!(!lv.is_nan());
                lv.partial_cmp(&(*rv as f64))
            }
            (Self::Float(lv), Self::Float(rv)) => {
                assert!(!lv.is_nan() && !rv.is_nan());
                lv.partial_cmp(rv)
            },
        };
        match opt_order {
            Some(order) => order,
            None => unreachable!(),
        }
    }
}

impl Eq for RealNumber {
}

impl Hash for RealNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(i) => i.hash(state),
            Self::Float(f) => f.to_bits().hash(state),
        }
    }
}

impl Into<Number> for RealNumber {
    fn into(self) -> Number {
        match self {
            Self::Integer(i) => Number::from(i),
            Self::Float(f) => Number::from(f),
        }
    }
}

impl TryFrom<Number> for RealNumber {
    type Error = RealNumberError;

    fn try_from(v: Number) -> Result<Self, Self::Error> {
        match v {
            Number::Integer(i) => Ok(RealNumber::Integer(i)),
            Number::Float(f) => {
                if f.is_nan() {
                    Err(RealNumberError::InvalidFloat)
                } else {
                    Ok(RealNumber::Float(f))
                }
            }
        }
    }
}

impl Into<Number> for &RealNumber {
    fn into(self) -> Number {
        (*self).into()
    }
}

impl ToString for RealNumber {
    fn to_string(&self) -> String {
        match self {
            Self::Integer(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PathExistence {
    Checked,
    NotChecked,
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Pattern {
    Name(String),
    Regex(String),
}

impl Pattern {
    pub fn new_regex(regex: &str) -> Pattern {
        Self::Regex("(?:".to_string() + regex + ")")
    }

    pub fn new_name(name: &str) -> Pattern {
        Self::Name(name.into())
    }

    pub fn is_fullmatch(&self, target: impl AsRef<str>) -> Result<bool, regex::Error> {
        let target = target.as_ref();
        match self {
            Self::Name(name) => Ok(name == target),
            Self::Regex(regex) => {
                let regex = Regex::new(&("^".to_string() + regex + "$"))?;
                Ok(regex.is_match(target))
            },
        }
    }

    pub fn join(patterns: &Vec<Pattern>) -> Self {
        if patterns.len() == 0 {
            return Self::new_name("")
        }
        if patterns.len() == 1 {
            return patterns[0].clone()
        }
        let pattern_strs: Vec<String> = patterns.iter().map(|p| {
            match p {
                Self::Name(name) => to_regex_string(name),
                Self::Regex(regex) => regex.clone(),
            }
        }).collect();
        Self::new_regex(&pattern_strs.join("|"))
    }
}

impl Add<&Pattern> for Pattern {
    type Output = Pattern;
    fn add(self, pattern: &Pattern) -> Self::Output {
        match (self, pattern) {
            (Self::Name(lhs), Self::Name(rhs)) => Self::Name(lhs + rhs),
            (Self::Name(lhs), Self::Regex(rhs)) => Self::Regex(to_regex_string(&lhs) + rhs),
            (Self::Regex(lhs), Self::Name(rhs)) => Self::Regex(lhs + &to_regex_string(rhs)),
            (Self::Regex(lhs), Self::Regex(rhs)) => Self::Regex(lhs + rhs),
        }
    }
}

impl Add<Pattern> for Pattern {
    type Output = Pattern;
    fn add(self, pattern: Pattern) -> Self::Output {
        self + &pattern
    }
}

impl Add<&Pattern> for &Pattern {
    type Output = Pattern;
    fn add(self, pattern: &Pattern) -> Self::Output {
        match (self, pattern) {
            (Pattern::Name(lhs), Pattern::Regex(rhs)) => Pattern::Regex(to_regex_string(lhs) + rhs),
            _ => self.clone() + pattern,
        }
    }
}

impl Add<Pattern> for &Pattern {
    type Output = Pattern;
    fn add(self, pattern: Pattern) -> Self::Output {
        self + &pattern
    }
}

impl ToString for Pattern {
    fn to_string(&self) -> String {
        self.clone().into()
    }
}

impl From<Pattern> for String {
    fn from(pattern: Pattern) -> String {
        match pattern {
            Pattern::Name(name) => name,
            Pattern::Regex(regex) => regex,
        }
    }
}

impl<'a> From<&'a Pattern> for &'a String {
    fn from(pattern: &'a Pattern) -> &'a String {
        match pattern {
            Pattern::Name(name) => name,
            Pattern::Regex(regex) => regex,
        }
    }
}

fn to_regex_string(name_string: &String) -> String {
    format!("(?:{})", regex::escape(name_string))
}

