#![deny(warnings, clippy::all, clippy::pedantic)]

use super::{
    parse_util::{
        parse_space,
        parse_identifier,
    },
    value::{
        Value,
        RealValue,
        RealValueError,
    },
    primitive::{
        Number,
    },
};

use std::{
    fmt::Debug,
    collections::{
        BTreeSet,
        HashMap,
    },
};

use nom::{
    IResult,
    bytes::complete::{
        tag,
        take_until,
    },
    number::complete::float,
    sequence::preceded,
    branch::alt,
    Err,
    error::{
        ParseError,
        ErrorKind,
    },
};

pub struct EvaluationContext {
    pub(crate) fn_map: HashMap<String, fn(&[Value]) -> Value>,
    pub(crate) var_map: HashMap<String, Value>,
}

impl EvaluationContext {
    pub fn new() -> EvaluationContext {
        let mut ctx = EvaluationContext { fn_map: HashMap::new(), var_map: HashMap::new() };
        ctx.register_function("string", |vs| {
            if vs.len() < 1 {
                Value::from("")
            } else {
                let v: String = (&vs[0]).into();
                Value::from(v)
            }
        });
        ctx.register_function("number", |vs| {
            if vs.len() < 1 {
                Value::from(f64::NAN)
            } else {
                let v: Number = (&vs[0]).into();
                Value::from(v)
            }
        });
        ctx.register_function("bool", |vs| {
            if vs.len() < 1 {
                Value::from(false)
            } else {
                let v: bool = (&vs[0]).into();
                Value::from(v)
            }
        });
        ctx.register_function("set", |vs| {
            let mut real_values: BTreeSet<RealValue> = BTreeSet::new();
            for v in vs {
                match v {
                    Value::Set(new_real_values) => {
                        real_values.extend(new_real_values.clone());
                    },
                    _ => {
                        match RealValue::try_from(v) {
                            Ok(real_value) => {
                                real_values.insert(real_value);
                            },
                            Err(RealValueError::InvalidFloatNumber) => (),
                            Err(RealValueError::EmptySet) => unreachable!(),
                        };
                    },
                };
            };
            Value::Set(real_values)
        });
        ctx
    }

    pub fn register_function(&mut self, key: impl AsRef<str>, function: fn(&[Value]) -> Value) {
        self.fn_map.insert(key.as_ref().to_string(), function);
    }

    pub fn get_function(&self, key: impl AsRef<str>) -> Option<&fn(&[Value]) -> Value> {
        self.fn_map.get(key.as_ref())
    }

    pub fn set_variable(&mut self, key: impl AsRef<str>, value: Value) {
        self.var_map.insert(key.as_ref().to_string(), value);
    }

    pub fn get_variable(&self, key: impl AsRef<str>) -> Option<&Value> {
        self.var_map.get(key.as_ref())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    FunctionNotFound(String),
}

pub trait Expr: Debug {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error>;
}

impl PartialEq for dyn Expr {
    fn eq(&self, _: &dyn Expr) -> bool {
        unreachable!()
    }
}

pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
    UnionExpr::parse(i)
}

pub fn parse_expr_list<'a>(delimiter: &'static str, i: &'a str, parse: fn(i: &str) -> IResult<&str, Box<dyn Expr>>) -> IResult<&'a str, Vec<Box<dyn Expr>>> {
    let mut exprs: Vec<Box<dyn Expr>> = Vec::new();
    let mut next_i = i;
    loop {
        let i = next_i;
        let (i, expr) = match parse(i) {
            Ok(r) => r,
            Err(Err::Error(_)) => break,
            Err(e) => return Err(e),
        };
        next_i = i;
        exprs.push(expr);

        let (i, _) = match preceded(parse_space, tag(delimiter))(i) {
            Ok(r) => r,
            Err(Err::Error(_)) => break,
            Err(e) => return Err(e),
        };
        next_i = i
    };
    Ok((next_i, exprs))
}

#[derive(Debug, PartialEq)]
pub struct UnionExpr {
    exprs: Vec<Box<dyn Expr>>,
}

impl UnionExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, exprs) = parse_expr_list("|", i, BinaryExpr::parse)?;

        if 0 == exprs.len() {
            Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
        } else if 1 == exprs.len() {
            let expr = exprs.into_iter().next().unwrap();
            Ok((i, expr))
        } else {
            Ok((i, Box::new(UnionExpr { exprs })))
        }
    }
}

impl Expr for UnionExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let mut real_values: BTreeSet<RealValue> = BTreeSet::new();
        for expr in &self.exprs {
            let value = expr.evaluate(ctx)?;
            match value {
                Value::Set(new_real_values) => {
                    real_values.extend(new_real_values);
                },
                _ => {
                    match RealValue::try_from(&value) {
                        Ok(real_value) => {
                            real_values.insert(real_value);
                        },
                        Err(RealValueError::InvalidFloatNumber) => (),
                        Err(RealValueError::EmptySet) => unreachable!(),
                    };
                },
            };
        };
        Ok(Value::Set(real_values))
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: UnaryOperator,
    expr: Box<dyn Expr>,
}

impl UnaryExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        match UnaryOperator::parse(i) {
            Ok((i, op)) => {
                let (i, expr) = UnaryExpr::parse(i)?;
                Ok((i, Box::new(UnaryExpr { op, expr })))
            }
            Err(Err::Error(_)) => alt((
                LiteralString::parse,
                LiteralNumber::parse,
                FunctionCall::parse,
            ))(i),
            Err(e) => Err(e),
        }
    }
}

impl Expr for UnaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        Ok(self.op.evaluate(&self.expr.evaluate(ctx)?))
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
}

impl UnaryOperator {
    fn parse(i: &str) -> IResult<&str, Self> {
        let op_map = [
            ("-", Self::Minus),
        ];

        for (op_str, op) in op_map {
            match preceded(parse_space, tag(op_str))(i) {
                Ok((i, _)) => return Ok((i, op)),
                Err(Err::Error(_)) => (),
                Err(e) => return Err(e),
            };
        }

        Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
    }

    pub fn evaluate(&self, v: &Value) -> Value {
        match self {
            Self::Minus => -v,
        }
    }
}

impl PartialEq for UnaryExpr {
    fn eq(&self, expr: &Self) -> bool {
        self.op == expr.op && *self.expr == *expr.expr
    }
}

#[derive(Debug, PartialEq)]
pub struct LiteralNumber {
    number: Number,
}

impl LiteralNumber {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, f) = preceded(parse_space, float)(i)?;
        let number = Number::from(f as f64);
        Ok((i, Box::new(LiteralNumber { number })))
    }
}

impl Expr for LiteralNumber {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, Error> {
        Ok(Value::Number(self.number))
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    identifier: String,
    arg_exprs: Vec<Box<dyn Expr>>,
}

impl FunctionCall {
    pub fn parse<'a>(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, identifier) = preceded(
            parse_space,
            parse_identifier,
        )(i)?;
        let (i, _) = preceded(
            parse_space,
            tag("("),
        )(i)?;

        let (i, arg_exprs) = parse_expr_list(",", i, UnionExpr::parse)?;

        let (i, _) = match preceded(parse_space, tag(")"))(i) {
            Ok(r) => r,
            Err(Err::Error(e)) => return Err(Err::Failure(e)),
            Err(e) => return Err(e),
        };

        let identifier = identifier.to_string();
        Ok((i, Box::new(FunctionCall { identifier, arg_exprs })))
    }
}

impl Expr for FunctionCall {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let function = match ctx.get_function(&self.identifier) {
            Some(function) => function,
            None => return Err(Error::FunctionNotFound(self.identifier.clone())),
        };
        let mut arg_values: Vec<Value> = Vec::new();
        for expr in &self.arg_exprs {
            let value = expr.evaluate(ctx)?;
            arg_values.push(value);
        }
        let return_value = function(&arg_values);
        Ok(return_value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralString {
    string: String,
}

impl LiteralString {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, quote_char) = preceded(
            parse_space,
            alt((tag("\""), tag("'")))
        )(i)?;
        let (i, string) = take_until(quote_char)(i)?;
        let (i, _) = match preceded(parse_space, tag(quote_char))(i) {
            Ok(r) => r,
            Err(Err::Error(e)) => return Err(Err::Failure(e)),
            Err(e) => return Err(e),
        };
        Ok((i, Box::new(LiteralString { string: string.to_string() })))
    }
}

impl Expr for LiteralString {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, Error> {
        Ok(Value::String(self.string.clone()))
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    op: BinaryOperator,
    left_expr: Box<dyn Expr>,
    right_expr: Box<dyn Expr>,
}

impl BinaryExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let mut stack: Vec<(&str, BinaryOperator, Box<dyn Expr>)> = Vec::new();
        let mut current_i = i;
        let (i, expr) = loop {
            let i = current_i;
            let (before_op_i, new_expr) = match UnaryExpr::parse(i) {
                Ok(r) => r,
                Err(Err::Error(err)) => {
                    match stack.pop() {
                        Some((i, _, expr)) => break (i, expr),
                        None => return Err(Err::Error(err)),
                    }
                },
                Err(e) => return Err(e),
            };
            let (after_op_i, new_op) = match BinaryOperator::parse(before_op_i) {
                Ok(r) => r,
                Err(Err::Error(_)) => break (before_op_i, new_expr),
                Err(e) => return Err(e),
            };

            let mut expr = new_expr;
            loop {
                match stack.pop() {
                    Some((i, stacked_op, stacked_expr)) => {
                        if new_op.precedence() <= stacked_op.precedence() {
                            expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr })
                        }
                        else {
                            stack.push((i, stacked_op, stacked_expr));
                            break;
                        }
                    },
                    None => break,
                };
            };

            stack.push((before_op_i, new_op, expr));
            current_i = after_op_i;
        };

        let mut expr = expr;
        loop {
            match stack.pop() {
                Some((_, stacked_op, stacked_expr)) => {
                    expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr });
                },
                None => break,
            };
        };

        Ok((i, expr))
    }
}

impl Expr for BinaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        Ok(self.op.evaluate(&self.left_expr.evaluate(ctx)?, &self.right_expr.evaluate(ctx)?))
    }
}

impl PartialEq for BinaryExpr {
    fn eq(&self, expr: &Self) -> bool {
        self.op == expr.op && *self.left_expr == *expr.left_expr && *self.right_expr == *expr.right_expr
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Division,
    Modulus,
    Multiplication,

    Addition,
    Subtraction,

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Equal,
    NotEqual,

    And,
    Or,
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            Self::Division | Self::Modulus | Self::Multiplication => 6,
            Self::Addition | Self::Subtraction => 5,
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => 4,
            Self::Equal | Self::NotEqual => 3,
            Self::And => 2,
            Self::Or => 1,
        }
    }

    fn parse(i: &str) -> IResult<&str, Self> {
        let op_map = [
            ("div", Self::Division),
            ("%", Self::Modulus),
            ("*", Self::Multiplication),
            ("+", Self::Addition),
            ("-", Self::Subtraction),
            ("<", Self::LessThan),
            ("<=", Self::LessThanEqual),
            (">", Self::GreaterThan),
            (">=", Self::GreaterThanEqual),
            ("=", Self::Equal),
            ("!=", Self::NotEqual),
            ("and", Self::And),
            ("or", Self::Or),
        ];

        for (op_str, op) in op_map {
            match preceded(parse_space, tag(op_str))(i) {
                Ok((i, _)) => return Ok((i, op)),
                Err(Err::Error(_)) => (),
                Err(e) => return Err(e),
            };
        }

        Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
    }

    pub fn evaluate(&self, lv: &Value, rv: &Value) -> Value {
        match self {
            Self::Division => lv / rv,
            Self::Modulus => lv % rv,
            Self::Multiplication => lv * rv,
            Self::Addition => lv + rv,
            Self::Subtraction => lv - rv,
            Self::LessThan => Value::Boolean(lv < rv),
            Self::LessThanEqual => Value::Boolean(lv <= rv),
            Self::GreaterThan => Value::Boolean(lv > rv),
            Self::GreaterThanEqual => Value::Boolean(lv >= rv),
            Self::Equal => Value::Boolean(lv == rv),
            Self::NotEqual => Value::Boolean(lv != rv),
            Self::And => Value::Boolean(lv.into() && rv.into()),
            Self::Or => Value::Boolean(lv.into() || rv.into()),
        }
    }
}

