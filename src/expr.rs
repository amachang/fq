#![deny(warnings, clippy::all, clippy::pedantic)]

use super::{
    parse_util::parse_space,
    value::Value,
    primitive::Number,
};

use std::{
    fmt::Debug,
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

pub trait Expr: Debug {
    fn evaluate(&self) -> Value;
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
            ))(i),
            Err(e) => Err(e),
        }
    }
}

impl Expr for UnaryExpr {
    fn evaluate(&self) -> Value {
        self.op.evaluate(&self.expr.evaluate())
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    fn evaluate(&self) -> Value {
        Value::Number(self.number)
    }
}

#[derive(Debug)]
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
        let (i, _) = preceded(parse_space, tag(quote_char))(i)?;
        Ok((i, Box::new(LiteralString { string: string.to_string() })))
    }
}

impl Expr for LiteralString {
    fn evaluate(&self) -> Value {
        Value::String(self.string.clone())
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
        let mut stack: Vec<(BinaryOperator, Box<dyn Expr>)> = Vec::new();
        let mut current_i = i;
        let (i, expr) = loop {
            let i = current_i;
            let (i, new_expr) = UnaryExpr::parse(i)?;
            let (i, new_op) = match BinaryOperator::parse(i) {
                Ok(r) => r,
                Err(Err::Error(_)) => break (i, new_expr),
                Err(e) => return Err(e),
            };

            let mut expr = new_expr;
            loop {
                match stack.pop() {
                    Some((stacked_op, stacked_expr)) => {
                        if new_op.precedence() <= stacked_op.precedence() {
                            expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr })
                        }
                        else {
                            stack.push((stacked_op, stacked_expr));
                            break;
                        }
                    },
                    None => break,
                };
            };

            stack.push((new_op, expr));
            current_i = i;
        };

        let mut expr = expr;
        loop {
            match stack.pop() {
                Some((stacked_op, stacked_expr)) => {
                    expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr });
                },
                None => break,
            };
        };

        Ok((i, expr))
    }
}

impl Expr for BinaryExpr {
    fn evaluate(&self) -> Value {
        self.op.evaluate(&self.left_expr.evaluate(), &self.right_expr.evaluate())
    }
}

#[derive(Debug)]
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

