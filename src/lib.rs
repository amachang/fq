#![deny(warnings, clippy::all, clippy::pedantic)]

use std::fmt::Debug;
use nom::{
    IResult,
    bytes::complete::{tag, take_while},
    character::complete::digit1,
    combinator::eof,
    sequence::preceded,
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
struct UnaryExpr {
    op: UnaryOperator,
    expr: Box<dyn Expr>,
}

impl UnaryExpr {
    fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        match UnaryOperator::parse(i) {
            Ok((i, op)) => {
                let (i, expr) = LiteralNumber::parse(i)?;
                Ok((i, Box::new(UnaryExpr { op, expr })))
            }
            Err(Err::Error(_)) => LiteralNumber::parse(i),
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
            Self::Minus => Value::Number(-v.as_number()),
        }
    }
}

#[derive(Debug)]
struct LiteralNumber {
    number: f64,
}

impl LiteralNumber {
    fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, number_str) = preceded(parse_space, digit1)(i)?;
        let number = number_str.parse().unwrap();
        Ok((i, Box::new(LiteralNumber { number })))
    }
}

impl Expr for LiteralNumber {
    fn evaluate(&self) -> Value {
        Value::Number(self.number)
    }
}

#[derive(Debug)]
struct BinaryExpr {
    op: BinaryOperator,
    left_expr: Box<dyn Expr>,
    right_expr: Box<dyn Expr>,
}

impl BinaryExpr {
    fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
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
            Self::Division => Value::Number(lv.as_number() / rv.as_number()),
            Self::Modulus => Value::Number(lv.as_number() % rv.as_number()),
            Self::Multiplication => Value::Number(lv.as_number() * rv.as_number()),
            Self::Addition => Value::Number(lv.as_number() + rv.as_number()),
            Self::Subtraction => Value::Number(lv.as_number() - rv.as_number()),
            Self::LessThan => Value::Boolean(lv.as_number() < rv.as_number()),
            Self::LessThanEqual => Value::Boolean(lv.as_number() <= rv.as_number()),
            Self::GreaterThan => Value::Boolean(lv.as_number() > rv.as_number()),
            Self::GreaterThanEqual => Value::Boolean(lv.as_number() >= rv.as_number()),
            Self::Equal => Value::Boolean(lv == rv),
            Self::NotEqual => Value::Boolean(lv != rv),
            Self::And => Value::Boolean(lv.as_boolean() && rv.as_boolean()),
            Self::Or => Value::Boolean(lv.as_boolean() || rv.as_boolean()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
}

impl Value {
    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(primitive_number) => *primitive_number,
            Value::Boolean(primitive_boolean) => if *primitive_boolean { 1.0 } else { 0.0 },
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            Value::Number(primitive_number) => if primitive_number.is_nan() || *primitive_number == 0.0 { false } else { true },
            Value::Boolean(primitive_boolean) => *primitive_boolean,
        }
    }
}

fn parse_eof(i: &str) -> IResult<&str, &str> {
    preceded(parse_space, eof)(i)
}

fn parse_space(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn parse_root(i: &str) -> IResult<&str, Box<dyn Expr>> {
    let (i, expr) = BinaryExpr::parse(i)?;
    let (i, _) = parse_eof(i)?;
    Ok((i, expr))
}

pub fn parse(i: &str) -> Result<Box<dyn Expr>, nom::Err<nom::error::Error<&str>>> {
    match parse_root(i) {
        Ok((_, expr)) => Ok(expr),
        Err(e) => Err(e),
    }
}

