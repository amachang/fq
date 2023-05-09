#![deny(warnings, clippy::all, clippy::pedantic)]

use std::fmt::Debug;
use nom::{
    IResult,
    bytes::complete::{tag, take_while},
    character::complete::digit1,
    combinator::eof,
    sequence::preceded,
};

pub trait AstNode: Debug {
    fn evaluate(&self) -> Value;
}

#[derive(Debug)]
struct UnaryExpr<'a> {
    op: &'a str,
    expr: Box<dyn AstNode + 'a>,
}

impl AstNode for UnaryExpr<'_> {
    fn evaluate(&self) -> Value {
        assert_eq!(self.op, "-");
        let value = self.expr.evaluate();
        let primitive_value = value.as_number();
        Value::Number(primitive_value * -1.0)
    }
}

#[derive(Debug)]
struct LiteralNumber {
    number: f64,
}

impl AstNode for LiteralNumber {
    fn evaluate(&self) -> Value {
        Value::Number(self.number)
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Number(f64),
}

impl Value {
    fn as_number(&self) -> f64 {
        let Value::Number(primitive_value) = self;
        *primitive_value
    }
}

fn parse_space(i: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(i)
}

fn parse_literal_number(i: &str) -> IResult<&str, Box<dyn AstNode + '_>> {
    let (i, number_str) = digit1(i)?;
    let number = number_str.parse().unwrap();
    Ok((i, Box::new(LiteralNumber { number })))
}

fn parse_unary_expr(i: &str) -> IResult<&str, Box<dyn AstNode + '_>> {
    let (i, op) = tag("-")(i)?;
    let (i, expr) = preceded(parse_space, parse_literal_number)(i)?;
    Ok((i, Box::new(UnaryExpr { op, expr })))
}

fn parse_root(i: &str) -> IResult<&str, Box<dyn AstNode + '_>> {
    let (i, ast_node) = preceded(
        parse_space,
        parse_unary_expr,
    )(i)?;
    let (i, _) = preceded(
        parse_space,
        eof,
    )(i)?;
    Ok((i, ast_node))
}

pub fn parse(i: &str) -> Result<Box<dyn AstNode + '_>, nom::Err<nom::error::Error<&str>>> {
    match parse_root(i) {
        Ok((_, ast_node)) => Ok(ast_node),
        Err(e) => Err(e),
    }
}

