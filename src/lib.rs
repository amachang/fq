#![deny(warnings, clippy::all, clippy::pedantic)]

mod parse_util;
mod primitive;
mod value;
mod expr;

use std::env::current_dir;

use parse_util::parse_eof;
pub use primitive::{
    Number,
    RealNumber,
};
pub use value::{
    Value,
    RealValue,
};
pub use expr::{
    EvaluationContext,
    Error,
    Expr,
    UnionExpr,
    LiteralNumber,
    LiteralString,
    UnaryExpr,
    UnaryOperator,
    BinaryExpr,
    BinaryOperator,
    PathRootExpr,
    PathStepExpr,
    LiteralRootPath,
};

use nom::{
    IResult,
};

fn parse_root(i: &str) -> IResult<&str, Box<dyn Expr>> {
    let (i, expr) = expr::parse(i)?;
    let (i, _) = parse_eof(i)?;
    Ok((i, expr))
}

pub fn parse(i: &str) -> Result<Box<dyn Expr>, nom::Err<nom::error::Error<&str>>> {
    match parse_root(i) {
        Ok((_, expr)) => Ok(expr),
        Err(e) => Err(e),
    }
}

pub fn evaluate(expr: &dyn Expr) -> Result<Value, Error> {
    let current_dir = match current_dir() {
        Ok(dir) => dir,
        Err(err) => return Err(Error::CouldntGetCurrentDir(err.kind(), err.to_string())),
    };
    let context_value = Value::from(current_dir);
    let ctx = EvaluationContext::new(&context_value);
    expr.evaluate(&ctx)
}

