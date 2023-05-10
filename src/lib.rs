#![deny(warnings, clippy::all, clippy::pedantic)]

mod parse_util;
mod primitive;
mod value;
mod expr;

use parse_util::parse_eof;
pub use primitive::Number;
pub use value::Value;
pub use expr::{
    Expr,
    LiteralNumber,
    LiteralString,
    UnaryExpr,
    UnaryOperator,
    BinaryExpr,
    BinaryOperator,
};

use nom::{
    IResult,
};

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

