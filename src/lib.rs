mod parse_util;
mod primitive;
mod value;
mod expr;

use std::path::PathBuf;
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
    FilterExpr,
    LiteralString,
    BinaryExpr,
    BinaryOperator,
    PathExpr,
    PathRootExpr,
    PathStepExpr,
    LiteralRootPath,
};

use nom::{
    IResult,
    Err,
    error::VerboseError,
};

fn parse_root(i: &str) -> IResult<&str, Box<dyn Expr>, VerboseError<&str>> {
    let (i, expr) = expr::parse(i)?;
    let (i, _) = parse_eof(i)?;
    Ok((i, expr))
}

pub fn parse(i: &str) -> Result<Box<dyn Expr>, VerboseError<&str>> {
    match parse_root(i) {
        Ok((_, expr)) => Ok(expr),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
        Err(Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn evaluate(expr: &dyn Expr) -> Result<Value, Error> {
    let ctx = EvaluationContext::new(Value::from(PathBuf::from("")));
    expr.evaluate(&ctx)
}

