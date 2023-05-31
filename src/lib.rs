mod parse_util;
mod primitive;
mod value;
mod expr;

use std::path::PathBuf;

use parse_util::{
    parse_eof,
    ParseResult,
};

pub use primitive::{
    Number,
    RealNumber,
};

pub use value::{
    Value,
    RealValue,
};

pub use expr::*;

use nom::{
    Err,
    error::VerboseError,
};

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    ParseError(VerboseError<&'a str>),
    EvaluateError(EvaluateError),
}

fn parse_root(i: &str) -> ParseResult<Box<dyn Expr>> {
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

pub fn evaluate(expr: &dyn Expr) -> Result<Value, EvaluateError> {
    let ctx = EvaluationContext::new(Value::from(PathBuf::from("")));
    expr.evaluate(&ctx)
}

pub fn query(i: &str) -> Result<Vec<PathBuf>, Error> {
    let expr = parse(i).map_err(|e| Error::ParseError(e))?;
    let values = evaluate(&*expr).map_err(|e| Error::EvaluateError(e))?;

    let mut paths = Vec::new();
    for value in &values {
        let path: PathBuf = value.into();
        paths.push(path);
    };

    Ok(paths)
}

