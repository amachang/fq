mod parse_util;
mod primitive;
mod value;
mod expr;

use std::path::PathBuf;

pub use primitive::*;
pub use value::*;
pub use expr::*;
pub use parse_util::*;

use nom::error::VerboseError;

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
    let (_, expr) = parse_root(i).unwrap_result()?;
    Ok(expr)
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

