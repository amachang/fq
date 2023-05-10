#![deny(warnings, clippy::all, clippy::pedantic)]

use fq::{
    parse,
    Value,
    BinaryOperator,
    Expr,
};
use nom::{
    Err,
    error::{
        Error,
        ErrorKind,
    }
};

#[test]
fn test_binary_operator() {
    assert_eq!(BinaryOperator::Division.evaluate(&Value::Number(1.0), &Value::Number(0.0)).as_number(), f64::INFINITY);
    assert!(BinaryOperator::Division.evaluate(&Value::Number(0.0), &Value::Number(0.0)).as_number().is_nan());
    assert!(BinaryOperator::Equal.evaluate(&Value::Number(1.0), &Value::Number(1.0)).as_boolean());

    let n = Value::Number(1.0);
    assert!(BinaryOperator::Equal.evaluate(&n, &n).as_boolean());
}

#[test]
fn test_parse() -> Result<(), String> {
    assert_eq!(err(parse("-30A"))?, Err::Error(Error::new("A", ErrorKind::Eof)));
    assert_eq!(ok(parse("-30"))?.evaluate(), Value::Number(-30.0));
    assert_eq!(ok(parse("30"))?.evaluate(), Value::Number(30.0));
    assert_eq!(ok(parse("1 + 2 * 3"))?.evaluate(), Value::Number(7.0));
    assert_eq!(ok(parse("4 div 3 > 2 % 1"))?.evaluate(), Value::Boolean(true));

    Ok(())
}

fn err(result: Result<Box<dyn Expr>, Err<Error<&str>>>) -> Result<Err<Error<&str>>, String> {
    match result {
        Ok(node) => Err(format!("{node:?}")),
        Err(e) => Ok(e),
    }
}

fn ok(result: Result<Box<dyn Expr>, Err<Error<&str>>>) -> Result<Box<dyn Expr>, String> {
    match result {
        Ok(node) => Ok(node),
        Err(e) => Err(format!("{e}")),
    }
}
