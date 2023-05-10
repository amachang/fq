#![deny(warnings, clippy::all, clippy::pedantic)]

use fq::{
    parse,
    Number,
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
fn test_number() {
    assert!(!(Number::from(10) < Number::from(10.0)));
    assert!(!(Number::from(10) > Number::from(10.0)));
    assert!(Number::from(10) <= Number::from(10.0));
    assert!(Number::from(10) >= Number::from(10.0));
    assert_eq!(Number::from(10), Number::from(10.0));
    assert_ne!(Number::from(10), Number::from(10.01));
    assert!(!(Number::from(0.0 / 0.0) == Number::from(0.0 / 0.0)));
    assert!(Number::from(0.0 / 0.0) != Number::from(0.0 / 0.0));
}

#[test]
fn test_binary_operator() {
    assert_eq!(BinaryOperator::Division.evaluate(&Value::from(1), &Value::from(0)), Value::from(f64::INFINITY),);
    assert!(BinaryOperator::Division.evaluate(&Value::from(0), &Value::from(0)).is_nan());
    assert_eq!(BinaryOperator::Equal.evaluate(&Value::from(1), &Value::from(1.0)), Value::from(true));

    let n = Value::from(1);
    assert_eq!(BinaryOperator::Equal.evaluate(&n, &n), Value::from(true));
}

#[test]
fn test_parse() -> Result<(), String> {
    assert_eq!(err(parse("-30A"))?, Err::Error(Error::new("A", ErrorKind::Eof)));
    assert_eq!(ok(parse("-30"))?.evaluate(), Value::from(-30));
    assert_eq!(ok(parse("30"))?.evaluate(), Value::from(30));
    assert_eq!(ok(parse("1 + 2 * 3"))?.evaluate(), Value::from(7));
    assert_eq!(ok(parse("4 div 3 > 2 % 1"))?.evaluate(), Value::from(true));
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
