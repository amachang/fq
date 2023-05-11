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
    let result_map = [
        ("-30", Value::from(-30)),
        ("30", Value::from(30)),
        ("1e10", Value::from(10000000000)),
        ("1 + 2 * - 3", Value::from(-5)),
        ("4 div 3 > 2 % 1", Value::from(true)),
        ("---1.0", Value::from(-1)),
        (" inf ", Value::from(f64::INFINITY)),
        ("'inf' + 1", Value::from(f64::INFINITY)),
        ("'1' + '2'", Value::from(3)),
        (" \"hello world!\" ", Value::from("hello world!")),
        ("1 | 2 | 3", Value::from([1, 2, 3])),
        ("1.1e3 | 2.2e3 | 3.3e3", Value::from([1100, 2200, 3300])),
        ("'1' | '2' | '3'", Value::from(["1", "2", "3"])),
        ("1.1e3 | nan | inf", Value::from([1100.0, f64::INFINITY])),
        ("1 = 1 | nan = nan", Value::from([true, false])),
        ("1.1e3 | '2' | nan | inf | 1 = 1 | nan = nan", Value::from([ Value::from(1100), Value::from("2"), Value::from(f64::INFINITY),
                                                                    Value::from(true), Value::from(false)])),
    ];

    for (source, result_value) in result_map {
        assert_eq!(ok(parse(source))?.evaluate(), result_value);
    }

    assert_eq!(err(parse("-30A"))?, Err::Error(Error::new("A", ErrorKind::Eof)));
    assert!(ok(parse("nan"))?.evaluate().is_nan());
    assert!(ok(parse("-'test'"))?.evaluate().is_nan());
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
