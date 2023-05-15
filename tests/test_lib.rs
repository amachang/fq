#![deny(warnings, clippy::all, clippy::pedantic)]

use std::path::PathBuf;

use fq::{
    parse,
    evaluate,
    Number,
    Value,
    BinaryOperator,
    BinaryExpr,
    UnionExpr,
};
use nom::{
    Err,
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
fn test_binary_expression() {
    let source_evaluated_value_remaining_input_map = [
        ("1 + 2", Value::from(3), ""),
        ("4 div 2 errorerror", Value::from(2), " errorerror"),
        ("1 == 1", Value::from(1), " == 1"),
    ];
    for (source, evaluated_value, remaining_input) in source_evaluated_value_remaining_input_map {
        let (i, expr) = BinaryExpr::parse(source).unwrap();
        assert_eq!(evaluate(&*expr).unwrap(), evaluated_value);
        assert_eq!(i, remaining_input);
    }
}

#[test]
fn test_union_expression() {
    let source_evaluated_value_remaining_input_map = [
        ("1 | 2", Value::from([1, 2]), ""),
        ("'1' | '2' | '3' errorerror", Value::from(["1", "2", "3"]), " errorerror"),
        ("1 == 1", Value::from(1), " == 1"),
    ];
    for (source, evaluated_value, remaining_input) in source_evaluated_value_remaining_input_map {
        let (i, expr) = UnionExpr::parse(source).unwrap();
        assert_eq!(evaluate(&*expr).unwrap(), evaluated_value);
        assert_eq!(i, remaining_input);
    }
}

#[test]
fn test_parse() {
    let result_map = [
        ("foo/bar/baz", Value::from([PathBuf::from("foo/bar/baz")])),
        ("/", Value::from([PathBuf::from("/")])),
        ("/*[name() = 'tmp']", Value::from([PathBuf::from("/tmp")])),
        ("/tmp/../*[name() = 'tmp']", Value::from([PathBuf::from("/tmp/../tmp")])),
        ("/tmp", Value::from([PathBuf::from("/tmp")])),
        ("/foo/bar", Value::from([PathBuf::from("/foo/bar")])),
        ("/foo/bar[1 = 1]", Value::from([PathBuf::from("/").join("foo").join("bar")])),
        ("/tmp[1 = 2]", Value::from(Vec::new())),
        ("path('/foo/bar') [ 1 = 1 ] [ 2 = 2 ]", Value::from([PathBuf::from("/").join("foo").join("bar")])),
        ("path('/tmp')[1 = 2]", Value::from(Vec::new())),
        ("string(123)", Value::from("123")),
        ("string(1 = 1)", Value::from("true")),
        ("set(1.1e3, '2', nan, inf | 1 = 1 | nan = nan)", Value::from([ Value::from(1100), Value::from("2"), Value::from(f64::INFINITY),
                                                                    Value::from(true), Value::from(false)])),
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
        assert_eq!(evaluate(&*parse(source).unwrap()).unwrap(), result_value);
    }

    let error_map = [
        ("-30A", "A"),
        ("1 == 1", "== 1"),
    ];

    for (source, remaining_input) in error_map {
        let result = parse(source);
        assert!(result.is_err());
        let err = match result {
            Err(Err::Error(err)) | Err(Err::Failure(err)) => err,
            _ => unreachable!(),
        };
        assert_eq!(err.input, remaining_input);
    }

    assert!(evaluate(&*parse("nan").unwrap()).unwrap().is_nan());
    assert!(evaluate(&*parse("-'test'").unwrap()).unwrap().is_nan());
}

