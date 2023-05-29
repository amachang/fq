#![deny(warnings, clippy::all, clippy::pedantic)]

use std::path::PathBuf;

use fq::{
    parse,
    evaluate,
    Number,
    Value,
    BinaryOperator,
    PathExpr,
    PathRootExpr,
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
fn test_path_expression() {
    let source_evaluated_value_remaining_input_map = [
        ("1", Value::from([PathBuf::from("1")]), ""),
    ];
    for (source, evaluated_value, remaining_input) in source_evaluated_value_remaining_input_map {
        let (i, expr) = PathExpr::parse(source).unwrap();
        assert_eq!(evaluate(&*expr).unwrap(), evaluated_value);
        assert_eq!(i, remaining_input);
    }
}

#[test]
fn test_path_root_expression() {
    let source_evaluated_value_remaining_input_map = [
        ("1", Value::from([PathBuf::from("1")]), ""),
    ];
    for (source, evaluated_value, remaining_input) in source_evaluated_value_remaining_input_map {
        let (i, expr) = PathRootExpr::parse(source).unwrap();
        assert_eq!(evaluate(&*expr).unwrap(), evaluated_value);
        assert_eq!(i, remaining_input);
    }
}

#[test]
fn test_binary_expression() {
    let source_evaluated_value_remaining_input_map = [
        ("1 + 2", Value::from(3), ""),
        ("4 div 2 errorerror", Value::from(2), " errorerror"),
        ("1 == 1", Value::from([PathBuf::from("1")]), " == 1"),
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
        ("1 | 2", Value::from([PathBuf::from("1"), PathBuf::from("2")]), ""),
        ("'1' | '2' | '3' errorerror", Value::from(["1", "2", "3"]), " errorerror"),
        ("1 == 1", Value::from([PathBuf::from("1")]), " == 1"),
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
        ("/t*p", Value::from([PathBuf::from("/tmp")])),
        ("foo{bar,baz}/{aaa,bbb}", Value::from([PathBuf::from("foobar/aaa"), PathBuf::from("foobar/bbb"), PathBuf::from("foobaz/aaa"), PathBuf::from("foobaz/bbb")])),
        ("foo/bar/baz", Value::from([PathBuf::from("foo/bar/baz")])),
        ("001/002/003", Value::from([PathBuf::from("001/002/003")])),
        ("(001 + 002)/003/004", Value::from([PathBuf::from("3/003/004")])),
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
        ("30", Value::from([PathBuf::from("30")])),
        ("1e10", Value::from([PathBuf::from("1e10")])),
        ("1 + 2 * -3", Value::from(-5)),
        ("(1 + 2) * -3", Value::from(-9)),
        ("4 div 3 > 2 % 1", Value::from(true)),
        ("'1' + '2'", Value::from(3)),
        (" \"hello world!\" ", Value::from("hello world!")),
        ("1 | 2 | 3", Value::from([PathBuf::from("1"), PathBuf::from("2"), PathBuf::from("3")])),
        ("1.1e3 | 2.2e3 | 3.3e3", Value::from([PathBuf::from("1.1e3"), PathBuf::from("2.2e3"), PathBuf::from("3.3e3")])),
        ("'1' | '2' | '3'", Value::from(["1", "2", "3"])),
        ("-30", Value::from([PathBuf::from("-30")])),
    ];

    for (source, result_value) in result_map {
        assert_eq!(evaluate(&*parse(source).unwrap()).unwrap(), result_value);
    }

    let error_map = [
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
}

