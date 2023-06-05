use fq::*;

use std::{
    path,
    path::PathBuf,
};

use nom::error::{
    VerboseErrorKind,
    ErrorKind,
};

use nom::{
    Err,
    error::{
        VerboseError,
    },
    sequence::{
        preceded,
        terminated,
    },
    character::{
        complete::{
            char,
        },
    },
    bytes::{
        complete::{
            tag,
        },
    },
    combinator::{
        cut,
    },
};

#[test]
fn test_query_general() {
    let q_map = [
        ("1", "path('1')"),
        ("'foo'", "string('foo')"),
        ("'foo' | string()", "string('foo')"),
        ("1 * 1", "number('1')"),
        ("1 = 1", "true()"),
        ("1 != 1", "false()"),
        ("boolean('')", "false()"),
        ("boolean('foo')", "true()"),
        ("{1, 'foo'}", "set(path('1'), string('foo'))"),
        ("foo/bar | name()", "set(string('bar'))"),
        ("path('') | name()", "set(string(''))"),
        ("foo/bar/baz | dir()", "set(path('foo/bar'))"),
        ("path('') | dir()", "set(path(''))"),
        ("/", "set(path('/'))"),
        ("number(0) = 0", "true()"),
    ];

    for (q, expected_q) in &q_map {
        let (r, expected_r) = (query(q), query(expected_q));
        let expected_r = expected_r.unwrap();

        let r = r.expect(&format!("Should not be error {:?}", q));
        assert_eq!(r, expected_r, "{:?} should equal {:?} in {:?}", r, expected_r, q);
        assert!(0 < format!("{:?}", r).len());
    }
}

#[test]
fn test_query_evaluate_error() {
    let q_map = [
        ("fn_not_found()", EvaluateError::FunctionNotFound("fn_not_found".to_string())),
        ("fn_not_found() | string()", EvaluateError::FunctionNotFound("fn_not_found".to_string())),
    ];

    for (q, err) in &q_map {
        let e = match query(q) {
            Ok(r) => panic!("Should return evaluate error: got Ok({:?}) from {:?}", r, q),
            Err(Error::ParseError(e)) => panic!("Should return evaluate error: got ParseError({:?}) from {:?}", e, q),
            Err(Error::EvaluateError(e)) => e,
        };
        assert_eq!(&e, err, "{:?} should equal {:?} in {:?}", e, err, q);
    }
}

#[test]
fn test_query_parse_error() {
    let q_map = [
        ("fn_not_found(!", VerboseErrorKind::Char(')'), "!"),
        ("foo/{bar!", VerboseErrorKind::Char('}'), "!"),
        ("foo/bar!", VerboseErrorKind::Nom(ErrorKind::Eof), "!"),
    ];

    for (q, err_kind, remain) in &q_map {
        let e = match query(q) {
            Ok(r) => panic!("Should return parse error: got Ok({:?}) from {:?}", r, q),
            Err(Error::ParseError(e)) => e,
            Err(Error::EvaluateError(e)) => panic!("Should return parse error: got ParseError({:?}) from {:?}", e, q),
        };
        let (expected_remain, expected_err_kind) = &e.errors[0];
        assert_eq!(err_kind, expected_err_kind, "{:?} should equal {:?} in {:?}", err_kind, expected_err_kind, q);
        assert_eq!(remain, expected_remain, "{:?} should equal {:?} in {:?}", remain, expected_remain, q);
    }
}

#[test]
fn just_for_coverage() {

    // Debug and PartialEq traits for errors
    let qs = [
        "foo/bar | name()",
    ];
    let another_q = "origianl_query/foo/bar";
    for q in &qs {
        let expr = parse(q).unwrap();
        assert!(expr == parse(q).unwrap());
        assert!(expr != parse(another_q).unwrap());
        assert!(0 < format!("{:?}", expr).len());
    }

    // PathExpr parser failures
    let result = PathExpr::parse_core(
        "foo<separator!>bar",
        preceded(char('<'), cut(terminated(tag("separator"), char('>')))),
        PathRootStepExpr::parse_separator_like_expr,
        PathRootStepExpr::parse,
        PathStep::parse,
    );
    assert_eq!(result, Err(Err::Failure(VerboseError { errors: vec![("!>bar", VerboseErrorKind::Char('>'))] })));

    let result = PathExpr::parse_core(
        "<root!>foo",
        preceded(parse_space, tag(path::MAIN_SEPARATOR_STR)),
        |i| {
            let (i, path) = preceded(char('<'), cut(terminated(tag("root"), char('>'))))(i)?;
            let expr = LiteralPathRootExpr { path: PathBuf::from(path) };
            let expr = PathRootExpr::LiteralPathRootExpr(expr);
            let expr = PathRootStepExpr { expr, predicate_exprs: vec![] };
            Ok((i, expr))
        },
        PathRootStepExpr::parse,
        PathStep::parse,
    );
    assert_eq!(result, Err(Err::Failure(VerboseError { errors: vec![("!>foo", VerboseErrorKind::Char('>'))] })));

    // Debug traits for errors
    assert_eq!(format!("{:?}", EvaluateError::FunctionNotFound("fn_not_found".to_string())), "FunctionNotFound(\"fn_not_found\")");
    assert_eq!(format!("{:?}", Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string()))), "EvaluateError(FunctionNotFound(\"fn_not_found\"))");

    // PartialEq traits for errors
    assert!(Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string())) == Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string())));
}

