use fq::*;
use nom::error::{
    VerboseErrorKind,
    ErrorKind,
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
        ("/foo/", "set(path('/foo'))"),
    ];

    for (q, expected_q) in &q_map {
        let (r, expected_r) = (query(q), query(expected_q));
        let expected_r = expected_r.unwrap();

        let r = r.expect(&format!("Should not be error {:?}", q));
        assert_eq!(r, expected_r, "{:?} should equal {:?} in {:?}", r, expected_r, q);
    }
}

#[test]
fn test_query_evaluate_error() {
    let q_map = [
        ("fn_not_found()", EvaluateError::FunctionNotFound("fn_not_found".to_string())),
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
    assert_eq!(format!("{:?}", EvaluateError::FunctionNotFound("fn_not_found".to_string())), "FunctionNotFound(\"fn_not_found\")");
    assert_eq!(format!("{:?}", Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string()))), "EvaluateError(FunctionNotFound(\"fn_not_found\"))");
    assert!(Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string())) == Error::EvaluateError(EvaluateError::FunctionNotFound("fn_not_found".to_string())));
    assert!(parse("1").unwrap() == Box::new(PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } }));

    let lhs: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } };
    let rhs0: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } };
    let rhs1: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("2".to_string())]), predicate_exprs: vec![] } };
    assert!(lhs == rhs0);
    assert!(lhs != rhs1);


}

