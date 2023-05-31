use fq::*;

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
    ];

    for (q, expected_q) in q_map {
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

    for (q, err) in q_map {
        let e = match query(q) {
            Ok(r) => panic!("Should return evaluate error: got Ok({:?}) from {:?}", r, q),
            Err(Error::ParseError(e)) => panic!("Should return evaluate error: got ParseError({:?}) from {:?}", e, q),
            Err(Error::EvaluateError(e)) => e,
        };
        assert_eq!(e, err, "{:?} should equal {:?} in {:?}", e, err, q);
    }
}

#[test]
fn test_deriving_for_coverage() {
    assert_eq!(format!("{:?}", EvaluateError::FunctionNotFound("fn_not_found".to_string())), "FunctionNotFound(\"fn_not_found\")");
    assert!(parse("1").unwrap() == Box::new(PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } }));

    let lhs: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } };
    let rhs0: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("1".to_string())]), predicate_exprs: vec![] } };
    let rhs1: &dyn Expr = &PathStepExpr { step: PathStep { op: PathStepOperation::Pattern(vec![PathStepPatternComponent::Name("2".to_string())]), predicate_exprs: vec![] } };
    assert!(lhs == rhs0);
    assert!(lhs != rhs1);
}

