#![deny(warnings, clippy::all, clippy::pedantic)]

use fq::{
    parse,
    Value,
    AstNode,
};
use nom::{
    Err,
    error::{
        Error,
        ErrorKind,
    }
};

#[test]
fn test_parse() -> Result<(), String> {
    assert_eq!(err(parse("-30A"))?, Err::Error(Error { input: "A", code: ErrorKind::Eof }));
    assert_eq!(ok(parse("-30"))?.evaluate(), Value::Number(-30.0));

    Ok(())
}

fn err(result: Result<Box<dyn AstNode>, Err<Error<&str>>>) -> Result<Err<Error<&str>>, String> {
    match result {
        Ok(node) => Err(format!("{node:?}")),
        Err(e) => Ok(e),
    }
}

fn ok(result: Result<Box<dyn AstNode>, Err<Error<&str>>>) -> Result<Box<dyn AstNode>, String> {
    match result {
        Ok(node) => Ok(node),
        Err(e) => Err(format!("{e}")),
    }
}
