#![deny(warnings, clippy::all, clippy::pedantic)]

use super::{
    parse_util::{
        parse_space,
        parse_identifier,
    },
    value::{
        Value,
    },
    primitive::{
        Number,
    },
};

use std::{
    fmt::Debug,
    collections::{
        HashMap,
    },
    io,
    path,
    path::PathBuf,
};

use nom::{
    IResult,
    bytes::complete::{
        tag,
        take_until,
    },
    character::complete::satisfy,
    number::complete::float,
    sequence::{
        preceded,
        pair,
    },
    branch::alt,
    combinator::recognize,
    Err,
    error::{
        ParseError,
        ErrorKind,
    },
};

pub struct EvaluationContext <'a> {
    pub(crate) parent: Option<Box<&'a EvaluationContext<'a>>>,
    pub(crate) fn_map: HashMap<String, fn(&[Value]) -> Value>,
    pub(crate) var_map: HashMap<String, Value>,
    pub(crate) context_value: &'a Value,
}

impl<'a> EvaluationContext <'a> {
    pub fn new(context_value: &'a Value) -> EvaluationContext {
        let mut ctx = EvaluationContext { parent: None, fn_map: HashMap::new(), var_map: HashMap::new(), context_value: context_value, };
        ctx.register_function("string", |vs| {
            if vs.len() < 1 {
                Value::from("")
            } else {
                /* TODO strip clone */
                vs[0].clone().as_string()
            }
        });
        ctx.register_function("number", |vs| {
            if vs.len() < 1 {
                Value::from(f64::NAN)
            } else {
                /* TODO strip clone */
                vs[0].clone().as_number()
            }
        });
        ctx.register_function("boolean", |vs| {
            if vs.len() < 1 {
                Value::from(false)
            } else {
                /* TODO strip clone */
                vs[0].clone().as_boolean()
            }
        });
        ctx.register_function("path", |vs| {
            if vs.len() < 1 {
                Value::from(false)
            } else {
                /* TODO strip clone */
                vs[0].clone().as_path()
            }
        });
        ctx.register_function("set", |vs| {
            Value::from(vs)
        });
        ctx
    }

    pub fn register_function(&mut self, key: impl AsRef<str>, function: fn(&[Value]) -> Value) {
        self.fn_map.insert(key.as_ref().to_string(), function);
    }

    pub fn get_function(&self, key: impl AsRef<str>) -> Option<&fn(&[Value]) -> Value> {
        match self.fn_map.get(key.as_ref()) {
            Some(function) => Some(function),
            None => match &self.parent {
                Some(parent) => parent.get_function(key),
                None => None,
            },
        }
    }

    pub fn set_variable(&mut self, key: impl AsRef<str>, value: Value) {
        self.var_map.insert(key.as_ref().to_string(), value);
    }

    pub fn get_variable(&self, key: impl AsRef<str>) -> Option<&Value> {
        match self.var_map.get(key.as_ref()) {
            Some(variable) => Some(variable),
            None => match &self.parent {
                Some(parent) => parent.get_variable(key),
                None => None,
            },
        }
    }

    pub fn get_context_value(&self) -> &Value {
        self.context_value
    }

    pub fn scope(&'a self, value: &'a Value) -> Self {
        Self {
            parent: Some(Box::new(self)),
            fn_map: HashMap::new(),
            var_map: HashMap::new(),
            context_value: value,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    FunctionNotFound(String),
    CouldntGetCurrentDir(io::ErrorKind, String),
}

pub trait Expr: Debug {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error>;
}

impl PartialEq for dyn Expr {
    fn eq(&self, _: &dyn Expr) -> bool {
        unreachable!()
    }
}

pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
    UnionExpr::parse(i)
}

pub fn parse_expr_list<'a>(delimiter: &'static str, i: &'a str, parse: fn(i: &str) -> IResult<&str, Box<dyn Expr>>) -> IResult<&'a str, Vec<Box<dyn Expr>>> {
    let mut exprs: Vec<Box<dyn Expr>> = Vec::new();
    let mut next_i = i;
    loop {
        let i = next_i;
        let (i, expr) = match parse(i) {
            Ok(r) => r,
            Err(Err::Error(_)) => break,
            Err(e) => return Err(e),
        };
        next_i = i;
        exprs.push(expr);

        let (i, _) = match preceded(parse_space, tag(delimiter))(i) {
            Ok(r) => r,
            Err(Err::Error(_)) => break,
            Err(e) => return Err(e),
        };
        next_i = i
    };
    Ok((next_i, exprs))
}

#[derive(Debug, PartialEq)]
pub struct UnionExpr {
    exprs: Vec<Box<dyn Expr>>,
}

impl UnionExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, exprs) = parse_expr_list("|", i, BinaryExpr::parse)?;

        if 0 == exprs.len() {
            Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
        } else if 1 == exprs.len() {
            let expr = exprs.into_iter().next().unwrap();
            Ok((i, expr))
        } else {
            Ok((i, Box::new(UnionExpr { exprs })))
        }
    }
}

impl Expr for UnionExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let values: Vec<Value> = self.exprs.iter().map(|expr| Ok(expr.evaluate(ctx)?)).collect::<Result<Vec<Value>, Error>>()?;
        Ok(Value::from(values))
    }
}

#[derive(Debug)]
pub struct PathExpr {
    root_expr: Box<dyn Expr>,
    step_exprs: Vec<Box<dyn Expr>>,
}

impl PathExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, root_expr) = match LiteralRootPath::parse(i) {
            Ok(r) => r,
            Err(Err::Error(_)) => {
                let (i, root_expr) = PathRootExpr::parse(i)?;
                match preceded(parse_space, tag(path::MAIN_SEPARATOR_STR))(i) {
                    Ok(_) => (),
                    Err(Err::Error(_)) => return Ok((i, root_expr)),
                    Err(e) => return Err(e),
                };
                (i, root_expr)
            },
            Err(e) => return Err(e),
        };
        let (i, step_exprs) = parse_expr_list(path::MAIN_SEPARATOR_STR, i, PathStepExpr::parse)?;
        Ok((i, Box::new(PathExpr { root_expr, step_exprs })))
    }
}

impl Expr for PathExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let mut next_value = self.root_expr.evaluate(ctx)?;
        for expr in &self.step_exprs {
            let value = next_value;
            let mut values = Vec::new();
            for context_value in &value {
                let ctx = ctx.scope(&context_value);
                let value = expr.evaluate(&ctx)?;
                values.push(value);
            };
            next_value = Value::from(values);
        };
        return Ok(next_value)
    }
}

impl PartialEq for PathExpr {
    fn eq(&self, expr: &Self) -> bool {
        *self.root_expr == *expr.root_expr && self.step_exprs == expr.step_exprs
    }
}

#[derive(Debug)]
pub struct PathRootExpr {
    expr: Box<dyn Expr>,
    predicate_exprs: Vec<Box<dyn Expr>>,
}

impl PathRootExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, expr) = alt((
                LiteralString::parse,
                LiteralNumber::parse,
                FunctionCall::parse,
        ))(i)?;

        let (i, predicate_exprs) = PathStepExpr::parse_predicates(i)?;

        if 0 == predicate_exprs.len() {
            Ok((i, expr))
        } else {
            Ok((i, Box::new(PathRootExpr { expr, predicate_exprs })))
        }
    }
}

impl Expr for PathRootExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let value = self.expr.evaluate(ctx)?;
        PathStepExpr::evaluate_predicates(ctx, &self.predicate_exprs, value)
    }
}

impl PartialEq for PathRootExpr {
    fn eq(&self, expr: &Self) -> bool {
        *self.expr == *expr.expr && self.predicate_exprs == expr.predicate_exprs
    }
}

#[derive(Debug, PartialEq)]
pub struct PathStepExpr {
    name: String,
    predicate_exprs: Vec<Box<dyn Expr>>,
}

impl PathStepExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, name) = preceded(
            parse_space,
            parse_identifier,
        )(i)?;

        let (i, predicate_exprs) = PathStepExpr::parse_predicates(i)?;

        let name = name.to_string();
        Ok((i, Box::new(PathStepExpr { name, predicate_exprs })))
    }

    fn parse_predicates(i: &str) -> IResult<&str, Vec<Box<dyn Expr>>> {
        let mut predicate_exprs: Vec<Box<dyn Expr>> = Vec::new();
        let mut next_i = i;
        loop {
            let i = next_i;
            let (i, _) = match preceded(parse_space, tag("["))(i) {
                Ok(r) => r,
                Err(Err::Error(_)) => break,
                Err(e) => return Err(e),
            };
            let (i, predicate_expr) = match parse(i) {
                Ok(r) => r,
                Err(Err::Error(e)) => return Err(Err::Failure(e)),
                Err(e) => return Err(e),
            };
            let (i, _) = match preceded(parse_space, tag("]"))(i) {
                Ok(r) => r,
                Err(Err::Error(e)) => return Err(Err::Failure(e)),
                Err(e) => return Err(e),
            };
            predicate_exprs.push(predicate_expr);
            next_i = i
        };
        Ok((next_i, predicate_exprs))
    }

    fn evaluate_predicates(ctx: &EvaluationContext, predicate_exprs: &Vec<Box<dyn Expr>>, value: Value) -> Result<Value, Error> {
        let mut next_value = value;
        for predicate_expr in predicate_exprs {
            let value = next_value;
            let mut values = Vec::new();
            for context_value in &value {
                let ctx = &ctx.scope(&context_value);
                let predicate_value = predicate_expr.evaluate(ctx)?;
                let predicate_value: bool = predicate_value.into();

                if predicate_value {
                    values.push(context_value.clone());
                };
            };
            next_value = Value::from(values);
        };
        Ok(next_value)
    }
}

impl Expr for PathStepExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let context_value = ctx.get_context_value();
        let value = Value::join_path(context_value, self.name.clone());
        Self::evaluate_predicates(ctx, &self.predicate_exprs, value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralRootPath {
    path: PathBuf
}

impl LiteralRootPath {
    #[cfg(not(windows))]
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        Self::parse_for_unix(i)
    }

    #[cfg(windows)]
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        Self::parse_for_windows(i)
    }

    pub fn parse_for_unix(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, path) = preceded(parse_space, tag("/"))(i)?;
        Ok((i, Box::new(LiteralRootPath { path: PathBuf::from(path) })))
    }

    pub fn parse_for_windows(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, path) = preceded(
            parse_space,
            recognize(
                alt((
                        tag(r"\"),
                        Self::parse_windows_drive_root,
                        recognize(pair(alt((tag(r"\\?\"), tag(r"\\.\"))), Self::parse_windows_drive_root)),
                ))
            )
        )(i)?;
        Ok((i, Box::new(LiteralRootPath { path: PathBuf::from(path) })))
    }

    fn parse_windows_drive_root(i: &str) -> IResult<&str, &str> {
        recognize(pair(satisfy(|c| 'A' <= c && c <= 'Z'), tag(r":\")))(i)
    }
}

impl Expr for LiteralRootPath {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, Error> {
        Ok(Value::from([self.path.clone()]))
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: UnaryOperator,
    expr: Box<dyn Expr>,
}

impl UnaryExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        match UnaryOperator::parse(i) {
            Ok((i, op)) => {
                let (i, expr) = UnaryExpr::parse(i)?;
                Ok((i, Box::new(UnaryExpr { op, expr })))
            }
            Err(Err::Error(_)) => PathExpr::parse(i),
            Err(e) => Err(e),
        }
    }
}

impl Expr for UnaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        Ok(self.op.evaluate(&self.expr.evaluate(ctx)?))
    }
}

impl PartialEq for UnaryExpr {
    fn eq(&self, expr: &Self) -> bool {
        self.op == expr.op && *self.expr == *expr.expr
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Minus,
}

impl UnaryOperator {
    fn parse(i: &str) -> IResult<&str, Self> {
        let op_map = [
            ("-", Self::Minus),
        ];

        for (op_str, op) in op_map {
            match preceded(parse_space, tag(op_str))(i) {
                Ok((i, _)) => return Ok((i, op)),
                Err(Err::Error(_)) => (),
                Err(e) => return Err(e),
            };
        }

        Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
    }

    pub fn evaluate(&self, v: &Value) -> Value {
        match self {
            Self::Minus => -v,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LiteralNumber {
    number: Number,
}

impl LiteralNumber {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, f) = preceded(parse_space, float)(i)?;
        let number = Number::from(f as f64);
        Ok((i, Box::new(LiteralNumber { number })))
    }
}

impl Expr for LiteralNumber {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, Error> {
        Ok(Value::from(self.number))
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    identifier: String,
    arg_exprs: Vec<Box<dyn Expr>>,
}

impl FunctionCall {
    pub fn parse<'a>(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, identifier) = preceded(
            parse_space,
            parse_identifier,
        )(i)?;
        let (i, _) = preceded(
            parse_space,
            tag("("),
        )(i)?;

        let (i, arg_exprs) = parse_expr_list(",", i, UnionExpr::parse)?;

        let (i, _) = match preceded(parse_space, tag(")"))(i) {
            Ok(r) => r,
            Err(Err::Error(e)) => return Err(Err::Failure(e)),
            Err(e) => return Err(e),
        };

        let identifier = identifier.to_string();
        Ok((i, Box::new(FunctionCall { identifier, arg_exprs })))
    }
}

impl Expr for FunctionCall {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        let function = match ctx.get_function(&self.identifier) {
            Some(function) => function,
            None => return Err(Error::FunctionNotFound(self.identifier.clone())),
        };
        let mut arg_values: Vec<Value> = Vec::new();
        for expr in &self.arg_exprs {
            let value = expr.evaluate(ctx)?;
            arg_values.push(value);
        }
        let return_value = function(&arg_values);
        Ok(return_value)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralString {
    string: String,
}

impl LiteralString {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let (i, quote_char) = preceded(
            parse_space,
            alt((tag("\""), tag("'")))
        )(i)?;
        let (i, string) = take_until(quote_char)(i)?;
        let (i, _) = match preceded(parse_space, tag(quote_char))(i) {
            Ok(r) => r,
            Err(Err::Error(e)) => return Err(Err::Failure(e)),
            Err(e) => return Err(e),
        };
        Ok((i, Box::new(LiteralString { string: string.to_string() })))
    }
}

impl Expr for LiteralString {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, Error> {
        Ok(Value::from(self.string.clone()))
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    op: BinaryOperator,
    left_expr: Box<dyn Expr>,
    right_expr: Box<dyn Expr>,
}

impl BinaryExpr {
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Expr>> {
        let mut stack: Vec<(&str, BinaryOperator, Box<dyn Expr>)> = Vec::new();
        let mut current_i = i;
        let (i, expr) = loop {
            let i = current_i;
            let (before_op_i, new_expr) = match UnaryExpr::parse(i) {
                Ok(r) => r,
                Err(Err::Error(err)) => {
                    match stack.pop() {
                        Some((i, _, expr)) => break (i, expr),
                        None => return Err(Err::Error(err)),
                    }
                },
                Err(e) => return Err(e),
            };
            let (after_op_i, new_op) = match BinaryOperator::parse(before_op_i) {
                Ok(r) => r,
                Err(Err::Error(_)) => break (before_op_i, new_expr),
                Err(e) => return Err(e),
            };

            let mut expr = new_expr;
            loop {
                match stack.pop() {
                    Some((i, stacked_op, stacked_expr)) => {
                        if new_op.precedence() <= stacked_op.precedence() {
                            expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr })
                        }
                        else {
                            stack.push((i, stacked_op, stacked_expr));
                            break;
                        }
                    },
                    None => break,
                };
            };

            stack.push((before_op_i, new_op, expr));
            current_i = after_op_i;
        };

        let mut expr = expr;
        loop {
            match stack.pop() {
                Some((_, stacked_op, stacked_expr)) => {
                    expr = Box::new(BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr });
                },
                None => break,
            };
        };

        Ok((i, expr))
    }
}

impl Expr for BinaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, Error> {
        Ok(self.op.evaluate(&self.left_expr.evaluate(ctx)?, &self.right_expr.evaluate(ctx)?))
    }
}

impl PartialEq for BinaryExpr {
    fn eq(&self, expr: &Self) -> bool {
        self.op == expr.op && *self.left_expr == *expr.left_expr && *self.right_expr == *expr.right_expr
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Division,
    Modulus,
    Multiplication,

    Addition,
    Subtraction,

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Equal,
    NotEqual,

    And,
    Or,
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            Self::Division | Self::Modulus | Self::Multiplication => 6,
            Self::Addition | Self::Subtraction => 5,
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => 4,
            Self::Equal | Self::NotEqual => 3,
            Self::And => 2,
            Self::Or => 1,
        }
    }

    fn parse(i: &str) -> IResult<&str, Self> {
        let op_map = [
            ("div", Self::Division),
            ("%", Self::Modulus),
            ("*", Self::Multiplication),
            ("+", Self::Addition),
            ("-", Self::Subtraction),
            ("<", Self::LessThan),
            ("<=", Self::LessThanEqual),
            (">", Self::GreaterThan),
            (">=", Self::GreaterThanEqual),
            ("=", Self::Equal),
            ("!=", Self::NotEqual),
            ("and", Self::And),
            ("or", Self::Or),
        ];

        for (op_str, op) in op_map {
            match preceded(parse_space, tag(op_str))(i) {
                Ok((i, _)) => return Ok((i, op)),
                Err(Err::Error(_)) => (),
                Err(e) => return Err(e),
            };
        }

        Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
    }

    pub fn evaluate(&self, lv: &Value, rv: &Value) -> Value {
        match self {
            Self::Division => lv / rv,
            Self::Modulus => lv % rv,
            Self::Multiplication => lv * rv,
            Self::Addition => lv + rv,
            Self::Subtraction => lv - rv,
            Self::LessThan => Value::from(lv < rv),
            Self::LessThanEqual => Value::from(lv <= rv),
            Self::GreaterThan => Value::from(lv > rv),
            Self::GreaterThanEqual => Value::from(lv >= rv),
            Self::Equal => Value::from(lv == rv),
            Self::NotEqual => Value::from(lv != rv),
            Self::And => Value::from(lv.into() && rv.into()),
            Self::Or => Value::from(lv.into() || rv.into()),
        }
    }
}

