use super::{
    parse_util::{
        parse_space,
        parse_identifier,
        parse_list,
        ParseResult,
        ParseResultWrapper,
    },
    value::{
        Value,
        RealValue,
    },
    primitive::{
        PathExistence,
    },
};

use std::{
    fmt::Debug,
    collections::{
        HashMap,
    },
    collections::{
        BTreeSet,
    },
    io,
    fs,
    path,
    any::Any,
    path::{
        Path,
        PathBuf,
    },
};

use regex;
use regex::Regex;

use nom::{
    bytes::complete::{
        tag,
        take_while,
    },
    character::complete::{
        alphanumeric1,
        satisfy,
        char,
    },
    sequence::{
        preceded,
        pair,
        terminated,
        delimited,
    },
    branch::alt,
    combinator::{
        recognize,
        cut,
    },
    multi::many1_count,
    Err,
    error::{
        ParseError,
        ErrorKind,
    },
};

use dirs::home_dir;

pub struct EvaluationContext <'a> {
    pub parent: Option<Box<&'a EvaluationContext<'a>>>,
    pub fn_map: HashMap<String, fn(&EvaluationContext, &[Value]) -> Value>,
    pub context_value: Value,
}

impl<'a> EvaluationContext <'a> {
    pub fn new(context_value: Value) -> EvaluationContext<'a> {
        let mut ctx = EvaluationContext {
            parent: None,
            fn_map: HashMap::new(),
            context_value: context_value,
        };

        fn fix_first_arg<'a>(ctx: &'a EvaluationContext, vs: &'a [Value]) -> (&'a Value, &'a [Value]) {
            if vs.len() < 1 {
                (ctx.get_context_value(), vs)
            } else {
                (&vs[0], &vs[1..])
            }
        }

        ctx.register_function("string", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            value.clone().as_string()
        });
        ctx.register_function("number", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            value.clone().as_number()
        });
        ctx.register_function("boolean", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            value.clone().as_boolean()
        });
        ctx.register_function("path", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            value.clone().as_path()
        });
        ctx.register_function("set", |_, vs| {
            Value::from(vs)
        });
        ctx.register_function("true", |_, _| {
            Value::from(true)
        });
        ctx.register_function("false", |_, _| {
            Value::from(false)
        });
        ctx.register_function("name", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            let path: PathBuf = value.into();
            match path.file_name() {
                Some(os_name) => Value::from(os_name.to_string_lossy().into_owned()), // TODO String should be
                                                                                      // contains OsString
                None => Value::from(""),
            }
        });
        ctx.register_function("dir", |ctx, vs| {
            let (value, _) = fix_first_arg(ctx, vs);
            let path: PathBuf = value.into();
            match path.parent() {
                Some(dir) => {
                    let dir: PathBuf = dir.into();
                    Value::from(dir)
                },
                None => Value::from(PathBuf::from("")),
            }
        });
        ctx
    }

    pub fn register_function(&mut self, key: impl AsRef<str>, function: fn(&EvaluationContext, &[Value]) -> Value) {
        self.fn_map.insert(key.as_ref().to_string(), function);
    }

    pub fn get_function(&self, key: impl AsRef<str>) -> Option<&fn(&EvaluationContext, &[Value]) -> Value> {
        match self.fn_map.get(key.as_ref()) {
            Some(function) => Some(function),
            None => match &self.parent {
                Some(parent) => parent.get_function(key),
                None => None,
            },
        }
    }

    pub fn get_context_value(&self) -> &Value {
        &self.context_value
    }

    pub fn scope(&'a self, value: Value) -> Self {
        let parent_dir: PathBuf = self.context_value.clone().into();
        let context_dir: PathBuf = value.into();

        Self {
            parent: Some(Box::new(self)),
            fn_map: HashMap::new(),
            context_value: Value::from(parent_dir.join(context_dir)),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum EvaluateError {
    FunctionNotFound(String),
    CouldntReadDir(io::ErrorKind, String),
}

pub trait Expr: Debug + Any {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError>;
    fn as_any(&self) -> &dyn Any;
    fn eq(&self, rhs: &dyn Any) -> bool;
}

impl PartialEq for dyn Expr {
    fn eq(&self, rhs: &dyn Expr) -> bool {
        Expr::eq(self, rhs.as_any())
    }
}

macro_rules! expr_eq {
    () => (
        fn as_any(&self) -> &dyn Any {
            self
        }

        fn eq(&self, rhs: &dyn Any) -> bool {
            rhs.downcast_ref::<Self>().map_or(false, |rhs| self == rhs)
        }
    );
}

pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
    FilterExpr::parse(i)
}

#[derive(Debug, PartialEq)]
pub struct FilterExpr {
    pub exprs: Vec<Box<dyn Expr>>,
}

impl FilterExpr {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, exprs) = parse_list(preceded(parse_space, char('|')), BinaryExpr::parse)(i)?;

        if 0 == exprs.len() {
            Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)))
        } else if 1 == exprs.len() {
            let expr = exprs.into_iter().next().unwrap();
            Ok((i, expr))
        } else {
            Ok((i, Box::new(FilterExpr { exprs })))
        }
    }
}

impl Expr for FilterExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let mut values = ctx.get_context_value().clone();
        for expr in &self.exprs {
            let mut value_vec = Vec::new();
            for value in &values {
                let ctx = ctx.scope(value);
                let value = expr.evaluate(&ctx)?;
                value_vec.push(value);
            }
            values = Value::from(value_vec);
        }
        Ok(Value::from(values))
    }

    expr_eq!();
}

#[derive(Debug)]
pub struct PathExpr {
    pub root_expr: Box<dyn Expr>,
    pub steps: Vec<PathStep>,
}

impl PathExpr {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, root_expr) = match LiteralRootPath::parse_separator_like_root_path(i).wrap_failure()? {
            Ok(r) => r,
            Err(_) => {
                let (i, root_expr) = PathRootExpr::parse(i)?;
                let i = match preceded(parse_space, tag(path::MAIN_SEPARATOR_STR))(i).wrap_failure()? {
                    Ok((i, _)) => i,
                    Err(_) => return Ok((i, root_expr)),
                };
                (i, root_expr)
            },
        };
        let (i, steps) = parse_list(preceded(parse_space, tag(path::MAIN_SEPARATOR_STR)), PathStep::parse)(i)?;
        Ok((i, Box::new(PathExpr { root_expr, steps })))
    }
}

impl Expr for PathExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let mut value = self.root_expr.evaluate(ctx)?;
        for step in &self.steps {
            value = step.evaluate(&ctx, &value)?;
        };
        return Ok(value)
    }

    expr_eq!();
}

impl PartialEq for PathExpr {
    fn eq(&self, expr: &Self) -> bool {
        *self.root_expr == *expr.root_expr && self.steps == expr.steps
    }
}

#[derive(Debug)]
pub struct PathRootExpr {
    pub expr: Box<dyn Expr>,
    predicate_exprs: Vec<Box<dyn Expr>>,
}

impl PathRootExpr {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, expr) = alt((
                delimited(
                    preceded(parse_space, char('(')),
                    cut(FilterExpr::parse),
                    cut(preceded(parse_space, char(')'))),
                    ),
                LiteralString::parse,
                FunctionCall::parse,
                LiteralRootPath::parse,
                |i| -> ParseResult<Box<dyn Expr>> {
                    let (i, step) = PathStep::parse(i)?;
                    Ok((i, Box::new(PathStepExpr { step })))
                },
        ))(i)?;

        let (i, predicate_exprs) = PathStep::parse_predicates(i)?;

        if 0 == predicate_exprs.len() {
            Ok((i, expr))
        } else {
            Ok((i, Box::new(PathRootExpr { expr, predicate_exprs })))
        }
    }
}

impl Expr for PathRootExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let value = self.expr.evaluate(ctx)?;
        PathStep::evaluate_predicates(ctx, &self.predicate_exprs, value)
    }

    expr_eq!();
}

impl PartialEq for PathRootExpr {
    fn eq(&self, expr: &Self) -> bool {
        *self.expr == *expr.expr && self.predicate_exprs == expr.predicate_exprs
    }
}

#[derive(Debug, PartialEq)]
pub struct PathStepExpr {
    pub step: PathStep,
}

impl Expr for PathStepExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        self.step.evaluate(&ctx, &Value::from(""))
    }

    expr_eq!();
}

#[derive(Debug)]
pub struct PathStep {
    pub op: PathStepOperation,
    pub predicate_exprs: Vec<Box<dyn Expr>>,
}

impl PathStep {
    pub fn parse(i: &str) -> ParseResult<PathStep> {
        let (i, _) = parse_space(i)?;

        if let Ok((i, _)) = tag("**")(i).wrap_failure()? {
            let op = PathStepOperation::Recursive;
            let (i, predicate_exprs) = PathStep::parse_predicates(i)?;
            return Ok((i, PathStep { op, predicate_exprs }));
        };

        let mut components: Vec<PathStepPatternComponent> = Vec::new();
        let mut next_i = i;
        loop {
            let i = next_i;

            if let Ok((i, _)) = tag("**")(i).wrap_failure()? {
                return Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)));
            };

            let Ok((i, component)) = Self::parse_component(i).wrap_failure()? else {
                break
            };

            components.push(component);

            next_i = i;
        };
        if components.len() == 0 {
            return Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Char)));
        };

        let i = next_i;
        let op = PathStepOperation::Pattern(components);
        let (i, predicate_exprs) = PathStep::parse_predicates(i)?;
        Ok((i, PathStep { op, predicate_exprs }))
    }

    fn parse_component(i: &str) -> ParseResult<PathStepPatternComponent> {
        alt((
                Self::parse_name_component,
                Self::parse_match_all_component,
                Self::parse_exprs_component,
        ))(i)
    }

    fn parse_name_component(i: &str) -> ParseResult<PathStepPatternComponent> {
        let (i, name) = recognize(many1_count(alt((alphanumeric1, tag("_"), tag("-"), tag(".")))))(i)?;
        Ok((i, PathStepPatternComponent::Name(name.to_string())))
    }

    fn parse_match_all_component(i: &str) -> ParseResult<PathStepPatternComponent> {
        let (i, _) = tag("*")(i)?;
        Ok((i, PathStepPatternComponent::Regex("(?:.*?)".to_string())))
    }

    fn parse_exprs_component(i: &str) -> ParseResult<PathStepPatternComponent> {
        let (i, exprs) = delimited(
            preceded(parse_space, char('{')),
            cut(parse_list(preceded(parse_space, char(',')), FilterExpr::parse)),
            cut(preceded(parse_space, char('}'))),
        )(i)?;
        Ok((i, PathStepPatternComponent::Exprs(exprs)))
    }

    fn parse_predicates(i: &str) -> ParseResult<Vec<Box<dyn Expr>>> {
        let mut predicate_exprs: Vec<Box<dyn Expr>> = Vec::new();
        let mut next_i = i;
        loop {
            let i = next_i;
            let Ok((i, predicate_expr)) = delimited(
                preceded(parse_space, char('[')),
                cut(FilterExpr::parse),
                cut(preceded(parse_space, char(']'))),
            )(i).wrap_failure()? else {
                break
            };
            predicate_exprs.push(predicate_expr);
            next_i = i
        };
        Ok((next_i, predicate_exprs))
    }

    fn evaluate_predicates(ctx: &EvaluationContext, predicate_exprs: &Vec<Box<dyn Expr>>, value: Value) -> Result<Value, EvaluateError> {
        let mut next_value = value;
        for predicate_expr in predicate_exprs {
            let value = next_value;
            let mut values = Vec::new();
            for context_value in &value {
                let ctx = &ctx.scope(context_value.clone());
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

    fn evaluate(&self, ctx: &EvaluationContext, value: &Value) -> Result<Value, EvaluateError> {
        let value = self.op.evaluate(ctx, value)?;
        Self::evaluate_predicates(ctx, &self.predicate_exprs, value)
    }
}

impl PartialEq for PathStep {
    fn eq(&self, expr: &Self) -> bool {
        self.op == expr.op && self.predicate_exprs == expr.predicate_exprs
    }
}

#[derive(Debug, PartialEq)]
pub enum PathStepOperation {
    Recursive,
    Pattern(Vec<PathStepPatternComponent>),
}

#[derive(Debug, PartialEq)]
pub enum PathStepPatternComponent {
    Name(String),
    Regex(String),
    Exprs(Vec<Box<dyn Expr>>),
}

impl PathStepOperation {
    fn evaluate(&self, ctx: &EvaluationContext, value: &Value) -> Result<Value, EvaluateError> {
        match self {
            Self::Recursive => Self::evaluate_recursive(value),
            Self::Pattern(components) => Self::evaluate_pattern(components, ctx, value),
        }
    }

    fn evaluate_recursive(values: &Value) -> Result<Value, EvaluateError> {
        let mut result_path_values = BTreeSet::new();
        for value in values {
            let path: PathBuf = value.into();
            if is_dir(&path) {
                Self::get_descendant_path_values(&path, &mut result_path_values)?;
            }
            result_path_values.insert(RealValue::Path(path));
        };
        Ok(Value::Set(result_path_values, PathExistence::Checked))
    }

    fn get_descendant_path_values(dir: impl AsRef<Path>, result_path_values: &mut BTreeSet<RealValue>) -> Result<(), EvaluateError> {
        let dir = dir.as_ref();
        let dir_entries = read_dir(dir).map_err(|e| EvaluateError::CouldntReadDir(e.kind(), e.to_string()))?;
        for dir_entry in dir_entries {
            let dir_entry = dir_entry.map_err(|e| EvaluateError::CouldntReadDir(e.kind(), e.to_string()))?;
            let path = dir.join(dir_entry.file_name());
            if is_dir(&path) {
                Self::get_descendant_path_values(&path, result_path_values)?;
            }
            result_path_values.insert(RealValue::Path(path));
        }
        Ok(())
    }

    fn evaluate_pattern(components: &Vec<PathStepPatternComponent>, ctx: &EvaluationContext, values: &Value) -> Result<Value, EvaluateError> {
        use PathStepPatternComponent as Comp;
        use PathExistence::*;
        let path_existence = match values.path_existence() {
            Checked => Checked,
            NotChecked => components.iter().fold(NotChecked, |pe, comp| match (pe, comp) {
                (Checked, _) => Checked,
                (NotChecked, Comp::Name(_)) => NotChecked,
                (NotChecked, Comp::Regex(_)) => Checked,
                (NotChecked, Comp::Exprs(_)) => NotChecked,
            }),
        };

        enum EvaluatedComp<'a> {
            Name(&'a str),
            Regex(&'a str),
            Exprs(Vec<String>),
        }

        let mut result_values = BTreeSet::new();

        for context_value in values {
            let ctx = ctx.scope(context_value.clone());
            let mut evaluated_comps = Vec::new();
            for comp in components {
                let evaluated_comp = match comp {
                    Comp::Name(name) => EvaluatedComp::Name(name),
                    Comp::Regex(regex) => EvaluatedComp::Regex(regex),
                    Comp::Exprs(exprs) => {
                        let mut result_strs = Vec::new();
                        for expr in exprs {
                            let results = expr.evaluate(&ctx)?;
                            for result in &results {
                                result_strs.push(result.into());
                            }
                        };
                        EvaluatedComp::Exprs(result_strs)
                    },
                };
                evaluated_comps.push(evaluated_comp)
            };

            let context_dir: PathBuf = context_value.clone().into();
            let mut paths = Vec::new();
            match path_existence {
                Checked => {
                    let mut regex_str = String::new();
                    regex_str.push_str("^");
                    for comp in evaluated_comps {
                        let regex_comp_str = match comp {
                            EvaluatedComp::Name(name) => regex::escape(name),
                            EvaluatedComp::Regex(regex) => regex.to_string(),
                            EvaluatedComp::Exprs(names) => {
                                let name_regex_str: Vec<_> = names.iter().map(|n| regex::escape(n)).collect();
                                name_regex_str.join("|")
                            },
                        };
                        let regex_comp_str = format!("(?:{})", regex_comp_str);
                        regex_str.push_str(&regex_comp_str);
                    };
                    regex_str.push_str("$");
                    let regex = Regex::new(&regex_str).unwrap();
                    if is_dir(&context_dir) {
                        let dir_entries = read_dir(&context_dir).map_err(|e| EvaluateError::CouldntReadDir(e.kind(), e.to_string()))?;
                        for dir_entry in dir_entries {
                            let dir_entry = dir_entry.map_err(|e| EvaluateError::CouldntReadDir(e.kind(), e.to_string()))?;
                            let filename = dir_entry.file_name().to_string_lossy().into_owned();
                            if regex.is_match(&filename) {
                                let path = PathBuf::from(filename);
                                paths.push(path);
                            }
                        }
                    };
                },
                NotChecked => {
                    let mut path_strs = vec!["".to_string()];
                    for comp in evaluated_comps {
                        match comp {
                            EvaluatedComp::Name(name) => {
                                for path_str in path_strs.iter_mut() {
                                    *path_str = path_str.to_owned() + name;
                                }
                            },
                            EvaluatedComp::Regex(_) => unreachable!(),
                            EvaluatedComp::Exprs(names) => {
                                let mut new_path_strs = Vec::new();
                                for path_str in path_strs {
                                    for name in &names {
                                        let new_path_str = path_str.clone() + name;
                                        new_path_strs.push(new_path_str);
                                    }
                                }
                                path_strs = new_path_strs;
                            },
                        };
                    };
                    for path_str in &path_strs {
                        let path = PathBuf::from(path_str);
                        paths.push(path);
                    };
                },
            };

            for path in paths {
                let path = context_dir.join(path);
                result_values.insert(RealValue::Path(path));
            }
        };
        Ok(Value::Set(result_values, path_existence))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralRootPath {
    pub path: PathBuf
}

impl LiteralRootPath {
    #[cfg(not(windows))]
    pub fn parse_separator_like_root_path(i: &str) -> ParseResult<Box<dyn Expr>> {
        Self::parse_for_unix(i)
    }

    #[cfg(windows)]
    pub fn parse_separator_like_root_path(i: &str) -> ParseResult<Box<dyn Expr>> {
        Self::parse_for_windows(i)
    }

    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, _) = preceded(parse_space, tag("~"))(i)?;
        Ok((i, Box::new(LiteralRootPath { path: home_dir().unwrap_or(PathBuf::from("/")) })))
    }

    pub fn parse_for_unix(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, path) = preceded(parse_space, tag("/"))(i)?;
        Ok((i, Box::new(LiteralRootPath { path: PathBuf::from(path) })))
    }

    pub fn parse_for_windows(i: &str) -> ParseResult<Box<dyn Expr>> {
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

    fn parse_windows_drive_root(i: &str) -> ParseResult<&str> {
        recognize(pair(satisfy(|c| 'A' <= c && c <= 'Z'), tag(r":\")))(i)
    }
}

impl Expr for LiteralRootPath {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(Value::from([self.path.clone()]))
    }

    expr_eq!();
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub identifier: String,
    arg_exprs: Vec<Box<dyn Expr>>,
}

impl FunctionCall {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, identifier) = preceded(
            parse_space,
            parse_identifier,
        )(i)?;

        let (i, arg_exprs) = delimited(
            preceded(parse_space, char('(')),
            cut(parse_list(preceded(parse_space, char(',')), FilterExpr::parse)),
            cut(preceded(parse_space, char(')'))),
        )(i)?;
        let identifier = identifier.to_string();

        Ok((i, Box::new(FunctionCall { identifier, arg_exprs })))
    }
}

impl Expr for FunctionCall {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let Some(function) = ctx.get_function(&self.identifier) else {
            return Err(EvaluateError::FunctionNotFound(self.identifier.clone()));
        };
        let mut arg_values: Vec<Value> = Vec::new();
        for expr in &self.arg_exprs {
            let value = expr.evaluate(ctx)?;
            arg_values.push(value);
        }
        let return_value = function(ctx, &arg_values);
        Ok(return_value)
    }

    expr_eq!();
}

#[derive(Debug, Eq, PartialEq)]
pub struct LiteralString {
    pub string: String,
}

impl LiteralString {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let (i, quote_char) = preceded(
            parse_space,
            alt((char('"'), char('\'')))
        )(i)?;
        let (i, string) = cut(terminated(take_while(|c| c != quote_char), char(quote_char)))(i)?;
        Ok((i, Box::new(LiteralString { string: string.to_string() })))
    }
}

impl Expr for LiteralString {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(Value::from(self.string.clone()))
    }

    expr_eq!();
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinaryOperator,
    pub left_expr: Box<dyn Expr>,
    pub right_expr: Box<dyn Expr>,
}

impl BinaryExpr {
    pub fn parse(i: &str) -> ParseResult<Box<dyn Expr>> {
        let mut stack: Vec<(&str, BinaryOperator, Box<dyn Expr>)> = Vec::new();
        let mut current_i = i;
        let (i, expr) = loop {
            let i = current_i;
            let (before_op_i, new_expr) = match PathExpr::parse(i).wrap_failure()? {
                Ok(r) => r,
                Err(err) => {
                    match stack.pop() {
                        Some((i, _, expr)) => break (i, expr),
                        None => return Err(Err::Error(err)),
                    }
                },
            };
            let Ok((after_op_i, new_op)) = BinaryOperator::parse(before_op_i).wrap_failure()? else {
                break (before_op_i, new_expr)
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
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(self.op.evaluate(&self.left_expr.evaluate(ctx)?, &self.right_expr.evaluate(ctx)?))
    }

    expr_eq!();
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

    fn parse(i: &str) -> ParseResult<Self> {
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
            if let Ok((i, _)) = preceded(parse_space, tag(op_str))(i).wrap_failure()? {
                return Ok((i, op))
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

fn ensure_not_empty_path(path: &Path) -> &Path {
    if path == Path::new("") {
        Path::new(".")
    } else {
        path
    }
}

fn is_dir(path: impl AsRef<Path>) -> bool {
    let path = path.as_ref();
    let path = ensure_not_empty_path(path);
    path.is_dir()
}

fn read_dir(path: impl AsRef<Path>) -> io::Result<fs::ReadDir> {
    let path = path.as_ref();
    let path = ensure_not_empty_path(path);
    fs::read_dir(path)
}

