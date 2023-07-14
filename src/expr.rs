use super::{
    parse_util::{
        parse_space,
        parse_identifier,
        ParseResult,
        ParseResultWrapper,
    },
    value::{
        Value,
        RealValue,
    },
    primitive::{
        PathExistence,
        Number,
        Pattern,
        PathInfo,
        PathInfoError,
    },
    function::{
        call_function,
    },
};

use std::{
    fmt::Debug,
    collections::{
        hash_map::Entry,
        HashMap,
    },
    collections::{
        HashSet,
    },
    path,
    hash::{
        Hash,
        Hasher,
        BuildHasher,
    },
};

use regex;

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
    multi::{
        separated_list0,
        separated_list1,
        many1_count,
    },
    Err,
    error::{
        ParseError,
        ErrorKind,
    },
};

#[derive(Debug)]
pub struct EvaluationContext {
    pub var_map: HashMap<String, Value>,
    pub context_value: Value,
    pub in_pattern: bool,
}

impl EvaluationContext {
    pub fn new(context_value: Value, in_pattern: bool) -> EvaluationContext {
        EvaluationContext {
            var_map: HashMap::new(),
            context_value,
            in_pattern,
        }
    }

    pub fn set_variable(&mut self, key: impl AsRef<str>, value: Value) {
        self.var_map.insert(key.as_ref().to_string(), value);
    }

    pub fn get_variable(&self, key: impl AsRef<str>) -> Option<&Value> {
        self.var_map.get(key.as_ref())
    }

    pub fn get_context_value(&self) -> &Value {
        &self.context_value
    }

    pub fn scope(&self, value: &Value, vars: &[(&str, &Value)], in_pattern: bool) -> Self {
        let parent_dir: PathInfo = self.context_value.clone().into();
        let context_dir: PathInfo = value.into();

        let mut var_map = self.var_map.clone();
        for (name, value) in vars {
            var_map.insert(name.to_string(), value.clone().clone());
        }

        let context_value = Value::from(parent_dir.join_not_checked(context_dir));

        Self {
            var_map,
            context_value,
            in_pattern,
        }
    }

    fn exactly_eq_value(self_value: &Value, other_value: &Value) -> bool {
        match (self_value, other_value) {
            (Value::Boolean(r), Value::Boolean(l)) => r == l,
            (Value::String(r), Value::String(l)) => r == l,
            (Value::Set(r, r_e), Value::Set(l, l_e)) => r == l && r_e == l_e,
            (Value::Path(r), Value::Path(l)) => r == l,
            (Value::Number(Number::Integer(r)), Value::Number(Number::Integer(l))) => r == l,
            (Value::Number(Number::Float(r)), Value::Number(Number::Float(l))) => {
                if r.is_nan() && l.is_nan() {
                    true
                } else {
                    r == l
                }
            },
            _ => false,
        }
    }

    fn hash_value<H: Hasher>(value: &Value, state: &mut H) {
        match value {
            Value::Boolean(b) => b.hash(state),
            Value::String(s) => s.hash(state),
            Value::Pattern(p) => p.hash(state),
            Value::Set(s, e) => {
                e.hash(state);
                s.into_iter().collect::<Vec<_>>().sort().hash(state)
            },
            Value::Path(p) => p.hash(state),
            Value::Number(Number::Integer(i)) => i.hash(state),
            Value::Number(Number::Float(f)) => f.to_bits().hash(state),
        }
    }
}

impl PartialEq for EvaluationContext {
    fn eq(&self, other: &EvaluationContext) -> bool {
        if self.var_map.len() != other.var_map.len() {
            return false;
        };

        if !self.var_map.iter().all(|(key, value)| other.var_map.get(key).map_or(false, |v| Self::exactly_eq_value(value, v))) {
            return false;
        };

        Self::exactly_eq_value(&self.context_value, &other.context_value)
    }
}

impl Eq for EvaluationContext {
}

impl Hash for EvaluationContext {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut v = self.var_map.iter().map(|(key, val)| {
            let mut st = self.var_map.hasher().build_hasher();
            key.hash(&mut st);
            (st.finish(), key, val)
        }).collect::<Vec<_>>();
        v.sort_by_key(|(h, _, _)| *h);

        for (_, key, val) in v.iter() {
            key.hash(state);
            Self::hash_value(val, state);
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum EvaluateError {
    FunctionNotFound(String),
    VariableNotFound(String),
    CsvReadError(PathInfo, String),
    RegexError(String),
    PathInfoError(PathInfoError),
    LiteralRootPathNotAllowedInPathPattern,
    RecursivePathStepNotAllowedInPathPattern,
    PathStepNotAllowedInPathPattern,
    PredicateNotAllowedInPathPattern,
}

impl From<regex::Error> for EvaluateError {
    fn from(err: regex::Error) -> Self {
        Self::RegexError(format!("{}", err))
    }
}

impl From<PathInfoError> for EvaluateError {
    fn from(err: PathInfoError) -> Self {
        Self::PathInfoError(err)
    }
}

pub type MemoizationCache<'a> = HashMap<MemoizationKey<'a>, Value>;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum MemoizationKey<'a> {
    FilterExpr(&'a FilterExpr, &'a EvaluationContext),
    PathExpr(&'a PathExpr, &'a EvaluationContext),
    PathRootStepExpr(&'a PathRootStepExpr, &'a EvaluationContext),
    PathRootExpr(&'a PathRootExpr, &'a EvaluationContext),
    LiteralPathRootExpr(&'a LiteralPathRootExpr, &'a EvaluationContext),
    MemberCallExpr(&'a MemberCallExpr, &'a EvaluationContext),
    PrimaryExpr(&'a PrimaryExpr, &'a EvaluationContext),
    VariableReferenceExpr(&'a VariableReferenceExpr, &'a EvaluationContext),
    FunctionCallExpr(&'a FunctionCallExpr, &'a EvaluationContext),
    LiteralStringExpr(&'a LiteralStringExpr, &'a EvaluationContext),
    BinaryExpr(&'a BinaryExpr, &'a EvaluationContext),
    BinaryOperandExpr(&'a BinaryOperandExpr, &'a EvaluationContext),
}

pub trait Expr: Debug {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError>;

    fn evaluate_then_cache<'a>(&'a self, ctx: &'a EvaluationContext, cache: &'a mut MemoizationCache<'a>) -> Result<Value, EvaluateError> {
        let value = match cache.entry(self.memoization_key(ctx)) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let value = self.evaluate(ctx)?;
                entry.insert(value)
            },
        };
        Ok(value.clone())
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey;
}

pub fn parse(i: &str) -> ParseResult<FilterExpr> {
    FilterExpr::parse(i)
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FilterExpr {
    pub exprs: Vec<BinaryOperandExpr>,
}

impl FilterExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, exprs) = separated_list1(preceded(parse_space, char('|')), BinaryOperandExpr::parse)(i)?;
        Ok((i, Self { exprs }))
    }
}

impl Expr for FilterExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let mut expr_iter = self.exprs.iter();
        let mut value = expr_iter.next().expect("more than one exprs").evaluate(ctx)?;
        for expr in expr_iter {
            let mut value_vec = Vec::new();
            for v in &value {
                let ctx = ctx.scope(&v, &[("_", &v)], false);
                let result_value = expr.evaluate(&ctx)?;
                value_vec.push(result_value);
            }
            value = Value::from(value_vec);
        }
        Ok(value)
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::FilterExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct PathExpr {
    pub root_expr: PathRootStepExpr,
    pub steps: Vec<PathStep>,
}

impl PathExpr {
    pub fn parse(i: &str) -> ParseResult<PathExpr> {
        Self::parse_core(
            i,
            preceded(parse_space, tag(path::MAIN_SEPARATOR_STR)),
            PathRootStepExpr::parse_separator_like_expr,
            PathRootStepExpr::parse,
            PathStep::parse,
        )
    }

    // remove parser dependencies for test
    pub fn parse_core<'a, T>(
        i: &'a str,
        mut parse_separator: impl FnMut(&'a str) -> ParseResult<'a, T>,
        mut parse_separator_like_expr: impl FnMut(&'a str) -> ParseResult<'a, PathRootStepExpr>,
        mut parse_path_root_expr: impl FnMut(&'a str) -> ParseResult<'a, PathRootStepExpr>,
        parse_path_step: impl FnMut(&'a str) -> ParseResult<'a, PathStep>,
    ) -> ParseResult<PathExpr> {
        let (i, root_expr) = match parse_separator_like_expr(i).wrap_failure()? {
            Ok(r) => r,
            Err(_) => {
                let (i, root_expr) = parse_path_root_expr(i)?;
                let i = match parse_separator(i).wrap_failure()? {
                    Ok((i, _)) => i,
                    Err(_) => return Ok((i, PathExpr { root_expr, steps: vec![] })),
                };
                (i, root_expr)
            },
        };
        let (i, steps) = separated_list0(parse_separator, parse_path_step)(i)?;
        Ok((i, PathExpr { root_expr, steps }))
    }
}

impl Expr for PathExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let mut value = self.root_expr.evaluate(ctx)?;
        if self.steps.len() > 0 {
            if ctx.in_pattern {
                return Err(EvaluateError::PathStepNotAllowedInPathPattern)
            }
        }
        for step in &self.steps {
            value = step.evaluate(&ctx, &value, false)?;
        };
        return Ok(value)
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::PathExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct PathRootStepExpr {
    pub expr: PathRootExpr,
    pub predicate_exprs: Vec<FilterExpr>,
}

impl PathRootStepExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, expr) = alt((
                |i| MemberCallExpr::parse(i).map(|(i, expr)| (i, PathRootExpr::MemberCallExpr(expr))),
                |i| LiteralPathRootExpr::parse(i).map(|(i, expr)| (i, PathRootExpr::LiteralPathRootExpr(expr))),
                |i| PathStepOperation::parse(i).map(|(i, op)| (i, PathRootExpr::PathStepOperation(op))),
        ))(i)?;

        let (i, predicate_exprs) = PathStep::parse_predicates(i)?;
        Ok((i, Self { expr, predicate_exprs }))
    }

    pub fn parse_separator_like_expr(i: &str) -> ParseResult<Self> {
        let (i, expr) = LiteralPathRootExpr::parse_separator_like_expr(i)?;
        let expr = PathRootExpr::LiteralPathRootExpr(expr);
        Ok((i, Self { expr, predicate_exprs: vec![] }))
    }
}

impl Expr for PathRootStepExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let value = self.expr.evaluate(ctx)?;
        let value = PathStep::evaluate_predicates(ctx, &self.predicate_exprs, value)?;
        Ok(value)
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::PathRootStepExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum PathRootExpr {
    MemberCallExpr(MemberCallExpr),
    LiteralPathRootExpr(LiteralPathRootExpr),
    PathStepOperation(PathStepOperation),
}

impl Expr for PathRootExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        use PathRootExpr::*;
        match self {
            MemberCallExpr(expr) => expr.evaluate(ctx),
            LiteralPathRootExpr(expr) => {
                if ctx.in_pattern {
                    return Err(EvaluateError::LiteralRootPathNotAllowedInPathPattern)
                }
                expr.evaluate(ctx)
            },
            PathStepOperation(op) => {
                op.evaluate_as_root(ctx, ctx.in_pattern)
            },
        }
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::PathRootExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct LiteralPathRootExpr {
    pub path: PathInfo
}

impl LiteralPathRootExpr {
    #[cfg(not(windows))]
    pub fn parse_separator_like_expr(i: &str) -> ParseResult<Self> {
        Self::parse_for_unix(i)
    }

    #[cfg(windows)]
    pub fn parse_separator_like_expr(i: &str) -> ParseResult<Self> {
        Self::parse_for_windows(i)
    }

    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, _) = preceded(parse_space, tag("~"))(i)?;
        Ok((i, LiteralPathRootExpr { path: PathInfo::home_dir().unwrap_or(PathInfo::from("/")) }))
    }

    pub fn parse_for_unix(i: &str) -> ParseResult<Self> {
        let (i, path) = preceded(parse_space, tag("/"))(i)?;
        Ok((i, LiteralPathRootExpr { path: PathInfo::from(path) }))
    }

    pub fn parse_for_windows(i: &str) -> ParseResult<Self> {
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
        Ok((i, LiteralPathRootExpr { path: PathInfo::from(path) }))
    }

    fn parse_windows_drive_root(i: &str) -> ParseResult<&str> {
        recognize(pair(satisfy(|c| 'A' <= c && c <= 'Z'), tag(r":\")))(i)
    }
}

impl Expr for LiteralPathRootExpr {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(Value::from(self.path.clone()))
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::LiteralPathRootExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct MemberCallExpr {
    pub expr: PrimaryExpr,
    pub function_calls: Vec<FunctionCall>,
}

impl MemberCallExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, expr) = PrimaryExpr::parse(i)?;
        let mut i = i;
        let mut function_calls = Vec::new();
        loop {
            i = match preceded(parse_space, char('.'))(i).wrap_failure()? {
                Err(_) => break,
                Ok((i, _)) => i,
            };
            let (sub_i, function_call) = cut(FunctionCall::parse)(i)?;
            i = sub_i;
            function_calls.push(function_call);
        }
        Ok((i, MemberCallExpr { expr, function_calls }))
    }
}

impl Expr for MemberCallExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let mut value = self.expr.evaluate(ctx)?;
        for function_call in &self.function_calls {
            value = function_call.evaluate(ctx, Some(value))?;
        };
        Ok(value)
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::MemberCallExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum PrimaryExpr {
    FilterExpr(FilterExpr),
    LiteralStringExpr(LiteralStringExpr),
    VariableReferenceExpr(VariableReferenceExpr),
    FunctionCallExpr(FunctionCallExpr),
}

impl PrimaryExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        alt((
                |i| delimited(
                    preceded(parse_space, char('(')),
                    cut(FilterExpr::parse),
                    cut(preceded(parse_space, char(')'))),
                )(i).map(|(i, expr)| (i, Self::FilterExpr(expr))),
                |i| LiteralStringExpr::parse(i).map(|(i, expr)| (i, Self::LiteralStringExpr(expr))),
                |i| VariableReferenceExpr::parse(i).map(|(i, expr)| (i, Self::VariableReferenceExpr(expr))),
                |i| FunctionCallExpr::parse(i).map(|(i, expr)| (i, Self::FunctionCallExpr(expr))),
        ))(i)
    }
}

impl Expr for PrimaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        use PrimaryExpr::*;
        match self {
            FilterExpr(expr) => expr.evaluate(ctx),
            LiteralStringExpr(expr) => expr.evaluate(ctx),
            VariableReferenceExpr(expr) => expr.evaluate(ctx),
            FunctionCallExpr(expr) => expr.evaluate(ctx),
        }
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::PrimaryExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct PathStep {
    pub op: PathStepOperation,
    pub predicate_exprs: Vec<FilterExpr>,
}

impl PathStep {
    pub fn parse(i: &str) -> ParseResult<PathStep> {
        let (i, op) = preceded(parse_space, PathStepOperation::parse)(i)?;
        let (i, predicate_exprs) = Self::parse_predicates(i)?;
        Ok((i, PathStep { op, predicate_exprs }))
    }

    fn parse_predicates(i: &str) -> ParseResult<Vec<FilterExpr>> {
        let mut predicate_exprs: Vec<FilterExpr> = Vec::new();
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

    fn evaluate_predicates(ctx: &EvaluationContext, predicate_exprs: &Vec<FilterExpr>, value: Value) -> Result<Value, EvaluateError> {
        let mut next_value = value;
        for predicate_expr in predicate_exprs {
            let value = next_value;
            let mut values = Vec::new();
            for context_value in &value {
                let ctx = &ctx.scope(&context_value, &[], false);
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

    fn evaluate(&self, ctx: &EvaluationContext, value: &Value, in_pattern: bool) -> Result<Value, EvaluateError> {
        let value = self.op.evaluate(ctx, value, in_pattern)?;
        if self.predicate_exprs.len() > 0 {
            if ctx.in_pattern {
                return Err(EvaluateError::PredicateNotAllowedInPathPattern)
            }
        }
        Self::evaluate_predicates(ctx, &self.predicate_exprs, value)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum PathStepOperation {
    Recursive,
    Pattern(Vec<PathStepPatternComponent>),
}

impl PathStepOperation {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, _) = parse_space(i)?;

        if let Ok((i, _)) = tag("**")(i).wrap_failure()? {
            return Ok((i, Self::Recursive));
        };

        let mut components: Vec<PathStepPatternComponent> = Vec::new();
        let mut next_i = i;
        loop {
            let i = next_i;

            if let Ok((i, _)) = tag("**")(i).wrap_failure()? {
                return Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Tag)));
            };

            let Ok((i, component)) = PathStepPatternComponent::parse(i).wrap_failure()? else {
                break
            };

            components.push(component);

            next_i = i;
        };
        if components.len() == 0 {
            return Err(Err::Error(ParseError::from_error_kind(i, ErrorKind::Char)));
        };

        let i = next_i;
        Ok((i, Self::Pattern(components)))
    }

    fn evaluate(&self, ctx: &EvaluationContext, value: &Value, in_pattern: bool) -> Result<Value, EvaluateError> {
        match self {
            Self::Recursive => {
                if in_pattern {
                    return Err(EvaluateError::RecursivePathStepNotAllowedInPathPattern)
                }
                Self::evaluate_recursive(value)
            },
            Self::Pattern(components) => Self::evaluate_pattern(components, ctx, value, in_pattern),
        }
    }

    fn evaluate_as_root(&self, ctx: &EvaluationContext, in_pattern: bool) -> Result<Value, EvaluateError> {
        if let Self::Pattern(components) = self {
            if let Some(PathStepPatternComponent::Exprs(exprs)) = components.get(0) {
                if components.len() == 1 {
                    // if the root's only component is expressions, it behaves as set(expr, ...)
                    let mut results = Vec::new();
                    for expr in exprs.iter() {
                        results.push(expr.evaluate(ctx)?);
                    }
                    return Ok(Value::from(results));
                }
            };
        }

        self.evaluate(ctx, &Value::from(""), in_pattern)
    }

    fn evaluate_recursive(values: &Value) -> Result<Value, EvaluateError> {
        let mut result_path_values = HashSet::new();
        for value in values {
            let path: PathInfo = value.into();
            if path.is_dir() {
                Self::get_descendant_path_values(&path, &mut result_path_values)?;
            }
            result_path_values.insert(RealValue::Path(path));
        };
        Ok(Value::Set(result_path_values, PathExistence::Checked))
    }

    fn get_descendant_path_values(dir: &PathInfo, result_path_values: &mut HashSet<RealValue>) -> Result<(), EvaluateError> {
        for path in dir.child_files()? {
            let path = path?;
            if path.is_dir_no_syscall().expect("No need syscall for checking dir path made from DirEntry") {
                Self::get_descendant_path_values(&path, result_path_values)?;
            }
            result_path_values.insert(RealValue::Path(path));
        }
        Ok(())
    }

    fn evaluate_pattern(components: &Vec<PathStepPatternComponent>, ctx: &EvaluationContext, values: &Value, in_pattern: bool) -> Result<Value, EvaluateError> {
        use PathExistence::*;

        let mut path_existence = values.path_existence();
        let mut context_value_patterns_pairs: Vec<(Value, Vec<Pattern>)> = Vec::new();

        for context_value in values {
            let mut cumulative_patterns = vec![Pattern::new_name("")];
            for comp in components {
                let comp_patterns = comp.evaluate(ctx, &context_value)?;
                cumulative_patterns = cumulative_patterns.iter().flat_map(|cumulative_pattern| {
                    comp_patterns.iter().map(|comp_pattern| cumulative_pattern + comp_pattern).collect::<Vec<Pattern>>()
                }).collect();
            };

            let all_name_patterns = cumulative_patterns.iter().all(|p| {
                match p {
                    Pattern::Name(_) => true,
                    Pattern::Regex(_) => false,
                }
            });

            path_existence = if path_existence == NotChecked && all_name_patterns {
                NotChecked
            } else {
                Checked
            };

            context_value_patterns_pairs.push((context_value, cumulative_patterns));
        }

        if in_pattern {
            assert_eq!(context_value_patterns_pairs.len(), 1);
            let (_, patterns) = &context_value_patterns_pairs[0];
            return Ok(Value::from(Pattern::join(&patterns)));
        }

        let mut result_values = HashSet::new();

        for (context_value, patterns) in context_value_patterns_pairs.into_iter() {
            let context_dir: PathInfo = context_value.into();
            let mut paths = Vec::new();

            match path_existence {
                Checked => {
                    let pattern = Pattern::join(&patterns);
                    if context_dir.is_dir() {
                        for path in context_dir.child_files()? {
                            let path = path?;
                            if let Some(filename) = path.file_name() {
                                if pattern.is_fullmatch(&filename)? {
                                    paths.push(path);
                                }
                            }
                        }
                    };
                },
                NotChecked => {
                    for pattern in patterns.into_iter() {
                        let Pattern::Name(filename) = pattern else {
                            unreachable!();
                        };
                        paths.push(context_dir.join_not_checked(PathInfo::from(filename)));
                    };
                },
            };

            for path in paths {
                result_values.insert(RealValue::Path(path));
            }
        };
        Ok(Value::Set(result_values, path_existence))
    }

}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum PathStepPatternComponent {
    Pattern(Pattern),
    Exprs(Vec<FilterExpr>),
}

impl PathStepPatternComponent {
    fn parse(i: &str) -> ParseResult<Self> {
        alt((
                Self::parse_name_component,
                Self::parse_match_all_component,
                Self::parse_exprs_component,
        ))(i)
    }

    fn parse_name_component(i: &str) -> ParseResult<Self> {
        let (i, name) = recognize(many1_count(alt((alphanumeric1, tag("_"), tag("-"), tag(".")))))(i)?;
        Ok((i, Self::Pattern(Pattern::Name(name.to_string()))))
    }

    fn parse_match_all_component(i: &str) -> ParseResult<Self> {
        let (i, _) = tag("*")(i)?;
        Ok((i, Self::Pattern(Pattern::Regex("(?:.*?)".to_string()))))
    }

    fn parse_exprs_component(i: &str) -> ParseResult<Self> {
        let (i, exprs) = delimited(
            preceded(parse_space, char('{')),
            cut(separated_list0(preceded(parse_space, char(',')), FilterExpr::parse)),
            cut(preceded(parse_space, char('}'))),
        )(i)?;
        Ok((i, Self::Exprs(exprs)))
    }

    fn evaluate(&self, ctx: &EvaluationContext, context_value: &Value) -> Result<Vec<Pattern>, EvaluateError> {
        match self {
            Self::Pattern(pattern) => {
                Ok(vec![pattern.clone()])
            },
            Self::Exprs(exprs) => {
                let mut patterns: Vec<Pattern> = Vec::new();
                for expr in exprs {
                    let ctx = ctx.scope(context_value, &[], true);
                    let expr_result = expr.evaluate(&ctx)?;
                    patterns.push(expr_result.into());
                };
                Ok(patterns)
            },
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VariableReferenceExpr {
    pub identifier: String,
}

impl VariableReferenceExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, identifier) = preceded(preceded(parse_space, char('$')), cut(parse_identifier))(i)?;
        let identifier = identifier.to_string();
        Ok((i, Self { identifier }))
    }
}

impl Expr for VariableReferenceExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        let Some(value) = ctx.get_variable(&self.identifier) else {
            return Err(EvaluateError::VariableNotFound(self.identifier.clone()));
        };
        Ok(value.clone())
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::VariableReferenceExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FunctionCallExpr {
    pub function_call: FunctionCall,
}

impl FunctionCallExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, function_call) = FunctionCall::parse(i)?;
        Ok((i, Self { function_call }))
    }
}

impl Expr for FunctionCallExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        self.function_call.evaluate(ctx, None)
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::FunctionCallExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FunctionCall {
    pub identifier: String,
    pub arg_exprs: Vec<FilterExpr>,
}

impl FunctionCall {
    pub fn parse(i: &str) -> ParseResult<FunctionCall> {
        let (i, identifier) = preceded(
            parse_space,
            parse_identifier,
        )(i)?;

        let (i, arg_exprs) = delimited(
            preceded(parse_space, char('(')),
            cut(separated_list0(preceded(parse_space, char(',')), FilterExpr::parse)),
            cut(preceded(parse_space, char(')'))),
        )(i)?;
        let identifier = identifier.to_string();

        Ok((i, FunctionCall { identifier, arg_exprs }))
    }

    pub fn evaluate(&self, ctx: &EvaluationContext, self_value: Option<Value>) -> Result<Value, EvaluateError> {
        let mut arg_values: Vec<Value> = Vec::new();
        if let Some(self_value) = self_value {
            arg_values.push(self_value);
        };
        for expr in &self.arg_exprs {
            let value = expr.evaluate(ctx)?;
            arg_values.push(value);
        }
        call_function(&self.identifier, ctx, &arg_values)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct LiteralStringExpr {
    pub string: String,
}

impl LiteralStringExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let (i, quote_char) = preceded(
            parse_space,
            alt((char('"'), char('\'')))
        )(i)?;
        let (i, string) = cut(terminated(take_while(|c| c != quote_char), char(quote_char)))(i)?;
        Ok((i, Self { string: string.to_string() }))
    }
}

impl Expr for LiteralStringExpr {
    fn evaluate(&self, _: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(Value::from(self.string.clone()))
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::LiteralStringExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOperator,
    pub left_expr: BinaryOperandExpr,
    pub right_expr: BinaryOperandExpr,
}

impl Expr for BinaryExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        Ok(self.op.evaluate(&self.left_expr.evaluate(ctx)?, &self.right_expr.evaluate(ctx)?))
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::BinaryExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum BinaryOperandExpr {
    BinaryExpr(Box<BinaryExpr>),
    PathExpr(PathExpr),
}

impl BinaryOperandExpr {
    pub fn parse(i: &str) -> ParseResult<Self> {
        let mut stack: Vec<(&str, BinaryOperator, BinaryOperandExpr)> = Vec::new();
        let mut current_i = i;
        let (i, expr) = loop {
            let i = current_i;
            let (before_op_i, new_expr): (&str, BinaryOperandExpr) = match PathExpr::parse(i).wrap_failure()? {
                Ok((i, expr)) => (i, BinaryOperandExpr::PathExpr(expr)),
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
                            let binary_expr = BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr };
                            expr = BinaryOperandExpr::BinaryExpr(Box::new(binary_expr));
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
                    let binary_expr = BinaryExpr { op: stacked_op, left_expr: stacked_expr, right_expr: expr };
                    expr = BinaryOperandExpr::BinaryExpr(Box::new(binary_expr));
                },
                None => break,
            };
        };

        Ok((i, expr))
    }
}

impl Expr for BinaryOperandExpr {
    fn evaluate(&self, ctx: &EvaluationContext) -> Result<Value, EvaluateError> {
        use BinaryOperandExpr::*;
        match self {
            BinaryExpr(expr) => expr.evaluate(ctx),
            PathExpr(expr) => expr.evaluate(ctx),
        }
    }

    fn memoization_key<'a>(&'a self, ctx: &'a EvaluationContext) -> MemoizationKey {
        MemoizationKey::BinaryOperandExpr(self, ctx)
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
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

