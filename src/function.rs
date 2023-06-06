use super::{
    expr::{
        EvaluationContext,
        EvaluateError,
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
    io,
    collections::{
        HashSet,
    },
    path::{
        PathBuf,
    },
};

use csv;

pub fn call_function(identifier: &str, ctx: &EvaluationContext, vs: &[Value]) -> Result<Value, EvaluateError> {
    match identifier {
        "string" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            Ok(value.as_string().into_owned())
        },
        "number" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            Ok(value.as_number().into_owned())
        },
        "boolean" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            Ok(value.as_boolean().into_owned())
        },
        "path" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            Ok(value.as_path().into_owned())
        },
        "set" => {
            let vs: Vec<Value> = vs.iter().map(|v| (*v).clone()).collect();
            Ok(Value::from(vs))
        },
        "true" => {
            Ok(Value::from(true))
        },
        "false" => {
            Ok(Value::from(false))
        },
        "not" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            let b: bool = value.into();
            Ok(Value::from(!b))
        },
        "name" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            let path: PathBuf = value.into();
            Ok(match path.file_name() {
                Some(os_name) => Value::from(os_name.to_string_lossy().into_owned()), // TODO String should be
                                                                                      // contains OsString
                None => Value::from(""),
            })
        },
        "len" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            let set: HashSet<_> = value.into();
            Ok(Value::from(set.len()))
        },
        "starts_with" => {
            let (value, vs) = fix_first_arg(ctx, vs, 2);
            let prefix = if 0 == vs.len() {
                "".to_string()
            } else {
                vs[0].clone().into()
            };
            let target_str: String = value.into();
            let result: bool = target_str.starts_with(&prefix);
            Ok(Value::from(result))
        },
        "dir" => {
            let (value, _) = fix_first_arg(ctx, vs, 1);
            let path: PathBuf = value.into();
            Ok(match path.parent() {
                Some(dir) => {
                    let dir: PathBuf = dir.into();
                    Value::from(dir)
                },
                None => Value::from(PathBuf::from("")),
            })
        },
        "select" => {
            let (value, vs) = fix_first_arg(ctx, vs, 2);
            Ok(if 0 == vs.len() {
                value.clone()
            } else {
                let result: bool = vs[0].clone().into();
                if result {
                    value.clone()
                } else {
                    Value::empty_set()
                }
            })
        },
        "csv_columns" => {
            let (value, vs) = fix_first_arg(ctx, vs, 2);
            enum ColumnSelector { All, Index(usize) }
            enum ColumnSelectorOrHeaderName { ColumnSelector(ColumnSelector), HeaderName(String) }
            let column_selector = if 0 == vs.len() {
                ColumnSelectorOrHeaderName::ColumnSelector(ColumnSelector::All)
            } else {
                let selector_value = vs[0].clone();
                if selector_value.likes_number() {
                    ColumnSelectorOrHeaderName::ColumnSelector(ColumnSelector::Index(selector_value.into()))
                } else {
                    ColumnSelectorOrHeaderName::HeaderName(selector_value.into())
                }
            };
            let path: PathBuf = value.into();
            let mut reader = match csv::ReaderBuilder::new().delimiter(b',').has_headers(false).from_path(&path) {
                Err(err) => {
                    match err.kind() {
                        csv::ErrorKind::Io(io_err) => {
                            if io_err.kind() == io::ErrorKind::NotFound {
                                return Ok(Value::empty_set());
                            } else {
                                return Err(EvaluateError::CsvReadError(err.to_string()));
                            }
                        },
                        _ => return Err(EvaluateError::CsvReadError(err.to_string())),
                    }
                },
                Ok(reader) => reader,
            };
            let mut column_values: HashSet<RealValue> = HashSet::new();
            let mut record_iter = reader.records();
            let column_selector = match column_selector {
                ColumnSelectorOrHeaderName::ColumnSelector(r) => r,
                ColumnSelectorOrHeaderName::HeaderName(name) => {
                    let first_record = record_iter.next();
                    let first_record = match first_record {
                        Some(Err(err)) => return Err(EvaluateError::CsvReadError(err.to_string())),
                        Some(Ok(record)) => record,
                        None => return Ok(Value::Set(HashSet::new(), PathExistence::NotChecked)),
                    };
                    match first_record.iter().position(|field| field == name) {
                        Some(index) => ColumnSelector::Index(index),
                        None => return Ok(Value::empty_set()),
                    }
                },
            };

            for record in record_iter {
                let record = match record {
                    Err(err) => return Err(EvaluateError::CsvReadError(err.to_string())),
                    Ok(record) => record,
                };
                match &column_selector {
                    ColumnSelector::All => {
                        column_values.insert(RealValue::String(record.as_slice().to_string()));
                    },
                    ColumnSelector::Index(index) => {
                        match record.get(*index) {
                            Some(field) => {
                                column_values.insert(RealValue::String(field.to_string()));
                            }
                            None => (),
                        }
                    },
                }
            }
            return Ok(Value::Set(column_values, PathExistence::NotChecked));
        },
        _ => Err(EvaluateError::FunctionNotFound(identifier.to_string())),
    }
}

fn fix_first_arg<'a>(ctx: &'a EvaluationContext, vs: &'a [Value], expected_len: usize) -> (&'a Value, &'a [Value]) {
    if vs.len() < expected_len {
        (ctx.get_context_value(), vs)
    } else {
        (&vs[0], &vs[1..])
    }
}
