use fq::*;

use std::path::PathBuf;

use nom::{
    error::convert_error,
};

use clap::{
    ArgAction,
    Parser,
};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    query: String,

    #[arg(long, action = ArgAction::SetTrue)]
    show_parsed_expr: bool,

    #[arg(long, action = ArgAction::SetTrue)]
    no_memoize: bool,
}

fn main() {
    let args = Args::parse();
    let query: &str = &args.query;
    let result = parse(query);

    let expr = match result {
        Err(e) => {
            println!("Parse Error: \n{}", convert_error(query, e));
            return ();
        },
        Ok(expr) => expr,
    };

    if args.show_parsed_expr {
        println!("Pasred Expression: {:?}", expr);
    };

    let result = if args.no_memoize {
        evaluate(&expr)
    } else {
        evaluate_with_cache(&expr)
    };
    let result = match result {
        Err(e) => {
            println!("Excution Error: {:?}", e);
            return ();
        },
        Ok(result) => result,
    };
    let mut paths: Vec<PathBuf> = result.into_iter().map(|v| v.into()).collect();
    paths.sort();
    for path in &paths {
        println!("{}", path.display());
    }
}

