use fq::{
    parse,
    evaluate,
};

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

    let result = evaluate(&*expr);
    let values = match result {
        Err(e) => {
            println!("Excution Error: {:?}", e);
            return ();
        },
        Ok(values) => values,
    };
    for value in &values {
        let line: String = value.into();
        println!("{}", line);
    }
}

