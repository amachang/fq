use fq::{
    parse,
    evaluate,
};

use nom::{
    Err,
    // error::convert_error,
};

use clap::{
    Parser,
};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    query: String,
}

fn main() {
    let args = Args::parse();
    let query = &args.query;
    let result = parse(query);

    let expr = match result {
        Err(Err::Incomplete(_)) => unreachable!(),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            // println!("Parse Error: \n{}", convert_error(query, e));
            println!("Parse Error: {}", e);
            return ();
        },
        Ok(expr) => expr,
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

