#![deny(warnings, clippy::all, clippy::pedantic)]

use fq::parse;

fn main() {
    for source_str in Vec::from(["/", " /", " / ", " .", "-30", "-30 ", "-30A"]) {
        match parse(source_str) {
            Ok(expr) => {
                println!("Parsed {:?}: {:?}", source_str, expr.evaluate());
            },
            Err(e) => println!("Parsed {:?}: {:?}", source_str, e),
        };
    }
}

