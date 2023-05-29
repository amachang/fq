use fq::{
    parse,
    evaluate,
};

fn main() {
    for source_str in Vec::from(["/", " /", " / ", " .", "-30", "-30 ", "-30A"]) {
        match parse(source_str) {
            Ok(expr) => {
                println!("Parsed {:?}: {:?}", source_str, evaluate(&*expr));
            },
            Err(e) => println!("Parsed {:?}: {:?}", source_str, e),
        };
    }
}

