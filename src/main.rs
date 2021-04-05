#![allow(unstable_name_collisions)]
#![allow(dead_code)]

mod ast;
mod compile;
mod parse;
mod resolve;

fn main() {
    use resolve::*;

    color_backtrace::install();

    let input = std::fs::read_to_string("test.noot").unwrap();
    match parse::parse(&input) {
        Ok(mut items) => {
            println!("{}", items);

            println!();

            let mut resolver = Resolver::new();
            items.resolve(&mut resolver);
            if resolver.errors.is_empty() {
                println!("No resolution errors");
            } else {
                for error in &resolver.errors {
                    println!("{}", error);
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
