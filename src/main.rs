#![allow(unstable_name_collisions)]

mod ast;
mod jit;
mod parse;

fn main() {
    color_backtrace::install();

    use jit::*;

    let input = std::fs::read_to_string("test.noot").unwrap();
    // Parse
    match parse::parse(&input) {
        Ok(nodes) => {
            #[cfg(feature = "debug")]
            println!("{:#?}", nodes);

            let mut jitter = Jitter::new();
            jitter.jit_nodes(&nodes);
            if jitter.errors.is_empty() {
            } else {
                for error in jitter.errors {
                    println!("{}", error);
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
