#![allow(unstable_name_collisions)]

mod ast;
mod parse;

fn main() {
    color_backtrace::install();

    let input = std::fs::read_to_string("test.noot").unwrap();
    // Parse
    match parse::parse(&input) {
        Ok(nodes) => {
            #[cfg(feature = "debug")]
            println!("{:#?}", nodes);
        }
        Err(e) => println!("{}", e),
    }
}
