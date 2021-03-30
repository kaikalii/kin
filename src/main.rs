#![allow(unstable_name_collisions)]

mod ast;
mod lexer;
mod num;
mod parse;

use std::fs::File;

fn main() {
    color_backtrace::install();

    let file = File::open("test.noot").unwrap();
    let tree = parse::parse(file).unwrap();
    println!("{:?}", tree);
}
