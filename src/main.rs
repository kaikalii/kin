#![allow(unstable_name_collisions)]

mod ast;
mod compile;
mod parse;

fn main() {
    use std::process::*;

    use compile::*;

    color_backtrace::install();

    let input = std::fs::read_to_string("test.noot").unwrap();
    match parse::parse(&input) {
        Ok(items) => {
            println!();

            let mut target = CTarget::new("main", true);
            target.compile_items(items, false);
            if target.errors.is_empty() {
                println!("Transpilation succeeded");
                target.write().unwrap();
                let status = Command::new("gcc")
                    .arg("build/main.c")
                    .arg("build/tgc.c")
                    .arg("-o")
                    .arg("test")
                    .spawn()
                    .unwrap()
                    .wait()
                    .unwrap();
                if status.success() {
                    println!("Compilation succeeded");
                }
            } else {
                for error in &target.errors {
                    println!("{}", error);
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
