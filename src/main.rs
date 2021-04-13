#![allow(unstable_name_collisions)]

mod ast;
mod parse;
mod transpile;

fn main() {
    use std::process::*;

    use transpile::*;

    color_backtrace::install();

    let input = std::fs::read_to_string("test.noot").unwrap();
    // Parse
    match parse::parse(&input) {
        Ok(items) => {
            #[cfg(feature = "debug")]
            println!("{:#?}", items);
            // Transpile
            let transpilation = transpile(items);
            if transpilation.errors.is_empty() {
                println!("Transpilation succeeded");
                // Compile
                transpilation.write().unwrap();
                let status = Command::new("gcc")
                    .arg("build/main.c")
                    .arg("clibs/tgc.c")
                    .arg("-o")
                    .arg("test")
                    .arg("-O3")
                    .spawn()
                    .unwrap()
                    .wait()
                    .unwrap();
                if status.success() {
                    println!("Compilation succeeded");
                }
            } else {
                for error in &transpilation.errors {
                    println!("{}", error);
                }
            }
        }
        Err(e) => println!("{}", e),
    }
}
