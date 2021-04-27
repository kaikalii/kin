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
            println!("Transpilation succeeded");
            #[cfg(feature = "debug")]
            println!("{:#?}", items);
            // Transpile
            let transpilation = transpile(items);
            // Compile
            transpilation.write().unwrap();
            let status = Command::new("gcc")
                .arg("build/main.c")
                .arg("-o")
                .arg("test")
                .arg("-O3")
                .arg("-std=c99")
                .spawn()
                .unwrap()
                .wait()
                .unwrap();
            if status.success() {
                println!("Compilation succeeded");
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", error)
            }
        }
    }
}
