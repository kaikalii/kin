#![allow(unstable_name_collisions)]

mod ast;
mod parse;
mod transpile;

fn main() {
    use std::{path::PathBuf, process::*};

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
            let status = Command::new(compiler_name())
                .arg("build/main.c")
                .arg("-o")
                .arg(PathBuf::from("test").with_extension(EXE_EXT))
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

const EXE_EXT: &str = if cfg!(windows) { "exe" } else { "" };

fn compiler_name() -> &'static str {
    use std::process::*;
    for name in &["gcc", "clang"] {
        if Command::new(name)
            .arg("-v")
            .output()
            .map_or(false, |output| output.status.success())
        {
            return name;
        }
    }
    println!("No compatible C compiler detected.");
    exit(1)
}
