#![allow(unstable_name_collisions)]

mod ast;
mod parse;
mod transpile;

use std::str::FromStr;

use clap::Clap;

fn main() {
    use std::{path::PathBuf, process::*};

    use transpile::*;

    color_backtrace::install();

    let app = App::parse();

    if app.sub < Sub::Check {
        return;
    }

    // Parse and check
    let input = std::fs::read_to_string("test.noot").unwrap();
    let items = match parse::parse(&input) {
        Ok(items) => items,
        Err(errors) => {
            for error in errors {
                println!("{}", error)
            }
            exit(1);
        }
    };
    println!("Check succeeded");

    // Transpile
    if app.sub < Sub::Trans {
        return;
    }
    let transpilation = transpile(items);
    transpilation.write().unwrap();
    println!("Transpilation succeeded");

    // Compile
    if app.sub < Sub::Trans {
        return;
    }

    let ccomp = app.compiler.unwrap_or_else(CCompiler::find);

    let compile_status = Command::new(ccomp.name())
        .arg("build/main.c")
        .arg("-o")
        .arg(PathBuf::from("test").with_extension(EXE_EXT))
        .arg("-O3")
        .arg("-std=c99")
        .arg(ccomp.stack_size_arg(app.stack_size.map_or(FOUR_MB, |size| size * 1024 * 2014)))
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if compile_status.success() {
        println!("Compilation succeeded");
    } else {
        exit(1);
    }

    // Run
    if app.sub < Sub::Run {
        return;
    }
    println!();
    let run_status = Command::new("./test").spawn().unwrap().wait().unwrap();
    if !run_status.success() {
        exit(1);
    }
}

const FOUR_MB: usize = 16_777_216;

#[derive(Clap)]
struct App {
    #[clap(subcommand)]
    sub: Sub,
    #[clap(long = "stack", about = "The executable stack size in MB")]
    stack_size: Option<usize>,
    #[clap(long = "compiler", about = "The C compiler to use")]
    compiler: Option<CCompiler>,
}

#[derive(Clap, PartialEq, Eq, PartialOrd, Ord)]
enum Sub {
    #[clap(alias = "c")]
    Check,
    #[clap(alias = "t")]
    Trans,
    #[clap(alias = "b")]
    Build,
    #[clap(alias = "r")]
    Run,
}

const EXE_EXT: &str = if cfg!(windows) { "exe" } else { "" };

#[derive(Debug, Clone, Copy)]
enum CCompiler {
    Gcc,
    Clang,
}

impl CCompiler {
    const ALL: &'static [Self] = &[CCompiler::Gcc, CCompiler::Clang];
    fn find() -> Self {
        use std::process::*;
        for &comp in Self::ALL {
            if Command::new(comp.name())
                .arg("-v")
                .output()
                .map_or(false, |output| output.status.success())
            {
                return comp;
            }
        }
        println!("No compatible C compiler detected.");
        exit(1)
    }
    pub fn name(&self) -> &'static str {
        ["gcc", "clang"][*self as usize]
    }
    pub fn stack_size_arg(&self, size: usize) -> String {
        match self {
            CCompiler::Gcc => format!("-Wl,--stack,{}", size),
            CCompiler::Clang => format!("-Wl,-stack:{}", size),
        }
    }
}

impl FromStr for CCompiler {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use anyhow::anyhow;
        Self::ALL
            .iter()
            .find(|comp| comp.name() == s)
            .copied()
            .ok_or_else(|| anyhow!("Unsupported or unknown compiler {:?}", s))
    }
}
