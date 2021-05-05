#![allow(unstable_name_collisions)]

mod ast;
mod parse;
mod transpile;

use std::str::FromStr;

use clap::Clap;

fn main() {
    use std::process::*;

    use transpile::*;

    color_backtrace::install();

    let app = App::parse();

    // Parse and check
    let input = std::fs::read_to_string("test.kin").unwrap();
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
    if !app.sub.transpiles() {
        return;
    }
    let transpilation = transpile(items);
    transpilation.write().unwrap();
    println!("Transpilation succeeded");

    // Compile
    let build_args = if let Some(args) = app.sub.build_args() {
        args
    } else {
        return;
    };

    let ccomp = build_args.compiler.unwrap_or_else(CCompiler::find);

    let mut args: Vec<String> = vec!["build/main.c".into(), "-o".into()];

    let name = "test";

    // Push target arg
    if build_args.assembly {
        args.push(format!("{}.asm", name));
        args.push("-S".into());
    } else {
        args.push(format!("{}{}", name, EXE_EXT));
    }

    // Push opt arg
    args.push("-O3".into());

    // Push C standard arg
    args.push("-std=c99".into());

    // Push stack size arg
    if let Some(size) = build_args.stack_size {
        args.push(ccomp.stack_size_arg(size * 1024 * 1024));
    }

    // Push profile arg
    if build_args.profile {
        args.push("-pg".into());
    }

    let compile_status = Command::new(ccomp.name())
        .args(args)
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
    if !matches!(app.sub, Sub::Run(_)) {
        return;
    }
    println!();
    let run_status = Command::new("./test").spawn().unwrap().wait().unwrap();
    if !run_status.success() {
        exit(1);
    }
}

#[derive(Clap)]
struct App {
    #[clap(subcommand)]
    sub: Sub,
}

#[derive(Clap)]
enum Sub {
    #[clap(alias = "c")]
    Check,
    #[clap(alias = "t")]
    Trans,
    #[clap(alias = "b")]
    Build(BuildArgs),
    #[clap(alias = "r")]
    Run(BuildArgs),
}

impl Sub {
    fn build_args(&self) -> Option<&BuildArgs> {
        match self {
            Sub::Build(args) | Sub::Run(args) => Some(args),
            _ => None,
        }
    }
    fn transpiles(&self) -> bool {
        !matches!(self, Sub::Check)
    }
}

#[derive(Clap)]
struct BuildArgs {
    #[clap(long = "stack", about = "The executable stack size in MB")]
    stack_size: Option<usize>,
    #[clap(about = "The C compiler to use")]
    compiler: Option<CCompiler>,
    #[clap(long = "asm")]
    assembly: bool,
    #[clap(long = "profile")]
    profile: bool,
}

const EXE_EXT: &str = if cfg!(windows) { ".exe" } else { "" };

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
