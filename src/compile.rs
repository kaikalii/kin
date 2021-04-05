use std::{
    collections::HashSet,
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use crate::ast::*;

pub struct CTarget {
    pub name: String,
    pub header_includes: HashSet<String>,
    pub source_includes: HashSet<String>,
    pub other_includes: HashSet<String>,
    pub functions: Vec<CFunction>,
    pub main: bool,
}

pub struct CFunction {
    pub sig: String,
    pub body: String,
}

impl CTarget {
    pub fn new(name: &str, main: bool) -> Self {
        let functions = if main {
            vec![CFunction {
                sig: "int main()".into(),
                body: String::new(),
            }]
        } else {
            Vec::new()
        };
        CTarget {
            name: name.into(),
            main,
            header_includes: once("value.h".into()).collect(),
            source_includes: once("value.h".into()).collect(),
            other_includes: once("utf8.h".into()).collect(),
            functions,
        }
    }
    pub fn write(self) -> io::Result<()> {
        fs::create_dir_all("build")?;
        let mut to_copy = Vec::new();
        to_copy.extend(&self.header_includes);
        to_copy.extend(&self.source_includes);
        to_copy.extend(&self.other_includes);
        for file_name in to_copy {
            fs::copy(
                format!("clibs/{}", file_name),
                format!("build/{}", file_name),
            )?;
        }
        let mut header = if self.main {
            None
        } else {
            Some(File::create(format!("build/{}.h", self.name))?)
        };
        let mut source = File::create(format!("build/{}.c", self.name))?;

        // Write includes
        if let Some(header) = &mut header {
            for include in self.header_includes {
                writeln!(header, "#include \"{}\"", include)?;
            }
            for include in self.source_includes {
                writeln!(source, "include \"{}\"", include)?;
            }
            writeln!(header)?;
        } else {
            for include in self.source_includes.union(&self.header_includes) {
                writeln!(source, "#include \"{}\"", include)?;
            }
        }
        writeln!(source)?;

        // Write functions
        for function in self.functions {
            if let Some(header) = &mut header {
                writeln!(header, "{};", function.sig)?;
            } else {
                writeln!(source, "{};", function.sig)?;
            }
            writeln!(source, "{} {{", function.sig)?;
            for line in function.body.lines() {
                writeln!(source, "    {}", line)?;
            }
            writeln!(source, "}}")?;
        }

        Ok(())
    }
    pub fn compile_items(&mut self, items: Items) {
        for item in items.items {
            self.compile_item(item);
        }
    }
    pub fn compile_item(&mut self, item: Item) {
        match item {
            Item::Def(def) => self.compile_def(def),
            Item::Node(node) => self.compile_node(node),
        }
    }
    fn body(&mut self) -> &mut String {
        &mut self.functions.last_mut().expect("No function").body
    }
    pub fn compile_def(&mut self, def: Def) {
        if def.params.params.is_empty() {
            // Variable
            let single_expr = def
                .items
                .items
                .iter()
                .filter(|item| matches!(item, Item::Node(_)))
                .count()
                == 1;
            if !single_expr {}
            let var_init = &format!("NootValue {} = ", def.ident.name);
            *self.body() += &var_init;
            if single_expr {
                for item in def.items.items {
                    self.compile_item(item);
                }
                *self.body() += ";\n";
            }
        } else {
            // Function
        }
    }
    pub fn compile_node(&mut self, node: Node) {
        match node {
            Node::Term(term) => self.compile_term(term),
            node => todo!("{:?}", node),
        }
    }
    pub fn compile_term(&mut self, term: Term) {
        match term {
            Term::Nat(i) => *self.body() += &format!("new_nat({})", i),
            Term::Int(i) => *self.body() += &format!("new_int({})", i),
            Term::Real(i) => *self.body() += &format!("new_real({})", i),
            _ => todo!(),
        }
    }
    pub fn compile_bin_expr(&mut self, expr: BinExpr) {
        match expr.op {
            BinOp::Add => {
                self.compile_node(*expr.left);
                *self.body() += " + ";
                self.compile_node(*expr.right);
            }
            _ => todo!(),
        }
    }
}
