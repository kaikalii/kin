use std::{
    collections::{HashSet, VecDeque},
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use crate::{ast::*, resolve::*};

pub struct CTarget<'a> {
    pub res: Resolver<'a>,
    pub name: String,
    pub header_includes: HashSet<String>,
    pub source_includes: HashSet<String>,
    pub other_includes: HashSet<String>,
    pub functions: Vec<CFunction>,
    pub block_vals: VecDeque<String>,
    pub main: bool,
}

pub struct CFunction {
    pub sig: String,
    pub body: String,
}

impl<'a> CTarget<'a> {
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
            res: Resolver::new(),
            name: name.into(),
            main,
            header_includes: once("value.h".into()).collect(),
            source_includes: once("value.h".into()).collect(),
            other_includes: once("utf8.h".into()).collect(),
            block_vals: VecDeque::new(),
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
    pub fn compile_items(&mut self, items: Items<'a>, push: bool) {
        let len = items.items.len();
        for (i, item) in items.items.into_iter().enumerate() {
            let push = push && i == len - 1 && matches!(item, Item::Node(_));
            self.compile_item(item, push);
        }
    }
    pub fn compile_item(&mut self, item: Item<'a>, push: bool) {
        match item {
            Item::Def(def) => self.compile_def(def),
            Item::Node(node) => {
                let compiled = self.compile_node(node);
                if push {
                    self.block_vals.push_back(compiled);
                } else {
                    *self.body() += &format!("{};\n", compiled);
                }
            }
        }
    }
    fn body(&mut self) -> &mut String {
        &mut self.functions.last_mut().expect("No function").body
    }
    pub fn compile_def(&mut self, def: Def<'a>) {
        // Push a scope for this def
        self.res.push_scope();
        if def.params.params.is_empty() {
            // Variable
            self.compile_items(def.items.clone(), true);
            let expr = self.block_vals.pop_front().unwrap();
            *self.body() += &format!("NootValue {} = {};\n", def.ident.name, expr);
        } else {
            // Function
            todo!("functions")
        }
        // Pop the def's scope
        self.res.pop_scope();
        // Push the def into its enclosing scope
        self.res
            .push_def(def.ident.name.clone(), CompileDef::Noot(def));
    }
    #[must_use]
    pub fn compile_node(&mut self, node: Node<'a>) -> String {
        match node {
            Node::Term(term) => self.compile_term(term),
            Node::BinExpr(expr) => self.compile_bin_expr(expr),
            Node::Call(expr) => self.compile_call_expr(expr),
            node => todo!("{:?}", node),
        }
    }
    pub fn compile_term(&mut self, term: Term<'a>) -> String {
        match term {
            Term::Nat(i) => format!("new_nat({})", i),
            Term::Int(i) => format!("new_int({})", i),
            Term::Real(i) => format!("new_real({})", i),
            Term::Ident(ident) => match self.res.find_def(&ident.name).cloned() {
                Some(def) => match def {
                    CompileDef::C(name) => format!("new_function(&{})", name),
                    CompileDef::Noot(def) => def.ident.name,
                },
                None => {
                    self.res.errors.push(
                        ResolutionErrorKind::UnknownDef(ident.name.clone())
                            .span(ident.span.clone()),
                    );
                    String::new()
                }
            },
            Term::Closure(_) => {
                self.res.push_scope();
                self.res.pop_scope();
                todo!()
            }
            Term::Expr(items) => {
                self.compile_items(items, true);
                self.block_vals.pop_front().unwrap()
            }
            term => todo!("{:?}", term),
        }
    }
    #[must_use]
    pub fn compile_bin_expr(&mut self, expr: BinExpr<'a>) -> String {
        let noot_fn = match expr.op {
            BinOp::Add => "noot_add",
            BinOp::Sub => "noot_sub",
            BinOp::Mul => "noot_mul",
            BinOp::Div => "noot_div",
            BinOp::Rem => "noot_rem",
            _ => todo!(),
        };
        format!(
            "{}({}, {})",
            noot_fn,
            self.compile_node(*expr.left),
            self.compile_node(*expr.right)
        )
    }
    pub fn compile_call_expr(&mut self, call: CallExpr<'a>) -> String {
        let f = self.compile_node(*call.expr);
        if call.args.is_empty() {
            return f;
        }
        let mut args = String::new();
        let arg_count = call.args.len();
        for (i, node) in call.args.into_iter().enumerate() {
            if i > 0 {
                args += ", ";
            }
            args += &self.compile_node(node);
        }
        format!("noot_call({}, {}, (NootValue[]){{{}}})", f, arg_count, args)
    }
}
