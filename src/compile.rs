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
    pub other_c_lib_files: HashSet<String>,
    pub functions: Vec<CFunction>,
    pub curr_function: Vec<usize>,
    pub block_vals: VecDeque<String>,
    pub main: bool,
    pub indent: usize,
}

pub struct CFunction {
    pub sig: String,
    pub body: String,
}

impl<'a> CTarget<'a> {
    pub fn new(name: &str, main: bool) -> Self {
        let (functions, curr_function) = if main {
            (
                vec![CFunction {
                    sig: "int main()".into(),
                    body: String::new(),
                }],
                vec![0],
            )
        } else {
            (Vec::new(), Vec::new())
        };
        CTarget {
            res: Resolver::new(),
            name: name.into(),
            main,
            header_includes: once("noot.h".into()).collect(),
            source_includes: once("noot.h".into()).collect(),
            other_c_lib_files: vec!["utf8.h".into(), "tgc.h".into(), "tgc.c".into()]
                .into_iter()
                .collect(),
            block_vals: VecDeque::new(),
            functions,
            curr_function,
            indent: 1,
        }
    }
    pub fn write(self) -> io::Result<()> {
        fs::create_dir_all("build")?;
        let mut to_copy = Vec::new();
        to_copy.extend(&self.header_includes);
        to_copy.extend(&self.source_includes);
        to_copy.extend(&self.other_c_lib_files);
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
        // Write declarations
        for function in &self.functions {
            if let Some(header) = &mut header {
                writeln!(header, "{};", function.sig)?;
            } else {
                writeln!(source, "{};", function.sig)?;
            }
        }
        writeln!(source)?;
        // Write definitions
        for function in &self.functions {
            writeln!(source, "{} {{", function.sig)?;
            write!(source, "{}", function.body)?;
            if function.sig.starts_with("int main") {
                writeln!(source, "    return 0;")?;
            }
            writeln!(source, "}}\n")?;
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
    pub fn push_line(&mut self, line: String) {
        let line = format!("{:indent$}{}\n", "", line, indent = self.indent * 4);
        *self.body() += &line;
    }
    pub fn compile_item(&mut self, item: Item<'a>, push: bool) {
        match item {
            Item::Def(def) => self.compile_def(def),
            Item::Node(node) => {
                let compiled = self.compile_node(node);
                if push {
                    self.block_vals.push_back(compiled);
                } else {
                    self.push_line(format!("{};", compiled));
                }
            }
        }
    }
    fn body(&mut self) -> &mut String {
        &mut self.functions[*self.curr_function.last().unwrap()].body
    }
    pub fn compile_def(&mut self, def: Def<'a>) {
        // Push a scope for this def
        self.res.push_scope();
        let c_name = self.res.c_name_for(&def.ident.name);
        if def.params.params.is_empty() {
            // Variable
            self.compile_items(def.items.clone(), true);
            let expr = self.block_vals.pop_front().unwrap();
            self.push_line(format!("NootValue {} = {};\n", c_name, expr));
        } else {
            // Function
            let mut params = String::new();
            for (i, param) in def.params.params.iter().enumerate() {
                if i > 0 {
                    params += ", ";
                }
                params += "NootValue ";
                params += &param.ident.name;
                self.res.push_param_def(&param.ident.name, i);
            }
            let sig = format!("NootValue {}(int count, NootValue* args)", c_name);
            self.curr_function.push(self.functions.len());
            self.functions.push(CFunction {
                sig,
                body: String::new(),
            });
            self.compile_items(def.items.clone(), true);
            let ret = self.block_vals.pop_front().unwrap();
            self.push_line(format!("return {};", ret));
            self.curr_function.pop();
        }
        // Pop the def's scope
        self.res.pop_scope();
        // Push the def into its enclosing scope
        self.res.push_noot_def(c_name, def);
    }
    #[must_use]
    pub fn compile_node(&mut self, node: Node<'a>) -> String {
        match node {
            Node::Term(term) => self.compile_term(term),
            Node::BinExpr(expr) => self.compile_bin_expr(expr),
            Node::Call(expr) => self.compile_call_expr(expr),
            Node::UnExpr(expr) => self.compile_un_expr(expr),
            Node::Insert(expr) => todo!("{:?}", expr),
        }
    }
    pub fn compile_bin_expr(&mut self, expr: BinExpr<'a>) -> String {
        let noot_fn = match expr.op {
            BinOp::Or | BinOp::And => {
                let temp_name = self.res.c_name_for("temp");
                self.res.push_c_def("temp", &temp_name);
                let left = self.compile_node(*expr.left);
                self.push_line(format!("NootValue {} = {};", temp_name, left));
                self.push_line(format!(
                    "if ({}noot_is_true({})) {{",
                    if expr.op == BinOp::Or { "!" } else { "" },
                    temp_name
                ));
                self.indent += 1;
                let right = self.compile_node(*expr.right);
                self.push_line(format!("{} = {};", temp_name, right));
                self.indent -= 1;
                self.push_line("}".into());
                return temp_name;
            }
            BinOp::Is => "noot_eq",
            BinOp::Isnt => "noot_neq",
            BinOp::Less => "noot_lt",
            BinOp::LessOrEqual => "noot_le",
            BinOp::Greater => "noot_gt",
            BinOp::GreaterOrEqual => "noot_ge",
            BinOp::Add => "noot_add",
            BinOp::Sub => "noot_sub",
            BinOp::Mul => "noot_mul",
            BinOp::Div => "noot_div",
            BinOp::Rem => "noot_rem",
        };
        format!(
            "{}({}, {})",
            noot_fn,
            self.compile_node(*expr.left),
            self.compile_node(*expr.right)
        )
    }
    pub fn compile_un_expr(&mut self, expr: UnExpr<'a>) -> String {
        let noot_fn = match expr.op {
            UnOp::Neg => "noot_neg",
            UnOp::Not => "noot_not",
        };
        format!("{}({})", noot_fn, self.compile_node(*expr.inner))
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
    pub fn compile_term(&mut self, term: Term<'a>) -> String {
        match term {
            Term::Nil => "NOOT_NIL".into(),
            Term::Bool(b) => format!("new_bool({})", b as u8),
            Term::Nat(i) => format!("new_nat({})", i),
            Term::Int(i) => format!("new_int({})", i),
            Term::Real(i) => format!("new_real({})", i),
            Term::String(s) => format!("new_string({:?}, {})", s, s.len()),
            Term::Ident(ident) => match self.res.find_def(&ident.name).cloned() {
                Some(def) => match def {
                    CompileDef::C(name) => format!("new_function(&{})", name),
                    CompileDef::Noot { name, def } => {
                        if def.params.params.is_empty() {
                            name
                        } else {
                            format!("new_function(&{})", name)
                        }
                    }
                    CompileDef::Param(i) => format!("args[{}]", i),
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
        }
    }
}
