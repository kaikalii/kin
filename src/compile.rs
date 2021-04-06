use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt,
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use indexmap::IndexMap;
use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};

use crate::{ast::*, parse::Rule};

type ShadowStack = Vec<String>;
type NootScope = HashMap<String, ShadowStack>;
type NootScopes = Vec<NootScope>;
type CScope = HashSet<String>;
type CScopes = Vec<CScope>;

#[derive(Debug, thiserror::Error)]
pub enum CompileErrorKind {
    #[error("Unknown definition {}", _0)]
    UnknownDef(String),
}

impl CompileErrorKind {
    pub fn span(self, span: Span) -> CompileError {
        CompileError { kind: self, span }
    }
}

#[derive(Debug)]
pub struct CompileError<'a> {
    pub kind: CompileErrorKind,
    pub span: Span<'a>,
}

impl<'a> fmt::Display for CompileError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let error = PestError::<Rule>::new_from_span(
            ErrorVariant::CustomError {
                message: self.kind.to_string(),
            },
            self.span.clone(),
        );
        write!(f, "{}", error)
    }
}

pub struct CTarget<'a> {
    pub name: String,
    pub header_includes: HashSet<String>,
    pub source_includes: HashSet<String>,
    pub other_c_lib_files: HashSet<String>,
    pub c_functions: IndexMap<String, CFunction>,
    pub curr_c_function: Vec<String>,
    pub block_vals: VecDeque<String>,
    pub noot_scopes: NootScopes,
    pub main: bool,
    pub indent: usize,
    pub errors: Vec<CompileError<'a>>,
}

pub struct CFunction {
    pub sig: String,
    pub body: String,
    pub scopes: CScopes,
}

impl<'a> CTarget<'a> {
    pub fn new(name: &str, main: bool) -> Self {
        let mut c_functions = IndexMap::new();
        let mut curr_function = Vec::new();
        if main {
            c_functions.insert(
                "main".into(),
                CFunction {
                    sig: "int main()".into(),
                    body: String::new(),
                    scopes: vec![CScope::new()],
                },
            );
            curr_function.push("main".into());
        }
        CTarget {
            name: name.into(),
            main,
            header_includes: once("noot.h".into()).collect(),
            source_includes: once("noot.h".into()).collect(),
            other_c_lib_files: vec!["utf8.h".into(), "tgc.h".into(), "tgc.c".into()]
                .into_iter()
                .collect(),
            block_vals: VecDeque::new(),
            noot_scopes: vec![NootScope::new()],
            c_functions,
            curr_c_function: curr_function,
            indent: 1,
            errors: Vec::new(),
        }
    }
    pub fn c_name_exists(&self, c_name: &str, function: bool) -> bool {
        function && self.c_functions.contains_key(c_name)
            || !function
                && self
                    .curr_c_function()
                    .scopes
                    .iter()
                    .any(|scope| scope.contains(c_name))
    }
    pub fn c_name_for(&self, name: &str, function: bool) -> String {
        if self.c_name_exists(&name, function) {
            let mut i = 2;
            loop {
                let potential = format!("{}{}", name, i);
                if !self.c_name_exists(&potential, function) {
                    break potential;
                }
                i += 1;
            }
        } else {
            name.into()
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
        for function in self.c_functions.values() {
            if let Some(header) = &mut header {
                writeln!(header, "{};", function.sig)?;
            } else {
                writeln!(source, "{};", function.sig)?;
            }
        }
        writeln!(source)?;
        // Write definitions
        for (name, function) in &self.c_functions {
            writeln!(source, "{} {{", function.sig)?;
            write!(source, "{}", function.body)?;
            if name == "main" {
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
    fn curr_c_function(&self) -> &CFunction {
        self.c_functions
            .get(self.curr_c_function.last().unwrap())
            .unwrap()
    }
    fn curr_c_function_mut(&mut self) -> &mut CFunction {
        self.c_functions
            .get_mut(self.curr_c_function.last().unwrap())
            .unwrap()
    }
    fn body(&mut self) -> &mut String {
        &mut self.curr_c_function_mut().body
    }
    fn top_noot_scope(&mut self) -> &mut NootScope {
        self.noot_scopes.last_mut().unwrap()
    }
    fn top_c_scope(&mut self) -> &mut CScope {
        self.curr_c_function_mut().scopes.last_mut().unwrap()
    }
    pub fn compile_def(&mut self, def: Def<'a>) {
        // Push a scope for this def
        self.noot_scopes.push(NootScope::default());
        let is_function = !def.params.params.is_empty();
        let c_name = self.c_name_for(&def.ident.name, is_function);
        if is_function {
            // Function
            let mut params = String::new();
            for (i, param) in def.params.params.iter().enumerate() {
                if i > 0 {
                    params += ", ";
                }
                params += "NootValue ";
                params += &param.ident.name;
                self.top_noot_scope()
                    .entry(param.ident.name.clone())
                    .or_default()
                    .push(format!("args[{}]", i));
            }
            let sig = format!("NootValue {}(int count, NootValue* args)", c_name);
            self.curr_c_function.push(c_name.clone());
            self.c_functions.insert(
                c_name.clone(),
                CFunction {
                    sig,
                    body: String::new(),
                    scopes: vec![CScope::new()],
                },
            );
            self.compile_items(def.items.clone(), true);
            let ret = self.block_vals.pop_front().unwrap();
            self.push_line(format!("return {};", ret));
            self.curr_c_function.pop();
        } else {
            // Variable
            self.top_c_scope().insert(c_name.clone());
            self.compile_items(def.items.clone(), true);
            let expr = self.block_vals.pop_front().unwrap();
            self.push_line(format!("NootValue {} = {};", c_name, expr));
        }
        // Pop the def's scope
        self.noot_scopes.pop();
        // Push the def into its enclosing scope
        self.top_noot_scope()
            .entry(def.ident.name)
            .or_default()
            .push(c_name);
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
                let temp_name = self.c_name_for("temp", false);
                self.top_noot_scope()
                    .entry(temp_name.clone())
                    .or_default()
                    .push(temp_name.clone());
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
            Term::Ident(ident) => {
                let c_name = self
                    .noot_scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.get(&ident.name).and_then(|ss| ss.last()));
                if let Some(c_name) = c_name {
                    c_name.clone()
                } else {
                    self.errors.push(
                        CompileErrorKind::UnknownDef(ident.name.clone()).span(ident.span.clone()),
                    );
                    String::new()
                }
            }
            Term::Closure(_) => {
                self.noot_scopes.push(NootScope::default());
                self.noot_scopes.pop();
                todo!()
            }
            Term::Expr(items) => {
                self.compile_items(items, true);
                self.block_vals.pop_front().unwrap()
            }
        }
    }
}
