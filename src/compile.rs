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

#[derive(Clone)]
pub enum CompileDef<'a> {
    Noot {
        c_name: String,
        def: Def<'a>,
        captures: Vec<String>,
    },
    C {
        c_name: String,
        function: bool,
    },
}

impl<'a> CompileDef<'a> {
    pub fn name(&self) -> &str {
        match self {
            CompileDef::Noot { c_name, .. } | CompileDef::C { c_name, .. } => c_name.as_str(),
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            CompileDef::Noot { def, captures, .. } => {
                !def.params.params.is_empty() && captures.is_empty()
            }
            CompileDef::C { function, .. } => *function,
        }
    }
}

#[derive(Default)]
pub struct NootScope<'a> {
    pub defs: HashMap<String, CompileDef<'a>>,
    pub captures: Vec<String>,
}
type NootScopes<'a> = Vec<NootScope<'a>>;
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
    pub noot_scopes: NootScopes<'a>,
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
                    sig: "int main(int argc, char** argv)".into(),
                    body: "    tgc_start(&noot_gc, &argc);\n".into(),
                    scopes: vec![CScope::new()],
                },
            );
            curr_function.push("main".into());
        }
        let mut noot_scope = NootScope::default();
        for &(noot_name, c_name) in &[("print", "noot_print")] {
            noot_scope.defs.insert(
                noot_name.into(),
                CompileDef::C {
                    c_name: c_name.into(),
                    function: true,
                },
            );
        }
        CTarget {
            name: name.into(),
            main,
            header_includes: once("noot.h".into()).collect(),
            source_includes: vec!["noot.h".into(), "tgc.h".into()].into_iter().collect(),
            other_c_lib_files: vec!["utf8.h".into(), "tgc.c".into()].into_iter().collect(),
            block_vals: VecDeque::new(),
            noot_scopes: vec![noot_scope],
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
                let potential = format!("{}_{}", name, i);
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

        // Statics
        if self.main {
            writeln!(source, "static tgc_t noot_gc;")?;
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
                writeln!(source, "    tgc_stop(&noot_gc);\n    return 0;")?;
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
    fn top_noot_scope(&mut self) -> &mut NootScope<'a> {
        self.noot_scopes.last_mut().unwrap()
    }
    fn top_c_scope(&mut self) -> &mut CScope {
        self.curr_c_function_mut().scopes.last_mut().unwrap()
    }
    pub fn compile_def(&mut self, def: Def<'a>) {
        let is_function = !def.params.params.is_empty();
        let c_name = self.c_name_for(&def.ident.name, is_function);
        // Push the def into its enclosing scope
        self.top_noot_scope().defs.insert(
            def.ident.name.clone(),
            CompileDef::Noot {
                c_name: c_name.clone(),
                def: def.clone(),
                captures: Vec::new(),
            },
        );
        // Push a scope for this def
        self.noot_scopes.push(NootScope::default());
        if is_function {
            // Function
            let expr = self.compile_function(&c_name, &def.params.params, def.items.clone());
            let c_name = self.c_name_for(&def.ident.name, is_function);
            // Pop the def's scope
            self.noot_scopes.pop();
            self.top_noot_scope().defs.insert(
                def.ident.name,
                CompileDef::C {
                    c_name: c_name.clone(),
                    function: false,
                },
            );
            self.push_line(format!("NootValue {} = {};", c_name, expr));
        } else {
            // Variable
            self.top_c_scope().insert(c_name.clone());
            self.compile_items(def.items.clone(), true);
            let expr = self.block_vals.pop_front().unwrap();
            self.push_line(format!("NootValue {} = {};", c_name, expr));
            // Pop the def's scope
            self.noot_scopes.pop();
        }
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
                self.top_noot_scope().defs.insert(
                    temp_name.clone(),
                    CompileDef::C {
                        c_name: temp_name.clone(),
                        function: false,
                    },
                );
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
                let compdef = self
                    .noot_scopes
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(i, scope)| scope.defs.get(&ident.name).map(|cd| (i, cd.clone())));
                if let Some((i, compdef)) = compdef {
                    if i < self.noot_scopes.len() - 1 {
                        if let Some(i) = self
                            .top_noot_scope()
                            .captures
                            .iter()
                            .position(|c_name| c_name == compdef.name())
                        {
                            format!("captures[{}]", i)
                        } else {
                            let i = self.top_noot_scope().captures.len();
                            self.top_noot_scope().captures.push(compdef.name().into());
                            format!("captures[{}]", i)
                        }
                    } else if compdef.is_function() {
                        format!("new_function({})", compdef.name())
                    } else {
                        compdef.name().into()
                    }
                } else {
                    self.errors.push(
                        CompileErrorKind::UnknownDef(ident.name.clone()).span(ident.span.clone()),
                    );
                    String::new()
                }
            }
            Term::Closure(closure) => {
                self.noot_scopes.push(NootScope::default());
                let res = self.compile_function("", &closure.params.params, closure.body.clone());
                self.noot_scopes.pop();
                res
            }
            Term::Expr(items) => {
                self.compile_items(items, true);
                self.block_vals.pop_front().unwrap()
            }
        }
    }
    pub fn compile_function(&mut self, name: &str, params: &[Param], body: Items<'a>) -> String {
        let c_name = self.c_name_for(if name.is_empty() { "closure" } else { name }, true);

        // Build signature
        let mut params_str = String::new();
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                params_str += ", ";
            }
            params_str += "NootValue ";
            params_str += &param.ident.name;
            self.top_noot_scope().defs.insert(
                param.ident.name.clone(),
                CompileDef::C {
                    c_name: format!("args[{}]", i),
                    function: false,
                },
            );
        }
        let sig = format!(
            "NootValue {}(int count, NootValue* args, NootValue* captures)",
            c_name
        );
        self.c_functions.insert(
            c_name.clone(),
            CFunction {
                sig,
                body: String::new(),
                scopes: vec![CScope::new()],
            },
        );

        // Compile
        self.curr_c_function.push(c_name.clone());
        self.compile_items(body, true);
        let ret = self.block_vals.pop_front().unwrap();
        self.push_line(format!("return {};", ret));
        self.curr_c_function.pop();

        // Build captures
        let captures = self.top_noot_scope().captures.clone();
        if captures.is_empty() {
            format!("new_function(&{})", c_name)
        } else {
            let captures_name = self.c_name_for("captures", false);
            self.push_line(format!(
                "NootValue* {} = (NootValue*) tgc_alloc(&noot_gc, {} * sizeof(NootValue));",
                captures_name,
                captures.len()
            ));
            for (i, c_name) in captures.iter().enumerate() {
                self.push_line(format!("{}[{}] = {};", captures_name, i, c_name));
            }
            self.top_c_scope().insert(captures_name.clone());
            if let Some(CompileDef::Noot { captures: caps, .. }) = self
                .noot_scopes
                .iter_mut()
                .rev()
                .find_map(|scope| scope.defs.get_mut(name))
            {
                *caps = captures;
            }

            format!("new_closure(&{}, {})", c_name, captures_name)
        }
    }
}
