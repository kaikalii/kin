use std::{
    collections::{BTreeMap, VecDeque},
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use itertools::*;
use rpds::{RedBlackTreeMap, Vector};

use crate::ast::*;

struct NootDef {
    is_function: bool,
    c_name: String,
}

macro_rules! builtin_functions {
    ($($name:literal),* $(,($noot_name:literal, $c_text:literal))* $(,)?) => {
        &[$(($name, concat!("noot_", $name))),* $(,($noot_name, $c_text))*]
    }
}

pub const BUILTIN_FUNCTIONS: &[(&str, &str)] = builtin_functions!(
    "mom",
    "dad",
    "print",
    "println",
    "error",
    "panic",
    "not",
    "assert",
    ("add", "noot_add_fn"),
    ("sub", "noot_sub_fn"),
    ("mul", "noot_mul_fn"),
    ("div", "noot_div_fn"),
    ("rem", "noot_rem_fn"),
    ("eq", "noot_eq_fn"),
    ("ne", "noot_ne_fn"),
    ("lt", "noot_lt_fn"),
    ("le", "noot_le_fn"),
    ("gt", "noot_gt_fn"),
    ("ge", "noot_ge_fn"),
);
pub const BUILTIN_VALUES: &[(&str, &str)] = &[
    ("_", "NOOT_NIL"),
    ("nil", "NOOT_NIL"),
    ("true", "NOOT_TRUE"),
    ("false", "NOOT_FALSE"),
];

static RESERVED_NAMES: &[&str] = &[
    // C keywords
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline ",
    "int",
    "long",
    "register",
    "restrict ",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    // Others
    "count",
    "argc",
    "argv",
];

#[derive(Clone)]
struct TranspileStack<'a> {
    noot_scopes: Vector<RedBlackTreeMap<&'a str, NootDef>>,
}

impl<'a> TranspileStack<'a> {
    pub fn new() -> Self {
        TranspileStack {
            noot_scopes: Vector::new().push_back(
                BUILTIN_FUNCTIONS
                    .iter()
                    .map(|&(noot_name, c_name)| {
                        (
                            noot_name,
                            NootDef {
                                c_name: c_name.into(),
                                is_function: true,
                            },
                        )
                    })
                    .chain(BUILTIN_VALUES.iter().map(|&(noot_name, c_name)| {
                        (
                            noot_name,
                            NootDef {
                                c_name: c_name.into(),
                                is_function: false,
                            },
                        )
                    }))
                    .collect(),
            ),
        }
    }
    pub fn with_noot_def(self, name: &'a str, def: NootDef) -> Self {
        TranspileStack {
            noot_scopes: self
                .noot_scopes
                .set(
                    self.noot_scopes.len() - 1,
                    self.noot_scopes.last().unwrap().insert(name, def),
                )
                .unwrap(),
        }
    }
}

#[derive(Clone)]
pub struct Transpilation<'a> {
    functions: BTreeMap<String, CFunction<'a>>,
    function_stack: Vec<String>,
}

#[derive(Clone)]
struct CFunction<'a> {
    noot_name: &'a str,
    exprs: VecDeque<String>,
    lines: Vec<CLine>,
    captures: Vec<CCapture>,
    indent: usize,
}

impl<'a> CFunction<'a> {
    pub fn new(noot_name: &'a str) -> CFunction {
        CFunction {
            noot_name,
            exprs: Default::default(),
            lines: Default::default(),
            captures: Default::default(),
            indent: 0,
        }
    }
}

#[derive(Clone)]
struct CLine {
    var_name: Option<String>,
    type_name: Option<&'static str>,
    value: String,
    indent: usize,
    semicolon: bool,
    deref: bool,
}

impl CLine {
    fn name(&mut self, name: impl Into<String>) -> &mut Self {
        self.var_name = Some(name.into());
        self.type_name = Some("NootValue");
        self
    }
    fn no_semicolon(&mut self) -> &mut Self {
        self.semicolon = false;
        self
    }
    fn ty(&mut self, ty: &'static str) -> &mut Self {
        self.type_name = Some(ty);
        self
    }
    fn no_type(&mut self) -> &mut Self {
        self.type_name = None;
        self
    }
    fn deref(&mut self) -> &mut Self {
        self.deref = true;
        self
    }
}

#[derive(Clone)]
struct CCapture {
    pub c_name: String,
    pub capture_name: String,
}

impl<'a> CFunction<'a> {
    pub fn push_line(&mut self, value: impl Into<String>) -> &mut CLine {
        let line = CLine {
            var_name: None,
            type_name: None,
            value: value.into(),
            indent: self.indent,
            semicolon: true,
            deref: false,
        };
        self.lines.push(line);
        self.lines.last_mut().unwrap()
    }
    pub fn push_expr(&mut self, expr: String) {
        self.exprs.push_back(expr)
    }
    pub fn pop_expr(&mut self) -> Option<String> {
        self.exprs.pop_front()
    }
    pub fn capture_index_of(&self, c_name: &str) -> usize {
        self.captures
            .iter()
            .position(|cap| cap.c_name == c_name)
            .unwrap()
    }
    pub fn push_capture(&mut self, c_name: String, capture_name: String) {
        if self.captures.iter().any(|cap| cap.c_name == c_name) {
            return;
        }
        self.captures.push(CCapture {
            c_name,
            capture_name,
        });
    }
    pub fn indent(&mut self) {
        self.indent += 1;
    }
    pub fn deindent(&mut self) {
        self.indent -= 1;
    }
}

pub fn transpile(items: Items) -> Transpilation {
    let mut transpilation = Transpilation::new();
    transpilation.items(items, TranspileStack::new());
    transpilation
}

impl<'a> Transpilation<'a> {
    pub fn new() -> Self {
        Transpilation {
            functions: once("main")
                // .chain(BUILTINS.iter().map(|bi| bi.0))
                .map(|name| (name.into(), CFunction::new(name)))
                .collect(),
            function_stack: once("main".into()).collect(),
        }
    }
    pub fn write(self) -> io::Result<()> {
        fs::create_dir_all("build")?;
        let mut source = File::create("build/main.c")?;

        // Write headers
        writeln!(source, "#include \"../clibs/noot.h\"")?;
        writeln!(source)?;

        // Write function declarations
        for (name, cf) in self.functions.iter().filter(|&(name, _)| name != "main") {
            if cf.captures.is_empty() {
                writeln!(
                    source,
                    "NootValue {}(uint8_t count, NootValue* args);",
                    name
                )?;
            } else {
                writeln!(
                    source,
                    "NootValue {}(uint8_t count, NootValue* args, NootValue* captures);",
                    name
                )?;
            }
        }
        writeln!(source)?;

        // Write function definitions
        for (name, cf) in &self.functions {
            let main = name == "main";
            // Write signature
            if main {
                writeln!(source, "int main(int argc, char** argv) {{")?;
            } else if cf.captures.is_empty() {
                writeln!(
                    source,
                    "NootValue {}(uint8_t count, NootValue* args) {{",
                    name
                )?;
            } else {
                writeln!(
                    source,
                    "NootValue {}(uint8_t count, NootValue* args, NootValue* captures) {{",
                    name
                )?;
            }
            // Write lines
            for line in &cf.lines {
                write!(source, "{:indent$}", "", indent = (line.indent + 1) * 4)?;
                if let Some(type_name) = line.type_name {
                    write!(source, "{} ", type_name)?;
                }
                if let Some(var_name) = &line.var_name {
                    write!(source, "{} = ", var_name)?;
                }
                writeln!(
                    source,
                    "{}{}",
                    line.value,
                    if line.semicolon { ";" } else { "" }
                )?;
            }
            // Clean up main
            if main {
                if let Some(expr) = cf.clone().pop_expr() {
                    writeln!(source, "    {};", expr)?;
                }
                writeln!(source, "    return 0;")?;
            }
            // Close function
            writeln!(source, "}}\n")?;
        }

        Ok(())
    }
    fn c_name_exists(&self, c_name: &str, function: bool) -> bool {
        RESERVED_NAMES.contains(&c_name)
            || function && self.functions.keys().any(|name| name == c_name)
            || !function
                && self
                    .functions
                    .values()
                    .flat_map(|cf| &cf.lines)
                    .filter_map(|cf| cf.var_name.as_ref())
                    .any(|var_name| var_name == c_name)
    }
    fn c_name_for(&self, noot_name: &str, function: bool) -> String {
        let mut c_name = noot_name.to_owned();
        if c_name.starts_with("noot") || c_name.starts_with("Noot") {
            c_name = "_".to_owned() + &c_name;
        }
        let mut i = 1;
        while self.c_name_exists(&c_name, function) {
            i += 1;
            c_name = format!("{}_{}", noot_name, i);
        }
        c_name
    }
    fn start_c_function(&mut self, c_name: String, noot_name: &'a str) {
        self.functions
            .insert(c_name.clone(), CFunction::new(noot_name));
        self.function_stack.push(c_name);
    }
    fn finish_c_function(&mut self) {
        let cf = self.c_function();
        let ret_expr = cf
            .exprs
            .front()
            .cloned()
            .unwrap_or_else(|| "NOOT_NIL".into());
        cf.exprs.pop_front().unwrap();
        cf.push_line(format!("return {}", ret_expr));
        self.function_stack.pop().unwrap();
    }
    fn curr_c_function(&mut self) -> &mut CFunction<'a> {
        self.functions
            .get_mut(self.function_stack.last().unwrap())
            .unwrap()
    }
    fn c_function_at(&mut self, i: usize) -> &mut CFunction<'a> {
        let function_name = self.function_stack.get(i).unwrap();
        self.functions.get_mut(function_name).unwrap()
    }
    fn c_function(&mut self) -> &mut CFunction<'a> {
        let last_index = self.function_stack.len() - 1;
        self.c_function_at(last_index)
    }
    fn push_expr(&mut self, expr: String) {
        self.c_function().push_expr(expr)
    }
    fn pop_expr(&mut self) -> String {
        self.c_function()
            .pop_expr()
            .unwrap_or_else(|| "NOOT_NIL".into())
    }
    fn items(&mut self, items: Items<'a>, mut stack: TranspileStack<'a>) {
        let item_count = items.len();
        for (i, item) in items.into_iter().enumerate() {
            stack = self.item(item, stack);
            if i < item_count - 1 {
                let cf = self.c_function();
                if let Some(expr) = cf.pop_expr() {
                    cf.push_line(expr);
                }
            }
        }
    }

    fn item(&mut self, item: Item<'a>, stack: TranspileStack<'a>) -> TranspileStack<'a> {
        match item {
            Item::Def(def) => self.def(def, stack),
            Item::Node(node) => {
                self.node(node, stack.clone());
                stack
            }
        }
    }

    fn def(&mut self, def: Def<'a>, stack: TranspileStack<'a>) -> TranspileStack<'a> {
        let c_name = self.c_name_for(&def.ident.name, def.is_function());
        if def.is_function() {
            // Function
            let stack = stack.with_noot_def(
                def.ident.name,
                NootDef {
                    c_name: c_name.clone(),
                    is_function: true,
                },
            );
            self.function(c_name, def.ident.name, def.params, def.items, stack.clone());
            stack
        } else {
            // Value
            self.items(def.items, stack.clone());
            let cf = self.c_function();
            let line = cf.pop_expr();
            if let Some(line) = line {
                cf.push_line(line).name(c_name.clone());
            }
            stack.with_noot_def(
                def.ident.name,
                NootDef {
                    c_name,
                    is_function: false,
                },
            )
        }
    }
    fn node(&mut self, node: Node<'a>, stack: TranspileStack<'a>) {
        match node.kind {
            NodeKind::Term(term, _) => self.term(term, stack),
            NodeKind::BinExpr(expr) => self.bin_expr(expr, stack),
            NodeKind::UnExpr(expr) => self.un_expr(expr, stack),
            NodeKind::Call(expr) => self.call_expr(expr, stack),
        }
    }
    fn bin_expr(&mut self, expr: BinExpr<'a>, stack: TranspileStack<'a>) {
        self.node(*expr.left, stack.clone());
        let left = self.pop_expr();
        let (f, can_fail) = match expr.op {
            BinOp::Or | BinOp::And => {
                let or = expr.op == BinOp::Or;
                let temp_name = self.c_name_for("temp", false);
                let cf = self.c_function();
                cf.push_line(left).name(&temp_name);
                cf.push_line(format!(
                    "if ({}noot_is_true({})) {{",
                    if or { "!" } else { "" },
                    temp_name
                ))
                .no_semicolon();
                cf.indent();
                self.node(*expr.right, stack);
                let right = self.pop_expr();
                let cf = self.c_function();
                cf.push_line(right).name(&temp_name).no_type();
                cf.deindent();
                cf.push_line("}").no_semicolon();
                cf.push_expr(temp_name);
                return;
            }
            BinOp::Mom | BinOp::Dad => {
                let mom = expr.op == BinOp::Mom;
                self.node(*expr.right, stack);
                let right = self.pop_expr();
                let head_name = self.c_name_for("head", false);
                let cf = self.c_function();
                cf.push_line(if mom { left.clone() } else { right.clone() })
                    .name(&head_name);
                cf.push_line(if mom {
                    format!("{}.mom = &{}", head_name, right)
                } else {
                    format!("{}.dad = &{}", head_name, left)
                });
                cf.push_expr(head_name);
                return;
            }
            BinOp::Equals => ("noot_eq", false),
            BinOp::NotEquals => ("noot_neq", false),
            BinOp::Less => ("noot_lt", true),
            BinOp::LessOrEqual => ("noot_le", true),
            BinOp::Greater => ("noot_gt", true),
            BinOp::GreaterOrEqual => ("noot_ge", true),
            BinOp::Add => ("noot_add", true),
            BinOp::Sub => ("noot_sub", true),
            BinOp::Mul => ("noot_mul", true),
            BinOp::Div => ("noot_div", true),
            BinOp::Rem => ("noot_rem", true),
        };
        self.node(*expr.right, stack);
        let right = self.pop_expr();
        if can_fail {
            let function_name = &self.curr_c_function().noot_name;
            let (line, col) = expr.op_span.split().0.line_col();
            let call_line = format!(
                "noot_call_bin_op({}, {}, {}, \"{} {}:{}\")",
                f, left, right, function_name, line, col
            );
            self.push_expr(call_line)
        } else {
            self.push_expr(format!("{}({}, {})", f, left, right))
        }
    }
    fn un_expr(&mut self, expr: UnExpr<'a>, stack: TranspileStack<'a>) {
        self.node(*expr.inner, stack);
        let inner = self.pop_expr();
        let f = match expr.op {
            UnOp::Neg => "noot_neg",
        };
        self.push_expr(format!("{}({})", f, inner))
    }
    fn call_expr(&mut self, call: CallExpr<'a>, stack: TranspileStack<'a>) {
        self.node(*call.caller, stack.clone());
        let f = self.pop_expr();
        let mut params = Vec::new();
        for node in call.args {
            let param = self.node_expr(node, "arg", stack.clone());
            params.push(param)
        }
        let param_count = params.len();
        let params: String = params.into_iter().intersperse(", ".into()).collect();
        let function_name = &self.curr_c_function().noot_name;
        let (line, col) = call.span.split().0.line_col();
        let params = if param_count == 1 {
            format!("&{}", params)
        } else {
            format!("(NootValue[]) {{ {} }}", params)
        };
        let call_line = format!(
            "noot_call({}, {}, {}, \"{} {}:{}\")",
            f, param_count, params, function_name, line, col
        );
        self.push_expr(call_line)
    }
    fn node_expr(&mut self, node: Node<'a>, name: &str, stack: TranspileStack<'a>) -> String {
        if node.kind.is_const() {
            self.node(node, stack.clone());
            self.pop_expr()
        } else {
            self.node(node, stack.clone());
            let left = self.pop_expr();
            let name = self.c_name_for(name, false);
            self.c_function().push_line(left).name(&name);
            name
        }
    }
    fn term(&mut self, term: Term<'a>, stack: TranspileStack<'a>) {
        match term {
            Term::Int(i) => self.push_expr(format!("new_int({})", i)),
            Term::Real(f) => self.push_expr(format!("new_real({})", f)),
            Term::String(s) => self.push_expr(format!("new_string({:?}, {})", s, s.len())),
            Term::Expr(items) => self.items(items, stack),
            Term::Closure(closure) => {
                let c_name = self.c_name_for("anon", true);
                self.function(
                    c_name.clone(),
                    "closure",
                    closure.params,
                    closure.body,
                    stack,
                );
                if self.functions.get(&c_name).unwrap().captures.is_empty() {
                    self.push_expr(format!("new_function(&{})", c_name))
                } else {
                    self.push_expr(format!("{}_closure", c_name))
                }
            }
            Term::Tree(terms) => {
                let [left, middle, right] = *terms;
                let left = self.node_expr(left, "left", stack.clone());
                let middle = self.node_expr(middle, "middle", stack.clone());
                let right = self.node_expr(right, "right", stack.clone());
                self.push_expr(format!("new_tree(&{}, &{}, &{})", left, middle, right))
            }
            Term::Ident(ident) => {
                dbg!(ident.name);
                if let Some(def) = stack
                    .noot_scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.get(ident.name))
                {
                    dbg!(&def.c_name);
                    if let Some((ident_i, value_name)) = self
                        .function_stack
                        .iter()
                        .enumerate()
                        .find_map(|(i, c_name)| {
                            let cf = self.functions.get(c_name).unwrap();
                            cf.lines
                                .iter()
                                .find_map(|line| {
                                    line.var_name
                                        .as_ref()
                                        .filter(|&vn| {
                                            vn == &def.c_name
                                                || vn == &format!("{}_closure", def.c_name)
                                        })
                                        .cloned()
                                })
                                .map(|name| (i, name))
                        })
                        .filter(|(i, _)| self.function_stack.len() - i > 1)
                    {
                        dbg!(ident_i);
                        dbg!(&value_name);
                        // Captures
                        let curr_stack_i = self.function_stack.len() - 1;
                        let mut prev = None;
                        for stack_i in ident_i..self.function_stack.len() {
                            let last = curr_stack_i == stack_i;
                            if last {
                                let cf = self.c_function();
                                let cap_i = cf.capture_index_of(&value_name);
                                cf.push_expr(format!("captures[{}]", cap_i))
                            } else {
                                let cf = self.c_function_at(stack_i + 1);
                                cf.push_capture(
                                    value_name.clone(),
                                    prev.clone().unwrap_or_else(|| value_name.clone()),
                                );
                                let cap_i = cf.capture_index_of(&value_name);
                                prev = Some(format!("captures[{}]", cap_i));
                            };
                        }
                    } else {
                        // Non-captures
                        self.push_expr(if def.is_function {
                            let is_closure = self
                                .functions
                                .get(&def.c_name)
                                .map_or(false, |cf| !cf.captures.is_empty());
                            dbg!(is_closure);
                            if is_closure {
                                format!("{}_closure", def.c_name)
                            } else {
                                format!("new_function(&{})", def.c_name)
                            }
                        } else {
                            def.c_name.clone()
                        })
                    }
                } else if let Some(&(_, c_name)) = BUILTIN_VALUES
                    .iter()
                    .find(|(noot_name, _)| noot_name == &ident.name)
                {
                    self.push_expr(c_name.into())
                } else {
                    panic!("Unknown def not resolved: {:?}", ident.name)
                }
            }
        }
    }
    fn function(
        &mut self,
        c_name: String,
        noot_name: &'a str,
        params: Params<'a>,
        items: Items<'a>,
        stack: TranspileStack<'a>,
    ) {
        self.start_c_function(c_name.clone(), noot_name);
        let cf = self.c_function();
        for i in 0..params.len() {
            cf.push_line(format!("{i} < count ? &args[{i}] : &NOOT_NIL", i = i))
                .name(format!("{}_arg{}", c_name, i))
                .ty("NootValue*");
        }
        let stack = params
            .into_iter()
            .enumerate()
            .fold(stack, |stack, (i, param)| {
                stack.with_noot_def(
                    param.ident.name,
                    NootDef {
                        c_name: format!("*{}_arg{}", c_name, i),
                        is_function: false,
                    },
                )
            });
        // Transpile body items and finish function
        self.items(items, stack);
        let captures = self.curr_c_function().captures.clone();
        self.finish_c_function();
        // Set captures in parent scope
        if captures.is_empty() {
            return;
        }
        let captures_name = format!("{}_captures", c_name);
        let closure_name = format!("{}_closure", c_name);
        self.c_function()
            .push_line(format!("NootValue {}[{}]", captures_name, captures.len()));
        let cf = self.c_function();
        for (i, cap) in captures.iter().enumerate() {
            cf.push_line(&cap.capture_name)
                .name(format!("{}[{}]", captures_name, i,))
                .no_type();
        }
        cf.push_line(format!("new_closure(&{}, {})", c_name, captures_name))
            .name(closure_name);
    }
}
