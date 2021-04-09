use std::{
    fmt,
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use itertools::*;
use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};
use rpds::{HashTrieMap, List, Queue, Vector};

use crate::{ast::*, parse::Rule};

#[derive(Debug, thiserror::Error)]
pub enum TranspileErrorKind {
    #[error("Unknown definition {}", _0)]
    UnknownDef(String),
}

use TranspileErrorKind::*;

impl TranspileErrorKind {
    pub fn span(self, span: Span) -> TranspileError {
        TranspileError { kind: self, span }
    }
}

#[derive(Debug)]
pub struct TranspileError<'a> {
    pub kind: TranspileErrorKind,
    pub span: Span<'a>,
}

impl<'a> fmt::Display for TranspileError<'a> {
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

struct NootDef {
    is_function: bool,
    c_name: String,
}

macro_rules! builtin_functions {
    ($($name:literal),*) => {
        &[$(($name, concat!("noot_", $name))),*]
    }
}

const BUILTIN_FUNCTIONS: &[(&str, &str)] = builtin_functions!("print", "println");
const BUILTIN_VALUES: &[(&str, &str)] = &[("list", "NOOT_EMPTY_LIST")];

#[derive(Clone)]
struct TranspileStack {
    noot_scopes: Vector<HashTrieMap<String, NootDef>>,
}

impl TranspileStack {
    pub fn new() -> Self {
        TranspileStack {
            noot_scopes: Vector::new().push_back(
                BUILTIN_FUNCTIONS
                    .iter()
                    .map(|&(noot_name, c_name)| {
                        (
                            noot_name.into(),
                            NootDef {
                                c_name: c_name.into(),
                                is_function: true,
                            },
                        )
                    })
                    .chain(BUILTIN_VALUES.iter().map(|&(noot_name, c_name)| {
                        (
                            noot_name.into(),
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
    pub fn with_noot_def(self, name: String, def: NootDef) -> Self {
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
    functions: HashTrieMap<String, CFunction>,
    function_stack: Vector<String>,
    pub errors: List<TranspileError<'a>>,
}

#[derive(Default, Clone)]
struct CFunction {
    exprs: Queue<String>,
    lines: Vector<CLine>,
    captures: Vector<CCapture>,
    indent: usize,
}

struct CLine {
    var_name: Option<String>,
    value: String,
    indent: usize,
    semicolon: bool,
}

struct CCapture {
    pub c_name: String,
    pub capture_name: String,
}

impl CFunction {
    pub fn with_line(self, var_name: Option<String>, value: String) -> Self {
        CFunction {
            lines: self.lines.push_back(CLine {
                var_name,
                value,
                indent: self.indent,
                semicolon: true,
            }),
            ..self
        }
    }
    pub fn with_raw_line(self, value: String) -> Self {
        CFunction {
            lines: self.lines.push_back(CLine {
                var_name: None,
                value,
                indent: self.indent,
                semicolon: false,
            }),
            ..self
        }
    }
    pub fn push_expr(self, expr: String) -> Self {
        CFunction {
            exprs: self.exprs.enqueue(expr),
            ..self
        }
    }
    pub fn pop_expr(self) -> (Self, Option<String>) {
        let expr = self.exprs.peek().cloned();
        (
            CFunction {
                exprs: self.exprs.dequeue().unwrap_or_default(),
                ..self
            },
            expr,
        )
    }
    pub fn capture_index_of(&self, c_name: &str) -> usize {
        self.captures
            .iter()
            .position(|cap| cap.c_name == c_name)
            .unwrap()
    }
    pub fn with_capture(self, c_name: String, capture_name: String) -> Self {
        if self.captures.iter().any(|cap| cap.c_name == c_name) {
            self
        } else {
            CFunction {
                captures: self.captures.push_back(CCapture {
                    c_name,
                    capture_name,
                }),
                ..self
            }
        }
    }
    pub fn indent(self) -> Self {
        CFunction {
            indent: self.indent + 1,
            ..self
        }
    }
    pub fn deindent(self) -> Self {
        CFunction {
            indent: self.indent - 1,
            ..self
        }
    }
}

pub fn transpile(items: Items) -> Transpilation {
    Transpilation::new().items(items, TranspileStack::new())
}

impl<'a> Transpilation<'a> {
    pub fn new() -> Self {
        Transpilation {
            functions: once("main")
                // .chain(BUILTINS.iter().map(|bi| bi.0))
                .map(|name| (name.into(), CFunction::default()))
                .collect(),
            function_stack: once("main".into()).collect(),
            errors: Default::default(),
        }
    }
    pub fn write(self) -> io::Result<()> {
        fs::create_dir_all("build")?;
        let mut source = File::create("build/main.c")?;

        // Write headers
        writeln!(source, "#include \"../clibs/noot.h\"")?;
        writeln!(source, "#include \"../clibs/tgc.h\"")?;
        writeln!(source)?;

        // Write function declarations
        for (name, cf) in self.functions.iter().filter(|&(name, _)| name != "main") {
            writeln!(source, "NootValue {}(int count, NootValue* args);", name)?;
            if !cf.captures.is_empty() {
                writeln!(source, "static NootValue* {}_captures;", name)?;
            }
        }
        writeln!(source)?;

        // Write function definitions
        for (name, cf) in &self.functions {
            let main = name == "main";
            // Write signature
            if main {
                writeln!(source, "int main(int argc, char** argv) {{")?;
                writeln!(source, "    tgc_start(&noot_gc, &argc);")?;
            } else {
                writeln!(source, "NootValue {}(int count, NootValue* args) {{", name)?;
            }
            // Write lines
            for line in &cf.lines {
                write!(source, "{:indent$}", "", indent = (line.indent + 1) * 4)?;
                if let Some(var_name) = &line.var_name {
                    write!(source, "NootValue {} = ", var_name)?;
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
                if let (_, Some(expr)) = cf.clone().pop_expr() {
                    writeln!(source, "    {};", expr)?;
                }
                writeln!(source, "    tgc_stop(&noot_gc);")?;
                writeln!(source, "    return 0;")?;
            }
            // Close function
            writeln!(source, "}}\n")?;
        }

        Ok(())
    }
    fn c_name_exists(&self, c_name: &str, function: bool) -> bool {
        function && self.functions.keys().any(|name| name == c_name)
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
        let mut i = 1;
        while self.c_name_exists(&c_name, function) {
            i += 1;
            c_name = format!("{}_{}", noot_name, i);
        }
        c_name
    }
    fn start_c_function(self, c_name: String) -> Self {
        Transpilation {
            functions: self.functions.insert(c_name.clone(), CFunction::default()),
            function_stack: self.function_stack.push_back(c_name),
            ..self
        }
    }
    fn finish_c_function(self) -> Self {
        let result = self.map_c_function(|cf| {
            let ret_expr = cf
                .exprs
                .peek()
                .cloned()
                .unwrap_or_else(|| "NOOT_NIL".into());
            let cf = CFunction {
                exprs: cf.exprs.dequeue().unwrap_or_default(),
                ..cf
            };
            cf.with_line(None, format!("return {}", ret_expr))
        });
        Transpilation {
            function_stack: result.function_stack.drop_last().unwrap(),
            ..result
        }
    }
    fn curr_c_function(&self) -> &CFunction {
        self.functions
            .get(self.function_stack.last().unwrap())
            .unwrap()
    }
    fn map_c_function_at<F>(self, i: usize, f: F) -> Self
    where
        F: FnOnce(CFunction) -> CFunction,
    {
        let function_name = self.function_stack.get(i).unwrap();
        let cf = self.functions.get(function_name).unwrap();
        Transpilation {
            functions: self.functions.insert(function_name.clone(), f(cf.clone())),
            ..self
        }
    }
    fn map_c_function<F>(self, f: F) -> Self
    where
        F: FnOnce(CFunction) -> CFunction,
    {
        let last_index = self.function_stack.len() - 1;
        self.map_c_function_at(last_index, f)
    }
    fn pop_expr(self) -> (Self, String) {
        let mut expr = None;
        let result = self.map_c_function(|cf| {
            let (cf, ex) = cf.pop_expr();
            expr = ex;
            cf
        });
        (result, expr.unwrap_or_else(|| "NOOT_NIL".into()))
    }
    fn error(self, error: TranspileError<'a>) -> Self {
        Transpilation {
            errors: self.errors.push_front(error),
            ..self
        }
    }
    fn items(self, items: Items<'a>, stack: TranspileStack) -> Self {
        let item_count = items.len();
        items
            .into_iter()
            .enumerate()
            .fold((self, stack), |(result, stack), (i, item)| {
                let (result, stack) = result.item(item, stack);
                let result = if i == item_count - 1 {
                    result
                } else {
                    result.map_c_function(|cf| {
                        if let Some(expr) = cf.exprs.peek().cloned() {
                            let cf = CFunction {
                                exprs: cf.exprs.dequeue().unwrap_or_default(),
                                ..cf
                            };
                            cf.with_line(None, expr)
                        } else {
                            cf
                        }
                    })
                };
                (result, stack)
            })
            .0
    }

    fn item(self, item: Item<'a>, stack: TranspileStack) -> (Self, TranspileStack) {
        match item {
            Item::Def(def) => self.def(def, stack),
            Item::Node(node) => {
                let result = self.node(node, stack.clone());
                (result, stack)
            }
        }
    }

    fn def(self, def: Def<'a>, stack: TranspileStack) -> (Self, TranspileStack) {
        let c_name = self.c_name_for(&def.ident.name, def.is_function());
        if def.is_function() {
            // Function
            let stack = stack.with_noot_def(
                def.ident.name.clone(),
                NootDef {
                    c_name: c_name.clone(),
                    is_function: true,
                },
            );
            let result = self.function(c_name, def.params, def.items, stack.clone());
            (result, stack)
        } else {
            // Value
            let result = self.items(def.items, stack.clone());
            let result = result.map_c_function(|cf| {
                let (cf, line) = cf.pop_expr();
                if let Some(line) = line {
                    cf.with_line(Some(c_name.clone()), line)
                } else {
                    cf
                }
            });
            let stack = stack.with_noot_def(
                def.ident.name,
                NootDef {
                    c_name,
                    is_function: false,
                },
            );
            (result, stack)
        }
    }

    fn node(self, node: Node<'a>, stack: TranspileStack) -> Self {
        match node {
            Node::Term(term) => self.term(term, stack),
            Node::BinExpr(expr) => self.bin_expr(expr, stack),
            Node::UnExpr(expr) => self.un_expr(expr, stack),
            Node::Call(expr) => self.call_expr(expr, stack),
            Node::Insert(expr) => self.insert_expr(expr, stack),
        }
    }
    fn bin_expr(self, expr: BinExpr<'a>, stack: TranspileStack) -> Self {
        let result = self.node(*expr.left, stack.clone());
        let (result, left) = result.pop_expr();
        let f = match expr.op {
            BinOp::Or | BinOp::And => {
                let or = expr.op == BinOp::Or;
                let temp_name = result.c_name_for("temp", false);
                let result = result.map_c_function(|cf| {
                    cf.with_line(Some(temp_name.clone()), left)
                        .with_raw_line(format!(
                            "if ({}noot_is_true({})) {{",
                            if or { "!" } else { "" },
                            temp_name
                        ))
                        .indent()
                });
                let result = result.node(*expr.right, stack);
                let (result, right) = result.pop_expr();
                return result.map_c_function(|cf| {
                    cf.with_line(Some(temp_name.clone()), right)
                        .deindent()
                        .with_raw_line("}".into())
                        .push_expr(temp_name)
                });
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
        let result = result.node(*expr.right, stack);
        let (result, right) = result.pop_expr();
        result.map_c_function(|cf| cf.push_expr(format!("{}({}, {})", f, left, right)))
    }
    fn un_expr(self, expr: UnExpr<'a>, stack: TranspileStack) -> Self {
        let result = self.node(*expr.inner, stack);
        let (result, inner) = result.pop_expr();
        let f = match expr.op {
            UnOp::Neg => "noot_neg",
            UnOp::Not => "noot_not",
        };
        result.map_c_function(|cf| cf.push_expr(format!("{}({})", f, inner)))
    }
    fn call_expr(self, call: CallExpr<'a>, stack: TranspileStack) -> Self {
        let result = self.node(*call.expr, stack.clone());
        let (result, f) = result.pop_expr();
        let (result, params) =
            call.args
                .into_iter()
                .fold((result, Vector::new()), |(result, params), node| {
                    let result = result.node(node, stack.clone());
                    let (result, param) = result.pop_expr();
                    (result, params.push_back(param))
                });
        let param_count = params.len();
        let params: String = params
            .into_iter()
            .cloned()
            .intersperse(", ".into())
            .collect();
        result.map_c_function(|cf| {
            cf.push_expr(format!(
                "noot_call({}, {}, (NootValue[]) {{ {} }})",
                f, param_count, params
            ))
        })
    }
    fn insert_expr(self, expr: InsertExpr<'a>, stack: TranspileStack) -> Self {
        let (result, term) = self.term(expr.term, stack.clone()).pop_expr();
        let (result, expr) =
            expr.insertions
                .into_iter()
                .fold((result, term), |(result, inner), ins| {
                    let (result, key) = result.term(ins.key, stack.clone()).pop_expr();
                    let (result, val) = if let Some(val) = ins.val {
                        let (result, val) = result.term(val, stack.clone()).pop_expr();
                        (result, format!("&{}", val))
                    } else {
                        (result, "NULL".into())
                    };
                    (result, format!("noot_insert({}, {}, {})", inner, key, val))
                });
        result.map_c_function(|cf| cf.push_expr(expr))
    }
    fn term(self, term: Term<'a>, stack: TranspileStack) -> Self {
        match term {
            Term::Nil => self.map_c_function(|cf| cf.push_expr("NOOT_NIL".into())),
            Term::Bool(b) => {
                self.map_c_function(|cf| cf.push_expr(format!("new_bool({})", b as u8)))
            }
            Term::Int(i) => self.map_c_function(|cf| cf.push_expr(format!("new_int({})", i))),
            Term::Real(f) => self.map_c_function(|cf| cf.push_expr(format!("new_real({})", f))),
            Term::String(s) => {
                self.map_c_function(|cf| cf.push_expr(format!("new_string({:?}, {})", s, s.len())))
            }
            Term::Expr(items) => self.items(items, stack),
            Term::Closure(closure) => {
                let c_name = self.c_name_for("closure", true);
                let result = self.function(c_name.clone(), closure.params, closure.body, stack);
                result.map_c_function(|cf| cf.push_expr(format!("new_function(&{})", c_name)))
            }
            Term::Ident(ident) => {
                if let Some(def) = stack
                    .noot_scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.get(&ident.name))
                {
                    if let Some(ident_i) = self.function_stack.iter().position(|c_name| {
                        let cf = self.functions.get(c_name).unwrap();
                        cf.lines.iter().any(|line| {
                            line.var_name.as_ref().map_or(false, |vn| vn == &def.c_name)
                        })
                    }) {
                        // Captures
                        let curr_stack_i = self.function_stack.len() - 1;
                        let (result, _) = (ident_i..self.function_stack.len()).fold(
                            (self, None),
                            |(result, mut prev), stack_i| {
                                let last = curr_stack_i == stack_i;
                                let function_name =
                                    result.function_stack.get(stack_i).unwrap().clone();
                                let capturer_name = result.function_stack.get(stack_i + 1).cloned();
                                let result = if last {
                                    result.map_c_function(|cf| {
                                        let cap_i = cf.capture_index_of(&def.c_name);
                                        cf.push_expr(format!(
                                            "{}_captures[{}]",
                                            function_name, cap_i
                                        ))
                                    })
                                } else {
                                    result.map_c_function_at(stack_i + 1, |cf| {
                                        let cf = cf.with_capture(
                                            def.c_name.clone(),
                                            prev.clone().unwrap_or_else(|| def.c_name.clone()),
                                        );
                                        let cap_i = cf.capture_index_of(&def.c_name);
                                        prev = Some(format!(
                                            "{}_captures[{}]",
                                            capturer_name.unwrap(),
                                            cap_i
                                        ));
                                        cf
                                    })
                                };
                                (result, prev)
                            },
                        );
                        result
                    } else {
                        // Non-captures
                        self.map_c_function(|cf| {
                            cf.push_expr(if def.is_function {
                                format!("new_function(&{})", def.c_name)
                            } else {
                                def.c_name.clone()
                            })
                        })
                    }
                } else {
                    self.error(UnknownDef(ident.name.clone()).span(ident.span))
                }
            }
        }
    }
    fn function(
        self,
        c_name: String,
        params: Params<'a>,
        items: Items<'a>,
        stack: TranspileStack,
    ) -> Self {
        let result = self.start_c_function(c_name.clone());
        let stack = params
            .into_iter()
            .enumerate()
            .fold(stack, |stack, (i, param)| {
                stack.with_noot_def(
                    param.ident.name,
                    NootDef {
                        c_name: format!("args[{}]", i),
                        is_function: false,
                    },
                )
            });
        // Transpile body items and finish function
        let result = result.items(items, stack);
        let captures = result.curr_c_function().captures.clone();
        let result = result.finish_c_function();
        // Set captures in parent scope
        if captures.is_empty() {
            result
        } else {
            let captures_name = format!("{}_captures", c_name);
            let result = result.map_c_function(|cf| {
                cf.with_raw_line(format!(
                    "{} = (NootValue*)tgc_alloc(&noot_gc, {} * sizeof(NootValue));",
                    captures_name,
                    captures.len()
                ))
            });
            captures
                .iter()
                .enumerate()
                .fold(result, |result, (i, cap)| {
                    result.map_c_function(|cf| {
                        cf.with_raw_line(format!(
                            "{}[{}] = {};",
                            captures_name, i, cap.capture_name
                        ))
                    })
                })
        }
    }
}
