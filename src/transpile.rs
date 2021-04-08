use std::{
    fmt,
    fs::{self, File},
    io::{self, Write},
    iter::once,
};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};
use rpds::{HashTrieMap, List, Queue, Stack, Vector};

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

#[derive(Clone)]
struct TranspileStack {
    noot_scopes: Vector<HashTrieMap<String, NootDef>>,
}

impl TranspileStack {
    pub fn new() -> Self {
        TranspileStack {
            noot_scopes: Vector::new().push_back(Default::default()),
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
    function_stack: Stack<String>,
    pub errors: List<TranspileError<'a>>,
}

#[derive(Default, Clone)]
struct CFunction {
    exprs: Queue<String>,
    lines: Vector<CLine>,
}

impl CFunction {
    pub fn with_line(self, var_name: Option<String>, value: String) -> Self {
        CFunction {
            lines: self.lines.push_back(CLine { var_name, value }),
            ..self
        }
    }
    pub fn push_expr(self, expr: String) -> Self {
        CFunction {
            exprs: self.exprs.enqueue(expr),
            ..self
        }
    }
    pub fn pop_expr(self) -> (Self, String) {
        let expr = self
            .exprs
            .peek()
            .cloned()
            .unwrap_or_else(|| "NOOT_NIL".into());
        (
            CFunction {
                exprs: self.exprs.dequeue().unwrap_or_default(),
                ..self
            },
            expr,
        )
    }
}

struct CLine {
    var_name: Option<String>,
    value: String,
}

pub fn transpile(items: Items) -> Transpilation {
    Transpilation::new().items(items, TranspileStack::new())
}

impl<'a> Transpilation<'a> {
    pub fn new() -> Self {
        Transpilation {
            functions: once(("main".into(), CFunction::default())).collect(),
            function_stack: once("main".into()).collect(),
            errors: Default::default(),
        }
    }
    pub fn write(self) -> io::Result<()> {
        fs::create_dir_all("build")?;
        let mut source = File::create("build/main.c")?;

        // Copy source files
        for name in &["noot.h", "tgc.c", "tgc.h", "utf8.h"] {
            fs::copy(format!("clibs/{}", name), format!("build/{}", name))?;
        }

        // Write headers
        writeln!(source, "#include \"noot.h\"")?;
        writeln!(source, "#include \"tgc.h\"")?;
        writeln!(source)?;

        // Write function declarations
        for name in self.functions.keys().filter(|&name| name != "main") {
            writeln!(source, "NootValue {}(int count, NootValue* args);", name)?;
        }
        writeln!(source)?;

        // Write function definitions
        for (name, function) in self.functions.into_iter() {
            let main = name == "main";
            let ty = if main { "int" } else { "NootValue" };
            writeln!(source, "{} {}(int count, NootValue* args) {{", ty, name)?;
            for line in &function.lines {
                write!(source, "    ")?;
                if let Some(var_name) = &line.var_name {
                    write!(source, "NootValue {} = ", var_name)?;
                }
                writeln!(source, "{};", line.value)?;
            }
            if main {
                writeln!(source, "    return 0;")?;
            }
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
        let mut i = 0;
        while self.c_name_exists(&c_name, function) {
            i += 1;
            c_name = format!("{}_{}", noot_name, i);
        }
        c_name
    }
    fn start_c_function(self, c_name: String) -> Self {
        Transpilation {
            functions: self.functions.insert(c_name.clone(), CFunction::default()),
            function_stack: self.function_stack.push(c_name),
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
            function_stack: result.function_stack.pop().unwrap(),
            ..result
        }
    }
    fn curr_c_function_name(&self) -> &str {
        self.function_stack.peek().unwrap()
    }
    fn curr_c_function(&self) -> &CFunction {
        self.functions.get(self.curr_c_function_name()).unwrap()
    }
    fn map_c_function<F>(self, f: F) -> Self
    where
        F: FnOnce(CFunction) -> CFunction,
    {
        Transpilation {
            functions: self.functions.insert(
                self.curr_c_function_name().into(),
                f(self.curr_c_function().clone()),
            ),
            ..self
        }
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
                        let expr = cf
                            .exprs
                            .peek()
                            .cloned()
                            .unwrap_or_else(|| "NOOT_NIL".into());
                        let cf = CFunction {
                            exprs: cf.exprs.dequeue().unwrap_or_default(),
                            ..cf
                        };
                        cf.with_line(None, expr)
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
            let result = self.start_c_function(c_name);
            let stack = def
                .params
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
            let result = result.items(def.items, stack.clone());
            let result = result.finish_c_function();
            // Use this for closures
            // let result = result.map_c_function(|cf| {
            //     let (cf, line) = cf.pop_expr();
            //     cf.with_line(Some(c_name), line)
            // });
            (result, stack)
        } else {
            // Value
            let result = self.items(def.items, stack.clone());
            let result = result.map_c_function(|cf| {
                let (cf, line) = cf.pop_expr();
                cf.with_line(Some(c_name.clone()), line)
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
            _ => todo!(),
        }
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
                self.map_c_function(|cf| cf.push_expr(format!("new_string({:?})", s)))
            }
            Term::Closure(_) => todo!(),
            Term::Expr(items) => self.items(items, stack),
            Term::Ident(ident) => {
                if let Some(def) = stack
                    .noot_scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.get(&ident.name))
                {
                    self.map_c_function(|cf| {
                        cf.push_expr(if def.is_function {
                            format!("new_function(&{})", def.c_name)
                        } else {
                            def.c_name.clone()
                        })
                    })
                } else {
                    self.error(UnknownDef(ident.name.clone()).span(ident.span))
                }
            }
        }
    }
}
