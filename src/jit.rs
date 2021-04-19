use std::{collections::HashMap, error::Error, fmt};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};

use crate::{ast::*, parse::Rule, value::*};

pub enum Instr {
    PushLiteral(Val),
    PushFromStack(usize),
    Call(usize),
    Ret,
    Builtin(BuiltinFn),
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::PushLiteral(val) => write!(f, "{:?}", val),
            Instr::PushFromStack(i) => write!(f, "[-{}]", i),
            Instr::Call(i) => write!(f, "call {}", i),
            Instr::Ret => write!(f, "return"),
            Instr::Builtin(_) => write!(f, "builtin"),
        }
    }
}

#[derive(Debug)]
pub struct Jitter<'a> {
    pub instrs: Vec<Instr>,
    scopes: Vec<Scope>,
    pub errors: Vec<JitError<'a>>,
}

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<String, usize>,
}

#[derive(Debug)]
enum JitErrorKind {
    UnknownDef(String),
}

impl JitErrorKind {
    fn span(self, span: Span) -> JitError {
        JitError { kind: self, span }
    }
}

impl fmt::Display for JitErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JitErrorKind::UnknownDef(name) => writeln!(f, "Unknown def {:?}", name),
        }
    }
}

#[derive(Debug)]
pub struct JitError<'a> {
    kind: JitErrorKind,
    span: Span<'a>,
}

impl<'a> fmt::Display for JitError<'a> {
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

impl<'a> Error for JitError<'a> {}

impl<'a> Jitter<'a> {
    pub fn new() -> Self {
        let mut jitter = Jitter {
            instrs: Vec::new(),
            scopes: vec![Scope::default()],
            errors: Vec::new(),
        };
        macro_rules! binops {
            ($($name:ident),*) => {
                $(jitter.add_builtin(stringify!($name), |stack| {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(Val::$name(left, right));
                }))*
            }
        }
        binops!(add);
        jitter
    }
    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn add_builtin(&mut self, name: &str, f: BuiltinFn) {
        let index = self.instrs.len();
        self.scope().names.insert(name.into(), index);
        self.instrs.push(Instr::Builtin(f));
        self.instrs.push(Instr::Ret);
    }
    fn find_name(&self, name: &str) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.names.get(name))
            .copied()
    }
    pub fn jit_nodes(&mut self, nodes: &[Node<'a>]) {
        for node in nodes {
            self.jit_node(node);
        }
    }
    fn jit_node(&mut self, node: &Node<'a>) {
        match node {
            Node::Nil => self.instrs.push(Instr::PushLiteral(Val::Nil)),
            Node::Bool(b) => self.instrs.push(Instr::PushLiteral(Val::Bool(*b))),
            Node::Int(i) => self.instrs.push(Instr::PushLiteral(Val::Int(*i))),
            Node::Real(r) => self.instrs.push(Instr::PushLiteral(Val::Real(*r))),
            Node::String(s) => self.instrs.push(Instr::PushLiteral(Val::String(s.clone()))),
            Node::Ident(ident) => {
                if let Some(i) = self.find_name(&ident.name) {
                    self.instrs.push(Instr::PushFromStack(i));
                } else {
                    self.errors.push(
                        JitErrorKind::UnknownDef(ident.name.clone()).span(ident.span.clone()),
                    );
                }
            }
            Node::Expr(nodes) => self.jit_nodes(nodes),
            Node::Call(call) => self.jit_call(call),
            Node::BinExpr(expr) => self.jit_bin(expr),
            Node::Def(def) => self.jit_def(def),
            node => todo!("{:?}", node),
        }
    }
    fn jit_call(&mut self, call: &CallExpr<'a>) {
        for node in &call.args {
            self.jit_node(node);
        }
        self.jit_node(&call.expr);
        self.instrs.push(Instr::Call(call.args.len()));
    }
    fn jit_bin(&mut self, expr: &BinExpr<'a>) {
        let function_name = match expr.op {
            BinOp::Add => "add",
            op => todo!("{:?}", op),
        };
        self.jit_node(&expr.left);
        self.jit_node(&expr.right);
        let index = self
            .find_name(function_name)
            .unwrap_or_else(|| panic!("No function named {:?}", function_name));
        self.instrs.push(Instr::PushLiteral(Val::Function(index)));
        self.instrs.push(Instr::Call(2));
    }
    fn jit_def(&mut self, def: &Def<'a>) {
        if def.params.is_empty() {
            // Value
            self.jit_nodes(&def.nodes);
            let index = self.instrs.len() - 1; //wrong
            self.scope().names.insert(def.ident.name.clone(), index);
        } else {
            //Function
        }
    }
}
