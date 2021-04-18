use std::{collections::HashMap, error::Error, fmt, rc::Rc};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};

use crate::{ast::*, parse::Rule};

#[derive(Debug, Clone)]
pub enum Val {
    Nil,
    Bool(bool),
    Int(i64),
    Real(f64),
    String(String),
    List(Rc<Val>, Rc<Val>),
    Tree(Rc<Val>, Rc<Val>, Rc<Val>),
    Function(usize),
    Error(Rc<Val>),
}

#[derive(Debug)]
pub enum Instr {
    PushLiteral(Val),
    PushFromStack(usize),
    Call,
    Ret,
}

#[derive(Debug)]
pub struct Jitter<'a> {
    instrs: Vec<Instr>,
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
        Jitter {
            instrs: Vec::new(),
            scopes: vec![Scope::default()],
            errors: Vec::new(),
        }
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
            node => todo!("{:?}", node),
        }
    }
    fn jit_call(&mut self, call: &CallExpr<'a>) {
        for node in &call.args {
            self.jit_node(node);
        }
        self.jit_node(&call.expr);
        self.instrs.push(Instr::Call);
    }
    fn jit_bin(&mut self, expr: &BinExpr<'a>) {
        match expr.op {
            _ => todo!(),
        }
    }
}
