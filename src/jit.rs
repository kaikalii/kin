use std::rc::Rc;

use crate::ast::*;

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
    PushVal(Val),
    Call,
    Ret,
}

#[derive(Debug)]
pub struct Jitter {
    instrs: Vec<Instr>,
}

impl Jitter {
    pub fn new() -> Self {
        Jitter { instrs: Vec::new() }
    }
    pub fn jit_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.jit_node(node);
        }
    }
    fn jit_node(&mut self, node: &Node) {
        match node {
            Node::Nil => self.instrs.push(Instr::PushVal(Val::Nil)),
            Node::Bool(b) => self.instrs.push(Instr::PushVal(Val::Bool(*b))),
            Node::Int(i) => self.instrs.push(Instr::PushVal(Val::Int(*i))),
            Node::Real(r) => self.instrs.push(Instr::PushVal(Val::Real(*r))),
            Node::String(s) => self.instrs.push(Instr::PushVal(Val::String(s.clone()))),
            Node::Ident(ident) => {}
            Node::Expr(nodes) => self.jit_nodes(nodes),
            Node::Call(call) => self.jit_call(call),
            node => todo!("{:?}", node),
        }
    }
    fn jit_call(&mut self, call: &CallExpr) {
        for node in &call.args {
            self.jit_node(node);
        }
        self.jit_node(&call.expr);
        self.instrs.push(Instr::Call);
    }
}
