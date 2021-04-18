#![allow(clippy::upper_case_acronyms, dead_code)]

use pest::Span;

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Nil,
    Bool(bool),
    Int(i64),
    Real(f64),
    String(String),
    Expr(Nodes<'a>),
    Ident(Ident<'a>),
    Closure(Box<Closure<'a>>),
    BinExpr(Box<BinExpr<'a>>),
    UnExpr(Box<UnExpr<'a>>),
    Call(Box<CallExpr<'a>>),
    Def(Def<'a>),
}

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub name: String,
    pub span: Span<'a>,
}

impl<'a> PartialEq for Ident<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'a> Eq for Ident<'a> {}

pub type Nodes<'a> = Vec<Node<'a>>;

pub type Params<'a> = Vec<Ident<'a>>;

#[derive(Debug, Clone)]
pub struct Def<'a> {
    pub ident: Ident<'a>,
    pub params: Params<'a>,
    pub nodes: Nodes<'a>,
}

impl<'a> Def<'a> {
    pub fn is_function(&self) -> bool {
        !self.params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct BinExpr<'a> {
    pub left: Node<'a>,
    pub right: Node<'a>,
    pub op: BinOp,
    pub span: Span<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Or,
    And,
    Is,
    Isnt,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone)]
pub struct UnExpr<'a> {
    pub inner: Node<'a>,
    pub op: UnOp,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct CallExpr<'a> {
    pub expr: Node<'a>,
    pub args: Vec<Node<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub span: Span<'a>,
    pub params: Params<'a>,
    pub body: Nodes<'a>,
}
