#![allow(clippy::upper_case_acronyms)]

use std::rc::Rc;

use pest::Span;

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub span: Span<'a>,
}

impl<'a> PartialEq for Ident<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'a> Eq for Ident<'a> {}
#[derive(Debug, Clone)]
pub enum Item<'a> {
    Node(Node<'a>),
    Def(Rc<Def<'a>>),
}

pub type Items<'a> = Vec<Item<'a>>;

#[derive(Debug, Clone)]
pub struct Param<'a> {
    pub ident: Ident<'a>,
}

pub type Params<'a> = Vec<Param<'a>>;

#[derive(Debug, Clone)]
pub struct Def<'a> {
    pub ident: Ident<'a>,
    pub params: Params<'a>,
    pub items: Items<'a>,
}

impl<'a> Def<'a> {
    pub fn is_function(&self) -> bool {
        !self.params.is_empty()
    }
}

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Term(Term<'a>),
    BinExpr(BinExpr<'a>),
    UnExpr(UnExpr<'a>),
    Call(CallExpr<'a>),
    Push(PushExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct BinExpr<'a> {
    pub left: Box<Node<'a>>,
    pub right: Box<Node<'a>>,
    pub op: BinOp,
    pub span: Span<'a>,
    pub op_span: Span<'a>,
}

impl<'a> BinExpr<'a> {
    pub fn new(
        left: Node<'a>,
        right: Node<'a>,
        op: BinOp,
        span: Span<'a>,
        op_span: Span<'a>,
    ) -> Self {
        BinExpr {
            left: left.into(),
            right: right.into(),
            op,
            span,
            op_span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Or,
    And,
    Equals,
    NotEquals,
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
    pub inner: Box<Node<'a>>,
    pub op: UnOp,
}

impl<'a> UnExpr<'a> {
    pub fn new(inner: Node<'a>, op: UnOp) -> Self {
        UnExpr {
            inner: inner.into(),
            op,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct CallExpr<'a> {
    pub caller: Box<Node<'a>>,
    pub args: Vec<Node<'a>>,
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub struct PushExpr<'a> {
    pub head: Box<Node<'a>>,
    pub tail: Box<Node<'a>>,
}

#[derive(Debug, Clone)]
pub enum Term<'a> {
    Expr(Items<'a>),
    Int(i64),
    Real(f64),
    Ident(Ident<'a>),
    Bool(bool),
    String(String),
    List(Vec<Term<'a>>),
    Tree(Box<[Term<'a>; 3]>),
    Closure(Box<Closure<'a>>),
    Nil,
}

impl<'a> Term<'a> {
    pub fn is_underscore(&self) -> bool {
        if let Term::Ident(ident) = self {
            ident.name == "_"
        } else {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub span: Span<'a>,
    pub params: Params<'a>,
    pub body: Items<'a>,
}
