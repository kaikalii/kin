#![allow(clippy::upper_case_acronyms)]

use pest::Span;

#[derive(Debug, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub span: Span<'a>,
}

impl<'a> Ident<'a> {
    pub fn is_underscore(&self) -> bool {
        self.name == "_"
    }
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
    Def(Def<'a>),
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
pub enum NodeKind<'a> {
    Term(Term<'a>, Span<'a>),
    BinExpr(BinExpr<'a>),
    UnExpr(UnExpr<'a>),
    Call(CallExpr<'a>),
    Push(PushExpr<'a>),
}

impl<'a> NodeKind<'a> {
    pub fn scope(self, scope: usize) -> Node<'a> {
        Node { kind: self, scope }
    }
    pub fn is_underscore(&self) -> bool {
        match self {
            NodeKind::Term(term, _) => term.is_underscore(),
            _ => false,
        }
    }
    pub fn span(&self) -> &Span<'a> {
        match self {
            NodeKind::Term(_, span) => span,
            NodeKind::BinExpr(expr) => &expr.span,
            NodeKind::UnExpr(expr) => &expr.span,
            NodeKind::Call(expr) => &expr.span,
            NodeKind::Push(expr) => &expr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub scope: usize,
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
    pub span: Span<'a>,
}

impl<'a> UnExpr<'a> {
    pub fn new(inner: Node<'a>, op: UnOp, span: Span<'a>) -> Self {
        UnExpr {
            inner: inner.into(),
            op,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
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
    pub span: Span<'a>,
}

#[derive(Debug, Clone)]
pub enum Term<'a> {
    Expr(Items<'a>),
    Int(i64),
    Real(f64),
    Ident(Ident<'a>),
    String(String),
    List(Vec<Node<'a>>),
    Tree(Box<[Node<'a>; 3]>),
    Closure(Box<Closure<'a>>),
}

impl<'a> Term<'a> {
    pub fn is_underscore(&self) -> bool {
        if let Term::Ident(ident) = self {
            ident.is_underscore()
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
