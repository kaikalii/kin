#![allow(clippy::upper_case_acronyms, dead_code)]

use std::fmt;

use derive_more::Display;
use itertools::Itertools;
use pest::Span;

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

impl<'a> fmt::Display for Ident<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Display, Clone)]
pub enum Item<'a> {
    Node(Node<'a>),
    Def(Def<'a>),
}

pub type Items<'a> = Vec<Item<'a>>;

fn format_items(f: &mut fmt::Formatter, items: &[Item]) -> fmt::Result {
    if items.len() == 1 {
        write!(f, "{}", items[0])?;
    } else {
        for item in items {
            write!(f, "\n    {}", item)?;
        }
        write!(f, "\nend")?;
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub struct Param<'a> {
    pub ident: Ident<'a>,
}

impl<'a> fmt::Display for Param<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)
    }
}

fn format_params(f: &mut fmt::Formatter, params: &[Param]) -> fmt::Result {
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            write!(f, " ")?;
        }
        write!(f, "{}", param)?;
    }
    Ok(())
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

impl<'a> fmt::Display for Def<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.params.is_empty() {
            write!(f, " ")?;
            format_params(f, &self.params)?;
        }
        write!(f, " = ")?;
        if self.items.len() == 1 {
            write!(f, "{}", self.items[0])?;
        } else {
            for item in &self.items {
                write!(f, "\n    {}", item)?;
            }
            write!(f, "\nend")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Node<'a> {
    Term(Term<'a>),
    BinExpr(BinExpr<'a>),
    UnExpr(UnExpr<'a>),
    Call(CallExpr<'a>),
    Insert(InsertExpr<'a>),
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Term(term) => term.fmt(f),
            Node::BinExpr(expr) => expr.fmt(f),
            Node::UnExpr(expr) => expr.fmt(f),
            Node::Call(expr) => expr.fmt(f),
            Node::Insert(expr) => expr.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinExpr<'a> {
    pub left: Box<Node<'a>>,
    pub right: Box<Node<'a>>,
    pub op: BinOp,
}

impl<'a> BinExpr<'a> {
    pub fn new(left: Node<'a>, right: Node<'a>, op: BinOp) -> Self {
        BinExpr {
            left: left.into(),
            right: right.into(),
            op,
        }
    }
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

impl<'a> fmt::Display for BinExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Or => "or".fmt(f),
            BinOp::And => "and".fmt(f),
            BinOp::Is => "is".fmt(f),
            BinOp::Isnt => "isnt".fmt(f),
            BinOp::Less => "<".fmt(f),
            BinOp::LessOrEqual => "<=".fmt(f),
            BinOp::Greater => ">".fmt(f),
            BinOp::GreaterOrEqual => ">=".fmt(f),
            BinOp::Add => "+".fmt(f),
            BinOp::Sub => "-".fmt(f),
            BinOp::Mul => "*".fmt(f),
            BinOp::Div => "/".fmt(f),
            BinOp::Rem => "%".fmt(f),
        }
    }
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

impl<'a> fmt::Display for UnExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.op, self.inner)
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOp::Not => "not ".fmt(f),
            UnOp::Neg => '-'.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallExpr<'a> {
    pub expr: Box<Node<'a>>,
    pub args: Vec<Node<'a>>,
    pub chained: Option<String>,
}

impl<'a> fmt::Display for CallExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #[cfg(feature = "debug")]
        write!(f, "{{")?;
        match &self.chained {
            Some(sep) if !cfg!(feature = "dechain") => {
                let prev = self.args[0].to_string();
                write!(
                    f,
                    "{}{}{} {}",
                    &prev[1..prev.len() - 1],
                    sep,
                    self.expr,
                    self.args
                        .iter()
                        .skip(1)
                        .map(ToString::to_string)
                        .intersperse(" ".into())
                        .collect::<String>()
                )?;
            }
            _ => {
                write!(
                    f,
                    "{}{}{}",
                    self.expr,
                    if self.args.is_empty() { "" } else { " " },
                    self.args
                        .iter()
                        .map(ToString::to_string)
                        .intersperse(" ".into())
                        .collect::<String>()
                )?;
            }
        }
        #[cfg(feature = "debug")]
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Insertion<'a> {
    pub key: Term<'a>,
    pub val: Option<Term<'a>>,
}

#[derive(Debug, Clone)]
pub struct InsertExpr<'a> {
    pub term: Term<'a>,
    pub insertions: Vec<Insertion<'a>>,
}

impl<'a> fmt::Display for InsertExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.term)?;
        match self.insertions.len() {
            0 => {}
            1 => {
                write!(f, " :{}", self.insertions[0].key)?;
                if let Some(val) = &self.insertions[0].val {
                    write!(f, " {}", val)?;
                }
            }
            _ => {
                writeln!(f)?;
                for ins in &self.insertions {
                    write!(f, "    :{}", ins.key)?;
                    if let Some(val) = &ins.val {
                        write!(f, " {}", val)?;
                    }
                    writeln!(f)?;
                }
                write!(f, "end")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Term<'a> {
    Expr(Items<'a>),
    Int(i64),
    Real(f64),
    Ident(Ident<'a>),
    Bool(bool),
    String(String),
    Closure(Box<Closure<'a>>),
    Nil,
}

impl<'a> fmt::Display for Term<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Expr(items) => format_items(f, items),
            Term::Int(i) => i.fmt(f),
            Term::Real(i) => i.fmt(f),
            Term::Ident(ident) => ident.fmt(f),
            Term::Bool(b) => b.fmt(f),
            Term::String(s) => write!(f, "{:?}", s),
            Term::Closure(closure) => closure.fmt(f),
            Term::Nil => "nil".fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub params: Params<'a>,
    pub body: Items<'a>,
}

impl<'a> fmt::Display for Closure<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        format_params(f, &self.params)?;
        write!(f, " | ")?;
        format_items(f, &self.body)?;
        Ok(())
    }
}
