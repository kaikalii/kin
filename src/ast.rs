#![allow(clippy::upper_case_acronyms, dead_code)]

use std::fmt;

use colored::Colorize;
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
    Expression(Node<'a>),
    Def(Def<'a>),
}

#[derive(Debug, Display, Clone)]
#[display(
    fmt = "{}",
    r#"items.iter().map(ToString::to_string).intersperse("\n\n".into()).collect::<String>()"#
)]
pub struct Items<'a> {
    pub items: Vec<Item<'a>>,
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

#[derive(Debug, Display, Clone)]
#[display(
    fmt = "{}",
    r#"params.iter().map(ToString::to_string).intersperse(" ".into()).collect::<String>()"#
)]
pub struct Params<'a> {
    pub params: Vec<Param<'a>>,
}

#[derive(Debug, Clone)]
pub struct Def<'a> {
    pub ident: Ident<'a>,
    pub params: Params<'a>,
    pub items: Items<'a>,
}

impl<'a> fmt::Display for Def<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.params.params.is_empty() {
            write!(f, " {}", self.params)?;
        }
        write!(f, " = ")?;
        if self.items.items.len() == 1 {
            write!(f, "{}", self.items.items[0])?;
        } else {
            for item in &self.items.items {
                write!(f, "\n    {}", item)?;
            }
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

#[derive(Debug, Clone)]
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
    pub ident: String,
    pub term: Term<'a>,
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
            1 => write!(
                f,
                " :{} {}",
                self.insertions[0].ident, self.insertions[0].term
            )?,
            _ => {
                for ins in &self.insertions {
                    write!(f, "\n    :{} {}", ins.ident, ins.term)?;
                }
                write!(f, "\nend")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Display, Clone)]
pub enum Term<'a> {
    #[display(fmt = "({})", _0)]
    Expr(Items<'a>),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Nat(u64),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Int(i64),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Real(f64),
    #[display(fmt = "{}", "_0.to_string().bright_white()")]
    Ident(Ident<'a>),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "format!(\"{:?}\", _0).yellow()")]
    String(String),
    Closure(Box<Closure<'a>>),
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
}

#[derive(Debug, Clone)]
pub struct Closure<'a> {
    pub params: Params<'a>,
    pub body: Items<'a>,
}

impl<'a> fmt::Display for Closure<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} | {}", self.params, self.body)
    }
}
