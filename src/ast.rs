#![allow(clippy::upper_case_acronyms, dead_code)]

use std::fmt;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use pest::Span;

use crate::types::*;

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

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Item<'a> {
    Expression(Expression<'a>),
    Def(Def<'a>),
}

impl<'a> Node for Item<'a> {
    type Child = Expression<'a>;
    fn wrapping(child: Self::Child) -> Self {
        Item::Expression(child)
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(
    fmt = "{}",
    r#"items.iter().map(ToString::to_string).intersperse("\n\n".into()).collect::<String>()"#
)]
pub struct Items<'a> {
    pub items: Vec<Item<'a>>,
}

impl<'a> Node for Items<'a> {
    type Child = Item<'a>;
    fn wrapping(child: Self::Child) -> Self {
        Items { items: vec![child] }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param<'a> {
    pub ident: Ident<'a>,
    pub ty: Type<'a>,
}

impl<'a> fmt::Display for Param<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.ty.unresolved.is_empty() {
            write!(f, ":{}", self.ty)?;
        }
        Ok(())
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(
    fmt = "{}",
    r#"params.iter().map(ToString::to_string).intersperse(" ".into()).collect::<String>()"#
)]
pub struct Params<'a> {
    pub params: Vec<Param<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def<'a> {
    pub ident: Ident<'a>,
    pub ret: Type<'a>,
    pub params: Params<'a>,
    pub items: Items<'a>,
}

impl<'a> fmt::Display for Def<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.ret.unresolved.is_empty() {
            write!(f, ":{}", self.ret)?;
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinExpr<O, T> {
    pub left: Box<T>,
    pub rights: Vec<Right<O, T>>,
}

impl<O, T> BinExpr<O, T> {
    pub fn new(left: T, rights: Vec<Right<O, T>>) -> Self {
        BinExpr {
            left: Box::new(left),
            rights,
        }
    }
}

impl<O, T> fmt::Display for BinExpr<O, T>
where
    O: fmt::Display,
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.left)?;
        for right in &self.rights {
            write!(f, " {} {}", right.op, right.expr)?;
        }
        Ok(())
    }
}

pub trait Node {
    type Child;
    fn wrapping(child: Self::Child) -> Self;
}

impl<O, T> Node for BinExpr<O, T>
where
    T: Node,
{
    type Child = T;
    fn wrapping(child: Self::Child) -> Self {
        BinExpr::new(child, Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Right<O, T> {
    pub op: O,
    pub expr: T,
}

impl<O, T> Right<O, T> {
    pub fn new(op: O, expr: T) -> Self {
        Right { op, expr }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{}{}", r#"if op.is_some() { "not " } else { "" }"#, expr)]
pub struct UnExpr<O, T> {
    pub op: Option<O>,
    pub expr: T,
}

impl<O, T> Node for UnExpr<O, T>
where
    T: Node,
    O: Default,
{
    type Child = T;
    fn wrapping(child: Self::Child) -> Self {
        UnExpr {
            op: None,
            expr: child,
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "or")]
pub struct OpOr;
#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "and")]
pub struct OpAnd;
#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "not")]
pub struct OpNot;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpCmp {
    #[display(fmt = "is")]
    Is,
    #[display(fmt = "isnt")]
    Isnt,
    #[display(fmt = "<")]
    Less,
    #[display(fmt = ">")]
    Greater,
    #[display(fmt = "<=")]
    LessOrEqual,
    #[display(fmt = ">=")]
    GreaterOrEqual,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpAS {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpMDR {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
}

pub type Expression<'a> = ExprOr<'a>;
pub type ExprOr<'a> = BinExpr<OpOr, ExprAnd<'a>>;
pub type ExprAnd<'a> = BinExpr<OpAnd, ExprIs<'a>>;

#[derive(Debug, Display, Clone, PartialEq)]
pub enum IsRight<'a> {
    Pattern(Param<'a>),
    Expression(ExprCmp<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIs<'a> {
    pub left: ExprCmp<'a>,
    pub right: Option<IsRight<'a>>,
}

impl<'a> fmt::Display for ExprIs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.left)?;
        if let Some(right) = &self.right {
            write!(f, " is {}", right)?;
        }
        Ok(())
    }
}

impl<'a> Node for ExprIs<'a> {
    type Child = ExprCmp<'a>;
    fn wrapping(child: Self::Child) -> Self {
        ExprIs {
            left: child,
            right: None,
        }
    }
}

pub type ExprCmp<'a> = BinExpr<OpCmp, ExprAS<'a>>;
pub type ExprAS<'a> = BinExpr<OpAS, ExprMDR<'a>>;
pub type ExprMDR<'a> = BinExpr<OpMDR, ExprNot<'a>>;
pub type ExprNot<'a> = UnExpr<OpNot, ExprCall<'a>>;

fn _expression_size() {
    #[allow(invalid_value)]
    let _: [u8; 32] = unsafe { std::mem::transmute::<Expression, _>(std::mem::zeroed()) };
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall<'a> {
    pub term: Term<'a>,
    pub args: Vec<Term<'a>>,
    pub chained: Option<String>,
}

impl<'a> fmt::Display for ExprCall<'a> {
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
                    self.term,
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
                    self.term,
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

impl<'a> Node for ExprCall<'a> {
    type Child = Term<'a>;
    fn wrapping(child: Self::Child) -> Self {
        ExprCall {
            term: child,
            args: Vec::new(),
            chained: None,
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Term<'a> {
    #[display(fmt = "({})", _0)]
    Expr(Box<Items<'a>>),
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

impl<'a> Node for Term<'a> {
    type Child = Items<'a>;
    fn wrapping(child: Self::Child) -> Self {
        Term::Expr(Box::new(child))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure<'a> {
    pub params: Params<'a>,
    pub body: Items<'a>,
}

impl<'a> fmt::Display for Closure<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} | {}", self.params, self.body)
    }
}
