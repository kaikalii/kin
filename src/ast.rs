#![allow(clippy::upper_case_acronyms, dead_code)]

use std::fmt;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;

use crate::types::Type;

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Item {
    Expression(Expression),
    Def(Def),
}

impl Node for Item {
    type Child = Expression;
    fn wrapping(child: Self::Child) -> Self {
        Item::Expression(child)
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(
    fmt = "{}",
    r#"items.iter().map(ToString::to_string).intersperse("\n\n".into()).collect::<String>()"#
)]
pub struct Items {
    pub items: Vec<Item>,
}

impl Node for Items {
    type Child = Item;
    fn wrapping(child: Self::Child) -> Self {
        Items { items: vec![child] }
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
pub struct UnresolvedType {
    pub ident: String,
}

#[derive(Debug, Display, Clone, PartialEq, Default)]
#[display(
    fmt = "{}",
    r#"unresolved.iter().map(ToString::to_string).intersperse(" ".into()).collect::<String>()"#
)]
pub struct TypePair {
    pub unresolved: Vec<UnresolvedType>,
    pub resolved: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub ident: String,
    pub types: TypePair,
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.types.unresolved.is_empty() {
            write!(f, ":{}", self.types)?;
        }
        Ok(())
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(
    fmt = "{}",
    r#"params.iter().map(ToString::to_string).intersperse(" ".into()).collect::<String>()"#
)]
pub struct Params {
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub ident: String,
    pub ret: TypePair,
    pub params: Params,
    pub items: Items,
}

impl fmt::Display for Def {
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

pub type Expression = ExprOr;
pub type ExprOr = BinExpr<OpOr, ExprAnd>;
pub type ExprAnd = BinExpr<OpAnd, ExprIs>;

#[derive(Debug, Display, Clone, PartialEq)]
pub enum IsRight {
    Pattern(Param),
    Expression(ExprCmp),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIs {
    pub left: ExprCmp,
    pub right: Option<IsRight>,
}

impl fmt::Display for ExprIs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.left)?;
        if let Some(right) = &self.right {
            write!(f, " is {}", right)?;
        }
        Ok(())
    }
}

impl Node for ExprIs {
    type Child = ExprCmp;
    fn wrapping(child: Self::Child) -> Self {
        ExprIs {
            left: child,
            right: None,
        }
    }
}

pub type ExprCmp = BinExpr<OpCmp, ExprAS>;
pub type ExprAS = BinExpr<OpAS, ExprMDR>;
pub type ExprMDR = BinExpr<OpMDR, ExprNot>;
pub type ExprNot = UnExpr<OpNot, ExprCall>;

fn _expression_size() {
    #[allow(invalid_value)]
    let _: [u8; 32] = unsafe { std::mem::transmute::<Expression, _>(std::mem::zeroed()) };
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCall {
    pub term: Term,
    pub args: Vec<Term>,
    pub chained: Option<String>,
}

impl fmt::Display for ExprCall {
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

impl Node for ExprCall {
    type Child = Term;
    fn wrapping(child: Self::Child) -> Self {
        ExprCall {
            term: child,
            args: Vec::new(),
            chained: None,
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Term {
    #[display(fmt = "({})", _0)]
    Expr(Box<Items>),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Nat(u64),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Int(i64),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Real(f64),
    #[display(fmt = "{}", "_0.bright_white()")]
    Ident(String),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "format!(\"{:?}\", _0).yellow()")]
    String(String),
    Closure(Box<Closure>),
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
}

impl Node for Term {
    type Child = Items;
    fn wrapping(child: Self::Child) -> Self {
        Term::Expr(Box::new(child))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub params: Params,
    pub body: Items,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} | {}", self.params, self.body)
    }
}
