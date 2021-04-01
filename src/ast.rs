#![allow(clippy::upper_case_acronyms, dead_code)]

use std::fmt;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;

#[derive(Debug, Display, Clone, PartialEq)]
pub enum Item {
    Expression(Expression),
    Def(Def),
}

impl Node for Item {
    type Child = Expression;
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Item::Expression(expr) => expr.contains_ident(ident),
            Item::Def(def) => def.items.contains_ident(ident),
        }
    }
    fn terms(&self) -> usize {
        todo!()
    }
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
    fn contains_ident(&self, ident: &str) -> bool {
        self.items.iter().any(|item| item.contains_ident(ident))
    }
    fn terms(&self) -> usize {
        self.items.iter().map(Item::terms).sum()
    }
    fn wrapping(child: Self::Child) -> Self {
        Items { items: vec![child] }
    }
}

#[derive(Debug, Display, Clone, PartialEq)]
#[display(fmt = "{}", ident)]
pub struct Param {
    pub ident: String,
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
    pub params: Params,
    pub items: Items,
}

impl fmt::Display for Def {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} = ", self.ident, self.params)?;
        if self.items.items.len() == 1 {
            write!(f, "{}", self.items.items[0])?;
        } else {
            for item in &self.items.items {
                write!(f, "\n\t{}", item)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(bound = "O: fmt::Display, T: fmt::Display")]
#[display(
    fmt = "{}{}",
    "left",
    r#"rights.iter().map(|r| { format!(" {} {}", r.op, r.expr) }).collect::<String>()"#
)]
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

pub trait Node {
    type Child;
    fn contains_ident(&self, ident: &str) -> bool;
    fn terms(&self) -> usize;
    fn wrapping(child: Self::Child) -> Self;
}

impl<O, T> Node for BinExpr<O, T>
where
    T: Node,
{
    type Child = T;
    fn contains_ident(&self, ident: &str) -> bool {
        self.left.contains_ident(ident)
            || self
                .rights
                .iter()
                .any(|right| right.expr.contains_ident(ident))
    }
    fn terms(&self) -> usize {
        self.left.terms()
            + self
                .rights
                .iter()
                .map(|right| right.expr.terms())
                .sum::<usize>()
    }
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
    fn contains_ident(&self, ident: &str) -> bool {
        self.expr.contains_ident(ident)
    }
    fn terms(&self) -> usize {
        self.expr.terms()
    }
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
pub type ExprAnd = BinExpr<OpAnd, ExprCmp>;
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
        match &self.chained {
            Some(sep) => {
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
                )
            }
            None => {
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
                )
            }
        }
    }
}

impl Node for ExprCall {
    type Child = Term;
    fn contains_ident(&self, ident: &str) -> bool {
        self.term.contains_ident(ident) || self.args.iter().any(|expr| expr.contains_ident(ident))
    }
    fn terms(&self) -> usize {
        self.term.terms() + self.args.iter().map(|expr| expr.terms()).sum::<usize>()
    }
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
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Term::Expr(items) => items.contains_ident(ident),
            Term::Ident(p) => p == ident,
            Term::Closure(f) => f.contains_ident(ident),
            _ => false,
        }
    }
    fn terms(&self) -> usize {
        if let Term::Expr(items) = self {
            items.terms()
        } else {
            1
        }
    }
    fn wrapping(child: Self::Child) -> Self {
        Term::Expr(Box::new(child))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub params: Params,
    pub body: Items,
}

impl Closure {
    fn contains_ident(&self, ident: &str) -> bool {
        self.body
            .items
            .iter()
            .any(|items| items.contains_ident(ident))
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.params, self.body)
    }
}
