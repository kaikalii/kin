use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};

use crate::{ast::*, parse::Rule};

#[derive(Debug, thiserror::Error)]
pub enum ResolutionErrorKind {
    #[error("Unknown type {}", _0)]
    UnknownType(String),
    #[error("Unknown definition {}", _0)]
    UnknownDef(String),
}

impl ResolutionErrorKind {
    pub fn span(self, span: Span) -> ResolutionError {
        ResolutionError { kind: self, span }
    }
}

use ResolutionErrorKind::*;

#[derive(Debug)]
pub struct ResolutionError<'a> {
    pub kind: ResolutionErrorKind,
    pub span: Span<'a>,
}

impl<'a> fmt::Display for ResolutionError<'a> {
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

pub struct Resolver<'a> {
    scopes: Vec<Scope<'a>>,
    pub errors: Vec<ResolutionError<'a>>,
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        // let mut res =
        Resolver {
            scopes: vec![Scope::default()],
            errors: Vec::new(),
        }
        // ;
        // res
    }
    pub fn find_def(&self, name: &str) -> Option<&Def> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.defs.get(name))
            .map(|stack| stack.last().unwrap())
    }
    pub fn def_exists(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.defs.contains_key(name) || scope.param_defs.contains(name))
    }
    pub fn push_def<N>(&mut self, name: N, def: Def<'a>)
    where
        N: Into<String>,
    {
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .entry(name.into())
            .or_default()
            .push(def);
    }
    pub fn push_param_def<N>(&mut self, name: N)
    where
        N: Into<String>,
    {
        self.scopes
            .last_mut()
            .unwrap()
            .param_defs
            .insert(name.into());
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    #[track_caller]
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("No scope to pop");
    }
}

#[derive(Default)]
pub struct Scope<'a> {
    pub defs: HashMap<String, Vec<Def<'a>>>,
    pub param_defs: HashSet<String>,
}

pub trait Resolve<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>);
}

impl<'a> Resolve<'a> for Param<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        res.push_param_def(self.ident.name.clone());
    }
}

impl<'a> Resolve<'a> for Params<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        for param in &mut self.params {
            param.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for Items<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        for item in &mut self.items {
            item.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for Item<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        match self {
            Item::Expression(expr) => expr.resolve(res),
            Item::Def(def) => def.resolve(res),
        }
    }
}

impl<'a> Resolve<'a> for Def<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        // Push a scope for this def
        res.push_scope();
        // Resolve parameters and items
        self.params.resolve(res);
        self.items.resolve(res);
        // Pop the def's scope
        res.pop_scope();
        // Push the def into its enclosing scope
        res.push_def(self.ident.name.clone(), self.clone());
    }
}

impl<'a> Resolve<'a> for ExprOr<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for ExprAnd<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for ExprCmp<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for ExprAS<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for ExprMDR<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for ExprNot<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.expr.resolve(res);
    }
}

impl<'a> Resolve<'a> for ExprCall<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        self.term.resolve(res);
        for arg in &mut self.args {
            arg.resolve(res);
        }
    }
}

impl<'a> Resolve<'a> for Term<'a> {
    fn resolve(&mut self, res: &mut Resolver<'a>) {
        match self {
            Term::Closure(closure) => {
                res.push_scope();
                closure.params.resolve(res);
                closure.body.resolve(res);
                res.pop_scope();
            }
            Term::Expr(expr) => expr.resolve(res),
            Term::Ident(ident) if !res.def_exists(&ident.name) => res
                .errors
                .push(UnknownDef(ident.name.clone()).span(ident.span.clone())),
            _ => {}
        }
    }
}
