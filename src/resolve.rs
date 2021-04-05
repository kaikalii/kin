use std::{collections::HashMap, fmt};

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

#[derive(Default)]
pub struct Scope<'a> {
    pub defs: HashMap<String, Vec<CompileDef<'a>>>,
}

#[derive(Debug, Clone)]
pub enum CompileDef<'a> {
    Noot(Def<'a>),
    C(&'static str),
    Param(usize),
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        let mut res = Resolver {
            scopes: vec![Scope::default()],
            errors: Vec::new(),
        };

        res.push_def("print", CompileDef::C("noot_print"));

        res
    }
    pub fn find_def(&self, name: &str) -> Option<&CompileDef> {
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
            .any(|scope| scope.defs.contains_key(name))
    }
    pub fn push_def<N>(&mut self, name: N, def: CompileDef<'a>)
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
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    #[track_caller]
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("No scope to pop");
    }
}
