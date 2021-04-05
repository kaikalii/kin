use std::{collections::HashMap, fmt};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};

use crate::{ast::*, parse::Rule};

#[derive(Debug, thiserror::Error)]
pub enum ResolutionErrorKind {
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
    Noot { name: String, def: Def<'a> },
    C(&'static str),
    Param(usize),
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        let mut res = Resolver {
            scopes: vec![Scope::default()],
            errors: Vec::new(),
        };

        res.push_c_def("print", "noot_print");

        res
    }
    pub fn find_def(&self, name: &str) -> Option<&CompileDef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.defs.get(name))
            .map(|stack| stack.last().unwrap())
    }
    pub fn c_name_exists(&self, c_name: &str) -> bool {
        self.scopes.iter().any(|scope| {
            scope.defs.values().flatten().any(|compdef| match compdef {
                CompileDef::Noot { name, .. } => name == c_name,
                CompileDef::C(name) => *name == c_name,
                CompileDef::Param(_) => false,
            })
        })
    }
    pub fn push_c_def(&mut self, name: &'static str, c_name: &'static str) {
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .entry(name.into())
            .or_default()
            .push(CompileDef::C(c_name));
    }
    pub fn c_name_for(&self, name: &str) -> String {
        if self.c_name_exists(&name) {
            let mut i = 2;
            loop {
                let potential = format!("{}{}", name, i);
                if !self.c_name_exists(&potential) {
                    break potential;
                }
                i += 1;
            }
        } else {
            name.into()
        }
    }
    pub fn push_noot_def<N>(&mut self, name: N, def: Def<'a>)
    where
        N: Into<String>,
    {
        let name = name.into();
        let c_name = self.c_name_for(&name);
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .entry(name)
            .or_default()
            .push(CompileDef::Noot { name: c_name, def });
    }
    pub fn push_param_def<N>(&mut self, name: N, index: usize)
    where
        N: Into<String>,
    {
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .entry(name.into())
            .or_default()
            .push(CompileDef::Param(index));
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    #[track_caller]
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("No scope to pop");
    }
}
