use std::collections::{BTreeSet, HashMap, HashSet};

use crate::{ast::*, types::*};

#[derive(Debug, thiserror::Error, PartialEq, Eq, Hash)]
pub enum ResolutionError {
    #[error("Unknown type {:}", _0)]
    UnknownType(String),
    #[error("Unknown definition {:}", _0)]
    UnknownDef(String),
}

pub struct Resolver {
    scopes: Vec<Scope>,
    pub errors: HashSet<ResolutionError>,
}

impl Resolver {
    pub fn new() -> Resolver {
        let mut res = Resolver {
            scopes: vec![Scope::default()],
            errors: HashSet::new(),
        };
        res.push_type("nil", Variant::Nil.into());
        res.push_type("bool", Variant::Bool.into());
        res.push_type("nat", Variant::Nat.into());
        res.push_type("int", Variant::Int.into());
        res.push_type("real", Variant::Real.into());
        res.push_type("text", Variant::Text.into());
        res
    }
    pub fn find_type(&self, name: &str) -> Option<&Type> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.types.get(name))
            .map(|stack| stack.last().unwrap())
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
            .any(|scope| scope.defs.contains_key(name) || scope.param_defs.contains_key(name))
    }
    pub fn push_type<N>(&mut self, name: N, ty: Type)
    where
        N: Into<String>,
    {
        self.scopes
            .last_mut()
            .unwrap()
            .types
            .entry(name.into())
            .or_default()
            .push(ty);
    }
    pub fn push_def<N>(&mut self, name: N, def: Def)
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
    pub fn push_param_def<N>(&mut self, name: N, type_pair: TypePair)
    where
        N: Into<String>,
    {
        self.scopes
            .last_mut()
            .unwrap()
            .param_defs
            .insert(name.into(), type_pair);
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
pub struct Scope {
    pub types: HashMap<String, Vec<Type>>,
    pub defs: HashMap<String, Vec<Def>>,
    pub param_defs: HashMap<String, TypePair>,
}

pub trait Resolve {
    fn resolve(&mut self, res: &mut Resolver);
}

impl Resolve for TypePair {
    fn resolve(&mut self, res: &mut Resolver) {
        let mut variants: BTreeSet<Variant> = BTreeSet::new();
        for unresolved in &self.unresolved {
            if let Some(resolved) = res.find_type(&unresolved.ident).cloned() {
                variants.extend(resolved.variants);
            } else {
                res.errors
                    .insert(ResolutionError::UnknownType(unresolved.ident.clone()));
            }
        }
        self.resolved = Some(Type { variants });
    }
}

impl Resolve for Param {
    fn resolve(&mut self, res: &mut Resolver) {
        self.types.resolve(res)
    }
}

impl Resolve for Params {
    fn resolve(&mut self, res: &mut Resolver) {
        for param in &mut self.params {
            param.resolve(res);
        }
    }
}

impl Resolve for Items {
    fn resolve(&mut self, res: &mut Resolver) {
        for item in &mut self.items {
            item.resolve(res);
        }
    }
}

impl Resolve for Item {
    fn resolve(&mut self, res: &mut Resolver) {
        match self {
            Item::Expression(expr) => expr.resolve(res),
            Item::Def(def) => def.resolve(res),
        }
    }
}

impl Resolve for Def {
    fn resolve(&mut self, res: &mut Resolver) {
        self.ret.resolve(res);
        self.params.resolve(res);

        res.push_scope();
        for param in &self.params.params {
            res.push_param_def(param.ident.clone(), param.types.clone());
        }

        self.items.resolve(res);

        res.pop_scope();
        res.push_def(self.ident.clone(), self.clone());
    }
}

impl Resolve for ExprOr {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl Resolve for ExprAnd {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl Resolve for ExprIs {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        match &mut self.right {
            Some(IsRight::Expression(expr)) => expr.resolve(res),
            Some(IsRight::Pattern(param)) => param.resolve(res),
            _ => {}
        }
    }
}

impl Resolve for ExprCmp {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl Resolve for ExprAS {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl Resolve for ExprMDR {
    fn resolve(&mut self, res: &mut Resolver) {
        self.left.resolve(res);
        for right in &mut self.rights {
            right.expr.resolve(res);
        }
    }
}

impl Resolve for ExprNot {
    fn resolve(&mut self, res: &mut Resolver) {
        self.expr.resolve(res);
    }
}

impl Resolve for ExprCall {
    fn resolve(&mut self, res: &mut Resolver) {
        self.term.resolve(res);
        for arg in &mut self.args {
            arg.resolve(res);
        }
    }
}

impl Resolve for Term {
    fn resolve(&mut self, res: &mut Resolver) {
        match self {
            Term::Closure(closure) => {
                closure.params.resolve(res);
                closure.body.resolve(res);
            }
            Term::Expr(expr) => expr.resolve(res),
            Term::Ident(ident) if !res.def_exists(ident) => {
                res.errors
                    .insert(ResolutionError::UnknownDef(ident.clone()));
            }
            _ => {}
        }
    }
}
