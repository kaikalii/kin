use std::{collections::BTreeSet, fmt};

use crate::ast::Ident;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Variant {
    Nil,
    Bool,
    Nat,
    Int,
    Real,
    Text,
    Err(Box<Self>),
    Function(Signature),
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Variant::Nil => write!(f, "nil"),
            Variant::Bool => write!(f, "bool"),
            Variant::Nat => write!(f, "nat"),
            Variant::Int => write!(f, "int"),
            Variant::Real => write!(f, "real"),
            Variant::Text => write!(f, "text"),
            Variant::Err(inner) => write!(f, "err({})", inner),
            Variant::Function(sig) => write!(f, "{}", sig),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConcreteType {
    pub variants: BTreeSet<Variant>,
}

impl From<Variant> for ConcreteType {
    fn from(variant: Variant) -> Self {
        let mut variants = BTreeSet::new();
        variants.insert(variant);
        ConcreteType { variants }
    }
}

impl fmt::Display for ConcreteType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, variant) in self.variants.iter().enumerate() {
            if i != 0 {
                write!(f, "/")?;
            }
            write!(f, "{}", variant)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Signature {
    pub params: Vec<ConcreteType>,
    pub ret: ConcreteType,
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, " -> {}", self.ret)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnresolvedVariant<'a> {
    Ident(Ident<'a>),
    Nil,
}

impl<'a> fmt::Display for UnresolvedVariant<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnresolvedVariant::Ident(ident) => write!(f, "{}", ident),
            UnresolvedVariant::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type<'a> {
    pub unresolved: Vec<UnresolvedVariant<'a>>,
    pub resolved: ResolvedType,
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, variant) in self.unresolved.iter().enumerate() {
            if i != 0 {
                write!(f, "/")?;
            }
            write!(f, "{}", variant)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    Resolved(ConcreteType),
    Error,
    Unresolved,
}
