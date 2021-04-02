use std::{collections::BTreeSet, env::var};

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type {
    pub variants: BTreeSet<Variant>,
}

impl From<Variant> for Type {
    fn from(variant: Variant) -> Self {
        let mut variants = BTreeSet::new();
        variants.insert(variant);
        Type { variants }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Signature {
    pub params: Vec<Type>,
    pub ret: Type,
}
