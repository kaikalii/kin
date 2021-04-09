use std::fmt;

use rpds::{RedBlackTreeSet, Vector};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Function(Vector<TypeSet>, TypeSet),
    List(TypeSet),
    String,
    Int,
    Real,
    Bool,
    Err(TypeSet),
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nil => "nil".fmt(f),
            Type::Bool => "bool".fmt(f),
            Type::Int => "int".fmt(f),
            Type::Real => "real".fmt(f),
            Type::String => "string".fmt(f),
            Type::List(inner) => write!(f, "list of {}", inner),
            Type::Function(params, ret) => {
                for param in params {
                    write!(f, "{} -> ", param)?;
                }
                write!(f, "{}", ret)
            }
            Type::Err(inner) => write!(f, "err({})", inner),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct TypeSet {
    types: RedBlackTreeSet<Type>,
}

impl fmt::Display for TypeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, ty) in self.types.iter().enumerate() {
            if i > 0 {
                '|'.fmt(f)?;
            }
            ty.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{}", message)]
pub struct TypeError {
    message: String,
}

pub type TypeResult = Result<Type, TypeError>;
pub type TypeSetResult = Result<TypeSet, TypeError>;

type ErrorMessageFn = fn(&dyn fmt::Display, &dyn fmt::Display) -> String;

impl Type {
    pub fn bin_math_op(&self, other: &Self, error_message: ErrorMessageFn) -> TypeResult {
        Ok(match (self, other) {
            (Type::Int, Type::Int) => Type::Int,
            (Type::Int, Type::Real) | (Type::Real, Type::Int) | (Type::Real, Type::Real) => {
                Type::Real
            }
            (a, b) => {
                return Err(TypeError {
                    message: error_message(a, b),
                })
            }
        })
    }
}

impl TypeSet {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn with(self, ty: Type) -> Self {
        TypeSet {
            types: self.types.insert(ty),
        }
    }
    pub fn join<F>(&self, other: &Self, f: F, error_message: ErrorMessageFn) -> TypeSetResult
    where
        F: Fn(&Type, &Type, ErrorMessageFn) -> TypeResult,
    {
        let f = &f;
        let type_results: Vec<_> = self
            .types
            .iter()
            .flat_map(|a| other.types.iter().map(move |b| f(a, b, error_message)))
            .collect();
        for res in &type_results {
            if let Err(e) = res {
                return Err(TypeError {
                    message: format!(
                        "Incompatible types: {} because {}",
                        error_message(&self, &other),
                        e.message
                    ),
                });
            }
        }
        Ok(TypeSet {
            types: type_results.into_iter().map(Result::unwrap).collect(),
        })
    }
    pub fn add(&self, other: &Self) -> TypeSetResult {
        self.join(other, Type::bin_math_op, |a, b| {
            format!("{} cannot be added to {}", b, a)
        })
    }
    pub fn sub(&self, other: &Self) -> TypeSetResult {
        self.join(other, Type::bin_math_op, |a, b| {
            format!("{} cannot be subtracted from {}", b, a)
        })
    }
    pub fn mul(&self, other: &Self) -> TypeSetResult {
        self.join(other, Type::bin_math_op, |a, b| {
            format!("{} cannot be multiplied by {}", b, a)
        })
    }
    pub fn div(&self, other: &Self) -> TypeSetResult {
        self.join(other, Type::bin_math_op, |a, b| {
            format!("{} cannot be divided by {}", b, a)
        })
    }
}
