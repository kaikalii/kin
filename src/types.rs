use std::fmt;

use rpds::RedBlackTreeSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Function(TypeSet),
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
            Type::Any => "any".fmt(f),
            Type::Nil => "nil".fmt(f),
            Type::Bool => "bool".fmt(f),
            Type::Int => "int".fmt(f),
            Type::Real => "real".fmt(f),
            Type::String => "string".fmt(f),
            Type::List(inner) => write!(f, "list of {}", inner),
            Type::Function(ret) => ret.fmt(f),
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

type BinErrorMessageFn = fn(&dyn fmt::Display, &dyn fmt::Display) -> String;
type UnErrorMessageFn = fn(&dyn fmt::Display) -> String;

impl Type {
    pub fn bin_math_op(&self, other: &Self, error_message: BinErrorMessageFn) -> TypeResult {
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

impl From<Type> for TypeSet {
    fn from(ty: Type) -> Self {
        TypeSet {
            types: RedBlackTreeSet::new().insert(ty),
        }
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
    pub fn join<F>(&self, other: &Self, f: F, error_message: BinErrorMessageFn) -> TypeSetResult
    where
        F: Fn(&Type, &Type, BinErrorMessageFn) -> TypeResult,
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
    pub fn union(self, other: Self) -> Self {
        TypeSet {
            types: self
                .types
                .iter()
                .chain(other.types.iter())
                .cloned()
                .collect(),
        }
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
    pub fn compare(&self, other: &Self) -> TypeSetResult {
        self.join(
            other,
            |a, b, error| {
                if a == b {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError {
                        message: error(a, b),
                    })
                }
            },
            |a, b| format!("{} cannot be compared to {}", a, b),
        )
    }
    pub fn un_op(
        &self,
        f: fn(&Type, UnErrorMessageFn) -> TypeResult,
        error_message: UnErrorMessageFn,
    ) -> TypeSetResult {
        let type_results: Vec<_> = self
            .types
            .iter()
            .map(move |ty| f(ty, error_message))
            .collect();
        for res in &type_results {
            if let Err(e) = res {
                return Err(TypeError {
                    message: format!(
                        "Invalid operation: {} because {}",
                        error_message(&self),
                        e.message
                    ),
                });
            }
        }
        Ok(TypeSet {
            types: type_results.into_iter().map(Result::unwrap).collect(),
        })
    }
    pub fn negate(&self) -> TypeSetResult {
        self.un_op(
            |ty, error| {
                Ok(match ty {
                    Type::Int => Type::Int,
                    Type::Real => Type::Real,
                    ty => return Err(TypeError { message: error(ty) }),
                })
            },
            |ty| format!("{} cannot be negated", ty),
        )
    }
}
