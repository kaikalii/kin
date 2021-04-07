use std::{fmt, io};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};
use rpds::{HashTrieMap, List};

use crate::{ast::*, parse::Rule};

#[derive(Debug, thiserror::Error)]
pub enum TranspileErrorKind {
    #[error("Unknown definition {}", _0)]
    UnknownDef(String),
}

impl TranspileErrorKind {
    pub fn span(self, span: Span) -> TranspileError {
        TranspileError { kind: self, span }
    }
}

#[derive(Debug)]
pub struct TranspileError<'a> {
    pub kind: TranspileErrorKind,
    pub span: Span<'a>,
}

impl<'a> fmt::Display for TranspileError<'a> {
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

#[derive(Clone)]
struct TranspileState {}

impl TranspileState {
    pub fn new() -> Self {
        TranspileState {}
    }
}

#[derive(Clone)]
pub struct Transpilation<'a> {
    functions: HashTrieMap<String, CFunction>,
    pub errors: List<TranspileError<'a>>,
}

impl<'a> Transpilation<'a> {
    pub fn new() -> Self {
        Transpilation {
            functions: Default::default(),
            errors: Default::default(),
        }
    }
    pub fn write(self) -> io::Result<()> {
        Ok(())
    }
}

struct CFunction {}

pub fn transpile(items: Items) -> Transpilation {
    transpile_items(items, TranspileState::new(), Transpilation::new())
}

fn transpile_items<'a>(
    items: Items<'a>,
    state: TranspileState,
    result: Transpilation<'a>,
) -> Transpilation<'a> {
    items.into_iter().fold(result, |result, item| {
        transpile_item(item, state.clone(), result)
    })
}

fn transpile_item<'a>(
    item: Item<'a>,
    state: TranspileState,
    result: Transpilation<'a>,
) -> Transpilation<'a> {
    match item {
        Item::Def(def) => transpile_def(def, state, result),
        Item::Node(node) => transpile_node(node, state, result),
    }
}

fn transpile_def<'a>(
    def: Def<'a>,
    state: TranspileState,
    result: Transpilation<'a>,
) -> Transpilation<'a> {
    if def.is_function() {
    } else {
    }
    todo!()
}

fn transpile_node<'a>(
    node: Node<'a>,
    state: TranspileState,
    result: Transpilation<'a>,
) -> Transpilation<'a> {
    todo!()
}
