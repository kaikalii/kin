use std::{fmt, io};

use pest::{
    error::{Error as PestError, ErrorVariant},
    Span,
};
use rpds::{HashTrieMap, List, Vector};

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

struct NootDef {
    is_function: bool,
    c_name: String,
}

#[derive(Clone)]
struct TranspileState {
    noot_scopes: Vector<HashTrieMap<String, NootDef>>,
}

impl TranspileState {
    pub fn new() -> Self {
        TranspileState {
            noot_scopes: Vector::new(),
        }
    }
    pub fn with_noot_def(&self, name: String, def: NootDef) -> Self {
        TranspileState {
            noot_scopes: self
                .noot_scopes
                .set(
                    self.noot_scopes.len() - 1,
                    self.noot_scopes.last().unwrap().insert(name, def),
                )
                .unwrap(),
        }
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
    fn c_name_for(&self, noot_name: &str, function: bool) -> String {
        todo!()
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
    let c_name = result.c_name_for(&def.ident.name, def.is_function());
    let state = state.with_noot_def(
        def.ident.name.clone(),
        NootDef {
            c_name: c_name.clone(),
            is_function: def.is_function(),
        },
    );
    if def.is_function() {
        // Function
    } else {
        // Value
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
