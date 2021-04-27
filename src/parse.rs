#![allow(clippy::upper_case_acronyms)]

use std::{collections::HashMap, fmt};

use itertools::Itertools;
use pest::{
    error::{Error as PestError, ErrorVariant},
    iterators::Pair,
    Parser, RuleType, Span,
};

use crate::ast::*;

#[derive(Debug)]
pub enum TranspileError<'a> {
    UnknownDef(Ident<'a>),
    Parse(PestError<Rule>),
    InvalidLiteral(Span<'a>),
    DefUnderscoreTerminus(Span<'a>),
    FunctionNamedUnderscore(Span<'a>),
    ReturnReferencesLocal(Span<'a>),
    ForbiddenRedefinition(Ident<'a>),
}

impl<'a> fmt::Display for TranspileError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TranspileError::UnknownDef(ident) => format_span(
                format!("Unknown def: {:?}", ident.name),
                ident.span.clone(),
                f,
            ),
            TranspileError::Parse(e) => e.fmt(f),
            TranspileError::InvalidLiteral(span) => format_span("Invalid literal", span.clone(), f),
            TranspileError::DefUnderscoreTerminus(span) => {
                format_span("Def names may not start or end with '_'", span.clone(), f)
            }
            TranspileError::FunctionNamedUnderscore(span) => {
                format_span("Function cannot be named '_'", span.clone(), f)
            }
            TranspileError::ReturnReferencesLocal(span) => {
                format_span("Return value references local value", span.clone(), f)
            }
            TranspileError::ForbiddenRedefinition(ident) => format_span(
                format!("{} cannot be redefined", ident.name),
                ident.span.clone(),
                f,
            ),
        }
    }
}

fn format_span(message: impl Into<String>, span: Span, f: &mut fmt::Formatter) -> fmt::Result {
    let error = PestError::<Rule>::new_from_span(
        ErrorVariant::CustomError {
            message: message.into(),
        },
        span.clone(),
    );
    write!(f, "{}", error)
}

fn only<R>(pair: Pair<R>) -> Pair<R>
where
    R: RuleType,
{
    pair.into_inner().next().unwrap()
}

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct NootParser;

pub fn parse(input: &str) -> Result<Items, Vec<TranspileError>> {
    match NootParser::parse(Rule::file, input) {
        Ok(mut pairs) => {
            let default_scope = Scope {
                bindings: crate::transpile::BUILTIN_FUNCTIONS
                    .iter()
                    .chain(crate::transpile::BUILTIN_VALUES)
                    .map(|&(name, _)| (name, Binding::Builtin))
                    .collect(),
            };
            let mut state = ParseState {
                input,
                scopes: vec![default_scope],
                errors: Vec::new(),
            };
            let items = state.items(only(pairs.next().unwrap()), false);
            if state.errors.is_empty() {
                Ok(items)
            } else {
                Err(state.errors)
            }
        }
        Err(e) => Err(vec![TranspileError::Parse(e)]),
    }
}

#[derive(Debug, Clone)]
enum Binding<'a> {
    Def(Def<'a>, Lifetime),
    Param(u8),
    Builtin,
    Unfinished(u8),
}

impl<'a> Binding<'a> {
    pub fn lifetime(&self) -> Lifetime {
        match self {
            Binding::Def(_, lt) => *lt,
            Binding::Param(depth) | Binding::Unfinished(depth) => Lifetime::new(*depth, *depth),
            Binding::Builtin => Lifetime::STATIC,
        }
    }
}

#[derive(Default)]
struct Scope<'a> {
    bindings: HashMap<&'a str, Binding<'a>>,
}

impl<'a> Scope<'a> {
    #[cfg(feature = "debug")]
    fn print_keys(&self) {
        println!(
            "{:#?}",
            unsafe { &*self.bindings.get() }.keys().collect::<Vec<_>>()
        );
    }
}

struct ParseState<'a> {
    input: &'a str,
    scopes: Vec<Scope<'a>>,
    errors: Vec<TranspileError<'a>>,
}

impl<'a> ParseState<'a> {
    fn push_scope(&mut self) {
        #[cfg(feature = "debug")]
        println!("push scope");
        self.scopes.push(Scope::default());
    }
    fn pop_scope(&mut self) {
        #[cfg(feature = "debug")]
        println!("pop scope");
        self.scopes.pop();
    }
    fn scope(&mut self) -> &mut Scope<'a> {
        self.scopes.last_mut().unwrap()
    }
    fn find_binding(&self, name: &str) -> Option<&Binding<'a>> {
        #[cfg(feature = "debug")]
        println!("lookup {:?}", name);
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(name))
    }
    fn span(&self, start: usize, end: usize) -> Span<'a> {
        Span::new(self.input, start, end).unwrap()
    }
    fn depth(&self) -> u8 {
        self.scopes.len() as u8
    }
    fn bind_def(&mut self, def: Def<'a>) {
        let depth = self.depth();
        let refs = def.items.last().unwrap().lifetime().refs;
        self.scope().bindings.insert(
            def.ident.name,
            Binding::Def(def, Lifetime::new(depth, refs)),
        );
    }
    fn bind_param(&mut self, name: &'a str) {
        let depth = self.depth() - 1;
        self.scope().bindings.insert(name, Binding::Param(depth));
    }
    fn bind_unfinished(&mut self, name: &'a str) {
        let depth = self.depth();
        self.scope()
            .bindings
            .insert(name, Binding::Unfinished(depth));
    }
    fn items(&mut self, pair: Pair<'a, Rule>, check_ref: bool) -> Items<'a> {
        let mut items = Vec::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::item => items.push(self.item(pair)),
                Rule::EOI => {}
                rule => unreachable!("{:?}", rule),
            }
        }
        if check_ref {
            if let Item::Node(node) = items.last().unwrap() {
                if node.lifetime.refs == self.depth() {
                    self.errors.push(TranspileError::ReturnReferencesLocal(
                        node.kind.span().clone(),
                    ))
                }
            }
        }
        items
    }
    fn item(&mut self, pair: Pair<'a, Rule>) -> Item<'a> {
        let pair = only(pair);
        match pair.as_rule() {
            Rule::expr => Item::Node(self.expr(pair)),
            Rule::def => self.def(pair),
            rule => unreachable!("{:?}", rule),
        }
    }
    fn ident(&mut self, pair: Pair<'a, Rule>) -> Ident<'a> {
        let name = pair.as_str();
        let span = pair.as_span();
        if (name.starts_with('_') || name.ends_with('_')) && name != "_" {
            self.errors
                .push(TranspileError::DefUnderscoreTerminus(span.clone()));
        }
        Ident { name, span }
    }
    fn bound_ident(&mut self, pair: Pair<'a, Rule>) -> Ident<'a> {
        let ident = self.ident(pair);
        if ["nil", "true", "false"].contains(&ident.name) {
            self.errors
                .push(TranspileError::ForbiddenRedefinition(ident.clone()));
        }
        ident
    }
    fn param(&mut self, pair: Pair<'a, Rule>) -> Param<'a> {
        let mut pairs = pair.into_inner();
        let ident = self.bound_ident(pairs.next().unwrap());
        Param { ident }
    }
    fn def(&mut self, pair: Pair<'a, Rule>) -> Item<'a> {
        let mut pairs = pair.into_inner();
        let ident = self.bound_ident(pairs.next().unwrap());
        let mut params = Vec::new();
        for pair in pairs.by_ref() {
            if let Rule::param = pair.as_rule() {
                params.push(self.param(pair));
            } else {
                break;
            }
        }
        let is_function = !params.is_empty();
        if is_function {
            if ident.is_underscore() {
                self.errors
                    .push(TranspileError::FunctionNamedUnderscore(ident.span.clone()));
            }
            self.bind_unfinished(ident.name);
            self.push_scope();
            for param in &params {
                self.bind_param(param.ident.name);
            }
        }
        let pair = pairs.next().unwrap();
        let items_span = pair.as_span();
        let items = self.function_body(pair, is_function);
        if is_function {
            self.pop_scope();
        } else if ident.is_underscore() {
            let refs = items.last().unwrap().lifetime().refs;
            return Item::Node(
                NodeKind::Term(Term::Expr(items), items_span).life(self.depth(), refs),
            );
        }
        let def = Def {
            ident,
            params,
            items,
        };
        self.bind_def(def.clone());
        Item::Def(def)
    }
    fn expr(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let pair = only(pair);
        match pair.as_rule() {
            Rule::expr_or => self.expr_or(pair),
            rule => unreachable!("{:?}", rule),
        }
    }
    fn expr_or(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_and(left);
        for (op, right) in pairs.tuples() {
            let op_span = op.as_span();
            let op = match op.as_str() {
                "or" => BinOp::Or,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_and(right);
            let refs = left.lifetime.refs.max(right.lifetime.refs);
            left = NodeKind::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span))
                .life(self.depth(), refs);
        }
        left
    }
    fn expr_and(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_cmp(left);
        for (op, right) in pairs.tuples() {
            let op_span = op.as_span();
            let op = match op.as_str() {
                "and" => BinOp::And,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_cmp(right);
            let refs = left.lifetime.refs.max(right.lifetime.refs);
            left = NodeKind::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span))
                .life(self.depth(), refs);
        }
        left
    }
    fn expr_cmp(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_as(left);
        for (op, right) in pairs.tuples() {
            let op_span = op.as_span();
            let op = match op.as_str() {
                "==" => BinOp::Equals,
                "!=" => BinOp::NotEquals,
                "<=" => BinOp::LessOrEqual,
                ">=" => BinOp::GreaterOrEqual,
                "<" => BinOp::Less,
                ">" => BinOp::Greater,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_as(right);
            left = NodeKind::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span))
                .life(self.depth(), 0);
        }
        left
    }
    fn expr_as(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_mdr(left);
        for (op, right) in pairs.tuples() {
            let op_span = op.as_span();
            let op = match op.as_str() {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_mdr(right);
            left = NodeKind::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span))
                .life(self.depth(), 0);
        }
        left
    }
    fn expr_mdr(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_not(left);
        for (op, right) in pairs.tuples() {
            let op_span = op.as_span();
            let op = match op.as_str() {
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Rem,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_not(right);
            left = NodeKind::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span))
                .life(self.depth(), 0);
        }
        left
    }
    fn expr_not(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let first = pairs.next().unwrap();
        let op = match first.as_str() {
            "-" => Some(UnOp::Neg),
            _ => None,
        };
        let inner = if op.is_some() {
            pairs.next().unwrap()
        } else {
            first
        };
        let inner = self.expr_call(inner);
        if let Some(op) = op {
            NodeKind::UnExpr(UnExpr::new(inner, op, span)).life(self.depth(), 0)
        } else {
            inner
        }
    }
    fn expr_call(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let pairs = pair.into_inner();
        let mut calls = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::expr_call_single => {
                    let span = pair.as_span();
                    let mut pairs = pair.into_inner();
                    let caller = self.expr_push(pairs.next().unwrap());
                    calls.push(CallExpr {
                        caller: caller.into(),
                        args: pairs.map(|pair| self.expr_push(pair)).collect(),
                        span,
                    });
                }
                rule => unreachable!("{:?}", rule),
            }
        }
        let mut calls = calls.into_iter();
        let first_call = calls.next().unwrap();
        let mut refs = first_call
            .args
            .iter()
            .map(|node| node.lifetime.refs)
            .max()
            .unwrap_or(first_call.caller.lifetime.refs);
        let mut call_node = if first_call.args.is_empty() {
            *first_call.caller
        } else {
            NodeKind::Call(first_call).life(self.depth(), refs)
        };
        for mut chained_call in calls {
            refs = chained_call
                .args
                .iter()
                .map(|node| node.lifetime.refs)
                .max()
                .unwrap_or(refs);
            chained_call.args.insert(0, call_node);
            call_node = NodeKind::Call(chained_call).life(self.depth(), refs);
        }
        call_node
    }
    fn expr_push(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let head = self.term(pairs.next().unwrap());
        if let Some(pair) = pairs.next() {
            let tail = self.expr_push(pair);
            let refs = head.lifetime.depth.max(tail.lifetime.depth);
            NodeKind::Push(PushExpr {
                head: head.into(),
                tail: tail.into(),
                span,
            })
            .life(self.depth(), refs)
        } else {
            head
        }
    }
    fn term(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        let span = pair.as_span();
        let pair = only(pair);
        let (term, lifetime) = match pair.as_rule() {
            Rule::int => match pair.as_str().parse::<i64>() {
                Ok(i) => (Term::Int(i), Lifetime::STATIC),
                Err(_) => {
                    self.errors
                        .push(TranspileError::InvalidLiteral(pair.as_span()));
                    (Term::Int(0), Lifetime::STATIC)
                }
            },
            Rule::real => match pair.as_str().parse::<f64>() {
                Ok(i) => (Term::Real(i), Lifetime::STATIC),
                Err(_) => {
                    self.errors
                        .push(TranspileError::InvalidLiteral(pair.as_span()));
                    (Term::Real(0.0), Lifetime::STATIC)
                }
            },
            Rule::ident => {
                let ident = self.ident(pair);
                let lifetime = if let Some(binding) = self.find_binding(ident.name) {
                    binding.lifetime()
                } else {
                    self.errors.push(TranspileError::UnknownDef(ident.clone()));
                    Lifetime::STATIC
                };
                (Term::Ident(ident), lifetime)
            }
            Rule::paren_expr => {
                let pair = only(pair);
                self.push_scope();
                let items = self.items(pair, true);
                self.pop_scope();
                let lifetime = Lifetime::new(self.depth(), items.last().unwrap().lifetime().refs);
                (Term::Expr(items), lifetime)
            }
            Rule::string => {
                let string = self.string_literal(pair);
                (Term::String(string), Lifetime::STATIC)
            }
            Rule::closure => {
                let span = pair.as_span();
                let mut pairs = pair.into_inner();
                let params_pairs = pairs.next().unwrap().into_inner();
                let params: Vec<Param> = params_pairs.map(|pair| self.param(pair)).collect();
                self.push_scope();
                for param in &params {
                    self.bind_param(param.ident.name);
                }
                let pair = pairs.next().unwrap();
                let body = self.function_body(pair, true);
                self.pop_scope();
                let lifetime = Lifetime::new(self.depth(), body.last().unwrap().lifetime().refs);
                (
                    Term::Closure(Closure { span, params, body }.into()),
                    lifetime,
                )
            }
            Rule::list_literal => {
                let list: Vec<Node> = pair.into_inner().map(|pair| self.term(pair)).collect();
                let lifetime = match list.len() {
                    0 => Lifetime::STATIC,
                    1 => list.last().unwrap().lifetime,
                    _ => Lifetime::new(self.depth(), self.depth()),
                };
                (Term::List(list), lifetime)
            }
            Rule::tree_literal => {
                let mut pairs = pair.into_inner();
                let left = self.term(pairs.next().unwrap());
                let middle = self.term(pairs.next().unwrap());
                let right = self.term(pairs.next().unwrap());
                let refs = left
                    .lifetime
                    .depth
                    .max(middle.lifetime.depth)
                    .max(right.lifetime.depth);
                (
                    Term::Tree(Box::new([left, middle, right])),
                    Lifetime::new(self.depth(), refs),
                )
            }
            rule => unreachable!("{:?}", rule),
        };
        NodeKind::Term(term, span).life(lifetime.depth, lifetime.refs)
    }
    fn function_body(&mut self, pair: Pair<'a, Rule>, check_ref: bool) -> Items<'a> {
        match pair.as_rule() {
            Rule::items => self.items(pair, check_ref),
            Rule::expr => {
                let node = self.expr(pair);
                if check_ref && node.lifetime.refs >= self.depth() {
                    self.errors.push(TranspileError::ReturnReferencesLocal(
                        node.kind.span().clone(),
                    ))
                }
                vec![Item::Node(node)]
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    fn string_literal(&mut self, pair: Pair<'a, Rule>) -> String {
        let mut s = String::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::raw_string => s.push_str(pair.as_str()),
                Rule::predefined => s.push(match pair.as_str() {
                    "0" => '\0',
                    "r" => '\r',
                    "t" => '\t',
                    "n" => '\n',
                    "\\" => '\\',
                    "'" => '\'',
                    "\"" => '"',
                    s => unreachable!("{}", s),
                }),
                Rule::byte => {
                    let byte = pair
                        .into_inner()
                        .map(|pair| pair.as_str())
                        .collect::<String>()
                        .parse::<u8>()
                        .unwrap();
                    s.push(byte as char);
                }
                Rule::unicode => {
                    let u = pair
                        .into_inner()
                        .map(|pair| pair.as_str())
                        .collect::<String>()
                        .parse::<u32>()
                        .unwrap();
                    s.push(
                        std::char::from_u32(u).unwrap_or_else(|| panic!("invalid unicode {}", u)),
                    );
                }
                rule => unreachable!("{:?}", rule),
            }
        }
        s
    }
}
