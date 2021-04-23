#![allow(clippy::upper_case_acronyms)]

use std::{cell::UnsafeCell, collections::HashMap, fmt, rc::Rc};

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

macro_rules! debug_pair {
    ($pair:expr) => {
        #[cfg(feature = "debug")]
        println!("{:?}: {}", $pair.as_rule(), $pair.as_str());
    };
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
                bindings: UnsafeCell::new(HashMap::new()),
            };
            let mut state = ParseState {
                input,
                scopes: UnsafeCell::new(vec![default_scope]),
                errors: Vec::new(),
            };
            let items = state.items(only(pairs.next().unwrap()));
            if state.errors.is_empty() {
                Ok(items)
            } else {
                Err(state.errors)
            }
        }
        Err(e) => Err(vec![TranspileError::Parse(e)]),
    }
}

#[derive(Clone)]
enum Binding<'a> {
    Def(Rc<Def<'a>>),
    Param,
}

#[derive(Default)]
struct Scope<'a> {
    bindings: UnsafeCell<HashMap<&'a str, Binding<'a>>>,
}

impl<'a> Scope<'a> {
    fn bind_def(&self, def: Def<'a>) -> Rc<Def<'a>> {
        let def = Rc::new(def);
        unsafe { &mut *self.bindings.get() }.insert(def.ident.name, Binding::Def(def.clone()));
        def
    }
    fn bind_param(&self, name: &'a str) {
        unsafe { &mut *self.bindings.get() }.insert(name, Binding::Param);
    }
}

struct ParseState<'a> {
    input: &'a str,
    scopes: UnsafeCell<Vec<Scope<'a>>>,
    errors: Vec<TranspileError<'a>>,
}

impl<'a> ParseState<'a> {
    fn push_scope(&self) {
        unsafe { &mut *self.scopes.get() }.push(Scope::default());
    }
    fn pop_scope(&self) {
        unsafe { &mut *self.scopes.get() }.pop();
    }
    fn scope(&self) -> &Scope<'a> {
        unsafe { &*self.scopes.get() }.last().unwrap()
    }
    fn find_binding(&self, name: &str) -> Option<Binding<'a>> {
        unsafe { &*self.scopes.get() }
            .iter()
            .rev()
            .find_map(|scope| unsafe { &*scope.bindings.get() }.get(name).cloned())
    }
    fn span(&self, start: usize, end: usize) -> Span<'a> {
        Span::new(self.input, start, end).unwrap()
    }
    fn items(&mut self, pair: Pair<'a, Rule>) -> Items<'a> {
        debug_pair!(pair);
        let mut items = Vec::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::item => items.push(self.item(pair)),
                Rule::EOI => {}
                rule => unreachable!("{:?}", rule),
            }
        }
        items
    }
    fn item(&mut self, pair: Pair<'a, Rule>) -> Item<'a> {
        debug_pair!(pair);
        let pair = only(pair);
        match pair.as_rule() {
            Rule::expr => Item::Node(self.expr(pair)),
            Rule::def => Item::Def(self.def(pair)),
            rule => unreachable!("{:?}", rule),
        }
    }
    fn ident(&mut self, pair: Pair<'a, Rule>) -> Ident<'a> {
        Ident {
            name: pair.as_str(),
            span: pair.as_span(),
        }
    }
    fn param(&mut self, pair: Pair<'a, Rule>) -> Param<'a> {
        let mut pairs = pair.into_inner();
        let ident = self.ident(pairs.next().unwrap());
        Param { ident }
    }
    fn def(&mut self, pair: Pair<'a, Rule>) -> Rc<Def<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let ident = self.ident(pairs.next().unwrap());
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
            self.push_scope();
            for param in &params {
                self.scope().bind_param(param.ident.name);
            }
        }
        let pair = pairs.next().unwrap();
        let items = match pair.as_rule() {
            Rule::items => self.items(pair),
            Rule::expr => vec![Item::Node(self.expr(pair))],
            rule => unreachable!("{:?}", rule),
        };
        if is_function {
            self.pop_scope();
        }
        let def = Def {
            ident,
            params,
            items,
        };
        self.scope().bind_def(def)
    }
    fn expr(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
        let pair = only(pair);
        match pair.as_rule() {
            Rule::expr_or => self.expr_or(pair),
            rule => unreachable!("{:?}", rule),
        }
    }
    fn expr_or(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span));
        }
        left
    }
    fn expr_and(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span));
        }
        left
    }
    fn expr_cmp(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span));
        }
        left
    }
    fn expr_as(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span));
        }
        left
    }
    fn expr_mdr(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone(), op_span));
        }
        left
    }
    fn expr_not(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let first = pairs.next().unwrap();
        let op = match first.as_str() {
            "not" => Some(UnOp::Not),
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
            Node::UnExpr(UnExpr::new(inner, op))
        } else {
            inner
        }
    }
    fn expr_call(&mut self, pair: Pair<'a, Rule>) -> Node<'a> {
        debug_pair!(pair);
        let pairs = pair.into_inner();
        let mut calls = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::expr_call_single => {
                    let span = pair.as_span();
                    let mut pairs = pair.into_inner();
                    let caller = Node::Term(self.term(pairs.next().unwrap()));
                    calls.push(CallExpr {
                        caller: caller.into(),
                        args: pairs.map(|pair| Node::Term(self.term(pair))).collect(),
                        span,
                    });
                }
                rule => unreachable!("{:?}", rule),
            }
        }
        let mut calls = calls.into_iter();
        let first_call = calls.next().unwrap();
        let mut call_node = if first_call.args.is_empty() {
            *first_call.caller
        } else {
            Node::Call(first_call)
        };
        for mut chained_call in calls {
            chained_call.args.insert(0, call_node);
            call_node = Node::Call(chained_call);
        }
        call_node
    }
    fn term(&mut self, pair: Pair<'a, Rule>) -> Term<'a> {
        debug_pair!(pair);
        let pair = only(pair);
        match pair.as_rule() {
            Rule::int => match pair.as_str().parse::<i64>() {
                Ok(i) => Term::Int(i),
                Err(_) => {
                    self.errors
                        .push(TranspileError::InvalidLiteral(pair.as_span()));
                    Term::Int(0)
                }
            },
            Rule::real => match pair.as_str().parse::<f64>() {
                Ok(i) => Term::Real(i),
                Err(_) => {
                    self.errors
                        .push(TranspileError::InvalidLiteral(pair.as_span()));
                    Term::Real(0.0)
                }
            },
            Rule::nil => Term::Nil,
            Rule::bool_literal => Term::Bool(pair.as_str() == "true"),
            Rule::ident => {
                let ident = self.ident(pair);
                if self.find_binding(ident.name).is_none() {
                    self.errors.push(TranspileError::UnknownDef(ident.clone()))
                }
                Term::Ident(ident)
            }
            Rule::paren_expr => {
                let pair = only(pair);
                self.push_scope();
                let items = self.items(pair);
                self.pop_scope();
                Term::Expr(items)
            }
            Rule::string => {
                let string = self.string_literal(pair);
                Term::String(string)
            }
            Rule::closure => {
                let span = pair.as_span();
                let mut pairs = pair.into_inner();
                let params_pairs = pairs.next().unwrap().into_inner();
                let params: Vec<Param> = params_pairs.map(|pair| self.param(pair)).collect();
                let pair = pairs.next().unwrap();
                let body = match pair.as_rule() {
                    Rule::items => self.items(pair),
                    Rule::expr => vec![Item::Node(self.expr(pair))],
                    rule => unreachable!("{:?}", rule),
                };
                Term::Closure(Closure { span, params, body }.into())
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    fn string_literal(&mut self, pair: Pair<'a, Rule>) -> String {
        debug_pair!(pair);
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
