#![allow(clippy::upper_case_acronyms)]

use itertools::Itertools;
use pest::{
    error::{Error as PestError, ErrorVariant},
    iterators::Pair,
    Parser, RuleType, Span,
};

use crate::ast::*;

pub type ParseResult<T> = Result<T, PestError<Rule>>;

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

pub fn parse(input: &str) -> ParseResult<Nodes> {
    match NootParser::parse(Rule::file, input) {
        Ok(mut pairs) => ParseState { input }.nodes(only(pairs.next().unwrap())),
        Err(e) => Err(e),
    }
}

struct ParseState<'a> {
    input: &'a str,
}

impl<'a> ParseState<'a> {
    fn span(&self, start: usize, end: usize) -> Span<'a> {
        Span::new(self.input, start, end).unwrap()
    }
    fn nodes(&self, pair: Pair<'a, Rule>) -> ParseResult<Nodes<'a>> {
        debug_pair!(pair);
        let mut nodes = Vec::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::node => nodes.push(self.node(pair)?),
                Rule::EOI => {}
                rule => unreachable!("{:?}", rule),
            }
        }
        Ok(nodes)
    }
    fn node(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let pair = only(pair);
        Ok(match pair.as_rule() {
            Rule::expr => self.expr(pair)?,
            Rule::def => Node::Def(self.def(pair)?),
            rule => unreachable!("{:?}", rule),
        })
    }
    fn ident(&self, pair: Pair<'a, Rule>) -> Ident<'a> {
        Ident {
            name: pair.as_str().into(),
            span: pair.as_span(),
        }
    }
    fn def(&self, pair: Pair<'a, Rule>) -> ParseResult<Def<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let ident = self.ident(pairs.next().unwrap());
        let mut params = Vec::new();
        for pair in pairs.by_ref() {
            if let Rule::param = pair.as_rule() {
                params.push(self.ident(pair));
            } else {
                break;
            }
        }
        let pair = pairs.next().unwrap();
        let nodes = match pair.as_rule() {
            Rule::nodes => self.nodes(pair)?,
            Rule::expr => vec![self.expr(pair)?],
            rule => unreachable!("{:?}", rule),
        };
        Ok(Def {
            ident,
            params,
            nodes,
        })
    }
    fn expr(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let pair = only(pair);
        Ok(match pair.as_rule() {
            Rule::expr_or => self.expr_or(pair)?,
            rule => unreachable!("{:?}", rule),
        })
    }
    fn expr_or(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_and(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "or" => BinOp::Or,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_and(right)?;
            left = Node::BinExpr(
                BinExpr {
                    left,
                    right,
                    op,
                    span: span.clone(),
                }
                .into(),
            );
        }
        Ok(left)
    }
    fn expr_and(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_cmp(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "and" => BinOp::And,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_cmp(right)?;
            left = Node::BinExpr(
                BinExpr {
                    left,
                    right,
                    op,
                    span: span.clone(),
                }
                .into(),
            );
        }
        Ok(left)
    }
    fn expr_cmp(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_as(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "is" => BinOp::Is,
                "isnt" => BinOp::Isnt,
                "<=" => BinOp::LessOrEqual,
                ">=" => BinOp::GreaterOrEqual,
                "<" => BinOp::Less,
                ">" => BinOp::Greater,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_as(right)?;
            left = Node::BinExpr(
                BinExpr {
                    left,
                    right,
                    op,
                    span: span.clone(),
                }
                .into(),
            );
        }
        Ok(left)
    }
    fn expr_as(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_mdr(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_mdr(right)?;
            left = Node::BinExpr(
                BinExpr {
                    left,
                    right,
                    op,
                    span: span.clone(),
                }
                .into(),
            );
        }
        Ok(left)
    }
    fn expr_mdr(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_un(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Rem,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_un(right)?;
            left = Node::BinExpr(
                BinExpr {
                    left,
                    right,
                    op,
                    span: span.clone(),
                }
                .into(),
            );
        }
        Ok(left)
    }
    fn expr_un(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
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
        let inner = self.expr_call(inner)?;
        Ok(if let Some(op) = op {
            Node::UnExpr(UnExpr { inner, op }.into())
        } else {
            inner
        })
    }
    fn expr_call(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let pairs = pair.into_inner();
        let mut calls = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::expr_call_single => {
                    let span = pair.as_span();
                    let mut pairs = pair.into_inner();
                    let expr = self.term(pairs.next().unwrap())?;
                    let mut args = Vec::new();
                    for pair in pairs {
                        let arg = self.term(pair)?;
                        args.push(arg);
                    }
                    calls.push(CallExpr { expr, args, span });
                }
                Rule::chain_call => {}
                rule => unreachable!("{:?}", rule),
            }
        }
        let mut calls = calls.into_iter();
        let first_call = calls.next().unwrap();
        let mut call_node = if first_call.args.is_empty() {
            first_call.expr
        } else {
            Node::Call(first_call.into())
        };
        for mut chained_call in calls {
            chained_call.args.insert(0, call_node);
            call_node = Node::Call(chained_call.into());
        }
        Ok(call_node)
    }
    fn term(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let pair = only(pair);
        macro_rules! number_literal {
            ($term:ident) => {
                pair.as_str().parse().map(Node::$term).map_err(|_| {
                    PestError::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "Invalid {:?} literal \"{}\"",
                                pair.as_rule(),
                                pair.as_str(),
                            ),
                        },
                        pair.as_span(),
                    )
                })
            };
        }
        Ok(match pair.as_rule() {
            Rule::int => number_literal!(Int)?,
            Rule::real => number_literal!(Real)?,
            Rule::nil => Node::Nil,
            Rule::bool_literal => Node::Bool(pair.as_str() == "true"),
            Rule::ident => Node::Ident(self.ident(pair)),
            Rule::paren_expr => {
                let pair = only(pair);
                let nodes = self.nodes(pair)?;
                Node::Expr(nodes)
            }
            Rule::string => {
                let string = self.string_literal(pair);
                Node::String(string)
            }
            Rule::closure => {
                let span = pair.as_span();
                let mut pairs = pair.into_inner();
                pairs.next();
                let mut params = Vec::new();
                for pair in pairs.by_ref() {
                    if let Rule::param = pair.as_rule() {
                        params.push(self.ident(pair));
                    } else {
                        break;
                    }
                }
                let pair = pairs.next().unwrap();
                let body = match pair.as_rule() {
                    Rule::nodes => self.nodes(pair)?,
                    Rule::expr => vec![self.expr(pair)?],
                    rule => unreachable!("{:?}", rule),
                };
                Node::Closure(Closure { span, params, body }.into())
            }
            rule => unreachable!("{:?}", rule),
        })
    }
    fn string_literal(&self, pair: Pair<'a, Rule>) -> String {
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
