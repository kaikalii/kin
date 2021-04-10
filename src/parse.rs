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

pub fn parse(input: &str) -> ParseResult<Items> {
    match NootParser::parse(Rule::file, input) {
        Ok(mut pairs) => ParseState { input }.items(only(pairs.next().unwrap())),
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
    fn items(&self, pair: Pair<'a, Rule>) -> ParseResult<Items<'a>> {
        debug_pair!(pair);
        let mut items = Vec::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::item => items.push(self.item(pair)?),
                Rule::EOI => {}
                rule => unreachable!("{:?}", rule),
            }
        }
        Ok(items)
    }

    fn item(&self, pair: Pair<'a, Rule>) -> ParseResult<Item<'a>> {
        debug_pair!(pair);
        let pair = only(pair);
        Ok(match pair.as_rule() {
            Rule::expr => Item::Node(self.expr(pair)?),
            Rule::def => Item::Def(self.def(pair)?),
            rule => unreachable!("{:?}", rule),
        })
    }

    fn ident(&self, pair: Pair<'a, Rule>) -> Ident<'a> {
        Ident {
            name: pair.as_str().into(),
            span: pair.as_span(),
        }
    }

    fn param(&self, pair: Pair<'a, Rule>) -> Param<'a> {
        let mut pairs = pair.into_inner();
        let ident = self.ident(pairs.next().unwrap());
        Param { ident }
    }

    fn def(&self, pair: Pair<'a, Rule>) -> ParseResult<Def<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let Param { ident } = self.param(pairs.next().unwrap());
        let mut params = Vec::new();
        for pair in pairs.by_ref() {
            if let Rule::param = pair.as_rule() {
                params.push(self.param(pair));
            } else {
                break;
            }
        }
        let pair = pairs.next().unwrap();
        let items = match pair.as_rule() {
            Rule::items => self.items(pair)?,
            Rule::expr => vec![Item::Node(self.expr(pair)?)],
            rule => unreachable!("{:?}", rule),
        };
        Ok(Def {
            ident,
            params,
            items,
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone()));
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone()));
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone()));
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
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone()));
        }
        Ok(left)
    }

    fn expr_mdr(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let left = pairs.next().unwrap();
        let mut span = left.as_span();
        let mut left = self.expr_not(left)?;
        for (op, right) in pairs.tuples() {
            let op = match op.as_str() {
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Rem,
                rule => unreachable!("{:?}", rule),
            };
            span = self.span(span.start(), right.as_span().end());
            let right = self.expr_not(right)?;
            left = Node::BinExpr(BinExpr::new(left, right, op, span.clone()));
        }
        Ok(left)
    }

    fn expr_not(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
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
            Node::UnExpr(UnExpr::new(inner, op))
        } else {
            inner
        })
    }

    fn expr_call(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let pairs = pair.into_inner();
        let mut calls = Vec::new();
        let mut chained = None;
        for pair in pairs {
            match pair.as_rule() {
                Rule::expr_call_single => {
                    let mut pairs = pair.into_inner();
                    let expr = self.expr_insert(pairs.next().unwrap())?;
                    let mut args = Vec::new();
                    for pair in pairs {
                        let arg = self.expr_insert(pair)?;
                        args.push(arg);
                    }
                    calls.push(CallExpr {
                        expr: expr.into(),
                        args,
                        chained: chained.take(),
                    });
                }
                Rule::chain_call => chained = Some(pair.as_str().into()),
                rule => unreachable!("{:?}", rule),
            }
        }
        let mut calls = calls.into_iter();
        let mut call = calls.next().unwrap();
        for mut chained_call in calls {
            chained_call.args.insert(0, Node::Call(call));
            call = chained_call;
        }
        Ok(if call.args.is_empty() {
            *call.expr
        } else {
            Node::Call(call)
        })
    }

    fn expr_insert(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let inner = self.expr_get(pairs.next().unwrap())?;
        let mut insertions = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::insertion => {
                    let mut pairs = pair.into_inner();
                    let first = self.expr_get(pairs.next().unwrap())?;
                    let (key, val) = if let Some(val) =
                        pairs.next().map(|expr| self.expr_get(expr)).transpose()?
                    {
                        (Some(first), val)
                    } else {
                        (None, first)
                    };
                    insertions.push(Insertion { key, val });
                }
                rule => unreachable!("{:?}", rule),
            }
        }
        Ok(if insertions.is_empty() {
            inner
        } else {
            Node::Insert(InsertExpr {
                inner: inner.into(),
                insertions,
            })
        })
    }

    fn expr_get(&self, pair: Pair<'a, Rule>) -> ParseResult<Node<'a>> {
        debug_pair!(pair);
        let mut pairs = pair.into_inner();
        let mut node = Node::Term(self.term(pairs.next().unwrap())?);
        for pair in pairs {
            let get = match pair.as_rule() {
                Rule::ident => Get::Field(self.ident(pair)),
                Rule::term => Get::Index(self.term(pair)?),
                rule => unreachable!("{:?}", rule),
            };
            node = Node::Get(GetExpr {
                inner: node.into(),
                get,
            })
        }
        Ok(node)
    }

    fn term(&self, pair: Pair<'a, Rule>) -> ParseResult<Term<'a>> {
        debug_pair!(pair);
        let pair = only(pair);
        macro_rules! number_literal {
            ($term:ident) => {
                pair.as_str().parse().map(Term::$term).map_err(|_| {
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
            Rule::nil => Term::Nil,
            Rule::bool_literal => Term::Bool(pair.as_str() == "true"),
            Rule::ident => Term::Ident(self.ident(pair)),
            Rule::paren_expr => {
                let pair = only(pair);
                let items = self.items(pair)?;
                Term::Expr(items)
            }
            Rule::string => {
                let string = self.string_literal(pair);
                Term::String(string)
            }
            Rule::closure => {
                let mut pairs = pair.into_inner();
                let mut params = Vec::new();
                for pair in pairs.by_ref() {
                    if let Rule::param = pair.as_rule() {
                        params.push(self.param(pair));
                    } else {
                        break;
                    }
                }
                let pair = pairs.next().unwrap();
                let body = match pair.as_rule() {
                    Rule::items => self.items(pair)?,
                    Rule::expr => vec![Item::Node(self.expr(pair)?)],
                    rule => unreachable!("{:?}", rule),
                };
                Term::Closure(Closure { params, body }.into())
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
