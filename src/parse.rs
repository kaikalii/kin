#![allow(clippy::upper_case_acronyms)]

use itertools::Itertools;
use pest::{
    error::{Error as PestError, ErrorVariant},
    iterators::Pair,
    Parser, RuleType,
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
    match NootParser::parse(Rule::file, &input) {
        Ok(mut pairs) => parse_items(only(pairs.next().unwrap())),
        Err(e) => Err(e),
    }
}

fn parse_items(pair: Pair<Rule>) -> ParseResult<Items> {
    debug_pair!(pair);
    let mut items = Vec::new();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::item => items.push(parse_item(pair)?),
            Rule::EOI => {}
            rule => unreachable!("{:?}", rule),
        }
    }
    Ok(Items { items })
}

fn parse_item(pair: Pair<Rule>) -> ParseResult<Item> {
    debug_pair!(pair);
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr => Item::Node(parse_expr(pair)?),
        Rule::def => Item::Def(parse_def(pair)?),
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_ident(pair: Pair<Rule>) -> Ident {
    Ident {
        name: pair.as_str().into(),
        span: pair.as_span(),
    }
}

fn parse_param(pair: Pair<Rule>) -> Param {
    let mut pairs = pair.into_inner();
    let ident = parse_ident(pairs.next().unwrap());
    Param { ident }
}

fn parse_def(pair: Pair<Rule>) -> ParseResult<Def> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let Param { ident } = parse_param(pairs.next().unwrap());
    let mut params = Vec::new();
    for pair in pairs.by_ref() {
        if let Rule::param = pair.as_rule() {
            params.push(parse_param(pair));
        } else {
            break;
        }
    }
    let pair = pairs.next().unwrap();
    let items = match pair.as_rule() {
        Rule::items => parse_items(pair)?,
        Rule::expr => Items {
            items: vec![Item::Node(parse_expr(pair)?)],
        },
        rule => unreachable!("{:?}", rule),
    };
    Ok(Def {
        ident,
        params: Params { params },
        items,
    })
}

fn parse_expr(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr_or => parse_expr_or(pair)?,
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_expr_or(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let mut left = parse_expr_and(left)?;
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "or" => BinOp::Or,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_and(right)?;
        left = Node::BinExpr(BinExpr::new(left, right, op));
    }
    Ok(left)
}

fn parse_expr_and(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let mut left = parse_expr_cmp(left)?;
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "and" => BinOp::And,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_cmp(right)?;
        left = Node::BinExpr(BinExpr::new(left, right, op));
    }
    Ok(left)
}

fn parse_expr_cmp(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let mut left = parse_expr_as(left)?;
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
        let right = parse_expr_as(right)?;
        left = Node::BinExpr(BinExpr::new(left, right, op));
    }
    Ok(left)
}

fn parse_expr_as(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let mut left = parse_expr_mdr(left)?;
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_mdr(right)?;
        left = Node::BinExpr(BinExpr::new(left, right, op));
    }
    Ok(left)
}

fn parse_expr_mdr(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let mut left = parse_expr_not(left)?;
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "%" => BinOp::Rem,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_not(right)?;
        left = Node::BinExpr(BinExpr::new(left, right, op));
    }
    Ok(left)
}

fn parse_expr_not(pair: Pair<Rule>) -> ParseResult<Node> {
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
    let inner = parse_expr_call(inner)?;
    Ok(if let Some(op) = op {
        Node::UnExpr(UnExpr::new(inner, op))
    } else {
        inner
    })
}

fn parse_expr_call(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let pairs = pair.into_inner();
    let mut calls = Vec::new();
    let mut chained = None;
    for pair in pairs {
        match pair.as_rule() {
            Rule::expr_call_single => {
                let mut pairs = pair.into_inner();
                let expr = parse_expr_insert(pairs.next().unwrap())?;
                let mut args = Vec::new();
                for pair in pairs {
                    let arg = parse_expr_insert(pair)?;
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

fn parse_expr_insert(pair: Pair<Rule>) -> ParseResult<Node> {
    debug_pair!(pair);
    let mut pairs = pair.into_inner();
    let term = parse_term(pairs.next().unwrap())?;
    let mut insertions = Vec::new();
    let mut curr_ident: Option<Ident> = None;
    for pair in pairs {
        match pair.as_rule() {
            Rule::ident => {
                if let Some(ident) = curr_ident.take() {
                    insertions.push(Insertion {
                        ident: ident.name.clone(),
                        term: Term::Ident(ident),
                    })
                }
                curr_ident = Some(parse_ident(pair));
            }
            Rule::term => {
                let ident = curr_ident.take().unwrap();
                let term = parse_term(pair)?;
                insertions.push(Insertion {
                    ident: ident.name,
                    term,
                });
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    Ok(if insertions.is_empty() {
        Node::Term(term)
    } else {
        Node::Insert(InsertExpr { term, insertions })
    })
}

fn parse_term(pair: Pair<Rule>) -> ParseResult<Term> {
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
        Rule::nat => number_literal!(Nat)?,
        Rule::int => number_literal!(Int)?,
        Rule::real => number_literal!(Real)?,
        Rule::nil => Term::Nil,
        Rule::bool_literal => Term::Bool(pair.as_str() == "true"),
        Rule::ident => Term::Ident(parse_ident(pair)),
        Rule::paren_expr => {
            let pair = only(pair);
            let items = parse_items(pair)?;
            Term::Expr(items)
        }
        Rule::string => {
            let string = parse_string_literal(pair);
            Term::String(string)
        }
        Rule::closure => {
            let mut pairs = pair.into_inner();
            let mut params = Vec::new();
            for pair in pairs.by_ref() {
                if let Rule::param = pair.as_rule() {
                    params.push(parse_param(pair));
                } else {
                    break;
                }
            }
            let pair = pairs.next().unwrap();
            let body = match pair.as_rule() {
                Rule::items => parse_items(pair)?,
                Rule::expr => Items {
                    items: vec![Item::Node(parse_expr(pair)?)],
                },
                rule => unreachable!("{:?}", rule),
            };
            Term::Closure(
                Closure {
                    params: Params { params },
                    body,
                }
                .into(),
            )
        }
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_string_literal(pair: Pair<Rule>) -> std::string::String {
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
                s.push(std::char::from_u32(u).unwrap_or_else(|| panic!("invalid unicode {}", u)));
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    s
}
