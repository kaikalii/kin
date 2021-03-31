#![allow(clippy::upper_case_acronyms)]

use itertools::Itertools;
use pest::{
    error::{Error as PestError, ErrorVariant},
    iterators::Pair,
    Parser, RuleType, Span,
};

use crate::ast::*;

pub type ParseResult<T> = Result<T, PestError<Rule>>;

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
    match NootParser::parse(Rule::items, &input) {
        Ok(mut pairs) => {
            let pair = pairs.next().unwrap();
            let parsed_len = pair.as_str().len();
            if parsed_len < input.len() {
                let extra = input[parsed_len..].split_whitespace().next().unwrap();
                return Err(PestError::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Invalid token: {}", extra),
                    },
                    Span::new(input, parsed_len, parsed_len + extra.len()).unwrap(),
                ));
            }
            parse_items(pair)
        }
        Err(e) => Err(e),
    }
}

fn parse_items(pair: Pair<Rule>) -> ParseResult<Items> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut items = Vec::new();
    for pair in pair.into_inner() {
        items.push(parse_item(pair)?);
    }
    Ok(Items { items })
}

fn parse_item(pair: Pair<Rule>) -> ParseResult<Item> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr_list => Item::Expressions(parse_exprs(pair)?),
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_exprs(pair: Pair<Rule>) -> ParseResult<Expressions> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    parse_exprs_impl(pair.into_inner())
}

fn parse_exprs_impl<'a, P>(pairs: P) -> ParseResult<Expressions>
where
    P: Iterator<Item = Pair<'a, Rule>>,
{
    let mut exprs = Vec::new();
    for pair in pairs {
        match pair.as_rule() {
            Rule::expr => {
                let expr = parse_expr(pair)?;
                exprs.push(expr);
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    Ok(Expressions { exprs })
}

fn parse_expr(pair: Pair<Rule>) -> ParseResult<Expression> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr_or => parse_expr_or(pair)?,
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_expr_or(pair: Pair<Rule>) -> ParseResult<ExprOr> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_and(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "or" => OpOr,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_and(right)?;
        rights.push(Right { op, expr: right });
    }
    Ok(ExprOr {
        left: left.into(),
        rights,
    })
}

fn parse_expr_and(pair: Pair<Rule>) -> ParseResult<ExprAnd> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_cmp(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "and" => OpAnd,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_cmp(right)?;
        rights.push(Right { op, expr: right });
    }
    Ok(ExprAnd {
        left: left.into(),
        rights,
    })
}

fn parse_expr_cmp(pair: Pair<Rule>) -> ParseResult<ExprCmp> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_as(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "is" => OpCmp::Is,
            "isnt" => OpCmp::Isnt,
            "<=" => OpCmp::LessOrEqual,
            ">=" => OpCmp::GreaterOrEqual,
            "<" => OpCmp::Less,
            ">" => OpCmp::Greater,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_as(right)?;
        rights.push(Right { op, expr: right });
    }
    Ok(ExprCmp {
        left: left.into(),
        rights,
    })
}

fn parse_expr_as(pair: Pair<Rule>) -> ParseResult<ExprAS> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_mdr(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "+" => OpAS::Add,
            "-" => OpAS::Sub,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_mdr(right)?;
        rights.push(Right { op, expr: right });
    }
    Ok(ExprAS {
        left: left.into(),
        rights,
    })
}

fn parse_expr_mdr(pair: Pair<Rule>) -> ParseResult<ExprMDR> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_not(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "*" => OpMDR::Mul,
            "/" => OpMDR::Div,
            "%" => OpMDR::Rem,
            rule => unreachable!("{:?}", rule),
        };
        let right = parse_expr_not(right)?;
        rights.push(Right { op, expr: right });
    }
    Ok(ExprMDR {
        left: left.into(),
        rights,
    })
}

fn parse_expr_not(pair: Pair<Rule>) -> ParseResult<ExprNot> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut pairs = pair.into_inner();
    let first = pairs.next().unwrap();
    let op = match first.as_str() {
        "not" => Some(OpNot),
        _ => None,
    };
    let pair = if op.is_some() {
        pairs.next().unwrap()
    } else {
        first
    };
    let expr = parse_expr_call(pair)?;
    Ok(ExprNot { op, expr })
}

fn parse_expr_call(pair: Pair<Rule>) -> ParseResult<ExprCall> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let pairs = pair.into_inner();
    let mut calls = Vec::new();
    let mut chained = None;
    for pair in pairs {
        match pair.as_rule() {
            Rule::expr_call_single => {
                let mut pairs = pair.into_inner();
                let term = parse_term(pairs.next().unwrap())?;
                let mut args = Vec::new();
                for pair in pairs {
                    let arg = parse_term(pair)?;
                    args.push(arg);
                }
                calls.push(ExprCall {
                    term,
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
        chained_call.args.insert(
            0,
            Term::wrapping(ExprOr::wrapping(ExprAnd::wrapping(ExprCmp::wrapping(
                ExprAS::wrapping(ExprMDR::wrapping(ExprNot::wrapping(call))),
            )))),
        );
        call = chained_call;
    }
    Ok(call)
}

fn parse_term(pair: Pair<Rule>) -> ParseResult<Term> {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::nat => Term::Nat(pair.as_str().parse().unwrap()),
        Rule::int => Term::Int(pair.as_str().parse().unwrap()),
        Rule::real => Term::Real(pair.as_str().parse().unwrap()),
        Rule::nil => Term::Nil,
        Rule::bool_literal => Term::Bool(pair.as_str() == "true"),
        Rule::ident => Term::Ident(pair.as_str().into()),
        Rule::paren_expr => {
            let pair = only(pair);
            let expr = parse_expr(pair)?;
            Term::Expr(expr.into())
        }
        Rule::string => {
            let string = parse_string_literal(pair);
            Term::String(string)
        }
        Rule::inline_function => {
            let mut pairs = pair.into_inner();
            let mut idents = Vec::new();
            while let Some(ident_pair) = pairs.next() {
                if ident_pair.as_rule() == Rule::ident {
                    idents.push(ident_pair.as_str().to_owned());
                } else {
                    break;
                }
            }
            let body = parse_exprs_impl(pairs.by_ref())?;
            Term::Function(
                Function {
                    params: Params { idents },
                    body,
                }
                .into(),
            )
        }
        rule => unreachable!("{:?}", rule),
    })
}

fn parse_string_literal(pair: Pair<Rule>) -> std::string::String {
    println!("{:?} {:?}", pair.as_rule(), pair.as_str());
    let mut s = String::new();
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::raw_string => s.push_str(pair.as_str()),
            Rule::escape => {
                let pair = only(pair);
                match pair.as_rule() {
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
                            std::char::from_u32(u)
                                .unwrap_or_else(|| panic!("invalid unicode {}", u)),
                        );
                    }
                    rule => unreachable!("{:?}", rule),
                }
            }
            rule => unreachable!("{:?}", rule),
        }
    }
    s
}
