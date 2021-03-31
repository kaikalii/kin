#![allow(clippy::upper_case_acronyms)]

use itertools::Itertools;
use pest::{error::Error as PestError, iterators::Pair, Parser, RuleType};

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
    NootParser::parse(Rule::items, &input).and_then(|mut pairs| parse_items(pairs.next().unwrap()))
}

fn parse_items(pair: Pair<Rule>) -> ParseResult<Items> {
    let mut items = Vec::new();
    for pair in pair.into_inner() {
        items.push(parse_item(pair)?);
    }
    Ok(Items { items })
}

fn parse_item(pair: Pair<Rule>) -> ParseResult<Item> {
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr_list => Item::Expressions(parse_exprs(pair)?),
        _ => unreachable!(),
    })
}

fn parse_exprs(pair: Pair<Rule>) -> ParseResult<Expressions> {
    let mut exprs = Vec::new();
    for pair in pair.into_inner() {
        let expr = parse_expr(pair)?;
        exprs.push(expr);
    }
    Ok(Expressions { exprs })
}

fn parse_expr(pair: Pair<Rule>) -> ParseResult<Expression> {
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::expr_or => parse_expr_or(pair)?,
        _ => unreachable!(),
    })
}

fn parse_expr_or(pair: Pair<Rule>) -> ParseResult<ExprOr> {
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_and(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "or" => OpOr,
            _ => unreachable!(),
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
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_cmp(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "and" => OpAnd,
            _ => unreachable!(),
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
            _ => unreachable!(),
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
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_mdr(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "+" => OpAS::Add,
            "-" => OpAS::Sub,
            _ => unreachable!(),
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
    let mut pairs = pair.into_inner();
    let left = pairs.next().unwrap();
    let left = parse_expr_not(left)?;
    let mut rights = Vec::new();
    for (op, right) in pairs.tuples() {
        let op = match op.as_str() {
            "*" => OpMDR::Mul,
            "/" => OpMDR::Div,
            "%" => OpMDR::Rem,
            _ => unreachable!(),
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
    let mut pairs = pair.into_inner();
    let term = pairs.next().unwrap();
    let term = parse_term(term)?;
    let mut args = Vec::new();
    for pair in pairs {
        let arg = parse_term(pair)?;
        args.push(arg);
    }
    Ok(ExprCall::Regular { term, args })
}

fn parse_term(pair: Pair<Rule>) -> ParseResult<Term> {
    let pair = only(pair);
    Ok(match pair.as_rule() {
        Rule::nat => Term::Nat(pair.as_str().parse().unwrap()),
        Rule::int => Term::Int(pair.as_str().parse().unwrap()),
        Rule::real => Term::Real(pair.as_str().parse().unwrap()),
        Rule::ident => unimplemented!(),
        Rule::paren_expr => {
            let pair = pair.into_inner().nth(1).unwrap();
            let expr = parse_expr(pair)?;
            Term::Expr(expr.into())
        }
        _ => unreachable!(),
    })
}
