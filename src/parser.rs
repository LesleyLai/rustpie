use crate::ast::{Expr, ExprList, Toplevel};
use num_bigint::BigUint;
use pest::Parser;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Parser)]
#[grammar = "pie.pest"]
pub struct PieParser;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    msg: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Parse Error:\n{}", self.msg)
    }
}

pub type ParseResult<T> = std::result::Result<T, ParseError>;

impl<T> From<pest::error::Error<T>> for ParseError
where
    T: Debug + Ord + Copy + Hash,
{
    fn from(error: pest::error::Error<T>) -> Self {
        ParseError {
            msg: format!("{}", error),
        }
    }
}

fn is_eoi(parsed: &pest::iterators::Pair<Rule>) -> bool {
    parsed.as_rule() == Rule::EOI
}

fn read_pie(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Vec<Toplevel>> {
    match parsed.as_rule() {
        Rule::pie => parsed
            .into_inner()
            .filter(|child| !is_eoi(child))
            .map(read_toplevel)
            .collect(),
        _ => unreachable!(),
    }
}

fn read_exprs(parsed: pest::iterators::Pairs<Rule>) -> ParseResult<ExprList> {
    parsed
        .filter(|child| !is_eoi(child))
        .map(read_expr)
        .collect()
}

fn read_params(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Vec<String>> {
    assert!(parsed.as_rule() == Rule::params);

    parsed
        .into_inner()
        .filter(|child| !is_eoi(child))
        .map(|parsed| Ok(parsed.as_str().to_string()))
        .collect()
}

fn read_unary(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Expr> {
    let mut inner_rule = parsed.into_inner();
    read_expr(inner_rule.next().unwrap())
}

fn read_binary(parsed: pest::iterators::Pair<Rule>) -> ParseResult<(Expr, Expr)> {
    let mut inner_rule = parsed.into_inner();
    let first = read_expr(inner_rule.next().unwrap())?;
    let second = read_expr(inner_rule.next().unwrap())?;
    assert!(inner_rule.next().is_none());
    Ok((first, second))
}

fn read_define(parsed: pest::iterators::Pair<Rule>) -> ParseResult<(String, Expr)> {
    let mut inner_rule = parsed.into_inner();
    let ident = inner_rule.next().unwrap().as_str().to_string();
    let expr = read_expr(inner_rule.next().unwrap())?;
    assert!(inner_rule.next().is_none());
    Ok((ident, expr))
}

fn read_toplevel(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Toplevel> {
    let inner = parsed.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::expr => read_expr(inner.into_inner().next().unwrap()).map(Toplevel::Expr),
        Rule::claim => {
            let (ident, expr) = read_define(inner)?;
            Ok(Toplevel::Claim(ident, expr))
        }
        Rule::define => {
            let (ident, expr) = read_define(inner)?;
            Ok(Toplevel::Define(ident, expr))
        }
        _ => unreachable!(),
    }
}

fn read_expr(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Expr> {
    match parsed.as_rule() {
        Rule::expr => read_expr(parsed.into_inner().next().unwrap()),
        Rule::cons => {
            let (first, second) = read_binary(parsed)?;
            Ok(Expr::Cons(Box::new(first), Box::new(second)))
        }
        Rule::pair => {
            let (first, second) = read_binary(parsed)?;
            Ok(Expr::TPair(Box::new(first), Box::new(second)))
        }
        Rule::lambda => {
            let mut inner_rule = parsed.into_inner();
            let params = read_params(inner_rule.next().unwrap())?;
            let body = read_expr(inner_rule.next().unwrap())?;

            Ok(Expr::Lambda(params, Box::new(body)))
        }
        Rule::arrow => {
            let mut exprs = read_exprs(parsed.into_inner())?;
            let ret_typ = exprs.pop().unwrap();
            Ok(Expr::TArr(exprs, Box::new(ret_typ)))
        }
        Rule::sexpr => {
            let mut inner_rule = parsed.into_inner();
            let first = read_expr(inner_rule.next().unwrap())?;
            Ok(Expr::App(Box::new(first), read_exprs(inner_rule)?))
        }
        Rule::ident => {
            let ident = parsed.as_str().to_string();
            match ident.as_str() {
                "Atom" => Ok(Expr::TAtom),
                "Nat" => Ok(Expr::TNat),
                "zero" => Ok(Expr::Zero),
                _ => Ok(Expr::Var(ident)),
            }
        }
        Rule::car => Ok(Expr::Car(Box::new(read_unary(parsed)?))),
        Rule::cdr => Ok(Expr::Cdr(Box::new(read_unary(parsed)?))),
        Rule::add1 => Ok(Expr::Succ(Box::new(read_unary(parsed)?))),
        Rule::nat_literal => Ok(Expr::Nat(parsed.as_str().parse::<BigUint>().unwrap())),
        Rule::atom => Ok(Expr::Atom(parsed.as_str()[1..].to_string())),
        _ => unreachable!(),
    }
}

pub fn parse(source: &str) -> ParseResult<Vec<Toplevel>> {
    let parsed = PieParser::parse(Rule::pie, source)?.next().unwrap();
    read_pie(parsed)
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn test_atom_parsing() {
        insta::assert_debug_snapshot!(parse("'x"));
    }

    #[test]
    fn test_apostrophe_parsing() {
        insta::assert_debug_snapshot!(parse("'"));
    }

    #[test]
    fn test_postrophe_with_num_parsing() {
        insta::assert_debug_snapshot!(parse("'1"));
    }

    #[test]
    fn test_tatom_parsing() {
        insta::assert_debug_snapshot!(parse("Atom"));
    }

    #[test]
    fn test_var_parsing() {
        insta::assert_debug_snapshot!(parse("x"));
    }

    #[test]
    fn test_cons_parsing() {
        insta::assert_debug_snapshot!(parse("(cons 'x 'x)"));
    }

    #[test]
    fn test_tpair_parsing() {
        insta::assert_debug_snapshot!(parse("(Pair Atom Atom)"));
    }

    #[test]
    fn test_nat_literal_parsing() {
        insta::assert_debug_snapshot!(parse("1"));
    }

    #[test]
    fn test_multi_nat_literal_parsing() {
        insta::assert_debug_snapshot!(parse("(cons 42 42)"));
    }

    #[test]
    fn test_big_nat_literal() {
        insta::assert_debug_snapshot!(parse("1232143434875236458243451231123"));
    }

    #[test]
    fn test_lambda() {
        insta::assert_debug_snapshot!("lambda", parse("(lambda (x) (cons x 42))"));
        insta::assert_debug_snapshot!("λ", parse("(λ (x) (cons x 42))"));
    }

    #[test]
    fn test_arrow() {
        insta::assert_debug_snapshot!("Arrow", parse("(-> Nat (Pair Nat Nat))"));
    }
}
