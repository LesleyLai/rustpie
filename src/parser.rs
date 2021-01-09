use crate::ast::{expr_list_to_string, Expr, ExprList};
use pest::Parser;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Parser)]
#[grammar = "pie.pest"]
pub struct PieParser;

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

fn read_pie(parsed: pest::iterators::Pair<Rule>) -> ParseResult<ExprList> {
    match parsed.as_rule() {
        Rule::pie => read_exprs(parsed.into_inner()),
        _ => unreachable!(),
    }
}

fn read_exprs(parsed: pest::iterators::Pairs<Rule>) -> ParseResult<ExprList> {
    parsed
        .filter(|child| !is_eoi(child))
        .map(read_expr)
        .collect()
}

fn read_unary(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Box<Expr>> {
    let mut inner_rule = parsed.into_inner();
    read_expr(inner_rule.next().unwrap())
}

fn read_binary(parsed: pest::iterators::Pair<Rule>) -> ParseResult<(Box<Expr>, Box<Expr>)> {
    let mut inner_rule = parsed.into_inner();
    let first = read_expr(inner_rule.next().unwrap())?;
    let second = read_expr(inner_rule.next().unwrap())?;
    assert!(inner_rule.next().is_none());
    Ok((first, second))
}

fn read_expr(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Box<Expr>> {
    match parsed.as_rule() {
        Rule::expr => read_expr(parsed.into_inner().next().unwrap()),
        Rule::cons => {
            let (first, second) = read_binary(parsed)?;
            Ok(Box::new(Expr::Cons(first, second)))
        }
        Rule::pair => {
            let (first, second) = read_binary(parsed)?;
            Ok(Box::new(Expr::TPair(first, second)))
        }
        Rule::sexpr => {
            let mut inner_rule = parsed.into_inner();
            let first = read_expr(inner_rule.next().unwrap())?;
            Ok(Box::new(Expr::App(first, read_exprs(inner_rule)?)))
        }
        Rule::ident => {
            let ident = parsed.as_str().to_string();
            match ident.as_str() {
                "Atom" => Ok(Box::new(Expr::TAtom)),
                "Nat" => Ok(Box::new(Expr::TNat)),
                "zero" => Ok(Box::new(Expr::Zero)),
                _ => Ok(Box::new(Expr::Var(ident))),
            }
        }
        Rule::car => Ok(Box::new(Expr::Car(read_unary(parsed)?))),
        Rule::cdr => Ok(Box::new(Expr::Cdr(read_unary(parsed)?))),
        Rule::add1 => Ok(Box::new(Expr::Succ(read_unary(parsed)?))),
        Rule::nat_literal => {
            let val = parsed
                .as_str()
                .lines()
                .next()
                .unwrap()
                .parse::<u64>()
                .unwrap();
            Ok(Box::new(Expr::Nat(val)))
        }
        Rule::atom => Ok(Box::new(Expr::Atom(parsed.as_str()[1..].to_string()))),
        _ => unreachable!(),
    }
}

pub fn parse(source: &str) -> ParseResult<Vec<Box<Expr>>> {
    let parsed = PieParser::parse(Rule::pie, source)?.next().unwrap();
    read_pie(parsed)
}

pub fn result_to_string(result: &ParseResult<Vec<Box<Expr>>>) -> String {
    match result {
        Ok(exprs) => format!("{}", expr_list_to_string(&exprs)),
        Err(ParseError { msg }) => format!("Parse Error:\n{}", msg),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::parser::result_to_string;

    fn parse_and_to_string(source: &str) -> String {
        result_to_string(&parse(source))
    }

    #[test]
    fn parsing_test() {
        assert_eq!(parse_and_to_string("'x"), "'x");
        assert!(parse("'").is_err());
        assert_eq!(parse_and_to_string("Atom"), "Atom");
        assert_eq!(parse_and_to_string("x"), "x");
        assert_eq!(parse_and_to_string("(cons 'x 'x)"), "(cons 'x 'x)");
        assert_eq!(parse_and_to_string("(Pair Atom Atom)"), "(Pair Atom Atom)");
        assert!(parse("'1").is_err());
        assert!(parse("(cons '1 'x)").is_err());
        assert_eq!(parse_and_to_string("1"), "1");
    }
}
