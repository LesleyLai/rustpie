use pest::Parser;
use std::fmt::Debug;
use std::hash::Hash;
use crate::ast::{Expr, ExprList, expr_list_to_string};

#[derive(Parser)]
#[grammar = "pie.pest"]
pub struct PieParser;


pub struct ParseError {
    msg: String
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
        ParseError { msg: format!("{}", error) }
    }
}

fn is_eoi(parsed: &pest::iterators::Pair<Rule>) -> bool {
    parsed.as_rule() == Rule::EOI
}

fn pie_read(parsed: pest::iterators::Pair<Rule>) -> ParseResult<ExprList> {
    match parsed.as_rule() {
        Rule::pie => {
            exprs_read(parsed)
        }
        _ => unreachable!(),
    }
}

fn exprs_read(parsed: pest::iterators::Pair<Rule>) -> ParseResult<ExprList> {
    parsed.into_inner().filter(|child| !is_eoi(child)).map(expr_read).collect()
}


fn expr_read(parsed: pest::iterators::Pair<Rule>) -> ParseResult<Box<Expr>> {
    match parsed.as_rule() {
        Rule::expr => expr_read(parsed.into_inner().next().unwrap()),
        Rule::sexpr => {
            let exprs = exprs_read(parsed)?;
            Ok(Box::new(Expr::App(exprs)))
        }
        Rule::ident => Ok(Box::new(Expr::Var(parsed.as_str().to_string()))),
        Rule::atom => Ok(Box::new(Expr::Atom(parsed.as_str()[1..].to_string()))),
        _ => unreachable!()
    }
}

pub fn parse(source: &str) -> ParseResult<Vec<Box<Expr>>> {
    let parsed = PieParser::parse(Rule::pie, source)?.next().unwrap();
    pie_read(parsed)
}

pub fn result_to_string(result: &ParseResult<Vec<Box<Expr>>>) -> String {
    match result {
        Ok(exprs) => format!("{}", expr_list_to_string(&exprs)),
        Err(ParseError { msg }) => format!("Parse Error:\n{}", msg)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::result_to_string;
    use crate::parser::parse;

    fn parse_and_to_string(source: &str) -> String {
        result_to_string(&parse(source))
    }

    #[test]
    fn parsing_test() {
        assert_eq!(parse_and_to_string("'x"), "'x");
        assert!(parse("'").is_err());
        assert_eq!(parse_and_to_string("x"), "x");
    }
}