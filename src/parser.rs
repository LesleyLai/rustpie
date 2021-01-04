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
        Rule::atom => Ok(Box::new(Expr::Atom(parsed.as_str()[1..].to_string()))),
        _ => unreachable!()
    }
}

pub fn parse(source: &str) -> ParseResult<Vec<Box<Expr>>> {
    let parsed = PieParser::parse(Rule::pie, source)?.next().unwrap();
    pie_read(parsed)
}

pub fn print_result(result: &ParseResult<Vec<Box<Expr>>>) {
    match result {
        Ok(exprs) => println!("{}", expr_list_to_string(&exprs)),
        Err(ParseError { msg }) => eprintln!("Parse Error:\n{}", msg)
    }
}