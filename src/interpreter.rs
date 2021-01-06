use crate::ast::{Expr};


pub fn is_a(expr: &Expr, typ: &Expr) -> bool {
    match expr {
        Expr::Atom(_) => *typ == Expr::TAtom,
        Expr::App(_) => unimplemented!(),
        Expr::Var(_) => false,
        Expr::TAtom => false
    }
}

pub fn is_the_same_as(expr1: &Expr, expr2: &Expr, typ: &Expr) -> bool
{
    is_a(expr1, typ) && is_a(expr2, typ) && expr1 == expr2
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::ast::{Expr};
    use crate::interpreter::{is_a, is_the_same_as};
    use std::borrow::Borrow;

    fn parse_to_expr(source: &str) -> Box<Expr>
    {
        match parse(source) {
            Ok(exprs) => {
                if exprs.len() != 1 {
                    panic!("Parse result of \"{}\" is not a single expression", source);
                }
                exprs.first().unwrap().clone()
            }
            Err(_) => {
                panic!("\"{}\" failed to parse", source)
            }
        }
    }

    fn source_is_a(source: &str, typ: &Expr) -> bool
    {
        is_a(parse_to_expr(source).borrow(), typ)
    }

    fn source_is_the_same_as(source1: &str, source2: &str, typ: &Expr) -> bool
    {
        is_the_same_as(parse_to_expr(source1).borrow(), parse_to_expr(source2).borrow(), typ)
    }


    #[test]
    fn atom_test() {
        assert!(source_is_a("'atom", &Expr::TAtom));
        assert!(source_is_a("'ratatouille", &Expr::TAtom));
        assert!(source_is_a("'obviously-an-atom", &Expr::TAtom));
        assert!(source_is_a("'---", &Expr::TAtom));
        assert!(!source_is_a("---", &Expr::TAtom));

        assert!(source_is_the_same_as("'citron", "'citron", &Expr::TAtom));
        assert!(!source_is_the_same_as("'pomme", "'orange", &Expr::TAtom));
    }
}