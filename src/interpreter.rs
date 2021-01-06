use crate::ast::{Expr, Type};


pub fn is_a(expr: &Expr, typ: &Type) -> bool {
    match expr {
        Expr::Atom(_) => *typ == Type::Atom,
        Expr::App(_) => unimplemented!(),
        Expr::Var(_) => false
    }
}

pub fn is_the_same_as(expr1: &Expr, expr2: &Expr, typ: &Type) -> bool
{
    is_a(expr1, typ) && is_a(expr2, typ) && expr1 == expr2
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::ast::{Type, Expr};
    use crate::interpreter::{is_a, is_the_same_as};
    use std::ops::Deref;

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

    fn source_is_a(source: &str, typ: &Type) -> bool
    {
        is_a(parse_to_expr(source).deref(), typ)
    }

    fn source_is_the_same_as(source1: &str, source2: &str, typ: &Type) -> bool
    {
        is_the_same_as(parse_to_expr(source1).deref(), parse_to_expr(source2).deref(), typ)
    }


    #[test]
    fn atom_test() {
        assert!(source_is_a("'atom", &Type::Atom));
        assert!(source_is_a("'ratatouille", &Type::Atom));
        assert!(source_is_a("'obviously-an-atom", &Type::Atom));
        assert!(source_is_a("'---", &Type::Atom));
        assert!(!source_is_a("---", &Type::Atom));

        assert!(source_is_the_same_as("'citron", "'citron", &Type::Atom));
        assert!(!source_is_the_same_as("'pomme", "'orange", &Type::Atom));
    }
}