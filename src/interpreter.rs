use crate::ast::{Expr, Type};


pub fn is_a(expr: &Expr, typ: &Type) -> bool {
    match expr {
        Expr::Atom(_) => *typ == Type::Atom,
        Expr::App(_) => unimplemented!(),
        Expr::Var(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::ast::{Type, Expr};
    use crate::interpreter::is_a;
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

    fn check_is_a(source: &str, typ: &Type)
    {
        assert!(is_a(parse_to_expr(source).deref(), typ))
    }

    fn check_is_not_a(source: &str, typ: &Type)
    {
        assert!(!is_a(parse_to_expr(source).deref(), typ))
    }

    #[test]
    fn is_a_test() {
        check_is_a("'atom", &Type::Atom);
        check_is_a("'ratatouille", &Type::Atom);
        check_is_a("'obviously-an-atom", &Type::Atom);
        check_is_a("'---", &Type::Atom);
        check_is_not_a("---", &Type::Atom);
    }
}