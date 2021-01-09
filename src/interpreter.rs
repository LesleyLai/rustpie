use crate::ast::Expr;

pub fn is_a(expr: &Expr, typ: &Expr) -> bool {
    has_type(expr).map_or(false, |t| t == *typ)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    CannotResolveType(String),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeError::CannotResolveType(e) => {
                write!(f, "Cannot resolve the type of expression {}", e)
            }
        }
    }
}

pub fn has_type(expr: &Expr) -> Result<Expr, TypeError> {
    match expr {
        Expr::Var(_) => Err(TypeError::CannotResolveType(format!("{}", expr))),
        Expr::TAtom => unimplemented!(),
        Expr::Atom(_) => Ok(Expr::TAtom),
        Expr::App(_, _) => unimplemented!(),
        Expr::TPair(_e1, _e2) => unimplemented!(),
        Expr::Car(e) => match &**e {
            Expr::Cons(e1, _) => has_type(&e1),
            _ => Err(TypeError::CannotResolveType(format!("{}", e))),
        },
        Expr::Cdr(e) => match &**e {
            Expr::Cons(_, e2) => has_type(&e2),
            _ => Err(TypeError::CannotResolveType(format!("{}", e))),
        },
        Expr::Cons(e1, e2) => {
            let t1 = has_type(e1)?;
            let t2 = has_type(e2)?;
            Ok(Expr::TPair(Box::new(t1), Box::new(t2)))
        }
        Expr::TNat => unimplemented!(),
        Expr::Zero => Ok(Expr::TNat),
        Expr::Succ(e) => {
            let t = has_type(e)?;
            match t {
                Expr::TNat => Ok(Expr::TNat),
                _ => Err(TypeError::CannotResolveType(format!("{}", e))),
            }
        }
        Expr::Nat(_) => Ok(Expr::TNat),
    }
}

pub fn eval(expr: &Expr) -> Result<Expr, String> {
    match expr {
        Expr::Var(_) => unimplemented!(),
        Expr::TAtom | Expr::Atom(_) => Ok(expr.clone()),
        Expr::App(_, _) => unimplemented!(),
        Expr::TPair(e1, e2) => {
            let new_e1 = eval(e1)?;
            let new_e2 = eval(e2)?;
            Ok(Expr::TPair(Box::new(new_e1), Box::new(new_e2)))
        }
        Expr::Car(e) => match &**e {
            Expr::Cons(e1, _) => eval(&e1),
            _ => unreachable!(),
        },
        Expr::Cdr(e) => match &**e {
            Expr::Cons(_, e2) => eval(&e2),
            _ => unreachable!(),
        },
        Expr::Cons(e1, e2) => {
            let t1 = eval(e1)?;
            let t2 = eval(e2)?;
            Ok(Expr::Cons(Box::new(t1), Box::new(t2)))
        }
        Expr::TNat => unimplemented!(),
        Expr::Zero => Ok(Expr::Nat(0)),
        Expr::Succ(e) => {
            let v = eval(e)?;
            match v {
                Expr::Nat(n) => Ok(Expr::Nat(n + 1)),
                _ => unreachable!(),
            }
        }
        nat @ Expr::Nat(_) => Ok(nat.clone()),
    }
}

pub fn is_type(expr: &Expr) -> bool {
    match expr {
        Expr::Var(_) => unimplemented!(),
        Expr::TAtom | Expr::TNat | Expr::TPair(_, _) => true,
        Expr::App(_, _) => unimplemented!(),
        _ => false,
    }
}

pub fn is_the_same_as(expr1: &Expr, typ: &Expr, expr2: &Expr) -> bool {
    is_a(expr1, typ) && is_a(expr2, typ) && eval(expr1) == eval(expr2)
}

pub fn is_the_same_type(typ1: &Expr, typ2: &Expr) -> bool {
    is_type(typ1) && is_type(typ2) && eval(typ1) == eval(typ2)
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr;
    use crate::interpreter::{is_a, is_the_same_as, is_the_same_type, is_type};
    use crate::parser::parse;
    use std::borrow::Borrow;

    fn parse_to_expr(source: &str) -> Box<Expr> {
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

    fn source_is_a(expr: &str, typ: &str) -> bool {
        is_a(parse_to_expr(expr).borrow(), parse_to_expr(typ).borrow())
    }

    fn source_is_type(expr: &str) -> bool {
        is_type(parse_to_expr(expr).borrow())
    }

    fn source_is_the_same_as(e1: &str, typ: &str, e2: &str) -> bool {
        is_the_same_as(
            parse_to_expr(e1).borrow(),
            parse_to_expr(typ).borrow(),
            parse_to_expr(e2).borrow(),
        )
    }

    fn source_is_the_same_type(typ1: &str, typ2: &str) -> bool {
        is_the_same_type(parse_to_expr(typ1).borrow(), parse_to_expr(typ2).borrow())
    }

    #[test]
    fn atom_test() {
        assert!(source_is_a("'atom", "Atom"));
        assert!(source_is_a("'ratatouille", "Atom"));
        assert!(source_is_a("'obviously-an-atom", "Atom"));
        assert!(source_is_a("'---", "Atom"));
        assert!(!source_is_a("---", "Atom"));

        assert!(source_is_the_same_as("'citron", "Atom", "'citron"));
        assert!(!source_is_the_same_as("'pomme", "Atom", "'orange"));

        assert!(!source_is_type("'atom"));
        assert!(source_is_type("Atom"));

        assert!(source_is_the_same_type("Atom", "Atom"));
    }

    #[test]
    fn pair_test() {
        assert!(source_is_a("(cons 'x 'y)", "(Pair Atom Atom)"));

        assert!(source_is_the_same_as(
            "(cons 'x 'x)",
            "(Pair Atom Atom)",
            "(cons 'x 'x)"
        ));

        assert!(!source_is_the_same_as(
            "(cons 'x 'y)",
            "(Pair Atom Atom)",
            "(cons 'y 'y)"
        ));

        assert!(!source_is_type("(cons 'x 'y)"));
        assert!(source_is_type("(Pair Atom Atom)"));

        assert!(!source_is_the_same_type("(Pair Atom Atom)", "Atom"));
        assert!(source_is_the_same_type(
            "(Pair Atom Atom)",
            "(Pair Atom Atom)"
        ));
    }

    #[test]
    fn car_cdr_test() {
        assert!(source_is_a(
            "(car (cons (cons 'x 'y) 'z))",
            "(Pair Atom Atom)"
        ));
        assert!(source_is_a("(cdr (cons (cons 'x 'y) 'z))", "Atom"));

        assert!(source_is_the_same_as(
            "(cdr (cons (cons 'x 'y) 'z))",
            "Atom",
            "'z"
        ));

        assert!(!source_is_type("(car (cons 'x 'y))"));
        assert!(!source_is_type("(cdr (cons 'x 'y))"));
    }

    #[test]
    fn natural_test() {
        assert!(source_is_a("0", "Nat"));
        assert!(source_is_a("42", "Nat"));
        assert!(!source_is_a("-42", "Nat"));

        assert!(source_is_the_same_as("0", "Nat", "zero"));
        assert!(source_is_the_same_as("1", "Nat", "(add1 zero)"));
    }
}
