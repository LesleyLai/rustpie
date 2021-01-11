use crate::ast::{Expr, Toplevel};
use im::{hashmap, HashMap};

type TypeEnv = HashMap<String, Expr>;
type Env = HashMap<String, Expr>;

fn init_global_tenv() -> TypeEnv {
    hashmap! {
        // "x".to_string() => Expr::TAtom
    }
}

fn init_global_env() -> Env {
    hashmap! {
        // "x".to_string() => Expr::Atom("x".to_string())
    }
}

pub struct Interpreter {
    global_tenv: Box<TypeEnv>,
    global_env: Box<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_tenv: Box::new(init_global_tenv()),
            global_env: Box::new(init_global_env()),
        }
    }

    // Executes a toplevel
    // If successful, optionally returns a message
    // If fail, returns an error message
    pub fn execute(&mut self, toplevel: &Toplevel) -> Result<Option<String>, String> {
        match toplevel {
            Toplevel::Expr(e) => match has_type(&e, &self.global_tenv) {
                Err(type_error) => Err(format!("{}", type_error)),
                Ok(typ) => match eval(&e, &self.global_tenv, &self.global_env) {
                    Err(runtime_error) => Err(format!("{}", runtime_error)),
                    Ok(val) => Ok(Some(format!("(the {} {})", typ, val))),
                },
            },
            Toplevel::Claim(ident, e) => {
                if !is_type(&e) {
                    Err(format!("{} is not a type!", e))
                } else if self.global_tenv.contains_key(ident) {
                    Err(format!("Error: the variable {} is already claimed!", ident))
                } else {
                    self.global_tenv = Box::new(self.global_tenv.update(
                        ident.clone(),
                        eval(&e, &self.global_tenv, &self.global_env).unwrap(),
                    ));
                    Ok(None)
                }
            }
            Toplevel::Define(ident, e) => match self.global_tenv.get(ident) {
                None => Err(format!("Error: the variable {} is never claimed!", ident)),
                Some(typ) => {
                    if !is_a(&e, typ, &self.global_tenv) {
                        Err(format!(
                            "Error: the variable {} is claimed to be a {}!",
                            ident, typ
                        ))
                    } else {
                        self.global_env = Box::new(self.global_env.update(
                            ident.clone(),
                            eval(&e, &self.global_tenv, &self.global_env).unwrap(),
                        ));
                        Ok(None)
                    }
                }
            },
        }
    }
}

pub fn is_a(expr: &Expr, typ: &Expr, tenv: &TypeEnv) -> bool {
    has_type(expr, tenv).map_or(false, |t| t == *typ)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    CannotResolveType(String),
    SyntaxUnaryArgumentTypeMismatch(String, String, String),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeError::CannotResolveType(e) => {
                write!(f, "Cannot resolve the type of expression {}", e)
            }
            TypeError::SyntaxUnaryArgumentTypeMismatch(syntax, expect, actual) => write!(
                f,
                "Argument of {} need to be an instance of {}\n Actual: {}",
                syntax, expect, actual
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    VarClaimedNotDefined(String, Expr),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RuntimeError::VarClaimedNotDefined(ident, typ) => write!(
                f,
                "The variable {} is claimed to be a {}, but is never defined",
                ident, typ
            ),
        }
    }
}

pub fn has_type(expr: &Expr, tenv: &TypeEnv) -> Result<Expr, TypeError> {
    match expr {
        Expr::Var(ident) => match tenv.get(ident) {
            Some(t) => Ok(t.clone()),
            _ => Err(TypeError::CannotResolveType(format!("{}", expr))),
        },
        Expr::TAtom => unimplemented!(),
        Expr::Atom(_) => Ok(Expr::TAtom),
        Expr::App(_, _) => unimplemented!(),
        Expr::TPair(_e1, _e2) => unimplemented!(),
        Expr::Car(e) => match has_type(&e, tenv)? {
            Expr::TPair(t1, _) => Ok(*t1),
            t => Err(TypeError::SyntaxUnaryArgumentTypeMismatch(
                "car".to_string(),
                "Pair".to_string(),
                format!("{}", t),
            )),
        },
        Expr::Cdr(e) => match has_type(&e, tenv)? {
            Expr::TPair(_, t2) => Ok(*t2),
            t => Err(TypeError::SyntaxUnaryArgumentTypeMismatch(
                "cdr".to_string(),
                "Pair".to_string(),
                format!("{}", t),
            )),
        },
        Expr::Cons(e1, e2) => {
            let t1 = has_type(e1, tenv)?;
            let t2 = has_type(e2, tenv)?;
            Ok(Expr::TPair(Box::new(t1), Box::new(t2)))
        }
        Expr::TNat => unimplemented!(),
        Expr::Zero => Ok(Expr::TNat),
        Expr::Succ(e) => match has_type(e, tenv)? {
            Expr::TNat => Ok(Expr::TNat),
            t => Err(TypeError::SyntaxUnaryArgumentTypeMismatch(
                "add1".to_string(),
                "Nat".to_string(),
                format!("{}", t),
            )),
        },
        Expr::Nat(_) => Ok(Expr::TNat),
    }
}

pub fn eval(expr: &Expr, tenv: &TypeEnv, env: &Env) -> Result<Expr, RuntimeError> {
    match expr {
        Expr::Var(ident) => match env.get(ident) {
            Some(e) => Ok(e.clone()),
            None => Err(RuntimeError::VarClaimedNotDefined(
                ident.clone(),
                tenv.get(ident).unwrap().clone(),
            )),
        },
        Expr::App(_, _) => unimplemented!(),
        Expr::Car(e) => match eval(e, &tenv, env)? {
            Expr::Cons(e1, _) => eval(&e1, tenv, env),
            _ => {
                eprintln!("{}", e);
                unreachable!();
            }
        },
        Expr::Cdr(e) => match &**e {
            Expr::Cons(_, e2) => eval(&e2, tenv, env),
            _ => unreachable!(),
        },
        nat @ Expr::Nat(_) => Ok(nat.clone()),
        // Eval for constructors does nothing interesting
        Expr::TAtom
        | Expr::Atom(_)
        | Expr::TPair(_, _)
        | Expr::Cons(_, _)
        | Expr::TNat
        | Expr::Zero
        | Expr::Succ(_) => Ok(expr.clone()),
    }
}

// Similar to eval, except it will eagerly evaluate arguments of constructors
#[allow(dead_code)]
pub fn to_normal_form(expr: &Expr, tenv: &TypeEnv, env: &Env) -> Result<Expr, String> {
    match expr {
        Expr::Var(ident) => Ok(env.get(ident).unwrap().clone()),
        Expr::TAtom | Expr::Atom(_) => Ok(expr.clone()),
        Expr::App(_, _) => unimplemented!(),
        Expr::TPair(e1, e2) => {
            let new_e1 = to_normal_form(e1, tenv, env)?;
            let new_e2 = to_normal_form(e2, tenv, env)?;
            Ok(Expr::TPair(Box::new(new_e1), Box::new(new_e2)))
        }
        Expr::Car(e) => match &**e {
            Expr::Cons(e1, _) => to_normal_form(&e1, tenv, env),
            _ => unreachable!(),
        },
        Expr::Cdr(e) => match &**e {
            Expr::Cons(_, e2) => to_normal_form(&e2, tenv, env),
            _ => unreachable!(),
        },
        Expr::Cons(e1, e2) => {
            let t1 = to_normal_form(e1, tenv, env)?;
            let t2 = to_normal_form(e2, tenv, env)?;
            Ok(Expr::Cons(Box::new(t1), Box::new(t2)))
        }
        Expr::TNat => unimplemented!(),
        Expr::Zero => Ok(Expr::Nat(0)),
        Expr::Succ(e) => {
            let v = to_normal_form(e, tenv, env)?;
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
        Expr::Var(_) => false,
        Expr::TAtom | Expr::TNat | Expr::TPair(_, _) => true,
        Expr::App(_, _) => unimplemented!(),
        _ => false,
    }
}

#[allow(dead_code)]
pub fn is_the_same_as(expr1: &Expr, typ: &Expr, expr2: &Expr, tenv: &TypeEnv, env: &Env) -> bool {
    is_a(expr1, typ, tenv)
        && is_a(expr2, typ, tenv)
        && to_normal_form(expr1, tenv, env) == to_normal_form(expr2, tenv, env)
}

#[allow(dead_code)]
pub fn is_the_same_type(typ1: &Expr, typ2: &Expr, tenv: &TypeEnv) -> bool {
    // Env shouldn't be used
    // TODO(lesley): Better solution for this? Probably seperate types expressions and normal expressions?
    let env = init_global_env();
    is_type(typ1)
        && is_type(typ2)
        && to_normal_form(typ1, tenv, &env) == to_normal_form(typ2, tenv, &env)
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Toplevel};
    use crate::interpreter::*;
    use crate::parser::parse;
    use std::borrow::Borrow;

    fn parse_to_expr(source: &str) -> Expr {
        match parse(source) {
            Ok(exprs) => {
                if exprs.len() != 1 {
                    panic!("Parse result of \"{}\" is not a single expression", source);
                }
                match exprs.first().unwrap() {
                    Toplevel::Expr(e) => e.clone(),
                    _ => panic!("{} is not an expression", source),
                }
            }
            Err(_) => panic!("\"{}\" failed to parse", source),
        }
    }

    fn source_is_a(expr: &str, typ: &str, tenv: &TypeEnv) -> bool {
        is_a(
            parse_to_expr(expr).borrow(),
            parse_to_expr(typ).borrow(),
            tenv,
        )
    }

    fn source_is_type(expr: &str) -> bool {
        is_type(parse_to_expr(expr).borrow())
    }

    fn source_is_the_same_as(e1: &str, typ: &str, e2: &str, tenv: &TypeEnv, env: &Env) -> bool {
        is_the_same_as(
            parse_to_expr(e1).borrow(),
            parse_to_expr(typ).borrow(),
            parse_to_expr(e2).borrow(),
            tenv,
            env,
        )
    }

    fn source_is_the_same_type(typ1: &str, typ2: &str, tenv: &TypeEnv) -> bool {
        is_the_same_type(
            parse_to_expr(typ1).borrow(),
            parse_to_expr(typ2).borrow(),
            tenv,
        )
    }

    #[test]
    fn atom_test() {
        let tenv = &init_global_tenv();
        let env = &init_global_env();

        assert!(source_is_a("'atom", "Atom", tenv));
        assert!(source_is_a("'ratatouille", "Atom", tenv));
        assert!(source_is_a("'obviously-an-atom", "Atom", tenv));
        assert!(source_is_a("'---", "Atom", tenv));
        assert!(!source_is_a("---", "Atom", tenv));

        assert!(source_is_the_same_as(
            "'citron", "Atom", "'citron", tenv, env
        ));
        assert!(!source_is_the_same_as(
            "'pomme", "Atom", "'orange", tenv, env
        ));

        assert!(!source_is_type("'atom"));
        assert!(source_is_type("Atom"));

        assert!(source_is_the_same_type("Atom", "Atom", tenv));
    }

    #[test]
    fn pair_test() {
        let tenv = &init_global_tenv();
        let env = &init_global_env();

        assert!(source_is_a("(cons 'x 'y)", "(Pair Atom Atom)", &tenv));

        assert!(source_is_the_same_as(
            "(cons 'x 'x)",
            "(Pair Atom Atom)",
            "(cons 'x 'x)",
            tenv,
            env
        ));

        assert!(!source_is_the_same_as(
            "(cons 'x 'y)",
            "(Pair Atom Atom)",
            "(cons 'y 'y)",
            tenv,
            env
        ));

        assert!(!source_is_type("(cons 'x 'y)"));
        assert!(source_is_type("(Pair Atom Atom)"));

        assert!(!source_is_the_same_type("(Pair Atom Atom)", "Atom", tenv));
        assert!(source_is_the_same_type(
            "(Pair Atom Atom)",
            "(Pair Atom Atom)",
            tenv
        ));
    }

    #[test]
    fn car_cdr_test() {
        let tenv = &init_global_tenv();
        let env = &init_global_env();

        assert!(source_is_a(
            "(car (cons (cons 'x 'y) 'z))",
            "(Pair Atom Atom)",
            tenv
        ));
        assert!(source_is_a("(cdr (cons (cons 'x 'y) 'z))", "Atom", tenv));

        assert!(source_is_the_same_as(
            "(cdr (cons (cons 'x 'y) 'z))",
            "Atom",
            "'z",
            tenv,
            env
        ));

        assert!(source_is_the_same_as(
            "(cons 'a (car (cons 'b 'c)))",
            "(Pair Atom Atom)",
            "(cons 'a 'b)",
            tenv,
            env
        ));

        assert!(!source_is_type("(car (cons 'x 'y))"));
        assert!(!source_is_type("(cdr (cons 'x 'y))"));
    }

    #[test]
    fn natural_test() {
        let tenv = &init_global_tenv();
        let env = &init_global_env();

        assert!(source_is_a("0", "Nat", tenv));
        assert!(source_is_a("42", "Nat", tenv));
        assert!(!source_is_a("-42", "Nat", tenv));

        assert!(source_is_the_same_as("0", "Nat", "zero", tenv, env));
        assert!(source_is_the_same_as("1", "Nat", "(add1 zero)", tenv, env));
    }
}
