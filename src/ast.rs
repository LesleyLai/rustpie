use num_bigint::BigUint;

pub type ExprList = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    // Atom
    TAtom,
    Atom(String),
    // Pair
    TPair(Box<Expr>, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    Car(Box<Expr>),
    Cdr(Box<Expr>),
    // Function
    Lambda(Vec<String>, Box<Expr>),
    TArr(ExprList, Box<Expr>),
    App(Box<Expr>, ExprList),
    // Natural number
    TNat,
    Zero,
    Succ(Box<Expr>),
    Nat(BigUint),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Toplevel {
    Expr(Expr),
    Claim(String, Expr),
    Define(String, Expr),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Atom(ident) => write!(f, "'{}", ident),
            Expr::Var(ident) => write!(f, "{}", ident),
            Expr::TAtom => write!(f, "Atom"),
            Expr::TPair(t1, t2) => write!(f, "(Pair {} {})", t1, t2),
            Expr::Cons(e1, e2) => write!(f, "(cons {} {})", e1, e2),
            Expr::Car(e) => write!(f, "(car {})", e),
            Expr::Cdr(e) => write!(f, "(cdr {})", e),
            Expr::Lambda(params, body) => write!(
                f,
                "(lambda ({}) {})",
                list_to_space_seperated_string(params),
                body
            ),
            Expr::TArr(params, ret) => write!(
                f,
                "(-> {} {})",
                list_to_space_seperated_string(params),
                *ret
            ),
            Expr::App(func, args) => {
                write!(f, "({} {})", *func, list_to_space_seperated_string(args))
            }
            Expr::TNat => write!(f, "Nat"),
            Expr::Zero => write!(f, "zero"),
            Expr::Succ(e) => write!(f, "(add1 {})", e),
            Expr::Nat(n) => write!(f, "{}", n),
        }
    }
}

fn list_to_space_seperated_string<T: std::fmt::Display>(list: &[T]) -> String {
    let mut ret = String::new();
    for i in 0..list.len() {
        ret.push_str(&format!("{}", list[i]));
        if i < list.len() - 1 {
            ret.push(' ');
        }
    }
    ret
}
