use num_bigint::BigUint;

pub type ExprList = Vec<Expr>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
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
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Toplevel {
    Expr(Expr),
    Claim(String, Expr),
    Define(String, Expr),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExprKind::Atom(ident) => write!(f, "'{}", ident),
            ExprKind::Var(ident) => write!(f, "{}", ident),
            ExprKind::TAtom => write!(f, "Atom"),
            ExprKind::TPair(t1, t2) => write!(f, "(Pair {} {})", t1, t2),
            ExprKind::Cons(e1, e2) => write!(f, "(cons {} {})", e1, e2),
            ExprKind::Car(e) => write!(f, "(car {})", e),
            ExprKind::Cdr(e) => write!(f, "(cdr {})", e),
            ExprKind::Lambda(params, body) => write!(
                f,
                "(lambda ({}) {})",
                list_to_space_seperated_string(params),
                body
            ),
            ExprKind::TArr(params, ret) => write!(
                f,
                "(-> {} {})",
                list_to_space_seperated_string(params),
                *ret
            ),
            ExprKind::App(func, args) => {
                write!(f, "({} {})", *func, list_to_space_seperated_string(args))
            }
            ExprKind::TNat => write!(f, "Nat"),
            ExprKind::Zero => write!(f, "zero"),
            ExprKind::Succ(e) => write!(f, "(add1 {})", e),
            ExprKind::Nat(n) => write!(f, "{}", n),
        }
    }
}

fn list_to_space_seperated_string<T: std::fmt::Display>(list: &[T]) -> String {
    let mut ret = String::new();
    for i in 0..list.len() {
        ret.push_str(&list[i].to_string());
        if i < list.len() - 1 {
            ret.push(' ');
        }
    }
    ret
}
