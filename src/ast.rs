pub type ExprList = Vec<Box<Expr>>;

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
    App(Box<Expr>, ExprList),
    // Natural number
    TNat,
    Zero,
    Succ(Box<Expr>),
    Nat(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Toplevel {
    Expr(Box<Expr>),
    Claim(String, Box<Expr>),
    Define(String, Box<Expr>),
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
            Expr::App(func, args) => write!(f, "({} {})", *func, expr_list_to_string(args)),
            Expr::TNat => write!(f, "Nat"),
            Expr::Zero => write!(f, "zero"),
            Expr::Succ(e) => write!(f, "(add1 {})", e),
            Expr::Nat(n) => write!(f, "{}", n),
        }
    }
}

pub fn expr_list_to_string(list: &[Box<Expr>]) -> String {
    let mut ret = String::new();
    for i in 0..list.len() {
        ret.push_str(&format!("{}", list[i]));
        if i < list.len() - 1 {
            ret.push_str(" ");
        }
    }
    ret
}
