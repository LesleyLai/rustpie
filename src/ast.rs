pub type ExprList = Vec<Box<Expr>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    TAtom,
    Atom(String),
    TPair(Box<Expr>, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    Car(Box<Expr>),
    Cdr(Box<Expr>),
    App(Box<Expr>, ExprList),
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
