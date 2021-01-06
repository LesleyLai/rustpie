pub type ExprList = Vec<Box<Expr>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Atom
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(String),
    Var(String),
    App(ExprList),
}


impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Atom(ident) => write!(f, "(atom {})", ident),
            Expr::Var(ident) => write!(f, "(var {})", ident),
            Expr::App(list) => write!(f, "(apply {})", expr_list_to_string(list)),
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