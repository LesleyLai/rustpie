mod ast;
mod interpreter;
mod parser;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::ast::Toplevel;
use crate::interpreter::{eval, global_env, global_tenv, has_type, is_a, is_type};

const RUSTPIE_VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".rustpie-history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Rustpie v{}", RUSTPIE_VERSION);
    println!("Use (exit), Ctrl-C, or Ctrl-D to exit prompt");
    let mut tenv = global_tenv();
    let mut env = global_env();
    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.starts_with("(exit)") {
                    break;
                }
                let ast = parser::parse(&line);
                match ast {
                    Err(e) => eprintln!("{}", e),
                    Ok(ast) => {
                        for toplevel in ast.iter() {
                            match toplevel {
                                Toplevel::Expr(e) => match has_type(e, &tenv) {
                                    Err(type_error) => {
                                        eprintln!("{}", type_error);
                                        break;
                                    }
                                    Ok(typ) => match eval(e, &tenv, &env) {
                                        Err(runtime_error) => {
                                            eprintln!("{}", runtime_error);
                                            break;
                                        }
                                        Ok(val) => println!("(the {} {})", typ, val),
                                    },
                                },
                                Toplevel::Claim(ident, e) => {
                                    if (!is_type(e)) {
                                        eprintln!("{} is not a type!", e);
                                    } else if (tenv.contains_key(ident)) {
                                        eprintln!(
                                            "Error: the variable {} is already claimed!",
                                            ident
                                        );
                                    } else {
                                        tenv.insert(ident.clone(), eval(e, &tenv, &env).unwrap());
                                    }
                                }
                                Toplevel::Define(ident, e) => match (tenv.get(ident)) {
                                    None => {
                                        eprintln!("Error: the variable {} is never claimed!", ident)
                                    }
                                    Some(typ) => {
                                        if !is_a(e, typ, &tenv) {
                                            eprintln!(
                                                "Error: the variable {} is claimed to be a {}!",
                                                ident, typ
                                            );
                                        } else {
                                            env.insert(
                                                ident.clone(),
                                                eval(e, &tenv, &env).unwrap(),
                                            );
                                        }
                                    }
                                },
                            }
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(".rustpie-history.txt").unwrap();
}

fn main() {
    repl();
}
