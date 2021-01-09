mod ast;
mod interpreter;
mod parser;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter::eval;
use crate::interpreter::has_type;

const RUSTPIE_VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".rustpie-history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Rustpie v{}", RUSTPIE_VERSION);
    println!("Use (exit), Ctrl-C, or Ctrl-D to exit prompt");
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
                        for (e, t) in ast.iter().map(|e| (e, has_type(&*e))) {
                            match t {
                                Err(type_error) => {
                                    eprintln!("{}", type_error);
                                    break;
                                }
                                Ok(typ) => match eval(e) {
                                    Err(runtime_error) => {
                                        eprintln!("{}", runtime_error);
                                        break;
                                    }
                                    Ok(val) => println!("(the {} {})", typ, val),
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
