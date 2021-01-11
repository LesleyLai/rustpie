#![feature(iterator_fold_self)]

mod ast;
mod interpreter;
mod parser;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter::Interpreter;

const RUSTPIE_VERSION: &str = env!("CARGO_PKG_VERSION");

fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".rustpie-history.txt").is_err() {
        println!("No previous history.");
    }
    println!("Rustpie v{}", RUSTPIE_VERSION);
    println!("Use (exit), Ctrl-C, or Ctrl-D to exit prompt");
    let mut interpreter = Interpreter::new();
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
                            match interpreter.execute(toplevel) {
                                Ok(None) => (),
                                Ok(Some(msg)) => println!("{}", msg),
                                Err(e) => eprintln!("{}", e),
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
