use rustyline::error::ReadlineError;
use rustyline::Editor;

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
                println!("Line: {}", line);
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