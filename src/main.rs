use std::{env, path::Path};

use tokens::Token;

use crate::tokens::Lexer;

mod errors;
mod tokens;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[2]),
        _ => println!("Usage: lox [script]"),
    }
}

fn run_prompt() {
    let stdin = std::io::stdin();
    loop {
        print!("> ");
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();
        if line.is_empty() {
            break;
        }

        // if there's an error, we just move on because we're in a repl
        let tokens = match Lexer::new(&line).tokenize() {
            Ok(tokens) => tokens,
            Err(e) => {
                print_syntax_errors!(e);
                continue;
            }
        };

        run(tokens);
    }
}

fn run_file(path: &str) {
    let path = Path::new(path);
    let source = std::fs::read_to_string(path).expect("file exists");

    let tokens = match Lexer::new(&source).tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            print_syntax_errors!(e);
            std::process::exit(65);
        }
    };

    run(tokens);
}

fn run(tokens: Vec<Token>) {
    display_tokens(tokens);

    // todo!()
}

fn display_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}", token);
    }
}
