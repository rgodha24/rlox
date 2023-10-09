use std::{env, path::Path};

use parser::Parser;
use tokens::Token;

use crate::{eval::Interpreter, tokens::Lexer};

mod errors;
mod eval;
mod expr;
mod tokens;
#[macro_use]
mod macros;
mod environment;
mod parser;

fn main() {
    let args = env::args().collect::<Vec<String>>();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => println!("Usage: lox [script]"),
    }
}

fn run_prompt() {
    let stdin = std::io::stdin();

    let mut interpreter = Interpreter::default();

    loop {
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

        run(tokens, &mut interpreter, false);
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

    let mut interpreter = Interpreter::default();

    run(tokens, &mut interpreter, true);
}

fn run(tokens: Vec<Token>, interpreter: &'_ mut Interpreter, is_running_file: bool) {
    // display_tokens(&tokens);

    let parser = Parser::new(tokens);
    let stmts = parser.parse();

    match interpreter.interpret(stmts) {
        Ok(result) => result,
        Err(e) => {
            println!("{e}");
            if is_running_file {
                std::process::exit(70);
            }
            return;
        }
    };

    // todo!()
}
