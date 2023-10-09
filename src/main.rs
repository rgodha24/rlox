use std::{env, path::Path};

use parser::Parser;
use tokens::Token;

use crate::{
    eval::Interpreter,
    expr::{BinaryOperator, Expr, Literal, UnaryOperator},
    tokens::Lexer,
};

mod errors;
mod eval;
mod expr;
mod tokens;
#[macro_use]
mod macros;
mod parser;

fn main() {
    let args = env::args().collect::<Vec<String>>();

    match args.len() {
        1 => run_prompt(),
        // 2 => run_file(&args[2]),
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

        run(tokens, &mut interpreter);
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

    run(tokens, &mut interpreter);
}

fn run(tokens: Vec<Token>, interpreter: &mut Interpreter) {
    // display_tokens(&tokens);

    let parser = Parser::new(tokens);
    let expr = match parser.parse() {
        Ok(expr) => expr,
        Err(_) => return,
    };

    println!("evaluating {expr}");

    let result = match expr.evaluate(interpreter) {
        Ok(result) => result,
        Err(e) => {
            println!("{e}");
            return;
        }
    };

    println!("{result}");

    // todo!()
}
