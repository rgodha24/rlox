use std::fmt::Display;

use crate::{
    expr::{Expr, Literal},
    tokens::{Token, TokenType},
};

use Literal as L;
use ParserErrorType as PET;
use TokenType as T;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var {
        name: String,
        initializer: Option<Expr>,
    },
}

type FaillableExpr = Result<Expr, ParserError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self) -> Vec<Stmt> {
        let mut statements = vec![];
        while !self.is_at_end() {
            let stmt = self.declaration();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            } else {
                return statements;
            }
        }

        statements
    }

    fn declaration(&mut self) -> Option<Stmt> {
        let stmt = if self.next_is(vec![T::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        match stmt {
            Ok(stmt) => Some(stmt),
            Err(e) => {
                e.report();
                self.synchronize();
                None
            }
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = match self
            .consume(T::Identifier("".to_string()), PET::MissingIdentifier)?
            .token_type
            .clone()
        {
            T::Identifier(name) => name,
            _ => unreachable!(),
        };

        let initializer = if self.next_is(vec![T::Equal]) {
            Some(self.expresssion()?)
        } else {
            None
        };

        self.consume(T::Semicolon, PET::MissingSemicolon)?;

        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.next_is(vec![T::Print]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expresssion()?;
        self.consume(T::Semicolon, PET::MissingSemicolon)?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expresssion()?;
        self.consume(T::Semicolon, PET::MissingSemicolon)?;
        Ok(Stmt::Expr(value))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            match self.peek().token_type {
                T::Class
                | T::Fun
                | T::Var
                | T::For
                | T::If
                | T::While
                | T::Print
                | T::Return
                | T::Semicolon => return,
                _ => self.advance(),
            };
        }
    }

    fn expresssion(&mut self) -> FaillableExpr {
        self.equality()
    }

    fn equality(&mut self) -> FaillableExpr {
        let mut expr = self.comparison()?;

        while self.next_is(vec![T::BangEqual, T::EqualEqual]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.comparison()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn comparison(&mut self) -> FaillableExpr {
        let mut expr = self.term()?;

        while self.next_is(vec![T::Greater, T::GreaterEqual, T::Less, T::LessEqual]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.term()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn term(&mut self) -> FaillableExpr {
        let mut expr = self.factor()?;

        while self.next_is(vec![T::Minus, T::Plus]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.factor()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn factor(&mut self) -> FaillableExpr {
        let mut expr = self.unary()?;

        while self.next_is(vec![T::Slash, T::Star]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.unary()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn unary(&mut self) -> FaillableExpr {
        if self.next_is(vec![T::Bang, T::Minus]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.unary()?;
            return Ok(Expr::unary(token.try_into().unwrap(), right));
        }

        return self.primary();
    }

    fn primary(&mut self) -> FaillableExpr {
        match self.peek().token_type.clone() {
            T::False | T::True | T::Nil | T::Number(_) | T::String(_) => {
                self.advance();
                let token_type = self.previous().token_type.clone();
                Ok(Expr::literal(token_type.try_into().unwrap()))
            }
            T::LeftParen => {
                self.advance();
                let expr = self.expresssion()?;
                self.consume(T::RightParen, PET::UnclosedParenthesis)?;
                Ok(Expr::grouping(expr))
            }

            T::Identifier(name) => {
                self.advance();
                Ok(Expr::variable(name))
            }

            _ => Err(ParserError::new(
                PET::ExpectedExpression,
                self.peek().clone(),
            )),
        }
    }

    fn next_is(&mut self, types: Vec<TokenType>) -> bool {
        for t in types {
            if self.is(t) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().is(TokenType::EOF)
    }

    fn is(&self, t: TokenType) -> bool {
        self.peek().is(t)
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&mut self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn consume(
        &mut self,
        t: TokenType,
        error_type: ParserErrorType,
    ) -> Result<&Token, ParserError> {
        if self.is(t) {
            Ok(self.advance())
        } else {
            Err(ParserError::new(error_type, self.peek().clone()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    error_type: ParserErrorType,
    token: Token,
}

impl ParserError {
    pub fn new(error_type: ParserErrorType, token: Token) -> Self {
        Self { error_type, token }
    }
    pub fn report(&self) {
        println!("{}", self)
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Err: {} at line {}, lexeme: {}",
            self.error_type, self.token.line, self.token.lexeme
        )
    }
}

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    UnclosedParenthesis,
    UnexpectedToken,
    ExpectedExpression,
    MissingSemicolon,
    MissingIdentifier,
}

impl Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PET::UnclosedParenthesis => write!(f, "Unclosed parenthesis"),
            PET::UnexpectedToken => write!(f, "Unexpected token"),
            PET::ExpectedExpression => write!(f, "Expected expression"),
            PET::MissingSemicolon => write!(f, "Missing semicolon"),
            PET::MissingIdentifier => write!(f, "Missing Variable Name"),
        }
    }
}
