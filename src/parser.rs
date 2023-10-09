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

type FaillableExpr = Result<Expr, ParserError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(mut self) -> FaillableExpr {
        self.expresssion()
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

        while self.is_any(vec![T::BangEqual, T::EqualEqual]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.comparison()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn comparison(&mut self) -> FaillableExpr {
        let mut expr = self.term()?;

        while self.is_any(vec![T::Greater, T::GreaterEqual, T::Less, T::LessEqual]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.term()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn term(&mut self) -> FaillableExpr {
        let mut expr = self.factor()?;

        while self.is_any(vec![T::Minus, T::Plus]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.factor()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn factor(&mut self) -> FaillableExpr {
        let mut expr = self.unary()?;

        while self.is_any(vec![T::Slash, T::Star]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.unary()?;
            expr = Expr::binary(expr, token.try_into().unwrap(), right);
        }

        return Ok(expr);
    }

    fn unary(&mut self) -> FaillableExpr {
        if self.is_any(vec![T::Bang, T::Minus]) {
            let operator = self.previous();
            let token = operator.token_type.clone();
            let right = self.unary()?;
            return Ok(Expr::unary(token.try_into().unwrap(), right));
        }

        return self.primary();
    }

    fn primary(&mut self) -> FaillableExpr {
        if self.is(T::False) {
            Ok(Expr::literal(L::False))
        } else if self.is(T::True) {
            Ok(Expr::literal(L::True))
        } else if self.is(T::Nil) {
            Ok(Expr::literal(L::Nil))
        } else if self.is_any(vec![T::Number(1.), T::String("".to_string())]) {
            let token = self.previous().token_type.clone();
            Ok(Expr::literal(token.try_into().unwrap()))
        } else if self.is(T::LeftParen) {
            self.advance();
            let expr = self.expresssion()?;
            self.consume(T::RightParen, PET::UnclosedParenthesis)?;
            Ok(Expr::grouping(expr))
        } else {
            Err(ParserError::reported(
                PET::ExpectedExpression,
                self.peek().clone(),
            ))
        }
    }

    fn is_any(&mut self, types: Vec<TokenType>) -> bool {
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
            Err(ParserError::reported(error_type, self.peek().clone()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError {
    error_type: ParserErrorType,
    token: Token,
}

impl ParserError {
    fn new(error_type: ParserErrorType, token: Token) -> Self {
        Self { error_type, token }
    }
    fn reported(error_type: ParserErrorType, token: Token) -> Self {
        let err = Self::new(error_type, token);

        println!("{}", err);

        err
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
enum ParserErrorType {
    UnclosedParenthesis,
    UnexpectedToken,
    ExpectedExpression,
}

impl Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserErrorType::UnclosedParenthesis => write!(f, "Unclosed parenthesis"),
            ParserErrorType::UnexpectedToken => write!(f, "Unexpected token"),
            ParserErrorType::ExpectedExpression => write!(f, "Expected expression"),
        }
    }
}
