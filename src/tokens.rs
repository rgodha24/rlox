use std::fmt::Display;

use crate::errors::{ErrorType, SyntaxError};

pub struct Lexer<'a> {
    lox: &'a str,
    start: usize,
    current: usize,
    line: usize,
    errs: Vec<SyntaxError<'a>>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn tokenize(mut self) -> Result<Vec<Token>, Vec<SyntaxError<'a>>> {
        use TokenType as T;

        while !self.is_at_end() {
            self.start = self.current;

            let c = self.advance();
            let line = self.line;
            match c {
                '(' => self.add_token(T::LeftParen),
                ')' => self.add_token(T::RightParen),
                '{' => self.add_token(T::LeftBrace),
                '}' => self.add_token(T::RightBrace),
                ',' => self.add_token(T::Comma),
                '.' => self.add_token(T::Dot),
                '-' => self.add_token(T::Minus),
                '+' => self.add_token(T::Plus),
                ';' => self.add_token(T::Semicolon),
                '*' => self.add_token(T::Star),

                '!' => {
                    let token_type = if self.next_is('=') {
                        T::BangEqual
                    } else {
                        T::Bang
                    };
                    self.add_token(token_type);
                }
                '=' => {
                    let token_type = if self.next_is('=') {
                        T::EqualEqual
                    } else {
                        T::Equal
                    };
                    self.add_token(token_type);
                }
                '<' => {
                    let token_type = if self.next_is('=') {
                        T::LessEqual
                    } else {
                        T::Less
                    };
                    self.add_token(token_type);
                }
                '>' => {
                    let token_type = if self.next_is('=') {
                        T::GreaterEqual
                    } else {
                        T::Greater
                    };
                    self.add_token(token_type);
                }

                '/' => {
                    if self.next_is('/') {
                        // comments
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        self.add_token(T::Slash);
                    }
                }

                ' ' | '\r' | '\t' => {}
                '\n' => self.line += 1,

                '"' => {
                    while self.peek() != '"' && !self.is_at_end() {
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }

                    if self.is_at_end() {
                        self.errs.push(SyntaxError {
                            line,
                            location: "",
                            error_type: ErrorType::UnterminatedString(
                                self.lox[self.start..self.current].to_string(),
                            ),
                        });
                    } else {
                        // last "
                        self.advance();

                        self.add_token(T::String(
                            self.lox[self.start + 1..self.current - 1].to_string(),
                        ));
                    }
                }

                // god i love rust
                '0'..='9' => {
                    while self.peek().is_numeric() {
                        self.advance();
                    }

                    if self.peek() == '.' && self.peek_next().is_numeric() {
                        self.advance();

                        while self.peek().is_numeric() {
                            self.advance();
                        }
                    }

                    self.add_token(T::Number(
                        self.lox[self.start..self.current].parse().unwrap(),
                    ));
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    while self.peek().is_alphanumeric() {
                        self.advance();
                    }

                    let text: &str = &self.lox[self.start..self.current];

                    let token_type = match text {
                        "and" => T::And,
                        "class" => T::Class,
                        "else" => T::Else,
                        "false" => T::False,
                        "for" => T::For,
                        "fun" => T::Fun,
                        "if" => T::If,
                        "nil" => T::Nil,
                        "or" => T::Or,
                        "print" => T::Print,
                        "return" => T::Return,
                        "super" => T::Super,
                        "this" => T::This,
                        "true" => T::True,
                        "var" => T::Var,
                        "while" => T::While,
                        t => T::Identifier(t.to_string()),
                    };

                    self.add_token(token_type);
                }

                c => {
                    self.errs.push(SyntaxError {
                        line,
                        location: "",
                        error_type: ErrorType::UnexpectedCharacter(c),
                    });
                }
            }
        }

        self.tokens.push(Token {
            token_type: T::EOF,
            line: self.line,
            lexeme: "".to_string(),
        });

        if self.errs.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errs)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.lox.len()
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            lexeme: self.lox[self.start..self.current].to_string(),
            line: self.line,
        });
    }

    fn next_is(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        } else {
            self.current += 1;
            return true;
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.lox.chars().nth(self.current - 1).unwrap()
    }

    fn peek(&self) -> char {
        self.lox.chars().nth(self.current).unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        self.lox.chars().nth(self.current + 1).unwrap_or('\0')
    }

    pub fn new(lox: &'a str) -> Lexer<'a> {
        Self {
            lox,
            start: 0,
            current: 0,
            line: 1,
            errs: vec![],
            tokens: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    // single char
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two chars
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals/values
    Identifier(String),
    String(String),
    Number(f64),
    Nil,
    True,
    False,

    // keywords
    And,
    Class,
    Else,
    Fun,
    For,
    If,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,

    EOF,
}

// I love chatgpt
impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // single char
            (TokenType::LeftParen, TokenType::LeftParen)
            | (TokenType::RightParen, TokenType::RightParen)
            | (TokenType::LeftBrace, TokenType::LeftBrace)
            | (TokenType::RightBrace, TokenType::RightBrace)
            | (TokenType::Comma, TokenType::Comma)
            | (TokenType::Dot, TokenType::Dot)
            | (TokenType::Minus, TokenType::Minus)
            | (TokenType::Plus, TokenType::Plus)
            | (TokenType::Semicolon, TokenType::Semicolon)
            | (TokenType::Slash, TokenType::Slash)
            | (TokenType::Star, TokenType::Star)
            | (TokenType::Bang, TokenType::Bang)
            | (TokenType::BangEqual, TokenType::BangEqual)
            | (TokenType::Equal, TokenType::Equal)
            | (TokenType::EqualEqual, TokenType::EqualEqual)
            | (TokenType::Greater, TokenType::Greater)
            | (TokenType::GreaterEqual, TokenType::GreaterEqual)
            | (TokenType::Less, TokenType::Less)
            | (TokenType::LessEqual, TokenType::LessEqual)
            | (TokenType::And, TokenType::And)
            | (TokenType::Class, TokenType::Class)
            | (TokenType::Else, TokenType::Else)
            | (TokenType::Fun, TokenType::Fun)
            | (TokenType::For, TokenType::For)
            | (TokenType::If, TokenType::If)
            | (TokenType::Or, TokenType::Or)
            | (TokenType::Print, TokenType::Print)
            | (TokenType::Return, TokenType::Return)
            | (TokenType::Super, TokenType::Super)
            | (TokenType::This, TokenType::This)
            | (TokenType::Var, TokenType::Var)
            | (TokenType::While, TokenType::While)
            | (TokenType::EOF, TokenType::EOF) => true,

            // literals/values
            (TokenType::Identifier(_), TokenType::Identifier(_))
            | (TokenType::String(_), TokenType::String(_))
            | (TokenType::Number(_), TokenType::Number(_))
            | (TokenType::Nil, TokenType::Nil)
            | (TokenType::True, TokenType::True)
            | (TokenType::False, TokenType::False) => true,

            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.token_type, self.lexeme)
    }
}

impl Into<String> for &TokenType {
    fn into(self) -> String {
        match self {
            // single char
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::LeftBrace => "{".to_string(),
            TokenType::RightBrace => "}".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Slash => "/".to_string(),
            TokenType::Star => "*".to_string(),

            // one or two chars
            TokenType::Bang => "!".to_string(),
            TokenType::BangEqual => "!=".to_string(),
            TokenType::Equal => "=".to_string(),
            TokenType::EqualEqual => "==".to_string(),
            TokenType::Greater => ">".to_string(),
            TokenType::GreaterEqual => ">=".to_string(),
            TokenType::Less => "<".to_string(),
            TokenType::LessEqual => "<=".to_string(),

            // literals
            TokenType::Identifier(t) => format!("Identifier({})", t),

            // values
            TokenType::String(s) => format!("String({})", s),
            TokenType::Number(n) => format!("Number({})", n.to_string()),
            TokenType::Nil => "nil".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::False => "false".to_string(),

            // keywords
            TokenType::And => "and".to_string(),
            TokenType::Class => "class".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::Fun => "fun".to_string(),
            TokenType::For => "for".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Print => "print".to_string(),
            TokenType::Return => "return".to_string(),
            TokenType::Super => "super".to_string(),
            TokenType::This => "this".to_string(),
            TokenType::Var => "var".to_string(),
            TokenType::While => "while".to_string(),

            TokenType::EOF => "EOF".to_string(),
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_str: String = self.into();

        write!(f, "{}", token_str)
    }
}
