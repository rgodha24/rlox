use std::fmt::{Debug, Display};

use crate::{
    convert,
    tokens::{Token, TokenType},
};

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: UnaryOperator,
        right: Box<Expr>,
    },
    Variable {
        name: String,
    },
}

impl Expr {
    pub fn binary(left: Expr, operator: BinaryOperator, right: Expr) -> Self {
        Self::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
    pub fn unary(operator: UnaryOperator, right: Expr) -> Self {
        Self::Unary {
            operator,
            right: Box::new(right),
        }
    }
    pub fn literal(value: Literal) -> Self {
        Self::Literal { value }
    }
    pub fn grouping(expression: Expr) -> Self {
        Self::Grouping {
            expression: Box::new(expression),
        }
    }
    pub fn variable(name: String) -> Self {
        Self::Variable { name }
    }
}

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
    Nil,
    True,
    False,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Slash,
    Star,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    Plus,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "({operator} ({left}) ({right}))"),
            Expr::Grouping { expression } => {
                write!(f, "group {expression}",)
            }
            Expr::Literal { value } => write!(f, "{value}",),
            Expr::Unary { operator, right } => write!(f, "{operator} {right}",),
            Expr::Variable { name } => write!(f, "var: {name}",),
        }
    }
}

impl TryFrom<TokenType> for BinaryOperator {
    type Error = ();
    fn try_from(token: TokenType) -> Result<BinaryOperator, ()> {
        convert!(TokenType; BinaryOperator; token; Slash,Star,BangEqual,Equal,EqualEqual,Greater,GreaterEqual,Less,LessEqual,Minus,Plus,;)
    }
}

impl Into<TokenType> for &BinaryOperator {
    fn into(self) -> TokenType {
        let token = convert!(BinaryOperator; TokenType; self; Slash,Star,BangEqual,Equal,EqualEqual,Greater,GreaterEqual,Less,LessEqual,Minus,Plus,;);
        // unfaillable but my macro just sucks lmao
        token.unwrap()
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token: TokenType = self.into();
        write!(f, "{token}")
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token: TokenType = self.into();
        write!(f, "{token}")
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token: TokenType = self.into();
        write!(f, "{token}")
    }
}

impl TryFrom<TokenType> for UnaryOperator {
    type Error = ();
    fn try_from(token: TokenType) -> Result<UnaryOperator, ()> {
        convert!(TokenType; UnaryOperator; token; Minus,Bang,;)
    }
}

impl Into<TokenType> for &UnaryOperator {
    fn into(self) -> TokenType {
        let token = convert!(UnaryOperator; TokenType; self; Minus,Bang,;);
        // unfaillable but my macro just sucks lmao
        token.unwrap()
    }
}

impl Into<TokenType> for &Literal {
    fn into(self) -> TokenType {
        let token = convert!(Literal; TokenType; self; Nil,True,False,;Identifier,Number,String,);
        // unfaillable but my macro just sucks lmao
        token.unwrap()
    }
}

impl TryFrom<TokenType> for Literal {
    type Error = ();
    fn try_from(token: TokenType) -> Result<Literal, ()> {
        convert!(TokenType; Literal; token; Nil,True,False,;Identifier,Number,String,)
    }
}
