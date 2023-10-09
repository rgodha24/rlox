use std::fmt::Display;

use crate::{
    environment::Environment,
    expr::{BinaryOperator, Expr, Literal, UnaryOperator},
    parser::Stmt,
};

use BinaryOperator as BO;
use RuntimeErrorType as RE;
use UnaryOperator as UO;
use Value as V;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Number(n) => Self::Number(n),
            Literal::String(s) => Self::String(s),
            Literal::True => Self::Boolean(true),
            Literal::False => Self::Boolean(false),
            Literal::Nil => Self::Nil,
            Literal::Identifier(_) => unreachable!(),
        }
    }
}

// probably gonna be useful eventually
#[derive(Debug, Clone, Default)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn interpret_stmt(&mut self, expr: Stmt) -> Result<(), RuntimeErrorType> {
        match expr {
            Stmt::Expr(expr) => {
                let value = expr.evaluate(self)?;
                println!("{}", value);
            }
            Stmt::Print(expr) => {
                let value = expr.evaluate(self)?;
                println!("{}", value);
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => expr.evaluate(self)?,
                    None => V::Nil,
                };

                self.env.define(name, value);
            }
        };
        Ok(())
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeErrorType> {
        for stmt in stmts {
            self.interpret_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Expr {
    pub fn evaluate(self, interpreter: &mut Interpreter) -> Result<Value, RuntimeErrorType> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left = left.evaluate(interpreter)?;
                let right = right.evaluate(interpreter)?;

                match (left, operator, right) {
                    (V::Number(l), BO::Minus, V::Number(r)) => Ok(V::Number(l - r)),
                    (V::Number(l), BO::Plus, V::Number(r)) => Ok(V::Number(l + r)),
                    (V::Number(l), BO::Slash, V::Number(r)) => Ok(V::Number(l / r)),
                    (V::Number(l), BO::Star, V::Number(r)) => Ok(V::Number(l * r)),

                    (V::String(l), BO::Plus, V::String(r)) => Ok(V::String(l + &r)),

                    (V::Number(l), BO::Greater, V::Number(r)) => Ok(V::Boolean(l > r)),
                    (V::Number(l), BO::GreaterEqual, V::Number(r)) => Ok(V::Boolean(l >= r)),
                    (V::Number(l), BO::Less, V::Number(r)) => Ok(V::Boolean(l < r)),
                    (V::Number(l), BO::LessEqual, V::Number(r)) => Ok(V::Boolean(l <= r)),

                    // rust's PartialEq handles all of the cases for us
                    (l, BO::EqualEqual, r) => Ok(V::Boolean(l == r)),
                    (l, BO::BangEqual, r) => Ok(V::Boolean(l != r)),

                    (l, o, r) => Err(RE::IllegalOperation {
                        operator: o,
                        left: l,
                        right: r,
                    }),
                }
            }
            Expr::Grouping { expression } => expression.evaluate(interpreter),
            Expr::Literal { value } => Ok(value.into()),
            Expr::Unary { operator, right } => {
                let right = right.evaluate(interpreter)?;
                match operator {
                    UO::Minus => match right {
                        V::Number(n) => Ok(V::Number(-n)),
                        _ => Err(RE::NegativeNonNumber(right)),
                    },
                    UO::Bang => Ok(V::Boolean(!right.is_truthy())),
                }
            }
            Expr::Variable { name } => interpreter.env.get(name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorType {
    /// trying to set non-number Value to a negative
    NegativeNonNumber(Value),

    /// operation that is not allowed/implemented
    IllegalOperation {
        operator: BinaryOperator,
        left: Value,
        right: Value,
    },

    /// trying to access a variable that doesn't exist
    UndefinedVariable(String),
}

impl Display for RuntimeErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RE::NegativeNonNumber(v) => write!(f, "Cannot negate non-number value: {}", v),
            RE::IllegalOperation {
                operator,
                left,
                right,
            } => {
                write!(f, "Illegal operation: {} {} {}", left, operator, right)
            }
            RE::UndefinedVariable(name) => write!(f, "Undefined variable access: {}", name),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil | Self::Boolean(false) => false,
            _ => true,
        }
    }
}
