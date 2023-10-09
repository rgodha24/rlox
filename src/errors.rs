use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct SyntaxError<'a> {
    pub line: usize,
    pub location: &'a str,
    pub error_type: ErrorType,
}

impl Display for SyntaxError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {}] Error {}: {}",
            self.line, self.location, self.error_type
        )
    }
}

#[derive(Debug)]
pub enum ErrorType {
    UnexpectedCharacter(char),
    UnterminatedString(String),
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorType::UnexpectedCharacter(c) => {
                write!(f, "ERROR: Unexpected character: \"{}\"", c)
            }
            ErrorType::UnterminatedString(s) => write!(f, "ERROR: Unterminated string: \"{}\"", s),
        }
    }
}

#[macro_export]
macro_rules! print_syntax_errors {
    ($errors:expr) => {
        for err in $errors {
            println!("{}", err);
        }
    };
}
