use std::fmt;

#[derive(Debug, Clone)]
pub struct ParserError {
    message: String,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl ParserError {
    pub fn new(message: String) -> Self {
        ParserError {
            message
        }
    }
}