use std::fmt;

#[derive(Debug, Clone)]
pub struct SintaxError {
    message: String,
}

impl fmt::Display for SintaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl SintaxError {
    pub fn new(message: String) -> Self {
        SintaxError {
            message
        }
    }
}