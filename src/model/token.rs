#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    PREFIX,
    SOURCE,
    IDENT,
    URI,
    COLON,
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub lexeme: String,
    pub token_type: TokenType,
    pub num_line: u16,
}

impl Token {
    pub fn new(lexeme: String, token_type: TokenType) -> Self {
        Token { 
            lexeme, 
            token_type,
            num_line: 0
        }
    }

    pub fn get_num_line(&self) -> u16 {
        self.num_line
    }

    pub fn set_num_line(&mut self, new_line: u16) {
        self.num_line = new_line;
    }

    pub fn create_eof_token() -> Self {
        Token { lexeme: " ".to_string(), 
            token_type: TokenType::EOF, 
            num_line: 0 
        }
    }
}