#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    PREFIX,
    SOURCE,
    IDENT,
    URI,
}

#[derive(Debug)]
pub struct Token {
    lexeme: String,
    token_type: TokenType,
}

impl Token {
    pub fn new(lexeme: String, token_type: TokenType) -> Self {
        Token { 
            lexeme, 
            token_type 
        }
    }

    pub fn get_token_type(&self) -> TokenType {
        self.token_type
    }

    pub fn get_lexeme(&self) -> &str {
        self.lexeme.as_str()
    }
}