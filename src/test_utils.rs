use crate::model::lexer::token::*;

pub struct TestUtilities;

impl TestUtilities {
    pub fn prefix_test_token(num_line: u16) -> Token {
        let mut token = Token::new(PREFIX.to_string(), TokenType::PREFIX);
        token.set_num_line(num_line);
        token
    }

    pub fn source_test_token(num_line: u16) -> Token {
        let mut token = Token::new(SOURCE.to_string(), TokenType::SOURCE);
        token.set_num_line(num_line);
        token
    }

    pub fn query_test_token(num_line: u16) -> Token {
        let mut token = Token::new(QUERY.to_string(), TokenType::QUERY);
        token.set_num_line(num_line);
        token
    }

    pub fn iterator_test_token(num_line: u16) -> Token {
        let mut token = Token::new(ITERATOR.to_string(), TokenType::ITERATOR);
        token.set_num_line(num_line);
        token
    }

    pub fn colon_test_token(num_line: u16) -> Token {
        let mut token = Token::new(COLON.to_string(), TokenType::COLON);
        token.set_num_line(num_line);
        token
    }

    pub fn ident_test_token(ident: &str, num_line: u16) -> Token {
        let mut token = Token::new(ident.to_string(), TokenType::IDENT);
        token.set_num_line(num_line);
        token
    }

    pub fn uri_test_token(uri: &str, num_line: u16) -> Token {
        let mut token = Token::new(uri.to_string(), TokenType::URI);
        token.set_num_line(num_line);
        token
    }

    pub fn source_path_test_token(file_path: &str, num_line: u16) -> Token {
        let mut token = Token::new(file_path.to_string(), TokenType::SOURCEPATH);
        token.set_num_line(num_line);
        token
    }

    pub fn query_definition_test_token(query_definition: &str, num_line: u16) -> Token {
        let mut token = Token::new(query_definition.to_string(), TokenType::QUERYDEFINITION);
        token.set_num_line(num_line);
        token
    }

    pub fn eof_test_token(num_line: u16) -> Token {
        let token = Token::create_eof_token(num_line);
        token
    }
}
