use crate::model::lexer::token::*;

pub struct TestUtilities;

impl TestUtilities {
    pub fn prefix_test_token(num_line: u16) -> Token {
        let mut token = Token::new(PREFIX.to_string(), TokenType::Prefix);
        token.set_num_line(num_line);
        token
    }

    pub fn source_test_token(num_line: u16) -> Token {
        let mut token = Token::new(SOURCE.to_string(), TokenType::Source);
        token.set_num_line(num_line);
        token
    }

    pub fn query_test_token(num_line: u16) -> Token {
        let mut token = Token::new(QUERY.to_string(), TokenType::Query);
        token.set_num_line(num_line);
        token
    }

    pub fn iterator_test_token(num_line: u16) -> Token {
        let mut token = Token::new(ITERATOR.to_string(), TokenType::Iterator);
        token.set_num_line(num_line);
        token
    }

    pub fn colon_test_token(num_line: u16) -> Token {
        let mut token = Token::new(COLON.to_string(), TokenType::Colon);
        token.set_num_line(num_line);
        token
    }

    pub fn left_angle_bracket_test_token(num_line: u16) -> Token {
        let mut token = Token::new(LEFT_ANGLE_BRACKET.to_string(), TokenType::LeftAngleBracket);
        token.set_num_line(num_line);
        token
    }

    pub fn right_angle_bracket_test_token(num_line: u16) -> Token {
        let mut token = Token::new(RIGHT_ANGLE_BRACKET.to_string(), TokenType::RightAngleBracket);
        token.set_num_line(num_line);
        token
    }

    pub fn ident_test_token(ident: &str, num_line: u16) -> Token {
        let mut token = Token::new(ident.to_string(), TokenType::Ident);
        token.set_num_line(num_line);
        token
    }

    pub fn uri_test_token(uri: &str, num_line: u16) -> Token {
        let mut token = Token::new(uri.to_string(), TokenType::Uri);
        token.set_num_line(num_line);
        token
    }

    pub fn jdbc_url_test_token(jdbc_url: &str, num_line: u16) -> Token {
        let mut token = Token::new(jdbc_url.to_string(), TokenType::JdbcUrl);
        token.set_num_line(num_line);
        token
    }

    pub fn file_path_test_token(file_path: &str, num_line: u16) -> Token {
        let mut token = Token::new(file_path.to_string(), TokenType::FilePath);
        token.set_num_line(num_line);
        token
    }

    pub fn path_test_token(path: &str, num_line: u16) -> Token {
        let mut token = Token::new(path.to_string(), TokenType::Path);
        token.set_num_line(num_line);
        token
    }

    pub fn query_definition_test_token(query_definition: &str, num_line: u16) -> Token {
        let mut token = Token::new(query_definition.to_string(), TokenType::QueryDefinition);
        token.set_num_line(num_line);
        token
    }

    pub fn eof_test_token(num_line: u16) -> Token {
        let token = Token::create_eof_token(num_line);
        token
    }
}
