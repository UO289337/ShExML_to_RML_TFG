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

    pub fn field_test_token(num_line: u16) -> Token {
        let mut token = Token::new(FIELD.to_string(), TokenType::Field);
        token.set_num_line(num_line);
        token
    }

    pub fn sql_type_test_token(num_line: u16) -> Token {
        let mut token = Token::new(SQL_TYPE.to_string(), TokenType::SqlType);
        token.set_num_line(num_line);
        token
    }

    pub fn csv_per_row_test_token(num_line: u16) -> Token {
        let mut token = Token::new(CSV_PER_ROW.to_string(), TokenType::CsvPerRow);
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
        let mut token = Token::new(
            RIGHT_ANGLE_BRACKET.to_string(),
            TokenType::RightAngleBracket,
        );
        token.set_num_line(num_line);
        token
    }

    pub fn opening_curly_brace_test_token(num_line: u16) -> Token {
        let mut token = Token::new(
            OPENING_CURLY_BRACE.to_string(),
            TokenType::OpeningCurlyBrace,
        );
        token.set_num_line(num_line);
        token
    }

    pub fn closing_curly_brace_test_token(num_line: u16) -> Token {
        let mut token = Token::new(
            CLOSING_CURLY_BRACE.to_string(),
            TokenType::ClosingCurlyBrace,
        );
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

    pub fn sql_query_test_token(query_definition: &str, num_line: u16) -> Token {
        let mut token = Token::new(query_definition.to_string(), TokenType::SqlQuery);
        token.set_num_line(num_line);
        token
    }

    pub fn eof_test_token(num_line: u16) -> Token {
        let token = Token::create_eof_token(num_line);
        token
    }
}
