//! Tokens del compilador

/// Tipos de tokens
///
/// Enumerador que contiene todos los tipos de tokens que puede haber en el compilador
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    PREFIX,
    SOURCE,
    QUERY,
    IDENT,
    URI,
    SOURCEPATH,
    QUERYDEFINITION,
    COLON,
    EOF,
}

pub const PREFIX: &str = "PREFIX";
pub const SOURCE: &str = "SOURCE";
pub const QUERY: &str = "QUERY";
pub const COLON: &str = ":";
pub const EOF: &str = " ";

/// Estructura de los token
///
/// # Campos
/// * lexema - El literal del token, es decir, su valor
/// * tipo de token - El tipo al que pertenece el token
/// * num_line - El número de línea del fichero de entrada en el que se encuentra el token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub lexeme: String,
    pub token_type: TokenType,
    pub num_line: u16,
}

impl Token {
    /// Crea un nuevo token y lo devuelve
    ///
    /// Los valores del token serán los que se le pasen como parámetro excepto el de número de línea, el cual es 0 dado que,
    /// cuando se crea el token, no es posible saber en que línea se encuentra
    ///
    /// # Argumentos
    /// * `lexeme` - El lexema del token
    /// * `token_type` - EL tipo del token
    pub fn new(lexeme: String, token_type: TokenType) -> Self {
        Token {
            lexeme,
            token_type,
            num_line: 0,
        }
    }

    /// Modifica el valor del número de línea en el que se encuentra el token en el fichero de entrada
    ///
    /// # Argumentos
    /// * `self` - El propio token
    /// * `new_line` - El nuevo número de línea donde está el token
    pub fn set_num_line(&mut self, new_line: u16) {
        self.num_line = new_line;
    }

    /// Crea un token de tipo EOF y lo devuelve
    ///
    /// Debido a que se trata de un token especial, se crea en una función con el lexema vacío
    ///
    /// # Retorna
    /// A si mismo
    pub fn create_eof_token(num_line: u16) -> Self {
        Token {
            lexeme: " ".to_string(),
            token_type: TokenType::EOF,
            num_line: num_line,
        }
    }
}

pub struct TestTokens;

impl TestTokens {
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