//! Tokens del compilador

/// Tipos de tokens
///
/// Enumerador que contiene todos los tipos de tokens que puede haber en el compilador
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Prefix,
    Source,
    Query,
    Iterator,
    Field,
    Expression,
    Union,
    SqlType,
    CsvPerRow,
    Ident,
    KeyIdentifier,
    Uri,
    JdbcUrl,
    Path,
    SqlQuery,
    Colon,
    SemiColon,
    AccessDot,
    LeftAngleBracket,
    RightAngleBracket,
    OpeningCurlyBrace,
    ClosingCurlyBrace,
    LeftBracket,
    RightBracket,
    EOF,
}

pub const PREFIX: &str = "PREFIX";
pub const SOURCE: &str = "SOURCE";
pub const QUERY: &str = "QUERY";
pub const ITERATOR: &str = "ITERATOR";
pub const FIELD: &str = "FIELD";
pub const EXPRESSION: &str = "EXPRESSION";
pub const UNION: &str = "UNION";
pub const SQL_TYPE: &str = "sql:";
pub const CSV_PER_ROW: &str = "csvperrow";
pub const COLON: &str = ":";
pub const SEMICOLON: &str = ";";
pub const ACCESS_DOT: &str = ".";
pub const LEFT_ANGLE_BRACKET: &str = "<";
pub const RIGHT_ANGLE_BRACKET: &str = ">";
pub const OPENING_CURLY_BRACE: &str = "{";
pub const CLOSING_CURLY_BRACE: &str = "}";
pub const LEFT_BRACKET: &str = "[";
pub const RIGHT_BRACKET: &str = "]";
pub const EOF: &str = " ";

/// Estructura de los token
///
/// # Campos
/// * lexema - El literal del token, es decir, su valor
/// * tipo de token - El tipo al que pertenece el token
/// * num_line - El número de línea del fichero de entrada en el que se encuentra el token
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    lexeme: String,
    token_type: TokenType,
    num_line: u16,
}

impl Token {
    /// Crea un nuevo token y lo devuelve
    ///
    /// Los valores del token serán los que se le pasen como parámetro excepto el de número de línea, el cual es 0 dado que,
    /// cuando se crea el token, no es posible saber en que línea se encuentra
    ///
    /// # Parámetros
    /// * `lexeme` - El lexema del token
    /// * `token_type` - EL tipo del token
    pub fn new(lexeme: &str, token_type: TokenType) -> Self {
        Token {
            lexeme: lexeme.to_string(),
            token_type,
            num_line: 0,
        }
    }

    /// Devuelve el valor del lexema del token
    ///
    /// # Parámetros
    /// * `self` - El propio token
    ///
    /// # Retorna
    /// El lexema del token
    pub fn get_lexeme(&self) -> String {
        self.lexeme.clone()
    }

    /// Devuelve el tipo del token
    ///
    /// # Parámetros
    /// * `self` - El propio token
    ///
    /// # Retorna
    /// El tipo del token
    pub fn get_token_type(&self) -> TokenType {
        self.token_type.clone()
    }

    /// Devuelve el número de línea de la entrada donde se encuentra el token
    ///
    /// # Parámetros
    /// * `self` - El propio token
    ///
    /// # Retorna
    /// El número de línea de la entrada donde se encuentra el token
    pub fn get_num_line(&self) -> u16 {
        self.num_line.clone()
    }

    /// Modifica el valor del número de línea en el que se encuentra el token en el fichero de entrada
    ///
    /// # Parámetros
    /// * `self` - El propio token
    /// * `new_line` - El nuevo número de línea donde está el token
    pub fn set_num_line(&mut self, new_line: u16) {
        self.num_line = new_line;
    }

    /// Crea un token de tipo EOF y lo devuelve
    ///
    /// Debido a que se trata de un token especial, se crea en una función con el lexema vacío
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea en el que se encuentra el token
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

    /// Crea un token para ser utilizado en los tests
    ///
    /// # Parámetros
    /// * `lexeme` - El lexeme (valor) del token
    /// * `num_line` - El número de línea en el que se encuentra el token
    /// * `token_type` - El tipo del token
    pub fn create_test_token(lexeme: &str, num_line: u16, token_type: TokenType) -> Token {
        let mut token = Token::new(lexeme, token_type);
        token.set_num_line(num_line);
        token
    }
}
