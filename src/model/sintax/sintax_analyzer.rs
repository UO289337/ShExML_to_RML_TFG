//! Módulo del analizador sintáctico
//!
//! Realiza el análisis sintáctico del compilador
//! Comprueba que los tokens resultado del analizador léxico se encuentran en el orden esperado y genera el AST

use chumsky::combinator::*;
use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::Parser;

use crate::model::lexer::token::*;

use super::super::ast::*;
use super::super::lexer::token::TokenType;

/// Parsea los tokens para generar el nodo File del AST
///
/// Realiza el parseo de los tokens de los prefijos y de las fuentes para poder crear el nodo File, que es el nodo raíz del AST
///
/// # Retorna
/// Un nodo File del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn file_parser() -> impl Parser<Token, FileASTNode, Error = Simple<Token>> {
    prefix_parser()
        .then(source_parser())
        .then(query_parser().or_not())
        .then(eof_detector())
        .map(|(((prefixes, sources), queries), _)| FileASTNode {
            prefixes,
            sources,
            queries,
        })
}

/// Parsea los tokens para generar el nodo Prefix del AST
///
/// Realiza el parseo de los tokens para detectar la secuencia: PREFIX IDENT: URI
///
/// # Retorna
/// Un nodo Prefix del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn prefix_parser() -> impl Parser<Token, Vec<PrefixASTNode>, Error = Simple<Token>> {
    prefix_detector()
        .then(identifier_detector(PREFIX.to_string()))
        .then(colon_detector())
        .then(left_angle_bracket_detector("URI".to_string()))
        .then(uri_detector())
        .then(right_angle_bracket_detector("URI".to_string()))
        .map(|(((((_, ident), _), _), uri), _)| PrefixASTNode {
            identifier: ident.lexeme.clone(),
            uri: uri.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar el nodo Source del AST
///
/// Realiza el parseo de los tokens para detectar la secuencia: SOURCE IDENT SOURCEPATH
///
/// # Retorna
/// Un nodo Source del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn source_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {
    source_detector()
        .then(identifier_detector(SOURCE.to_string()))
        .then(left_angle_bracket_detector("URL o ruta".to_string()))
        .then(
            uri_detector()
                .or(file_path_detector())
                .or(path_detector())
                .or(jdbc_url_detector()),
        )
        .then(right_angle_bracket_detector("URL o ruta".to_string()))
        .map(|((((_, ident), _), source_definition), _)| SourceASTNode {
            identifier: ident.lexeme.clone(),
            source_definition: source_definition.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar el nodo Query del AST
///
/// Realiza el parseo de los tokens para detectar la secuencia: QUERY IDENT QUERYDEFINITION
///
/// # Retorna
/// Un nodo Query del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn query_parser() -> impl Parser<Token, Vec<QueryASTNode>, Error = Simple<Token>> {
    query_detector()
        .then(identifier_detector(QUERY.to_string()))
        .then(left_angle_bracket_detector("consulta SQL".to_string()))
        .then(sql_type_detector())
        .then(sql_query_detector())
        .then(right_angle_bracket_detector("consulta SQL".to_string()))
        .map(|(((((_, ident), _), _), sql_query), _)| QueryASTNode {
            identifier: ident.lexeme.clone(),
            sql_query: sql_query.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

// Detectors

/// Detecta el token Prefix en los tokens
///
/// # Retorna
/// Un token de tipo Prefix si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Prefix
fn prefix_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Prefix,
        format!("Se esperaba un PREFIX en la línea"),
    )
}

/// Detecta el token Source en los tokens
///
/// # Retorna
/// Un token de tipo Source si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source
fn source_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    // Se indica que 'Se esperaba un PREFIX' porque el analizador no tiene manera de saber si lo que hay en la línea del error
    // es un PREFIX o un SOURCE en el caso, por ejemplo, de que encuentre SOURC
    general_detector(
        TokenType::Source,
        format!("Se esperaba un PREFIX o un SOURCE en la línea"),
    )
}

/// Detecta el token QUERY en los tokens
///
/// # Retorna
/// Un token de tipo Query si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Query
fn query_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Query,
        format!("Se esperaba un QUERY en la línea"),
    )
}

/// Detecta el token SqlType en los tokens
///
/// # Retorna
/// Un token de tipo SqlType si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo SqlType
fn sql_type_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::SqlType,
        format!("Se esperaba 'sql:' después de '<' en la línea"),
    )
}

/// Detecta el token IDENT en los tokens
///
/// # Argumentos
/// * `previous_token` - El token previo al identificador, que puede ser un PREFIX o un SOURCE
///
/// # Retorna
/// Un token de tipo Ident si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Ident
fn identifier_detector(
    previous_token: String,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Ident,
        format!("Se esperaba un identificador después de {previous_token} en la línea"),
    )
}

/// Detecta el token URI en los tokens
///
/// # Retorna
/// Un token de tipo Uri si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn uri_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Uri,
        format!("Se esperaba una URI entre '<' y '>' en la línea"),
    )
}

/// Detecta el token JdbcUrl en los tokens
///
/// # Retorna
/// Un token de tipo JdbcUrl si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo JdbcUrl
fn jdbc_url_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::JdbcUrl,
        format!("Se esperaba una URL JDBC entre '<' y '>' en la línea"),
    )
}

/// Detecta el token FilePath en los tokens
///
/// # Retorna
/// Un token de tipo FilePath si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo FilePath
fn file_path_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::FilePath,
        format!("Se esperaba una ruta con file entre '<' y '>' en la línea"),
    )
}

/// Detecta el token Path en los tokens
///
/// # Retorna
/// Un token de tipo Path si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Path
fn path_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Path,
        format!("Se esperaba una ruta entre '<' y '>' en la línea"),
    )
}

/// Detecta el token SqlQuery en los tokens
///
/// # Retorna
/// Un token de tipo SqlQuery si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn sql_query_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::SqlQuery,
        format!("Se esperaba una consulta SQL entre '<' y '>' en la línea"),
    )
}

/// Detecta el token ':' en los tokens
///
/// # Retorna
/// Un token de tipo : si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo :
fn colon_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::Colon,
        format!("Faltan los ':' después del identificador en la línea"),
    )
}

/// Detecta el token '<' en los tokens
///
/// # Retorna
/// Un token de tipo < si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo <
fn left_angle_bracket_detector(
    next_token: String,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::LeftAngleBracket,
        format!("Se esperaba un '<' antes de la {next_token} en la línea"),
    )
}

/// Detecta el token '>' en los tokens
///
/// # Retorna
/// Un token de tipo > si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo >
fn right_angle_bracket_detector(
    previous_token: String,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::RightAngleBracket,
        format!("Se esperaba un '>' después de la {previous_token} en la línea"),
    )
}

/// Detecta el token EOF en los tokens
///
/// # Retorna
/// Un token de tipo EOF si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo >
fn eof_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(
        TokenType::EOF,
        format!("Se ha encontrado una cadena donde debería estar el final del fichero, en la línea"),
    )
}

/// Detecta cualquier token válido
///
/// /// # Argumentos
/// * `token_type` - El tipo de token esperado
/// * `message` - El mensaje de error que se muestra en el caso de que el tipo del token no sea el esperado
///
/// # Retorna
/// El token reconocido
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea del tipo esperado
fn general_detector(
    token_type: TokenType,
    message: String,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    filter(move |token: &Token| token.token_type == token_type)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            let line = token.found().map(|t| t.num_line).unwrap_or(0);
            Simple::custom(token.span(), format!("{message} {}", line))
        })
}

/// Parsea el vector de tokens para generar el AST
///
/// Toma como entrada el vector de tokens resultado del análisis léxico y genera un árbol AST que tiene un nodo File como raíz
///
/// # Argumentos
/// * `tokens` - El vector de tokens resultado del análisis léxico
///
/// # Retorna
/// Un nodo File del AST que será el nodo raíz de este
///
/// # Errores
/// * `[Vec<Simple<Token>>]` - Un vector con los errores que pueden aparecer al realizar el análisis sintáctico
pub fn parser(tokens: Vec<Token>) -> Result<FileASTNode, Vec<Simple<Token>>> {
    let file_parser = file_parser();
    let parsed = file_parser.parse(tokens);

    match parsed {
        Ok(node) => Ok(node),
        Err(e) => Err(e),
    }
}

// Tests

/// Módulo de los tests de los detectors analizador sintáctico
///
/// Contiene los tests de los detectors que se encargan de detectar los tokens provenientes del análisis léxico
#[cfg(test)]
mod sintax_detectors_tests {
    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que se detectan los tokens Prefix
    #[doc(hidden)]
    #[test]
    fn detect_valid_prefix() {
        let expected_token = TestUtilities::prefix_test_token(1);
        let actual = prefix_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Prefix aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_prefix() {
        let actual = prefix_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Source
    #[doc(hidden)]
    #[test]
    fn detect_valid_source() {
        let expected_token = TestUtilities::source_test_token(1);
        let actual = source_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Source aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_source() {
        let actual = source_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Query
    #[doc(hidden)]
    #[test]
    fn detect_valid_query() {
        let expected_token = TestUtilities::query_test_token(1);
        let actual = query_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Query aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_query() {
        let actual = query_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens SqlType
    #[doc(hidden)]
    #[test]
    fn detect_valid_sql_type() {
        let expected_token = TestUtilities::sql_type_test_token(1);
        let actual = sql_type_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens SqlType aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_sql_type() {
        let actual = sql_type_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Ident
    #[doc(hidden)]
    #[test]
    fn detect_valid_identifier() {
        let expected_token = TestUtilities::ident_test_token("ident", 1);
        let actual = identifier_detector("PREFIX".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Ident aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_identifier() {
        let actual = identifier_detector("SOURCE".to_string())
            .parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens URI
    #[doc(hidden)]
    #[test]
    fn detect_valid_uri() {
        let expected_token = TestUtilities::uri_test_token("https://ejemplo.com", 1);
        let actual = uri_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens URI aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_uri() {
        let actual = uri_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens JDBC URL
    #[doc(hidden)]
    #[test]
    fn detect_valid_jdbc_url() {
        let expected_token =
            TestUtilities::jdbc_url_test_token("jdbc:mysql://localhost:3306/mydb", 1);
        let actual = jdbc_url_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens JDBC URL aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_jdbc_url() {
        let actual = jdbc_url_detector().parse(vec![TestUtilities::uri_test_token(
            "https://ejemplo.com",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens FilePath
    #[doc(hidden)]
    #[test]
    fn detect_valid_file_path() {
        let expected_token =
            TestUtilities::file_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 1);
        let actual = file_path_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens FilePath aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_file_path() {
        let actual = jdbc_url_detector().parse(vec![TestUtilities::uri_test_token(
            "https://ejemplo.com",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Path
    #[doc(hidden)]
    #[test]
    fn detect_valid_path() {
        let expected_token = TestUtilities::path_test_token("ejemplo/fichero.csv", 1);
        let actual = path_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Path aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_path() {
        let actual = path_detector().parse(vec![TestUtilities::file_path_test_token(
            "file:///ejemplo/path/a/fichero/fichero.csv",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens SqlQuery
    #[doc(hidden)]
    #[test]
    fn detect_valid_sql_query() {
        let expected_token = TestUtilities::sql_query_test_token("SELECT * FROM example;", 1);
        let actual = sql_query_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens SqlQuery aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_sql_query() {
        let actual = sql_query_detector().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Colon (:)
    #[doc(hidden)]
    #[test]
    fn detect_valid_colon() {
        let expected_token = TestUtilities::colon_test_token(1);
        let actual = colon_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Colon (:) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_colon() {
        let actual = colon_detector().parse(vec![TestUtilities::ident_test_token("ident", 1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens LeftAngleBracket (<)
    #[doc(hidden)]
    #[test]
    fn detect_valid_left_angle_bracket() {
        let expected_token = TestUtilities::left_angle_bracket_test_token(1);
        let actual =
            left_angle_bracket_detector("URI".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens LeftAngleBracket (<) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_left_angle_bracket() {
        let actual = left_angle_bracket_detector("URI".to_string())
            .parse(vec![TestUtilities::right_angle_bracket_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens RightAngleBracket (>)
    #[doc(hidden)]
    #[test]
    fn detect_valid_right_angle_bracket() {
        let expected_token = TestUtilities::right_angle_bracket_test_token(1);
        let actual =
            right_angle_bracket_detector("URI".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens RightAngleBracket (>) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_detect_invalid_right_angle_bracket() {
        let actual = right_angle_bracket_detector("URI".to_string())
            .parse(vec![TestUtilities::left_angle_bracket_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detecta el token EOF
    #[doc(hidden)]
    #[test]
    fn detect_eof() {
        let expected_token = TestUtilities::eof_test_token(1);
        let actual =
            eof_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que el resultado actual del test es igual al esperado
    ///
    /// # Argumentos
    /// * `expected` - El token esperado
    /// * `actual` - El token detectado real
    fn check_ok(expected: Token, actual: Result<Token, Vec<Simple<Token>>>) {
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el resultado actual del test es un error
    ///
    /// # Argumentos
    /// * `actual` - Un Result con el error esperado
    fn check_error(actual: Result<Token, Vec<Simple<Token>>>) {
        assert!(
            actual.is_err(),
            "Se esperaba un error, pero se obtuvo: {:?}",
            actual
        );
    }
}

/// Módulo para los tests del analizador sintáctico
///
/// Contiene los tests que se encargan de comprobar que los diferentes parsers del analizador sintáctico funcionan correctamente
#[cfg(test)]
mod sintax_tests {
    use chumsky::error::SimpleReason;

    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax() {
        let tokens_vector: Vec<Token> = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("example", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://example.com/", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::source_test_token(2),
            TestUtilities::ident_test_token("films_csv_file", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::uri_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::query_test_token(3),
            TestUtilities::ident_test_token("query_sql", 3),
            TestUtilities::left_angle_bracket_test_token(3),
            TestUtilities::sql_type_test_token(3),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 3),
            TestUtilities::right_angle_bracket_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
        };
        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST si no hay query y no hay errores sintácticos
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax_and_withouth_query() {
        let tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("example", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://example.com/", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::source_test_token(2),
            TestUtilities::ident_test_token("films_csv_file", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::uri_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::eof_test_token(3),
        ];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: None,
        };
        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay prefixes
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_prefixes() {
        let tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay sources
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_sources() {
        let tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error dentro porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX o un SOURCE en la línea 1");
    }

    /// Comprueba que el parser de Prefix detecta la secuencia de tokens: Prefix Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_prefix_sintax() {
        let mut tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = PrefixASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más PREFIX
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::prefix_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::colon_test_token(2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::uri_test_token("https://ejemplo2.com", 2));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = PrefixASTNode {
            identifier: "ident2".to_string(),
            uri: "https://ejemplo2.com".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = prefix_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_prefix() {
        let fail_tokens_vector = vec![
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(
            actual,
            "Se esperaba un identificador después de PREFIX en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Ident LeftAngleBracket Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_colon() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(
            actual,
            "Faltan los ':' después del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Ident Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '<' antes de la URI en la línea 1");
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_uri() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba una URI entre '<' y '>' en la línea 1");
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '>' después de la URI en la línea 1");
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que son: Prefix Ident Colon Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '<' antes de la URI en la línea 1");
    }

    /// Comprueba que el parser de Source detecta la secuencia de tokens: Source Ident LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_uri() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "https://ejemplo.com/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::uri_test_token(
            "https://ejemplo2.com/fichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "https://ejemplo2.com/fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source detecta la secuencia de tokens: Source Ident LeftAngleBracket JdbcUrl RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_jdbc_url() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::jdbc_url_test_token("jdbc:mysql://localhost:3306/mydb", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "jdbc:mysql://localhost:3306/mydb".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::jdbc_url_test_token(
            "jdbc:mysql://localhost:3356/anotherdb",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "jdbc:mysql://localhost:3356/anotherdb".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source detecta la secuencia de tokens: Source Ident LeftAngleBracket FilePath RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_file_path() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::file_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "file:///ejemplo/path/a/fichero/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::file_path_test_token(
            "file:///otroejemplo/path/a/fichero/otrofichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "file:///otroejemplo/path/a/fichero/otrofichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source detecta la secuencia de tokens: Source Ident LeftAngleBracket Path RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_path() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::path_test_token("ejemplo/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "ejemplo/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::path_test_token(
            "C:\\ejemplo\\path\\a\\fichero\\fichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "C:\\ejemplo\\path\\a\\fichero\\fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_source() {
        let fail_tokens_vector = vec![
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un PREFIX o un SOURCE en la línea 1");
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Source LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un identificador después de SOURCE en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_url_or_path() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba una URI entre '<' y '>' en la línea 1");
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '>' después de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Query detecta la secuencia de tokens: Query Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_query_sintax() {
        let mut tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = QueryASTNode {
            identifier: "ident".to_string(),
            sql_query: "SELECT * FROM example;".to_string(),
        };
        let actual = query_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más QUERY
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::query_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::sql_type_test_token(2));
        tokens_vector.push(TestUtilities::sql_query_test_token(
            "SELECT * FROM example;",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = QueryASTNode {
            identifier: "ident2".to_string(),
            sql_query: "SELECT * FROM example;".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = query_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_query() {
        let fail_tokens_vector = vec![
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba un QUERY en la línea 1");
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un identificador después de QUERY en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_type() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba 'sql:' después de '<' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_query() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba una consulta SQL entre '<' y '>' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '>' después de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::eof_test_token(1),
        ];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el resultado actual del test es un error y que el mensaje de este concuerda con el esperado
    ///
    /// Utiliza como tipo genérico el tipo de nodo del AST que se esté testeando
    ///
    /// #Argumentos
    /// * `actual` - El Result con el error
    /// * `error_message` - El mensaje de error esperado
    fn check_error<T>(actual: Result<Vec<T>, Vec<Simple<Token>>>, expected_error_message: &str) {
        assert!(actual.is_err(), "Se esperaba un error");

        let _ = actual.map_err(|e| {
            let mut error_message_find = false;

            for error in e {
                let actual_error = match error.reason() {
                    SimpleReason::Custom(msg) => msg,
                    _ => "Otro error",
                };

                // println!("{}", actual_error);
                if actual_error == expected_error_message {
                    error_message_find = true;
                    break;
                }
            }

            assert!(error_message_find);
        });
    }
}
