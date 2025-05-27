//! Módulo del analizador sintáctico
//!
//! Realiza el análisis sintáctico del compilador
//! Comprueba que los tokens resultado del analizador léxico se encuentran en el orden esperado y genera el AST

use chumsky::combinator::*;
use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::Parser;

use crate::model::token::*;

use super::ast::*;
use super::token::TokenType;

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
        .map(|((prefixes, sources), queries)| {
            FileASTNode { prefixes, sources, queries}
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
        .then(uri_detector())
        .map(|(((_, ident), _), uri)| PrefixASTNode {
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
        .then(source_path_detector())
        .map(|((_, ident), source_path)| SourceASTNode {
            identifier: ident.lexeme.clone(),
            source_path: source_path.lexeme.clone(),
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
        .then(query_definition_detector())
        .map(|((_, ident), query_definition)| QueryASTNode {
            identifier: ident.lexeme.clone(),
            query_definition: query_definition.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

// Detectors

/// Detecta el token PREFIX en los tokens
///
/// # Retorna
/// Un token de tipo Prefix si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Prefix
fn prefix_detector(
    ) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
    > {
    general_detector(TokenType::PREFIX, format!("Se esperaba un PREFIX en la línea"))
}

/// Detecta el token SOURCE en los tokens
///
/// # Retorna
/// Un token de tipo Source si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source
fn source_detector(
    ) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
    > {
    general_detector(TokenType::SOURCE, format!("Se esperaba un SOURCE en la línea"))
}

/// Detecta el token QUERY en los tokens
///
/// # Retorna
/// Un token de tipo Query si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source
fn query_detector(
    ) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
    > {
    general_detector(TokenType::QUERY, format!("Se esperaba un QUERY en la línea"))
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
    general_detector(TokenType::IDENT, format!("Se esperaba un identificador después de {previous_token} en la línea"))
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
    general_detector(TokenType::URI, format!("Se esperaba una URI después del identificador en la línea"))
}

/// Detecta el token QUERYDEFINITION en los tokens
///
/// # Retorna
/// Un token de tipo Query definition si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn query_definition_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_detector(TokenType::QUERYDEFINITION, format!("Se esperaba una consulta SQL o un path o URL a un fichero .sql o .sparql después del identificador en la línea"))
}

/// Detecta el token SOURCEPATH en los tokens
///
/// # Retorna
/// Un token de tipo Source path si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source path
fn source_path_detector() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
    > {
    general_detector(TokenType::SOURCEPATH, format!("Se esperaba una ruta o URL a un fichero o base de datos después del identificador en la línea"))
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
    general_detector(TokenType::COLON, format!("Faltan los ':' después del identificador en la línea"))
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
fn general_detector(token_type: TokenType, message: String) -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
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
    use super::*;

    /// Comprueba que se detectan los tokens PREFIX
    #[doc(hidden)]
    #[test]
    fn test_prefix_detector_ok() {
        let expected_token = TestTokens::prefix_test_token(1);
        let actual = prefix_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens PREFIX aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_prefix_detector_fail() {
        let actual = prefix_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens SOURCE
    #[doc(hidden)]
    #[test]
    fn test_source_detector_ok() {
        let expected_token = TestTokens::source_test_token(1);
        let actual = source_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens SOURCE aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_source_detector_fail() {
        let actual = source_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens QUERY
    #[doc(hidden)]
    #[test]
    fn test_query_detector_ok() {
        let expected_token = TestTokens::query_test_token(1);
        let actual = query_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens QUERY aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_query_detector_fail() {
        let actual = query_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens IDENT
    #[doc(hidden)]
    #[test]
    fn test_identifier_detector_ok() {
        let expected_token = TestTokens::ident_test_token("ident", 1);
        let actual = identifier_detector("PREFIX".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens IDENT aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_identifier_detector_fail() {
        let actual = identifier_detector("SOURCE".to_string()).parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens URI
    #[doc(hidden)]
    #[test]
    fn test_uri_detector_ok() {
        let expected_token = TestTokens::uri_test_token("https://ejemplo.com", 1);
        let actual = uri_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens URI aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_uri_detector_fail() {
        let actual = uri_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens Query Definition
    #[doc(hidden)]
    #[test]
    fn test_query_definition_detector_ok() {
        let expected_token = TestTokens::query_definition_test_token("SELECT * FROM example;", 1);
        let actual = query_definition_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens Query Definition aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_query_definition_detector_fail() {
        let actual = query_definition_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se detectan los tokens COLON (:)
    #[doc(hidden)]
    #[test]
    fn test_colon_detector_ok() {
        let expected_token = TestTokens::colon_test_token(1);
        let actual = colon_detector().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se detectan como tokens COLON (:) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn test_colon_detector_fail() {
        let actual = colon_detector().parse(vec![TestTokens::ident_test_token("ident", 1)]);
        check_error(actual);
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
        assert!(actual.is_err(), "Se esperaba un error, pero se obtuvo: {:?}", actual);
    }
}

/// Módulo para los tests del analizador sintáctico
/// 
/// Contiene los tests que se encargan de comprobar que los diferentes parsers del analizador sintáctico funcionan correctamente
#[cfg(test)]
mod sintax_tests {
    use chumsky::error::SimpleReason;

    use super::*;

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST
    #[doc(hidden)]
    #[test]
    fn test_file_parser_ok() {
        let tokens_vector = vec![
            TestTokens::prefix_test_token(1), TestTokens::ident_test_token("example", 1), TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://example.com/", 1), 
            TestTokens::source_test_token(2), TestTokens::ident_test_token("films_csv_file", 2), TestTokens::source_path_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2), 
            TestTokens::query_test_token(3), TestTokens::ident_test_token("query_sql", 3), TestTokens::query_definition_test_token("SELECT * FROM example;", 3),
            TestTokens::eof_test_token(3)];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_path: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                query_definition: "SELECT * FROM example;".to_string(),
            }]),
        };
        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si hay algún error sintáctico
    #[doc(hidden)]
    #[test]
    fn test_file_parser_fail() {
        // Test con Sources faltantes
        let tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error dentro porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un SOURCE en la línea 1");

        // Test con prefixes faltantes
        let tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1), 
        TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    /// Comprueba que el parser de Prefix detecta la secuencia de tokens: PREFIX IDENT COLON URI
    #[doc(hidden)]
    #[test]
    fn test_prefix_parser_ok() {
        let mut tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];

        // Test con un solo PREFIX
        let expected = PrefixASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Test con más de un PREFIX
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestTokens::prefix_test_token(2));
        tokens_vector.push(TestTokens::ident_test_token("ident2", 2));
        tokens_vector.push(TestTokens::colon_test_token(2));
        tokens_vector.push(TestTokens::uri_test_token("https://ejemplo2.com", 2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = PrefixASTNode {
            identifier: "ident2".to_string(),
            uri: "https://ejemplo2.com".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = prefix_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Prefix no detecta como tales aquellas secuencias de tokens que no son: PREFIX IDENT COLON URI
    #[doc(hidden)]
    #[test]
    fn test_prefix_parser_fail() {
        // Test con el token PREFIX faltante
        let fail_tokens_vector = vec![TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1),
            TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un PREFIX en la línea 1");

        // Test con el token IDENT (identificador) faltante
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1),
            TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un identificador después de PREFIX en la línea 1");

        // Test con el token COLON (:) faltante
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1),
             TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Faltan los ':' después del identificador en la línea 1");

        // Test con el token URI faltante
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1),
        TestTokens::colon_test_token(1), TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba una URI después del identificador en la línea 1");
    }

    /// Comprueba que el parser de Source detecta la secuencia de tokens: SOURCE IDENT URI
    #[doc(hidden)]
    #[test]
    fn test_source_parser_ok() {
        let mut tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::source_path_test_token("https://ejemplo.com/fichero.csv", 1), TestTokens::eof_test_token(1)];

        // Test con un solo SOURCE
        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_path: "https://ejemplo.com/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Test con más de un SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestTokens::source_test_token(2));
        tokens_vector.push(TestTokens::ident_test_token("ident2", 2));
        tokens_vector.push(TestTokens::source_path_test_token("https://ejemplo2.com/fichero.csv", 2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_path: "https://ejemplo2.com/fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source no detecta como tales aquellas secuencias de tokens que no son: SOURCE IDENT URI
    #[doc(hidden)]
    #[test]
    fn test_source_parser_fail() {
        // Test con el token SOURCE faltante
        let fail_tokens_vector = vec![TestTokens::ident_test_token("ident", 1), 
            TestTokens::source_path_test_token("https://ejemplo.com/fichero.csv", 1), TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un SOURCE en la línea 1");

        // Test con el token IDENT (identificador) faltante
        let fail_tokens_vector = vec![TestTokens::source_test_token(1), 
            TestTokens::source_path_test_token("https://ejemplo.com/fichero.csv", 1), TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un identificador después de SOURCE en la línea 1");

        // Test con el token SOURCEPATH faltante
        let fail_tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1),
        TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba una ruta o URL a un fichero o base de datos después del identificador en la línea 1");
    }

    /// Comprueba que el parser de Query detecta la secuencia de tokens: QUERY IDENT QueryDefinition
    #[doc(hidden)]
    #[test]
    fn test_query_parser_ok() {
        let mut tokens_vector = vec![TestTokens::query_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::query_definition_test_token("SELECT * FROM example;", 1), TestTokens::eof_test_token(1)];

        // Test con una sola QUERY
        let expected = QueryASTNode {
            identifier: "ident".to_string(),
            query_definition: "SELECT * FROM example;".to_string(),
        };
        let actual = query_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Test con más de una QUERY
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestTokens::query_test_token(2));
        tokens_vector.push(TestTokens::ident_test_token("ident2", 2));
        tokens_vector.push(TestTokens::query_definition_test_token("/path/to/example_query.sparql", 2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = QueryASTNode {
            identifier: "ident2".to_string(),
            query_definition: "/path/to/example_query.sparql".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = query_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Query no detecta como tales aquellas secuencias de tokens que no son: QUERY IDENT QueryDefinition
    #[doc(hidden)]
    #[test]
    fn test_query_parser_fail() {
        // Test con el token QUERY faltante
        let fail_tokens_vector = vec![TestTokens::ident_test_token("ident", 1), 
            TestTokens::query_definition_test_token("SELECT * FROM example;", 1), TestTokens::eof_test_token(1)];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba un QUERY en la línea 1");

        // Test con el token IDENT (identificador) faltante
        let fail_tokens_vector = vec![TestTokens::query_test_token(1), 
            TestTokens::query_definition_test_token("SELECT * FROM example;", 1), TestTokens::eof_test_token(1)];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba un identificador después de QUERY en la línea 1");

        // Test con el token QUERYDEFINITION faltante
        let fail_tokens_vector = vec![TestTokens::query_test_token(1), TestTokens::ident_test_token("ident", 1),
        TestTokens::eof_test_token(1)];
        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba una consulta SQL o un path o URL a un fichero .sql o .sparql después del identificador en la línea 1");
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