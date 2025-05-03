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
        .map(|(prefixes, sources)| FileASTNode { prefixes, sources })
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
/// Realiza el parseo de los tokens para detectar la secuencia: SOURCE IDENT: URI
///
/// # Retorna
/// Un nodo Source del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn source_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {
    source_detector()
        .then(identifier_detector(SOURCE.to_string()))
        .then(uri_detector())
        .map(|((_, ident), uri)| SourceASTNode {
            identifier: ident.lexeme.clone(),
            uri: uri.lexeme.clone(),
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
    filter(|token: &Token| token.token_type == TokenType::PREFIX)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            Simple::custom(
                token.span(),
                format!(
                    "Se esperaba un PREFIX en la línea {}",
                    token.found().map(|t| t.num_line).unwrap()
                ),
            )
        })
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
    filter(|token: &Token| token.token_type == TokenType::SOURCE)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            Simple::custom(
                token.span(),
                format!(
                    "Se esperaba un SOURCE en la línea {}",
                    token.found().map(|t| t.num_line).unwrap()
                ),
            )
        })
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
    filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            Simple::custom(
                token.span(),
                format!(
                    "Se esperaba un identificador después de {previous_token} en la línea {}",
                    token.found().map(|t| t.num_line).unwrap()
                ),
            )
        })
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
    filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| {
            Simple::custom(
                token.span(),
                format!(
                    "Se esperaba una URI después del identificador en la línea {}",
                    token.found().map(|t| t.num_line).unwrap()
                ),
            )
        })
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
    filter(|token: &Token| token.token_type == TokenType::COLON)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| {
            Simple::custom(
                token.span(),
                format!(
                    "Faltan los ':' después del identificador en la línea {}",
                    token.found().map(|t| t.num_line).unwrap()
                ),
            )
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

#[cfg(test)]
mod sintax_detectors_tests {
    use super::*;

    #[test]
    fn test_prefix_detector() {
        let expected_token = TestTokens::prefix_test_token(1);

        // Ok test
        let actual = prefix_detector().parse(vec![expected_token.clone()]);
        assert_eq!(expected_token, actual.unwrap());

        // Fail test
        let actual = prefix_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    #[test]
    fn test_source_detector() {
        let expected_token = TestTokens::source_test_token(1);

        // Ok test
        let actual = source_detector().parse(vec![expected_token.clone()]);
        assert_eq!(expected_token, actual.unwrap());

        // Fail test
        let actual = source_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    #[test]
    fn test_identifier_detector() {
        let expected_token = TestTokens::ident_test_token("ident", 1);

        // Ok test
        let actual = identifier_detector("PREFIX".to_string()).parse(vec![expected_token.clone()]);
        assert_eq!(expected_token, actual.unwrap());

        // Fail test
        let actual = identifier_detector("SOURCE".to_string()).parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    #[test]
    fn test_uri_detector() {
        let expected_token = TestTokens::uri_test_token("https://ejemplo.com", 1);

        // Ok test
        let actual = uri_detector().parse(vec![expected_token.clone()]);
        assert_eq!(expected_token, actual.unwrap());

        // Fail test
        let actual = uri_detector().parse(vec![TestTokens::colon_test_token(1)]);
        check_error(actual);
    }

    #[test]
    fn test_colon_detector() {
        let expected_token = TestTokens::colon_test_token(1);

        // Ok test
        let actual = colon_detector().parse(vec![expected_token.clone()]);
        assert_eq!(expected_token, actual.unwrap());

        // Fail test
        let actual = colon_detector().parse(vec![TestTokens::ident_test_token("ident", 1)]);
        check_error(actual);
    }

    fn check_error(actual: Result<Token, Vec<Simple<Token>>>) {
        assert!(actual.is_err(), "Se esperaba un error, pero se obtuvo: {:?}", actual);
    }
}

#[cfg(test)]
mod sintax_tests {
    use chumsky::error::SimpleReason;

    use super::*;

    #[test]
    fn test_prefix_parser() {
        let mut tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];

        // Ok test
        let expected = PrefixASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Ok test
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

        // Fail test
        let fail_tokens_vector = vec![TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1),
            TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un PREFIX en la línea 1");

        // Fail test
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1),
            TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un identificador después de PREFIX en la línea 1");

        // Fail test
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1),
             TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Faltan los ':' después del identificador en la línea 1");

        // Fail test 
        let fail_tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1),
        TestTokens::colon_test_token(1), TestTokens::eof_test_token(1)];
        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba una URI después del identificador en la línea 1");
    }

    #[test]
    fn test_source_parser() {
        let mut tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];

        // Ok test
        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Ok test
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestTokens::source_test_token(2));
        tokens_vector.push(TestTokens::ident_test_token("ident2", 2));
        tokens_vector.push(TestTokens::uri_test_token("https://ejemplo2.com", 2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            uri: "https://ejemplo2.com".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());

        // Fail test
        let fail_tokens_vector = vec![TestTokens::ident_test_token("ident", 1), 
            TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un SOURCE en la línea 1");

        // Fail test
        let fail_tokens_vector = vec![TestTokens::source_test_token(1), 
            TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un identificador después de SOURCE en la línea 1");

        // Fail test 
        let fail_tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1),
        TestTokens::eof_test_token(1)];
        let actual = source_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba una URI después del identificador en la línea 1");
    }

    #[test]
    fn test_file_parser() {
        // Ok test
        let tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::source_test_token(1), 
            TestTokens::ident_test_token("ident", 1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "ident".to_string(),
                uri: "https://ejemplo.com".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "ident".to_string(),
                uri: "https://ejemplo.com".to_string(),
            }],
        };
        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());

        // Fail test
        let tokens_vector = vec![TestTokens::prefix_test_token(1), TestTokens::ident_test_token("ident", 1), 
            TestTokens::colon_test_token(1), TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error dentro porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un SOURCE en la línea 1");

        // Fail test
        let tokens_vector = vec![TestTokens::source_test_token(1), TestTokens::ident_test_token("ident", 1), 
        TestTokens::uri_test_token("https://ejemplo.com", 1), TestTokens::eof_test_token(1)];
        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    fn check_error<T>(actual: Result<Vec<T>, Vec<Simple<Token>>>, error_message: &str) {
        assert!(actual.is_err(), "Se esperaba un error diferente al obtenido");
        
        let _ = actual.map_err(|e| {
            let mut error_message_find = false;

            for error in e {
                let actual_error = match error.reason() {
                    SimpleReason::Custom(msg) => msg,
                    _ => "Otro error",
                };

                // println!("{}", actual_error);
                if actual_error == error_message {
                    error_message_find = true;
                    break;
                }
            }

            assert!(error_message_find);
        });
    }
}