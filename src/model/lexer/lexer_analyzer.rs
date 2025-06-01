//! Módulo del analizador léxico
//!
//! Toma como entrada el fichero .shexml que indique el usuario y extrae los tokens de este
//! También puede indicar errores léxicos en el que caso de que encuentre algún lexema en la entrada que no esté incluido en la especificación ShExML

use winnow::combinator::{alt, delimited};
use winnow::error::{AddContext, ContextError, ErrMode, StrContext};
use winnow::prelude::*;
use winnow::token::{literal, take_while};

use super::super::compiler_error::CompilerError;
use super::token::*;

use regex::Regex;

/// Encuentra el token PREFIX en la entrada
///
/// Acepta la entrada 'PREFIX'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Prefix
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn prefix(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(PREFIX).parse_next(input)?;
    Ok(Token::new(PREFIX.to_string(), TokenType::PREFIX))
}

/// Encuentra el token : en la entrada
///
/// Acepta la entrada ':'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token :
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn colon(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(COLON).parse_next(input)?;
    Ok(Token::new(COLON.to_string(), TokenType::COLON))
}

/// Encuentra el token SOURCE en la entrada
///
/// Acepta la entrada 'SOURCE'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Source
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn source(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(SOURCE).parse_next(input)?;
    Ok(Token::new(SOURCE.to_string(), TokenType::SOURCE))
}

/// Encuentra el token QUERY en la entrada
///
/// Acepta la entrada 'QUERY'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Query
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn query(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(QUERY).parse_next(input)?;
    Ok(Token::new(QUERY.to_string(), TokenType::QUERY))
}

/// Encuentra un token identificador en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres alfabéticos; también acepta '_'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token identificador
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let ident = take_while(1.., |c: char| c.is_alphabetic() || c == '_').parse_next(input)?;

    Ok(Token::new(ident.to_string(), TokenType::IDENT))
}

/// Encuentra un token URI en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres entre < y > que cumpla con la expresión regular de URIs
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token URI
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn uri(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let uri = delimited('<', take_while(1.., |c: char| c != '>'), '>').parse_next(input)?;
    let re_uri = Regex::new(r"^[a-zA-Z][a-zA-Z0-9+.-]*://[^\s<>]+$").unwrap();
    
    if !re_uri.is_match(uri) {
        let error = &ContextError::new().add_context(&"Formato incorrecto", &uri.checkpoint(), StrContext::Label("URI inválida"));
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(uri.to_string(), TokenType::URI))
}

/// Encuentra un token SOURCEPATH en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres entre < y > que cumpla con la expresión regular que detecta:
/// * Ficheros CSV con un path en remoto o en local
/// * Bases de datos con una URL JDBC
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token SOURCEPATH
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn source_path(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let source_path = delimited('<', take_while(1.., |c: char| c != '>'), '>').parse_next(input)?;
    let re_source_path = Regex::new(r"(?ix)                    
            ^(
                file://[/\\][^ \n\r\t]+\.csv    # Ficheros
                |
                [a-zA-Z]:[\\/](?:[\w\-. ]+[\\/]?)*[\w\-. ]+\.csv    # rutas absolutas
                |
                (\.{0,2}[\\/])?(?:[\w\-.\\\/]+[\\/])*[\w\-.\\\/*]+\.csv     # rutas locales
                |
                jdbc:[\w]+://[^ \n\r\t]+    # JBDC URLs
                |
                https?://[\w\-.]+(?:/[^\s\r\n\t]*)*\.csv    # URLs remotas a ficheros
            )$
            "
        ).unwrap();
    
    // Es necesario hacer una comprobación extra con las URLs JDBC
    if !re_source_path.is_match(source_path) || (source_path.starts_with("jdbc:") && source_path.ends_with(".csv")) {
        let error = &ContextError::new()
            .add_context(&"Formato incorrecto", &source_path.checkpoint(), StrContext::Label("Path o URI del fichero CSV o base de datos inválida"));
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(source_path.to_string(), TokenType::SOURCEPATH))
}

/// Encuentra un token QUERYDEFINITION en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres entre < y > que cumpla con la expresión regular que detecta:
/// * Consultas SQL literales o en ficheros externos .sql
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token QUERYDEFINITION
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el parseo de la entrada
fn query_definition(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let mut query_definition = delimited('<', take_while(1.., |c: char| c != '>'), '>').parse_next(input)?;
    let re_query_definition = Regex::new(r"(?ix)
            ^(?:                                                          
                sql:\s*\bSELECT\b\s+.+?\bFROM\b\s+.+?(?:\bWHERE\b\s+.+?)?(?:\bGROUP\s+BY\b\s+.+?)?(?:\bORDER\s+BY\b\s+.+?)?    # SQL
            )$
        "
        ).unwrap();
    
    if !re_query_definition.is_match(query_definition) {
        let error = &ContextError::new()
            .add_context(&"Formato incorrecto", &query_definition.checkpoint(), StrContext::Label("Consulta SQL o path a fichero inválido"));
        return Err(ErrMode::Backtrack(error.clone()));
    }

    if query_definition.starts_with("sql:") {
        query_definition = query_definition.strip_prefix("sql:").unwrap_or(query_definition).trim_start()
    }

    Ok(Token::new(query_definition.to_string(), TokenType::QUERYDEFINITION))
}

/// Realiza el análisis léxico de la entrada
///
/// Analiza la entrada y va encontrando tokens. A medida que los encuentra los va almacenando en un vector
///
/// # Argumentos
/// * `input` - La entrada del fichero
///
/// # Retorna
/// Un vector con los tokens detectados
///
/// # Errores
/// Devuelve un `[Vec<ParserError>]` con los errores detectados por el analizador léxico
pub fn lexer(input: &mut &str) -> Result<Vec<Token>, Vec<CompilerError>> {
    let mut tokens = Vec::new();
    let mut errors: Vec<CompilerError> = Vec::new();

    let num_line = look_over_input(input, &mut tokens, &mut errors);

    end_lexer(tokens, errors, num_line)
}

/// Finaliza el análisis léxico
///
/// Si no hay errores devuelve el vector de tokens detectado y, si hay errores, los devuelve
///
/// # Argumentos
/// * `tokens` - El vector de tokens resultado del análisis léxico
/// * `errors` - El vector de errores léxicos detectados
/// * `num_line` - El último número de línea
///
/// # Retorna
/// El vector de tokens si no hay errores
///
/// # Errores
/// Devuelve un `[Vec<ParserError>]` El vector de errores detectados en el análisis léxico
fn end_lexer(
    mut tokens: Vec<Token>,
    errors: Vec<CompilerError>,
    num_line: u16
) -> Result<Vec<Token>, Vec<CompilerError>> {
    if errors.is_empty() {
        tokens.push(Token::create_eof_token(num_line));
        Ok(tokens)
    } else {
        Err(errors)
    }
}

/// Itera sobre la entrada con el fin de analizarla lexicamente
///
/// # Argumentos
/// * `input` - La entrada del fichero
/// * `tokens` - El vector que contendrá los tokens detectados
/// * `errors` - El vector que contendrá los errores léxicos detectados
/// 
/// # Retorna
/// El último número de línea
fn look_over_input(input: &mut &str, tokens: &mut Vec<Token>, errors: &mut Vec<CompilerError>) -> u16 {
    let mut num_line = 1;

    while !input.is_empty() {
        match_alternatives(input, tokens, num_line, errors);

        let new_line: Result<&str, ErrMode<ContextError>> =
            take_while(1.., |c: char| c == '\n' || c == '\r').parse_next(input); // Se ignoran los saltos de línea
        if new_line.is_ok() {
            num_line += 1;
        }

        let _: Result<&str, ErrMode<ContextError>> =
            take_while(1.., |c: char| c.is_whitespace() || c == '\t').parse_next(input);
        // Se ignoran los espacios
    }

    num_line
}

/// Busca entre las distintas alternativas de tokens
///
/// A partir de la parte de la entrada que se le pase, busca a que token se corresponde
///
/// # Argumentos
/// * `input` - La parte de la entrada del fichero cuyo token se busca
/// * `tokens` - El vector donde se insertará el token resultante de la búsqueda
/// * `num_line` - El número de línea en el que se encuentra el token
/// * `errors` - EL vector donde se insertará el error léxico en caso de que ocurra
fn match_alternatives(
    input: &mut &str,
    tokens: &mut Vec<Token>,
    num_line: u16,
    errors: &mut Vec<CompilerError>,
) {
    match alt((colon, prefix, source, query, source_path, query_definition, uri, identifier)).parse_next(input) {
        Ok(mut token) => {
            token.set_num_line(num_line);
            tokens.push(token);
        }

        // Si no es ningún token, se pasa
        Err(ErrMode::Backtrack(_)) => {
            let token_error: Result<&str, ErrMode<ContextError>> =
                take_while(1, |c: char| c.is_ascii()).parse_next(input);
            if token_error.is_ok() {
                errors.push(CompilerError::new(format!(
                    "Error léxico: '{}'; en la línea {}",
                    token_error.unwrap().to_string(),
                    num_line
                )));
            }
        }
        Err(e) => panic!("{}", e),
    }
}

// Tests

/// Módulo de los tests del analizador léxico
/// 
/// Contiene los tests que se encargan de probar que se detectan todos los tokens válidos y se descartan los inválidos
/// Los tests se hacen tanto a nivel de tokens individuales como a nivel de tokens en conjunto
#[cfg(test)]
mod lexer_tests {
    use crate::test_utils::TestUtilities;

    use super::*;

    // En los tests el número de línea de los tokens es 0 porque todavía no se le asigna

    /// Comprueba que se detecta el token PREFIX
    #[doc(hidden)]
    #[test]
    fn valid_prefix() {
        let expected = TestUtilities::prefix_test_token(0);
        let actual = prefix(&mut "PREFIX");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token aquellas cadenas que no sean PREFIX
    #[doc(hidden)]
    #[test]
    fn invalid_prefix() {
        let actual = prefix(&mut "PRFIX");
        check_error(actual);
    }

    /// Comprueba que se detecta el token :
    #[doc(hidden)]
    #[test]
    fn valid_colon() {
        let expected = TestUtilities::colon_test_token(0);
        let actual = colon(&mut ":");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token : aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_colon() {
        let actual = colon(&mut ";");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SOURCE
    #[doc(hidden)]
    #[test]
    fn valid_source() {
        let expected = TestUtilities::source_test_token(0);
        let actual = source(&mut "SOURCE");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token SOURCE aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_source() {
        // Fail test
        let actual = source(&mut "SOUR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) sin que tenga un '_'
    #[doc(hidden)]
    #[test]
    fn valid_identifier_withouth_underscore() {
        let expected = TestUtilities::ident_test_token("ident",0);
        let actual = identifier(&mut "ident");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) con un '_' en medio
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_inside() {
        let expected = TestUtilities::ident_test_token("ident_valid",0);
        let actual = identifier(&mut "ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) con un '_ ' al comienzo
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_begining() {
        let expected = TestUtilities::ident_test_token("_ident_valid", 0);
        let actual = identifier(&mut "_ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) con un '_' al final
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_end() {
        // Test con identificador con _ al final del identificador
        let expected = TestUtilities::ident_test_token("ident_valid_", 0);
        let actual = identifier(&mut "ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) con un '_' al comienzo y final
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_begining_and_end() {
        // Test con identificador con _ al final del identificador
        let expected = TestUtilities::ident_test_token("_ident_valid_", 0);
        let actual = identifier(&mut "_ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token IDENT aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_identifier() {
        let actual = identifier(&mut "123ident_invalid");
        check_error(actual);
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTPS
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_https() {
        let expected = TestUtilities::uri_test_token("https://ejemplo.com",0);
        let actual = uri(&mut "<https://ejemplo.com>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTP
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_http() {
        let expected = TestUtilities::uri_test_token("http://ejemplo.com",0);
        let actual = uri(&mut "<http://ejemplo.com>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con una '/' al final
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_slash_at_the_end() {
        let expected = TestUtilities::uri_test_token("https://ejemplo.com/",0);
        let actual = uri(&mut "<https://ejemplo.com/>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token URI aquellas cadenas que no tengan un '>' al final
    #[doc(hidden)]
    #[test]
    fn uri_withouth_right_angle_bracket() {
        let actual = uri(&mut "<https://ejemplo.com");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token URI aquellas cadenas que no tengan un '<' al comienzo
    #[doc(hidden)]
    #[test]
    fn uri_withouth_left_angle_bracket() {
        let actual = uri(&mut "https://ejemplo.com>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token URI aquellas cadenas que no tengan un '<' al comienzo y un '>' al final
    #[doc(hidden)]
    #[test]
    fn uri_withouth_angle_brackets() {
        let actual = uri(&mut "https://ejemplo.com");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token URI las URIs incorrectas
    #[doc(hidden)]
    #[test]
    fn invalid_format_uri() {
        let actual = uri(&mut "<https:ejemplo.com>");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de un fichero CSV remoto
    #[doc(hidden)]
    #[test]
    fn valid_source_path_with_csv_remote_file() {
        let expected = TestUtilities::source_path_test_token("https://ejemplo.com/fichero.csv",0);
        let actual = source_path(&mut "<https://ejemplo.com/fichero.csv>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de un fichero CSV local usando una ruta relativa
    #[doc(hidden)]
    #[test]
    fn valid_source_path_with_csv_local_file_relative_path() {
        let expected = TestUtilities::source_path_test_token("ejemplo/fichero.csv", 0);
        let actual = source_path(&mut "<ejemplo/fichero.csv>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de un fichero CSV local usando una ruta absoluta con file
    #[doc(hidden)]
    #[test]
    fn valid_source_path_with_csv_local_file_absolute_path_with_file() {
        let expected = TestUtilities::source_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 0);
        let actual = source_path(&mut "<file:///ejemplo/path/a/fichero/fichero.csv>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de un fichero CSV local usando una ruta absoluta sin file
    #[doc(hidden)]
    #[test]
    fn valid_source_path_with_csv_local_file_absolute_path_withouth_file() {
        let expected = TestUtilities::source_path_test_token("C:\\ejemplo\\path\\a\\fichero\\fichero.csv", 0);
        let actual = source_path(&mut "<C:\\ejemplo\\path\\a\\fichero\\fichero.csv>");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de una base de datos
    #[doc(hidden)]
    #[test]
    fn valid_source_path_with_database() {
        let expected = TestUtilities::source_path_test_token("jdbc:mysql://localhost:3306/mydb", 0);
        let actual = source_path(&mut "<jdbc:mysql://localhost:3306/mydb>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que no tengan un '>' al final
    #[doc(hidden)]
    #[test]
    fn source_path_withouth_right_angle_bracket() {
        let actual = source_path(&mut "<https://ejemplo.com");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que no tengan un '<' al comienzo
    #[doc(hidden)]
    #[test]
    fn source_path_withouth_left_angle_bracket() {
        let actual = source_path(&mut "https://ejemplo.com>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que no tengan un '<' al comienzo y un '>' al final
    #[doc(hidden)]
    #[test]
    fn source_path_withouth_angle_brackets() {
        let actual = source_path(&mut "https://ejemplo.com");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan una URI incorrecta
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_uri() {
        let actual = source_path(&mut "<https:ejemplo.com/fichero.csv>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan un path absoluto con file incorrecto
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_absolute_path_with_file() {
        let actual = source_path(&mut "<file//>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan un path absoluto sin file incorrecto
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_absolute_path_withouth_file() {
        let actual = source_path(&mut "<//..>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan un path relativo incorrecto
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_relative_path() {
        let actual = source_path(&mut "<ejemplo/>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan un tipo de fichero incorrecto
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_file_type() {
        let actual = source_path(&mut "<ejemplo/fichero.xml>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SOURCEPATH aquellas cadenas que tengan una URL JDBC incorrecta
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_jdbc_url() {
        let actual = source_path(&mut "<jdbc:/localhost:3306/db>");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH de una base de datos a un fichero .csv
    #[doc(hidden)]
    #[test]
    fn source_path_with_invalid_database_to_csv() {
        let actual = source_path(&mut "<jdbc:mysql://localhost:3306/mydb.csv>");
        check_error(actual);
    }

    /// Comprueba que se detecta el token QUERYDEFINITION con una consulta SQL
    #[doc(hidden)]
    #[test]
    fn valid_query_definition_with_sql_query() {
        let expected = TestUtilities::query_definition_test_token("SELECT * FROM tabla WHERE id = '1'", 0);
        let actual = query_definition(&mut "<sql: SELECT * FROM tabla WHERE id = '1'>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que no tengan un '>' al final
    #[doc(hidden)]
    #[test]
    fn query_definition_withouth_right_angle_bracket() {
        let actual = query_definition(&mut "<sql: SELECT * FROM tabla WHERE id = '1'");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que no tengan un '<' al comienzo
    #[doc(hidden)]
    #[test]
    fn query_definition_withouth_left_angle_bracket() {
        let actual = query_definition(&mut "sql: SELECT * FROM tabla WHERE id = '1'>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que no tengan un '<' al comienzo y un '>' al final
    #[doc(hidden)]
    #[test]
    fn query_definition_withouth_angle_bracket() {
        let actual = query_definition(&mut "sql: SELECT * FROM tabla WHERE id = '1'");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que tengan una consulta SQL incorrecta
    #[doc(hidden)]
    #[test]
    fn query_definition_with_invalid_sql_query() {
        let actual = query_definition(&mut "<sql: SELECT FROM tabla>");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que tengan una consulta SQL sin ':sql' al comienzo
    #[doc(hidden)]
    #[test]
    fn query_definition_with_invalid_sql_query_withouth_sql_begining() {
        let actual = query_definition(&mut "<SELECT * FROM tabla>");
        check_error(actual);
    }

    /// Comprueba que se detectan múltiples tokens distintos
    #[doc(hidden)]
    #[test]
    fn lexer_with_multiple_tokens() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";

        let expected: Vec<Token> = vec![
            TestUtilities::prefix_test_token(1), TestUtilities::ident_test_token("example", 1), TestUtilities::colon_test_token(1), TestUtilities::uri_test_token("http://example.com/", 1), 
            TestUtilities::source_test_token(2), TestUtilities::ident_test_token("films_csv_file", 2), TestUtilities::source_path_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2), 
            TestUtilities::query_test_token(3), TestUtilities::ident_test_token("query_sql", 3), TestUtilities::query_definition_test_token("SELECT * FROM example;", 3),
            TestUtilities::eof_test_token(3)];
        let actual = lexer(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en el PREFIX
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_prefix() {
        let mut input = "PREFIX example123: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en el SOURCE
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_source() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv
            QUERY query_sql <sql: SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en el QUERY
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_query() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv
            QUERY query_sql SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());
    }

    /// Comprueba que el resultado actual del test es igual al esperado
    /// 
    /// # Argumentos
    /// * `expected` - El token esperado
    /// * `actual` - El token real
    fn check_ok(expected: Token, actual: Result<Token, ErrMode<ContextError>>) {
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el resultado actual del test es un error
    /// 
    /// # Argumentos
    /// * `actual` - Un Result con el error esperado
    fn check_error(actual: Result<Token, ErrMode<ContextError>>) {
        assert!(actual.is_err(), "Se esperaba un error, pero se obtuvo: {:?}", actual);
    }
}