//! Módulo del analizador léxico
//!
//! Toma como entrada el fichero .shexml que indique el usuario y extrae los tokens de este
//! También puede indicar errores léxicos en el que caso de que encuentre algún lexema en la entrada que no esté incluido en la especificación ShExML

use winnow::combinator::alt;
use winnow::error::{AddContext, ContextError, ErrMode, StrContext};
use winnow::prelude::*;
use winnow::token::{literal, take_while};

use super::super::compiler_error::CompilerError;
use super::token::*;

use regex::Regex;

/// Encuentra el token Prefix en la entrada
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
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn prefix(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(PREFIX).parse_next(input)?;
    Ok(Token::new(PREFIX.to_string(), TokenType::Prefix))
}

/// Encuentra el token Colon en la entrada
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
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn colon(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(COLON).parse_next(input)?;
    Ok(Token::new(COLON.to_string(), TokenType::Colon))
}

/// Encuentra el token LeftAngleBracket en la entrada
///
/// Acepta la entrada '<'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token < 
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn left_angle_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(LEFT_ANGLE_BRACKET).parse_next(input)?;
    Ok(Token::new(LEFT_ANGLE_BRACKET.to_string(), TokenType::LeftAngleBracket))
}

/// Encuentra el token RightAngleBracket en la entrada
///
/// Acepta la entrada '>'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token >
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn right_angle_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(RIGHT_ANGLE_BRACKET).parse_next(input)?;
    Ok(Token::new(RIGHT_ANGLE_BRACKET.to_string(), TokenType::RightAngleBracket))
}

/// Encuentra el token Source en la entrada
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
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn source(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(SOURCE).parse_next(input)?;
    Ok(Token::new(SOURCE.to_string(), TokenType::Source))
}

/// Encuentra el token Query en la entrada
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
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn query(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(QUERY).parse_next(input)?;
    Ok(Token::new(QUERY.to_string(), TokenType::Query))
}

/// Encuentra el token Iterator en la entrada
///
/// Acepta la entrada 'ITERATOR'
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Iterator
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn iterator(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(ITERATOR).parse_next(input)?;
    Ok(Token::new(ITERATOR.to_string(), TokenType::Iterator))
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
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let ident = take_while(1.., |c: char| c.is_alphabetic() || c == '_').parse_next(input)?;
    Ok(Token::new(ident.to_string(), TokenType::Ident))
}

/// Encuentra un token URI en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de URIs
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token URI
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn uri(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let uri = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_uri = Regex::new(r"^[a-zA-Z][a-zA-Z0-9+.-]*://[^\s<>]+$").unwrap();

    if !re_uri.is_match(uri) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &uri.checkpoint(),
            StrContext::Label("URI incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(uri.to_string(), TokenType::Uri))
}

/// Encuentra un token JdbcUrl en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de JDBC URLs
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token URI
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn jdbc_url(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let jdbc_url = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_jdbc_url = Regex::new(r"^jdbc:[a-zA-Z0-9]+://[^\s<>]+$").unwrap();

    if !re_jdbc_url.is_match(jdbc_url) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &jdbc_url.checkpoint(),
            StrContext::Label("JDBC URL incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(jdbc_url.to_string(), TokenType::JdbcUrl))
}

/// Encuentra un token FilePath en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de file
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token FilePath
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn file_path(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let file_path = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_file_path = Regex::new(r"^file://[/\\][^ \n\r\t]+\.\w+$").unwrap();

    if !re_file_path.is_match(file_path)
    {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &file_path.checkpoint(),
            StrContext::Label("Ruta file incorrecto"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(file_path.to_string(), TokenType::FilePath))
}

/// Encuentra un token Path en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de una ruta relativa o absoluta
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Path
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn path(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let path = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_path = Regex::new(
        r"(?ix)                    
            ^(
                [a-zA-Z]:[\\/](?:[\w\-. ]+[\\/]?)*[\w\-. ]+\.\w+    # rutas absolutas
                |
                (\.{0,2}[\\/])?(?:[\w\-.\\\/]+[\\/])*[\w\-.\\\/*]+\.\w+     # rutas relativas
            )$
            ",
    )
    .unwrap();

    if !re_path.is_match(path)
    {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &path.checkpoint(),
            StrContext::Label("Ruta absoluta o relativa incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(path.to_string(), TokenType::Path))
}

/// Encuentra un token QueryDefinition en la entrada
///
/// Acepta como entrada consultas SQL
///
/// # Argumentos
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token QueryDefinition
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn query_definition(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let mut query_definition = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_query_definition = Regex::new(r"(?ix)
            ^(?:                                                          
                sql:\s*\bSELECT\b\s+.+?\bFROM\b\s+.+?(?:\bWHERE\b\s+.+?)?(?:\bGROUP\s+BY\b\s+.+?)?(?:\bORDER\s+BY\b\s+.+?)?    # SQL
            )$
        "
        ).unwrap();

    if !re_query_definition.is_match(query_definition) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &query_definition.checkpoint(),
            StrContext::Label("Consulta SQL incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    query_definition = query_definition
            .strip_prefix("sql:")
            .unwrap_or(query_definition)
            .trim_start();

    Ok(Token::new(
        query_definition.to_string(),
        TokenType::QueryDefinition,
    ))
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
    num_line: u16,
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
fn look_over_input(
    input: &mut &str,
    tokens: &mut Vec<Token>,
    errors: &mut Vec<CompilerError>,
) -> u16 {
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
    match alt((
        colon,
        left_angle_bracket,
        right_angle_bracket,
        prefix,
        source,
        iterator,
        query,
        query_definition,
        file_path,
        path,
        jdbc_url,
        uri,
        identifier,
    ))
    .parse_next(input)
    {
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

    /// Comprueba que se detecta el token <
    #[doc(hidden)]
    #[test]
    fn valid_left_angle_bracket() {
        let expected = TestUtilities::left_angle_bracket_test_token(0);
        let actual = left_angle_bracket(&mut "<");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token < aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_left_angle_bracket() {
        let actual = left_angle_bracket(&mut ">");
        check_error(actual);
    }

    /// Comprueba que se detecta el token >
    #[doc(hidden)]
    #[test]
    fn valid_right_angle_bracket() {
        let expected = TestUtilities::right_angle_bracket_test_token(0);
        let actual = right_angle_bracket(&mut ">");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token > aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_right_angle_bracket() {
        let actual = right_angle_bracket(&mut "<");
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

    /// Comprueba que se detecta el token QUERY
    #[doc(hidden)]
    #[test]
    fn valid_query() {
        let expected = TestUtilities::query_test_token(0);
        let actual = query(&mut "QUERY");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token QUERY aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_query() {
        // Fail test
        let actual = query(&mut "QURY");
        check_error(actual);
    }

    /// Comprueba que se detecta el token ITERATOR
    #[doc(hidden)]
    #[test]
    fn valid_iterator() {
        let expected = TestUtilities::iterator_test_token(0);
        let actual = iterator(&mut "ITERATOR");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token ITERATOR aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_iterator() {
        // Fail test
        let actual = query(&mut "ITR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) sin que tenga un '_'
    #[doc(hidden)]
    #[test]
    fn valid_identifier_withouth_underscore() {
        let expected = TestUtilities::ident_test_token("ident", 0);
        let actual = identifier(&mut "ident");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token IDENT (identificadores) con un '_' en medio
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_inside() {
        let expected = TestUtilities::ident_test_token("ident_valid", 0);
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

    /// Comprueba que no se detecta como token IDENT aquellas cadenas que tengan números
    #[doc(hidden)]
    #[test]
    fn invalid_identifier_with_numbers() {
        let actual = identifier(&mut "123ident_invalid");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token IDENT aquellas cadenas que tengan caracteres especiales
    #[doc(hidden)]
    #[test]
    fn invalid_identifier_with_special_characters() {
        let actual = identifier(&mut "ident@invalid");
        assert_ne!(actual.unwrap(), Token::new("ident@invalid".to_string(), TokenType::Ident));
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTPS
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_https() {
        let expected = TestUtilities::uri_test_token("https://ejemplo.com", 0);
        let actual = uri(&mut "https://ejemplo.com");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTP
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_http() {
        let expected = TestUtilities::uri_test_token("http://ejemplo.com", 0);
        let actual = uri(&mut "http://ejemplo.com");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con una '/' al final
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_slash_at_the_end() {
        let expected = TestUtilities::uri_test_token("https://ejemplo.com/", 0);
        let actual = uri(&mut "https://ejemplo.com/");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI de un fichero CSV remoto
    #[doc(hidden)]
    #[test]
    fn uri_to_a_csv_remote_file() {
        let expected = TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 0);
        let actual = uri(&mut "https://ejemplo.com/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token URI las URIs incorrectas
    #[doc(hidden)]
    #[test]
    fn invalid_format_uri() {
        let actual = uri(&mut "https:ejemplo.com");
        check_error(actual);
    }

    /// Comprueba que se detecta el token JdbcUrl a una base de datos
    #[doc(hidden)]
    #[test]
    fn valid_jdbc_url() {
        let expected = TestUtilities::jdbc_url_test_token("jdbc:mysql://localhost:3306/mydb", 0);
        let actual = jdbc_url(&mut "jdbc:mysql://localhost:3306/mydb");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token URI aquellas cadenas que tengan una URL JDBC incorrecta
    #[doc(hidden)]
    #[test]
    fn invalid_jdbc_url() {
        let actual = jdbc_url(&mut "jdbc:/localhost:3306/db");
        check_error(actual);
    }

    /// Comprueba que se detecta el token FilePath de un fichero CSV usando una ruta con file
    #[doc(hidden)]
    #[test]
    fn valid_file_absolute_path() {
        let expected =
            TestUtilities::file_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 0);
        let actual = file_path(&mut "file:///ejemplo/path/a/fichero/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta relativa
    #[doc(hidden)]
    #[test]
    fn valid_relative_path() {
        let expected = TestUtilities::path_test_token("ejemplo/fichero.csv", 0);
        let actual = path(&mut "ejemplo/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta absoluta
    #[doc(hidden)]
    #[test]
    fn valid_absolute_path() {
        let expected =
            TestUtilities::path_test_token("C:\\ejemplo\\path\\a\\fichero\\fichero.csv", 0);
        let actual = path(&mut "C:\\ejemplo\\path\\a\\fichero\\fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token FilePath aquellas cadenas que tengan un path incorrecto
    #[doc(hidden)]
    #[test]
    fn file_path_with_invalid_path() {
        let actual = file_path(&mut "file//");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path absoluto incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_absolute_path() {
        let actual = path(&mut "//..");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path relativo incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_relative_path() {
        let actual = path(&mut "ejemplo/");
        check_error(actual);
    }

    /// Comprueba que se detecta el token QUERYDEFINITION con una consulta SQL
    #[doc(hidden)]
    #[test]
    fn valid_query_definition_with_sql_query() {
        let expected =
            TestUtilities::query_definition_test_token("SELECT * FROM tabla WHERE id = '1'", 0);
        let actual = query_definition(&mut "sql: SELECT * FROM tabla WHERE id = '1'");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que tengan una consulta SQL incorrecta
    #[doc(hidden)]
    #[test]
    fn query_definition_with_invalid_sql_query() {
        let actual = query_definition(&mut "sql: SELECT FROM tabla");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token QUERYDEFINITION aquellas cadenas que tengan una consulta SQL sin ':sql' al comienzo
    #[doc(hidden)]
    #[test]
    fn query_definition_with_invalid_sql_query_withouth_sql_begining() {
        let actual = query_definition(&mut "SELECT * FROM tabla");
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
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("example", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("http://example.com/", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::source_test_token(2),
            TestUtilities::ident_test_token("films_csv_file", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::uri_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::query_test_token(3),
            TestUtilities::ident_test_token("query_sql", 3),
            TestUtilities::left_angle_bracket_test_token(3),
            TestUtilities::query_definition_test_token("SELECT * FROM example;", 3),
            TestUtilities::right_angle_bracket_test_token(3),
            TestUtilities::eof_test_token(3),
        ];
        let actual = lexer(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en un identificador
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_identifier() {
        let mut input = "PREFIX exam123ple: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en alguna cadena 'fija' (PREFIX, SOURCE, ...)
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_fix_string() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv
            QUERY query_sql <sql: SELECT * FROM example;>";
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
        assert!(
            actual.is_err(),
            "Se esperaba un error, pero se obtuvo: {:?}",
            actual
        );
    }
}
