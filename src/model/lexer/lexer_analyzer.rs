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
    
    if !re_source_path.is_match(source_path) {
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
/// * Consultas externas SPARQL en ficheros externos .sparql
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
                |
                file://[/\\]?[^\s\r\n]+\.(?:sparql|sql)   # Ficheros .sparql y .sql
                |
                [a-zA-Z]:[\\/](?:[\w\-. ]+[\\/]?)*[\w\-. ]+\.(?:sparql|sql) # rutas absolutas 
                |
                (\.{0,2}[\\/])?(?:[\w\-.]+[\\/])*[\w\-.]+\.(?:sparql|sql)  # rutas locales
                |                            
                sql:\s*\bSELECT\b\s+.+?\bFROM\b\s+.+?(?:\bWHERE\b\s+.+?)?(?:\bGROUP\s+BY\b\s+.+?)?(?:\bORDER\s+BY\b\s+.+?)?    # SQL
                |
                https?://[\w\-.]+(?:/[^\s\r\n\t]*)*\.(?:sparql|sql)  # URLs remotas a ficheros
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
    use super::*;

    // En los tests el número de línea de los tokens es 0 porque todavía no se le asigna

    /// Comprueba que se detecta el token PREFIX
    #[doc(hidden)]
    #[test]
    fn test_prefix_ok() {
        let expected = TestTokens::prefix_test_token(0);
        let actual = prefix(&mut "PREFIX");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token aquellas cadenas que no sean PREFIX
    #[doc(hidden)]
    #[test]
    fn test_prefix_fail() {
        let actual = prefix(&mut "PRFIX");
        check_error(actual);
    }

    /// Comprueba que se detecta el token :
    #[doc(hidden)]
    #[test]
    fn test_colon_ok() {
        let expected = TestTokens::colon_test_token(0);
        let actual = colon(&mut ":");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token : aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_colon_fail() {
        let actual = colon(&mut ";");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SOURCE
    #[doc(hidden)]
    #[test]
    fn test_source_ok() {
        let expected = TestTokens::source_test_token(0);
        let actual = source(&mut "SOURCE");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token SOURCE aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_source_fail() {
        // Fail test
        let actual = source(&mut "SOUR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token IDENT; los identificadores
    #[doc(hidden)]
    #[test]
    fn test_identifier_ok() {
        // Test con identificador sin _
        let expected = TestTokens::ident_test_token("ident",0);
        let actual = identifier(&mut "ident");
        check_ok(expected, actual);

        // Test con identificador con _ entre 2 secuencias de caracteres
        let expected = TestTokens::ident_test_token("ident_valid",0);
        let actual = identifier(&mut "ident_valid");
        check_ok(expected, actual);

        // Test con identificador con _ al principio del identificador
        let expected = TestTokens::ident_test_token("_ident_valid", 0);
        let actual = identifier(&mut "_ident_valid");
        check_ok(expected, actual);

        // Test con identificador con _ al final del identificador
        let expected = TestTokens::ident_test_token("ident_valid_", 0);
        let actual = identifier(&mut "ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token IDENT aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_identifier_fail() {
        let actual = identifier(&mut "123ident_invalid");
        check_error(actual);
    }

    /// Comprueba que se detecta el token URI
    #[doc(hidden)]
    #[test]
    fn test_uri_ok() {
        // Test con URI con el protocolo HTTPS
        let expected = TestTokens::uri_test_token("https://ejemplo.com",0);
        let actual = uri(&mut "<https://ejemplo.com>");
        check_ok(expected, actual);

        // Test con URI con el protocolo HTTP
        let expected = TestTokens::uri_test_token("http://ejemplo.com", 0);
        let actual = uri(&mut "<http://ejemplo.com>");
        check_ok(expected, actual);

        // Test con URI acabada en /
        let expected = TestTokens::uri_test_token("https://ejemplo.com/", 0);
        let actual = uri(&mut "<https://ejemplo.com/>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token uri aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_uri_fail() {
        // Test con > faltante al final de la URI
        let actual = uri(&mut "<https://ejemplo.com");
        check_error(actual);

        // Test con < faltante al comienzo de la URI
        let actual = uri(&mut "https://ejemplo.com>");
        check_error(actual);

        // Test con < y > faltantes
        let actual = uri(&mut "https://ejemplo.com");
        check_error(actual);

        // Test con una URI incorrecta
        let actual = uri(&mut "<https:ejemplo.com>");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SOURCEPATH
    #[doc(hidden)]
    #[test]
    fn test_source_path_ok() {
        // Test con SOURCEPATH con un fichero CSV en remoto
        let expected = TestTokens::source_path_test_token("https://ejemplo.com/fichero.csv",0);
        let actual = source_path(&mut "<https://ejemplo.com/fichero.csv>");
        check_ok(expected, actual);

        // Test con SOURCEPATH con un fichero CSV en local con ruta relativa
        let expected = TestTokens::source_path_test_token("ejemplo/fichero.csv", 0);
        let actual = source_path(&mut "<ejemplo/fichero.csv>");
        check_ok(expected, actual);

        // Test con SOURCEPATH con un fichero CSV en local con ruta absoluta con file
        let expected = TestTokens::source_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 0);
        let actual = source_path(&mut "<file:///ejemplo/path/a/fichero/fichero.csv>");
        check_ok(expected, actual);

        // Test con SOURCEPATH con un fichero CSV en local con ruta absoluta sin file
        let expected = TestTokens::source_path_test_token("C:\\ejemplo\\path\\a\\fichero\\fichero.csv", 0);
        let actual = source_path(&mut "<C:\\ejemplo\\path\\a\\fichero\\fichero.csv>");
        check_ok(expected, actual);

        // Test con SOURCEPATH con una base de datos
        let expected = TestTokens::source_path_test_token("jdbc:mysql://localhost:3306/mydb", 0);
        let actual = source_path(&mut "<jdbc:mysql://localhost:3306/mydb>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token source_path aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_source_path_fail() {
        // Test con > faltante al final del SOURCEPATH
        let actual = source_path(&mut "<https://ejemplo.com");
        check_error(actual);

        // Test con < faltante al final del SOURCEPATH
        let actual = source_path(&mut "https://ejemplo.com>");
        check_error(actual);

        // Test con < y > faltantes 
        let actual = source_path(&mut "https://ejemplo.com");
        check_error(actual);

        // Test con una URI incorrecta
        let actual = source_path(&mut "<https:ejemplo.com/fichero.csv>");
        check_error(actual);

        // Test con un path absoluto con file incorrecto
        let actual = source_path(&mut "<file//>");
        check_error(actual);

        // Test con un path absoluto sin file incorrecto
        let actual = source_path(&mut "<//..>");
        check_error(actual);

        // Test con un path relativo incorrecto
        let actual = source_path(&mut "<ejemplo/>");
        check_error(actual);

        // Test con un tipo de fichero incorrecto
        let actual = source_path(&mut "<ejemplo/fichero.xml>");
        check_error(actual);

        // Test con una JBDC URL incorrecta
        let actual = source_path(&mut "<jdbc:/localhost:3306/db>");
        check_error(actual);
    }

    /// Comprueba que se detecta el token QUERYDEFINITION
    #[doc(hidden)]
    #[test]
    fn test_query_definition_ok() {
        // Test con QUERYDEFINITION con un fichero .sql en remoto
        let expected = TestTokens::query_definition_test_token("https://ejemplo.com/fichero.sql",0);
        let actual = query_definition(&mut "<https://ejemplo.com/fichero.sql>");
        check_ok(expected, actual);

        // Test con QUERYDEFINITION con un fichero .sparql en local con ruta relativa
        let expected = TestTokens::query_definition_test_token("ejemplo/fichero.sparql", 0);
        let actual = query_definition(&mut "<ejemplo/fichero.sparql>");
        check_ok(expected, actual);

        // Test con QUERYDEFINITION con un fichero .sql en local con ruta absoluta con file
        let expected = TestTokens::query_definition_test_token("file:///ejemplo/path/a/fichero/fichero.sql", 0);
        let actual = query_definition(&mut "<file:///ejemplo/path/a/fichero/fichero.sql>");
        check_ok(expected, actual);

        // Test con QUERYDEFINITION con un fichero .sparql en local con ruta absoluta sin file
        let expected = TestTokens::query_definition_test_token("C:\\ejemplo\\path\\a\\fichero\\fichero.sparql", 0);
        let actual = query_definition(&mut "<C:\\ejemplo\\path\\a\\fichero\\fichero.sparql>");
        check_ok(expected, actual);

        // Test con QUERYDEFINITION con una consulta SQL
        let expected = TestTokens::query_definition_test_token("SELECT * FROM tabla WHERE id = '1'", 0);
        let actual = query_definition(&mut "<sql: SELECT * FROM tabla WHERE id = '1'>");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token query_definition aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn test_query_definition_fail() {
        // Test con > faltante al final del QUERYDEFINITION
        let actual = query_definition(&mut "<https://ejemplo.com");
        check_error(actual);

        // Test con < faltante al final del QUERYDEFINITION
        let actual = query_definition(&mut "https://ejemplo.com>");
        check_error(actual);

        // Test con < y > faltantes 
        let actual = query_definition(&mut "https://ejemplo.com");
        check_error(actual);

        // Test con una URL incorrecta
        let actual = query_definition(&mut "<https:ejemplo.com/fichero.sparql>");
        check_error(actual);

        // Test con un path absoluto con file incorrecto
        let actual = query_definition(&mut "<file//>");
        check_error(actual);

        // Test con un path absoluto sin file incorrecto
        let actual = query_definition(&mut "<//..>");
        check_error(actual);

        // Test con un path relativo incorrecto
        let actual = query_definition(&mut "<ejemplo/>");
        check_error(actual);

        // Test con un tipo de fichero incorrecto
        let actual = query_definition(&mut "<ejemplo/fichero.csv>");
        check_error(actual);

        // Test con una consulta SQL incorrecta
        let actual = query_definition(&mut "<sql: SELECT FROM tabla>");
        check_error(actual);

        // Test con una consulta SQL sin :sql
        let actual = query_definition(&mut "<SELECT * FROM tabla>");
        check_error(actual);
    }

    /// Comprueba que se detectan múltiples tokens distintos
    #[doc(hidden)]
    #[test]
    fn test_lexer_ok() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";

        let expected: Vec<Token> = vec![
            TestTokens::prefix_test_token(1), TestTokens::ident_test_token("example", 1), TestTokens::colon_test_token(1), TestTokens::uri_test_token("http://example.com/", 1), 
            TestTokens::source_test_token(2), TestTokens::ident_test_token("films_csv_file", 2), TestTokens::source_path_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2), 
            TestTokens::query_test_token(3), TestTokens::ident_test_token("query_sql", 3), TestTokens::query_definition_test_token("SELECT * FROM example;", 3),
            TestTokens::eof_test_token(3)];
        let actual = lexer(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay alguna cadena con algún error
    #[doc(hidden)]
    #[test]
    fn test_lexer_fail() {
        // Test con un error en el identificador de PREFIX
        let mut input = "PREFIX example123: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());

        // Test con un error en el path de SOURCE
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv
            QUERY query_sql <sql: SELECT * FROM example;>";
        let actual = lexer(&mut input);
        assert!(actual.is_err());

        // Test con un error en QUERY
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