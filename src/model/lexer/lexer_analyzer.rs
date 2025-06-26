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
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Prefix
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn prefix(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(PREFIX).parse_next(input)?;
    Ok(Token::new(PREFIX, TokenType::Prefix))
}

/// Encuentra el token Colon en la entrada
///
/// Acepta la entrada ':'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token :
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn colon(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(COLON).parse_next(input)?;
    Ok(Token::new(COLON, TokenType::Colon))
}

/// Encuentra el token SemiColon en la entrada
///
/// Acepta la entrada ';'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token ;
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn semicolon(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(SEMICOLON).parse_next(input)?;
    Ok(Token::new(SEMICOLON, TokenType::SemiColon))
}

/// Encuentra el token Equal en la entrada
///
/// Acepta la entrada '='
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token =
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn equal(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(EQUAL).parse_next(input)?;
    Ok(Token::new(EQUAL, TokenType::Equal))
}

/// Encuentra el token AccessDot en la entrada
///
/// Acepta la entrada '.'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token .
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn access_dot(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(ACCESS_DOT).parse_next(input)?;
    Ok(Token::new(ACCESS_DOT, TokenType::AccessDot))
}

/// Encuentra el token LeftAngleBracket en la entrada
///
/// Acepta la entrada '<'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token <
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn left_angle_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(LEFT_ANGLE_BRACKET).parse_next(input)?;
    Ok(Token::new(LEFT_ANGLE_BRACKET, TokenType::LeftAngleBracket))
}

/// Encuentra el token RightAngleBracket en la entrada
///
/// Acepta la entrada '>'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token >
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn right_angle_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(RIGHT_ANGLE_BRACKET).parse_next(input)?;
    Ok(Token::new(
        RIGHT_ANGLE_BRACKET,
        TokenType::RightAngleBracket,
    ))
}

/// Encuentra el token OpeningCurlyBrace en la entrada
///
/// Acepta la entrada '{'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token {
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn opening_curly_brace(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(OPENING_CURLY_BRACE).parse_next(input)?;
    Ok(Token::new(
        OPENING_CURLY_BRACE,
        TokenType::OpeningCurlyBrace,
    ))
}

/// Encuentra el token ClosingCurlyBrace en la entrada
///
/// Acepta la entrada '}'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token }
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn closing_curly_brace(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(CLOSING_CURLY_BRACE).parse_next(input)?;
    Ok(Token::new(
        CLOSING_CURLY_BRACE,
        TokenType::ClosingCurlyBrace,
    ))
}

/// Encuentra el token LeftBracket en la entrada
///
/// Acepta la entrada '['
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token [
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn left_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(LEFT_BRACKET).parse_next(input)?;
    Ok(Token::new(LEFT_BRACKET, TokenType::LeftBracket))
}

/// Encuentra el token RightBracket en la entrada
///
/// Acepta la entrada ']'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token ]
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn right_bracket(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(RIGHT_BRACKET).parse_next(input)?;
    Ok(Token::new(RIGHT_BRACKET, TokenType::RightBracket))
}

/// Encuentra el token Source en la entrada
///
/// Acepta la entrada 'SOURCE'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Source
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn source(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(SOURCE).parse_next(input)?;
    Ok(Token::new(SOURCE, TokenType::Source))
}

/// Encuentra el token Query en la entrada
///
/// Acepta la entrada 'QUERY'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Query
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn query(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(QUERY).parse_next(input)?;
    Ok(Token::new(QUERY, TokenType::Query))
}

/// Encuentra el token Iterator en la entrada
///
/// Acepta la entrada 'ITERATOR'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Iterator
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn iterator(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(ITERATOR).parse_next(input)?;
    Ok(Token::new(ITERATOR, TokenType::Iterator))
}

/// Encuentra el token Field en la entrada
///
/// Acepta la entrada 'FIELD'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Field
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn field(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(FIELD).parse_next(input)?;
    Ok(Token::new(FIELD, TokenType::Field))
}

/// Encuentra el token Expression en la entrada
///
/// Acepta la entrada 'EXPRESSION'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Expression
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn expression(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(EXPRESSION).parse_next(input)?;
    Ok(Token::new(EXPRESSION, TokenType::Expression))
}

/// Encuentra el token Union en la entrada
///
/// Acepta la entrada 'UNION'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Union
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn union(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(UNION).parse_next(input)?;
    Ok(Token::new(UNION, TokenType::Union))
}

/// Encuentra el token Join en la entrada
///
/// Acepta la entrada 'JOIN'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Join
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn join(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(JOIN).parse_next(input)?;
    Ok(Token::new(JOIN, TokenType::Join))
}

/// Encuentra el token On en la entrada
///
/// Acepta la entrada 'ON'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token On
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn on(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(ON).parse_next(input)?;
    Ok(Token::new(ON, TokenType::On))
}

/// Encuentra el token SqlType en la entrada
///
/// Acepta la entrada ':sql'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token SqlType
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn sql_type(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(SQL_TYPE).parse_next(input)?;
    Ok(Token::new(SQL_TYPE, TokenType::SqlType))
}

/// Encuentra el token CsvPerRow en la entrada
///
/// Acepta la entrada 'csvperrow'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token CsvPerRow
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn csv_per_row(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(CSV_PER_ROW).parse_next(input)?;
    Ok(Token::new(CSV_PER_ROW, TokenType::CsvPerRow))
}

/// Encuentra un token identificador en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres alfanuméricos; también acepta '_'
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Ident
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let ident = take_while(1.., |c: char| c.is_alphanumeric() || c == '_').parse_next(input)?;

    if ident.chars().next().unwrap().is_numeric() {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &ident.checkpoint(),
            StrContext::Label("No se permiten números al comienzo de los identificadores"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }
    Ok(Token::new(ident, TokenType::Ident))
}

/// Encuentra un token identificador clave en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres alfanuméricos; también acepta '@' al principio
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token KeyIdentifier
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn key_identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let key_ident =
        take_while(1.., |c: char| c.is_alphanumeric() || c == '_' || c == '@').parse_next(input)?;
    let re_ident = Regex::new(r"^@[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();

    if !re_ident.is_match(key_ident) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &key_ident.checkpoint(),
            StrContext::Label("Identificador clave incorrecto"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(key_ident, TokenType::KeyIdentifier))
}

/// Encuentra un token URI en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de URIs
///
/// # Parámetros
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

    Ok(Token::new(uri, TokenType::Uri))
}

/// Encuentra un token JdbcUrl en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de JDBC URLs
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token URI
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn jdbc_url(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let jdbc_url = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_jdbc_url = Regex::new(r"^jdbc:[a-zA-Z0-9]+:.*$").unwrap();

    if !re_jdbc_url.is_match(jdbc_url) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &jdbc_url.checkpoint(),
            StrContext::Label("JDBC URL incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(jdbc_url, TokenType::JdbcUrl))
}

/// Encuentra un token Path en la entrada
///
/// Acepta como entrada cualquier cadena de caracteres que cumpla con la expresión regular de una ruta relativa o absoluta
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token Path
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn path(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let mut path = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_path = Regex::new(
        r"(?ix)                    
            ^(
                file://[a-zA-Z]:[\\/](?:[\w\-. ]+[\\/]?)*[\w\-. ]+\.\w+    # rutas absolutas
                |
                file://(\.{0,2}[\\/])?(?:[\w\-.\\\/]+[\\/])+[\w\-.\\\/*]+\.\w+   # rutas relativas
                |
                [a-zA-Z]:[\\/](?:[\w\-. ]+[\\/]?)*[\w\-. ]+\.\w+    # rutas absolutas sin file://
                |
                (\.{0,2}[\\/])?(?:[\w\-.\\\/]+[\\/])+[\w\-.\\\/*]+\.\w+   # rutas relativas sin file://
            )$
            ",
    )
    .unwrap();

    if !re_path.is_match(path) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &path.checkpoint(),
            StrContext::Label("Ruta absoluta o relativa incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    if path.starts_with("file://") {
        path = path.strip_prefix("file://").unwrap_or(path);
    }

    Ok(Token::new(path, TokenType::Path))
}

/// Encuentra un token SqlQuery en la entrada
///
/// Acepta como entrada consultas SQL
///
/// # Parámetros
/// * `input` - Parte del fichero que se está analizando
///
/// # Retorna
/// Un token SqlQuery
///
/// # Errores
/// Devuelve un `[ErrMode<ContextError>]` en el caso de que ocurra algún fallo durante el análisis de la entrada
fn sql_query(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let mut query_definition = take_while(1.., |c: char| c != '>').parse_next(input)?;
    let re_query_definition = Regex::new(r"(?ix)
            ^(?:                                                          
                SELECT\b\s+.+?\bFROM\b\s+.+?(?:\bWHERE\b\s+.+?)?(?:\bGROUP\s+BY\b\s+.+?)?(?:\bORDER\s+BY\b\s+.+?)?
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

    Ok(Token::new(query_definition, TokenType::SqlQuery))
}

/// Realiza el análisis léxico de la entrada
///
/// Analiza la entrada y va encontrando tokens. A medida que los encuentra los va almacenando en un vector
///
/// # Parámetros
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
/// # Parámetros
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
/// # Parámetros
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
/// # Parámetros
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
    // alt tiene un límite de 21 parámetros, por lo que es necesario crear más alt dentro de uno
    match alt((
        // Elementos básicos
        alt((
            colon,
            semicolon,
            equal,
            access_dot,
            left_angle_bracket,
            right_angle_bracket,
            opening_curly_brace,
            closing_curly_brace,
            left_bracket,
            right_bracket,
        )),
        // Palabras reservadas
        alt((
            prefix,
            source,
            query,
            iterator,
            field,
            expression,
            union,
            join,
            on,
            sql_type,
            csv_per_row,
        )),
        // Elementos variables; no tienen un valor fijo
        alt((sql_query, path, jdbc_url, uri, key_identifier, identifier)),
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
                    token_error.unwrap(),
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

    /// Comprueba que se detecta el token Prefix
    #[doc(hidden)]
    #[test]
    fn valid_prefix() {
        let expected = Token::create_test_token(PREFIX, 0, TokenType::Prefix);
        let actual = prefix(&mut "PREFIX");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token aquellas cadenas que no sean Prefix
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
        let expected = Token::create_test_token(COLON, 0, TokenType::Colon);
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

    /// Comprueba que se detecta el token ;
    #[doc(hidden)]
    #[test]
    fn valid_semicolon() {
        let expected = Token::create_test_token(SEMICOLON, 0, TokenType::SemiColon);
        let actual = semicolon(&mut ";");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token ; aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_semicolon() {
        let actual = semicolon(&mut ":");
        check_error(actual);
    }

    /// Comprueba que se detecta el token =
    #[doc(hidden)]
    #[test]
    fn valid_equal() {
        let expected = Token::create_test_token(EQUAL, 0, TokenType::Equal);
        let actual = equal(&mut "=");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token = aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_equal() {
        let actual = equal(&mut ":");
        check_error(actual);
    }

    /// Comprueba que se detecta el token .
    #[doc(hidden)]
    #[test]
    fn valid_access_dot() {
        let expected = Token::create_test_token(ACCESS_DOT, 0, TokenType::AccessDot);
        let actual = access_dot(&mut ".");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token . aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_access_dot() {
        let actual = access_dot(&mut ":");
        check_error(actual);
    }

    /// Comprueba que se detecta el token <
    #[doc(hidden)]
    #[test]
    fn valid_left_angle_bracket() {
        let expected = Token::create_test_token(LEFT_ANGLE_BRACKET, 0, TokenType::LeftAngleBracket);
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
        let expected =
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 0, TokenType::RightAngleBracket);
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

    /// Comprueba que se detecta el token {
    #[doc(hidden)]
    #[test]
    fn valid_opening_curly_brace() {
        let expected =
            Token::create_test_token(OPENING_CURLY_BRACE, 0, TokenType::OpeningCurlyBrace);
        let actual = opening_curly_brace(&mut "{");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token { aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_opening_curly_brace() {
        let actual = left_angle_bracket(&mut "}");
        check_error(actual);
    }

    /// Comprueba que se detecta el token }
    #[doc(hidden)]
    #[test]
    fn valid_closing_curly_brace() {
        let expected =
            Token::create_test_token(CLOSING_CURLY_BRACE, 0, TokenType::ClosingCurlyBrace);
        let actual = closing_curly_brace(&mut "}");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token } aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_closing_curly_brace() {
        let actual = closing_curly_brace(&mut "{");
        check_error(actual);
    }

    /// Comprueba que se detecta el token [
    #[doc(hidden)]
    #[test]
    fn valid_left_bracket() {
        let expected = Token::create_test_token(LEFT_BRACKET, 0, TokenType::LeftBracket);
        let actual = left_bracket(&mut "[");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token [ aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_left_bracket() {
        let actual = left_bracket(&mut "]");
        check_error(actual);
    }

    /// Comprueba que se detecta el token ]
    #[doc(hidden)]
    #[test]
    fn valid_right_bracket() {
        let expected = Token::create_test_token(RIGHT_BRACKET, 0, TokenType::RightBracket);
        let actual = right_bracket(&mut "]");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token ] aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_right_bracket() {
        let actual = right_bracket(&mut "[");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Source
    #[doc(hidden)]
    #[test]
    fn valid_source() {
        let expected = Token::create_test_token(SOURCE, 0, TokenType::Source);
        let actual = source(&mut "SOURCE");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Source aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_source() {
        let actual = source(&mut "SOUR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Query
    #[doc(hidden)]
    #[test]
    fn valid_query() {
        let expected = Token::create_test_token(QUERY, 0, TokenType::Query);
        let actual = query(&mut "QUERY");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Query aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_query() {
        let actual = query(&mut "QURY");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Iterator
    #[doc(hidden)]
    #[test]
    fn valid_iterator() {
        let expected = Token::create_test_token(ITERATOR, 0, TokenType::Iterator);
        let actual = iterator(&mut "ITERATOR");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Iterator aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_iterator() {
        let actual = query(&mut "ITR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Field
    #[doc(hidden)]
    #[test]
    fn valid_field() {
        let expected = Token::create_test_token(FIELD, 0, TokenType::Field);
        let actual = field(&mut "FIELD");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Field aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_field() {
        let actual = field(&mut "ITR");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Expression
    #[doc(hidden)]
    #[test]
    fn valid_expression() {
        let expected = Token::create_test_token(EXPRESSION, 0, TokenType::Expression);
        let actual = expression(&mut "EXPRESSION");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Expression aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_expression() {
        let actual = expression(&mut "FIELD");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Union
    #[doc(hidden)]
    #[test]
    fn valid_union() {
        let expected = Token::create_test_token(UNION, 0, TokenType::Union);
        let actual = union(&mut "UNION");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Union aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_union() {
        let actual = union(&mut "FIELD");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Join
    #[doc(hidden)]
    #[test]
    fn valid_join() {
        let expected = Token::create_test_token(JOIN, 0, TokenType::Join);
        let actual = join(&mut "JOIN");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Join aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_join() {
        let actual = join(&mut "UNION");
        check_error(actual);
    }

    /// Comprueba que se detecta el token On
    #[doc(hidden)]
    #[test]
    fn valid_on() {
        let expected = Token::create_test_token(ON, 0, TokenType::On);
        let actual = on(&mut "ON");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token On aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_on() {
        let actual = on(&mut "UNION");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SqlType
    #[doc(hidden)]
    #[test]
    fn valid_sql_type() {
        let expected = Token::create_test_token(SQL_TYPE, 0, TokenType::SqlType);
        let actual = sql_type(&mut "sql:");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token SqlType aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_sql_type() {
        let actual = sql_type(&mut "sql");
        check_error(actual);
    }

    /// Comprueba que se detecta el token CsvPerRow
    #[doc(hidden)]
    #[test]
    fn valid_csv_per_row() {
        let expected = Token::create_test_token(CSV_PER_ROW, 0, TokenType::CsvPerRow);
        let actual = csv_per_row(&mut "csvperrow");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token CsvPerRow aquellas cadenas que no lo sean
    #[doc(hidden)]
    #[test]
    fn invalid_csv_per_row() {
        let actual = csv_per_row(&mut "csv_per_row");
        check_error(actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier sin que tenga un '_'
    #[doc(hidden)]
    #[test]
    fn valid_identifier_withouth_underscore() {
        let expected = Token::create_test_token("ident", 0, TokenType::Ident);
        let actual = identifier(&mut "ident");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Ident (identificadores) con un '_' en medio
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_inside() {
        let expected = Token::create_test_token("ident_valid", 0, TokenType::Ident);
        let actual = identifier(&mut "ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Ident (identificadores) con un '_ ' al comienzo
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_begining() {
        let expected = Token::create_test_token("_ident_valid", 0, TokenType::Ident);
        let actual = identifier(&mut "_ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Ident (identificadores) con un '_' al final
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_end() {
        let expected = Token::create_test_token("ident_valid_", 0, TokenType::Ident);
        let actual = identifier(&mut "ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Ident (identificadores) con un '_' al comienzo y final
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_underscore_at_the_begining_and_end() {
        let expected = Token::create_test_token("_ident_valid_", 0, TokenType::Ident);
        let actual = identifier(&mut "_ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Ident (identificadores) con números no al comienzo
    #[doc(hidden)]
    #[test]
    fn valid_identifier_with_numbers() {
        let expected = Token::create_test_token("ident1_2valid", 0, TokenType::Ident);
        let actual = identifier(&mut "ident1_2valid");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Ident aquellas cadenas que empiezan por números
    #[doc(hidden)]
    #[test]
    fn invalid_identifier_with_numbers_at_the_begining() {
        let actual = identifier(&mut "123ident_invalid");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token Ident aquellas cadenas que contienen caracteres especiales
    #[doc(hidden)]
    #[test]
    fn invalid_identifier_with_special_characters() {
        let actual = identifier(&mut "@ident_invalid");
        check_error(actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier sin que tenga un '_'
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_withouth_underscore() {
        let expected = Token::create_test_token("@ident", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@ident");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier con un '_' en medio
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_with_underscore_inside() {
        let expected = Token::create_test_token("@Ident_valid", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@Ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier con un '_ ' al comienzo
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_with_underscore_at_the_begining() {
        let expected = Token::create_test_token("@_ident_valid", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@_ident_valid");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier con un '_' al final
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_with_underscore_at_the_end() {
        let expected = Token::create_test_token("@ident_valid_", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier con un '_' al comienzo y final
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_with_underscore_at_the_begining_and_end() {
        let expected = Token::create_test_token("@_ident_valid_", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@_ident_valid_");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token KeyIdentifier con números no al comienzo
    #[doc(hidden)]
    #[test]
    fn valid_key_identifier_with_numbers() {
        let expected = Token::create_test_token("@ident1_2valid", 0, TokenType::KeyIdentifier);
        let actual = key_identifier(&mut "@ident1_2valid");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token KeyIdentifier aquellas cadenas que empiezan por números
    #[doc(hidden)]
    #[test]
    fn invalid_key_identifier_with_numbers_at_the_begining() {
        let actual = key_identifier(&mut "@1invalid_identifier");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token KeyIdentifier aquellas cadenas que no comienzan por '@'
    #[doc(hidden)]
    #[test]
    fn invalid_key_identifier_withouth_at_the_begining() {
        let actual = key_identifier(&mut "ident_invalid");
        check_error(actual);
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTPS
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_https() {
        let expected = Token::create_test_token("https://ejemplo.com", 0, TokenType::Uri);
        let actual = uri(&mut "https://ejemplo.com");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con el protocolo HTTP
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_http() {
        let expected = Token::create_test_token("http://ejemplo.com", 0, TokenType::Uri);
        let actual = uri(&mut "http://ejemplo.com");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI con una '/' al final
    #[doc(hidden)]
    #[test]
    fn valid_uri_with_slash_at_the_end() {
        let expected = Token::create_test_token("https://ejemplo.com/", 0, TokenType::Uri);
        let actual = uri(&mut "https://ejemplo.com/");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token URI de un fichero CSV remoto
    #[doc(hidden)]
    #[test]
    fn uri_to_a_csv_remote_file() {
        let expected =
            Token::create_test_token("https://ejemplo.com/fichero.csv", 0, TokenType::Uri);
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
        let expected =
            Token::create_test_token("jdbc:mysql://localhost:3306/mydb", 0, TokenType::JdbcUrl);
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

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta relativa
    #[doc(hidden)]
    #[test]
    fn valid_relative_path() {
        let expected = Token::create_test_token("files/fichero.csv", 0, TokenType::Path);
        let actual = path(&mut "files/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta absoluta
    #[doc(hidden)]
    #[test]
    fn valid_absolute_path() {
        let expected = Token::create_test_token(
            "C:\\ejemplo\\path\\a\\fichero\\fichero.csv",
            0,
            TokenType::Path,
        );
        let actual = path(&mut "file://C:\\ejemplo\\path\\a\\fichero\\fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path absoluto incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_absolute_path() {
        let actual = path(&mut "C:ejemplo\\path\\a\\fichero\\fichero.csv");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path relativo incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_relative_path() {
        let actual = path(&mut "file:ejemplo.csv");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SqlQuery con una consulta SQL
    #[doc(hidden)]
    #[test]
    fn valid_sql_query() {
        let expected =
            Token::create_test_token("SELECT * FROM tabla WHERE id = '1'", 0, TokenType::SqlQuery);
        let actual = sql_query(&mut "SELECT * FROM tabla WHERE id = '1'");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token SqlQuery aquellas cadenas que tengan una consulta SQL incorrecta
    #[doc(hidden)]
    #[test]
    fn sql_query_with_invalid_sql_query() {
        let actual = sql_query(&mut "SELECT FROM tabla");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token SqlQuery aquellas cadenas que tengan una consulta SQL sin ':sql' al comienzo
    #[doc(hidden)]
    #[test]
    fn sql_query_with_invalid_sql_query_with_sql_begining() {
        let actual = sql_query(&mut "sql: SELECT * FROM tabla");
        check_error(actual);
    }

    /// Comprueba que se detectan múltiples tokens distintos
    #[doc(hidden)]
    #[test]
    fn lexer_with_multiple_tokens() {
        let mut input = "PREFIX example: <http://example.com/>
            PREFIX dbr: <http://dbpedia.org/resource/>
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY inline_query <sql: SELECT * FROM example;>
            ITERATOR films_csv <csvperrow> {
                FIELD id <@id>
                FIELD name <name>
                FIELD year <year>
                FIELD country <country>
                FIELD director <director>
            }
            EXPRESSION films <films_csv_file.films_csv>
            example:Films example:[films.id] {
                example:name [films.name] ;
                example:year [films.year] ;
                example:country dbr:[films.country] ;
                example:director dbr:[films.director] ;
            }";

        let expected: Vec<Token> = vec![
            // Prefix
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("http://example.com/", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(PREFIX, 2, TokenType::Prefix),
            Token::create_test_token("dbr", 2, TokenType::Ident),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("http://dbpedia.org/resource/", 2, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            // Source
            Token::create_test_token(SOURCE, 3, TokenType::Source),
            Token::create_test_token("films_csv_file", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "https://shexml.herminiogarcia.com/files/films.csv",
                3,
                TokenType::Uri,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            // Query
            Token::create_test_token(QUERY, 4, TokenType::Query),
            Token::create_test_token("inline_query", 4, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 4, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 4, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 4, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 4, TokenType::RightAngleBracket),
            // Iterator
            Token::create_test_token(ITERATOR, 5, TokenType::Iterator),
            Token::create_test_token("films_csv", 5, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 5, TokenType::LeftAngleBracket),
            Token::create_test_token(CSV_PER_ROW, 5, TokenType::CsvPerRow),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 5, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 5, TokenType::OpeningCurlyBrace),
            // Field
            Token::create_test_token(FIELD, 6, TokenType::Field),
            Token::create_test_token("id", 6, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 6, TokenType::LeftAngleBracket),
            Token::create_test_token("@id", 6, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 6, TokenType::RightAngleBracket),
            // Field
            Token::create_test_token(FIELD, 7, TokenType::Field),
            Token::create_test_token("name", 7, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 7, TokenType::LeftAngleBracket),
            Token::create_test_token("name", 7, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 7, TokenType::RightAngleBracket),
            // Field
            Token::create_test_token(FIELD, 8, TokenType::Field),
            Token::create_test_token("year", 8, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 8, TokenType::LeftAngleBracket),
            Token::create_test_token("year", 8, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 8, TokenType::RightAngleBracket),
            // Field
            Token::create_test_token(FIELD, 9, TokenType::Field),
            Token::create_test_token("country", 9, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 9, TokenType::LeftAngleBracket),
            Token::create_test_token("country", 9, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 9, TokenType::RightAngleBracket),
            // Field
            Token::create_test_token(FIELD, 10, TokenType::Field),
            Token::create_test_token("director", 10, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 10, TokenType::LeftAngleBracket),
            Token::create_test_token("director", 10, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 10, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 11, TokenType::ClosingCurlyBrace),
            // Expression
            Token::create_test_token(EXPRESSION, 12, TokenType::Expression),
            Token::create_test_token("films", 12, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 12, TokenType::LeftAngleBracket),
            Token::create_test_token("films_csv_file", 12, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 12, TokenType::AccessDot),
            Token::create_test_token("films_csv", 12, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 12, TokenType::RightAngleBracket),
            // Shape
            Token::create_test_token("example", 13, TokenType::Ident),
            Token::create_test_token(COLON, 13, TokenType::Colon),
            Token::create_test_token("Films", 13, TokenType::Ident),
            Token::create_test_token("example", 13, TokenType::Ident),
            Token::create_test_token(COLON, 13, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 13, TokenType::LeftBracket),
            Token::create_test_token("films", 13, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 13, TokenType::AccessDot),
            Token::create_test_token("id", 13, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 13, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 13, TokenType::OpeningCurlyBrace),
            // ShapeTuple
            Token::create_test_token("example", 14, TokenType::Ident),
            Token::create_test_token(COLON, 14, TokenType::Colon),
            Token::create_test_token("name", 14, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 14, TokenType::LeftBracket),
            Token::create_test_token("films", 14, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 14, TokenType::AccessDot),
            Token::create_test_token("name", 14, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 14, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 14, TokenType::SemiColon),
            // ShapeTuple
            Token::create_test_token("example", 15, TokenType::Ident),
            Token::create_test_token(COLON, 15, TokenType::Colon),
            Token::create_test_token("year", 15, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 15, TokenType::LeftBracket),
            Token::create_test_token("films", 15, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 15, TokenType::AccessDot),
            Token::create_test_token("year", 15, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 15, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 15, TokenType::SemiColon),
            // ShapeTuple
            Token::create_test_token("example", 16, TokenType::Ident),
            Token::create_test_token(COLON, 16, TokenType::Colon),
            Token::create_test_token("country", 16, TokenType::Ident),
            Token::create_test_token("dbr", 16, TokenType::Ident),
            Token::create_test_token(COLON, 16, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 16, TokenType::LeftBracket),
            Token::create_test_token("films", 16, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 16, TokenType::AccessDot),
            Token::create_test_token("country", 16, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 16, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 16, TokenType::SemiColon),
            // ShapeTuple
            Token::create_test_token("example", 17, TokenType::Ident),
            Token::create_test_token(COLON, 17, TokenType::Colon),
            Token::create_test_token("director", 17, TokenType::Ident),
            Token::create_test_token("dbr", 17, TokenType::Ident),
            Token::create_test_token(COLON, 17, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 17, TokenType::LeftBracket),
            Token::create_test_token("films", 17, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 17, TokenType::AccessDot),
            Token::create_test_token("director", 17, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 17, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 17, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 18, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 18, TokenType::EOF),
        ];
        let actual = lexer(&mut input).unwrap();
        assert_eq!(expected, actual);
    }

    /// Comprueba que se detectan errores si, entre múltiples tokens, hay algún error en un identificador
    #[doc(hidden)]
    #[test]
    fn lexer_with_invalid_identifier() {
        let mut input = "PREFIX 123example: <http://example.com/>
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
    /// # Parámetros
    /// * `expected` - El token esperado
    /// * `actual` - El token real
    fn check_ok(expected: Token, actual: Result<Token, ErrMode<ContextError>>) {
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el resultado actual del test es un error
    ///
    /// # Parámetros
    /// * `actual` - Un Result con el error esperado
    fn check_error(actual: Result<Token, ErrMode<ContextError>>) {
        assert!(
            actual.is_err(),
            "Se esperaba un error, pero se obtuvo: {:?}",
            actual
        );
    }
}
