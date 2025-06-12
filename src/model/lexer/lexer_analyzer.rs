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
    Ok(Token::new(PREFIX.to_string(), TokenType::Prefix))
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
    Ok(Token::new(COLON.to_string(), TokenType::Colon))
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
    Ok(Token::new(SEMICOLON.to_string(), TokenType::SemiColon))
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
    Ok(Token::new(EQUAL.to_string(), TokenType::Equal))
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
    Ok(Token::new(ACCESS_DOT.to_string(), TokenType::AccessDot))
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
    Ok(Token::new(
        LEFT_ANGLE_BRACKET.to_string(),
        TokenType::LeftAngleBracket,
    ))
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
        RIGHT_ANGLE_BRACKET.to_string(),
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
        OPENING_CURLY_BRACE.to_string(),
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
        CLOSING_CURLY_BRACE.to_string(),
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
    Ok(Token::new(
        LEFT_BRACKET.to_string(),
        TokenType::LeftBracket,
    ))
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
    Ok(Token::new(
        RIGHT_BRACKET.to_string(),
        TokenType::RightBracket,
    ))
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
    Ok(Token::new(SOURCE.to_string(), TokenType::Source))
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
    Ok(Token::new(QUERY.to_string(), TokenType::Query))
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
    Ok(Token::new(ITERATOR.to_string(), TokenType::Iterator))
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
    Ok(Token::new(FIELD.to_string(), TokenType::Field))
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
    Ok(Token::new(EXPRESSION.to_string(), TokenType::Expression))
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
    Ok(Token::new(UNION.to_string(), TokenType::Union))
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
    Ok(Token::new(JOIN.to_string(), TokenType::Join))
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
    Ok(Token::new(ON.to_string(), TokenType::On))
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
    Ok(Token::new(SQL_TYPE.to_string(), TokenType::SqlType))
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
    Ok(Token::new(CSV_PER_ROW.to_string(), TokenType::CsvPerRow))
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
    Ok(Token::new(ident.to_string(), TokenType::Ident))
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

    Ok(Token::new(key_ident.to_string(), TokenType::KeyIdentifier))
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

    Ok(Token::new(uri.to_string(), TokenType::Uri))
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
/// # Parámetros
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

    if !re_file_path.is_match(file_path) {
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
/// # Parámetros
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

    if !re_path.is_match(path) {
        let error = &ContextError::new().add_context(
            &"Formato incorrecto",
            &path.checkpoint(),
            StrContext::Label("Ruta absoluta o relativa incorrecta"),
        );
        return Err(ErrMode::Backtrack(error.clone()));
    }

    Ok(Token::new(path.to_string(), TokenType::Path))
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

    Ok(Token::new(
        query_definition.to_string(),
        TokenType::SqlQuery,
    ))
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
        alt((
            sql_query,
            file_path,
            path,
            jdbc_url,
            uri,
            key_identifier,
            identifier,
        )),
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
        let expected = Token::create_test_token(RIGHT_ANGLE_BRACKET, 0, TokenType::RightAngleBracket);
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
        let expected = Token::create_test_token(OPENING_CURLY_BRACE, 0, TokenType::OpeningCurlyBrace);
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
        let expected = Token::create_test_token(CLOSING_CURLY_BRACE, 0, TokenType::ClosingCurlyBrace);
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
        let expected = Token::create_test_token("https://ejemplo.com/fichero.csv", 0, TokenType::Uri);
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
        let expected = Token::create_test_token("jdbc:mysql://localhost:3306/mydb", 0, TokenType::JdbcUrl);
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
    fn valid_file_path() {
        let expected =
            Token::create_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 0, TokenType::FilePath);
        let actual = file_path(&mut "file:///ejemplo/path/a/fichero/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token FilePath aquellas cadenas que tengan un path incorrecto
    #[doc(hidden)]
    #[test]
    fn file_path_with_invalid_path() {
        let actual = file_path(&mut "file//");
        check_error(actual);
    }

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta relativa
    #[doc(hidden)]
    #[test]
    fn valid_relative_path() {
        let expected = Token::create_test_token("ejemplo/fichero.csv", 0, TokenType::Path);
        let actual = path(&mut "ejemplo/fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que se detecta el token Path de un fichero CSV usando una ruta absoluta
    #[doc(hidden)]
    #[test]
    fn valid_absolute_path() {
        let expected =
            Token::create_test_token("C:\\ejemplo\\path\\a\\fichero\\fichero.csv", 0, TokenType::Path);
        let actual = path(&mut "C:\\ejemplo\\path\\a\\fichero\\fichero.csv");
        check_ok(expected, actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path absoluto incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_absolute_path() {
        let actual = path(&mut "C://..");
        check_error(actual);
    }

    /// Comprueba que no se detecta como token Path aquellas cadenas que tengan un path relativo incorrecto
    #[doc(hidden)]
    #[test]
    fn path_with_invalid_relative_path() {
        let actual = path(&mut "ejemplo/");
        check_error(actual);
    }

    /// Comprueba que se detecta el token SqlQuery con una consulta SQL
    #[doc(hidden)]
    #[test]
    fn valid_sql_query() {
        let expected = Token::create_test_token("SELECT * FROM tabla WHERE id = '1'", 0, TokenType::SqlQuery);
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
            SOURCE films_csv_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>
            ITERATOR iterator <query_sql> {
                FIELD field1 <@key>
                FIELD field2 <attribute>
            }";

        let expected: Vec<Token> = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("http://example.com/", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),

            Token::create_test_token(SOURCE, 2, TokenType::Source),
            Token::create_test_token("films_csv_file", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),

            Token::create_test_token(QUERY, 3, TokenType::Query),
            Token::create_test_token("query_sql", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 3, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 3, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),

            Token::create_test_token(ITERATOR, 4, TokenType::Iterator),
            Token::create_test_token("iterator", 4, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 4, TokenType::LeftAngleBracket),
            Token::create_test_token("query_sql", 4, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 4, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 4, TokenType::OpeningCurlyBrace),

            Token::create_test_token(FIELD, 5, TokenType::Field),
            Token::create_test_token("field1", 5, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 5, TokenType::LeftAngleBracket),
            Token::create_test_token("@key", 5, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 5, TokenType::RightAngleBracket),

            Token::create_test_token(FIELD, 6, TokenType::Field),
            Token::create_test_token("field2", 6, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 6, TokenType::LeftAngleBracket),
            Token::create_test_token("attribute", 6, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 6, TokenType::RightAngleBracket),

            Token::create_test_token(CLOSING_CURLY_BRACE, 7, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 7, TokenType::EOF),
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
