//! Módulo del analizador léxico
//! 
//! Toma como entrada el fichero .shexml que indique el usuario y extrae los tokens de este
//! También puede indicar errores léxicos en el que caso de que encuentre algún lexema en la entrada que no esté incluido en la especificación ShExML

use winnow::error::{ContextError, ErrMode};
use winnow::prelude::*;
use winnow::token::{take_while, literal};
use winnow::combinator::{alt, delimited};

use super::parser_error::ParserError;
use super::token::*;

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
    let ident = take_while(1.., |c: char| c.is_alphabetic() || c == '_')
        .parse_next(input)?;

    Ok(Token::new(ident.to_string(), TokenType::IDENT))
}

/// Encuentra un token URI en la entrada
/// 
/// Acepta como entrada cualquier cadena de caracteres entre < y >
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
    let uri = delimited('<', take_while(1.., |c: char| c != '>'), '>')
        .parse_next(input)?;

    Ok(Token::new(uri.to_string(), TokenType::URI))
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
pub fn lexer(input: &mut &str) -> Result<Vec<Token>, Vec<ParserError>> {
    let mut tokens = Vec::new();
    let mut errors: Vec<ParserError> = Vec::new();

    look_over_input(input, &mut tokens, &mut errors);

    end_lexer(tokens, errors)
}

/// Finaliza el análisis léxico
/// 
/// Si no hay errores devuelve el vector de tokens detectado y, si hay errores, los devuelve
/// 
/// # Argumentos
/// * `tokens` - El vector de tokens resultado del análisis léxico
/// * `errors` - El vector de errores léxicos detectados
/// 
/// # Retorna
/// El vector de tokens si no hay errores
/// 
/// # Errores
/// Devuelve un `[Vec<ParserError>]` El vector de errores detectados en el análisis léxico
fn end_lexer(mut tokens: Vec<Token>, errors: Vec<ParserError>) -> Result<Vec<Token>, Vec<ParserError>> {
    if errors.is_empty() {
        tokens.push(Token::create_eof_token());
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
fn look_over_input(input: &mut &str, tokens: &mut Vec<Token>, errors: &mut Vec<ParserError>) {
    let mut num_line = 1;

    while !input.is_empty() {
        match_alternatives(input, tokens, num_line, errors);

        let new_line: Result<&str, ErrMode<ContextError>> = take_while(1.., |c: char| c == '\n' || c == '\r').parse_next(input); // Se ignoran los saltos de línea
        if new_line.is_ok() {
            num_line += 1;
        }   

        let _: Result<&str, ErrMode<ContextError>> = take_while(1.., |c: char| c.is_whitespace() || c == '\t').parse_next(input); // Se ignoran los espacios
    }
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
fn match_alternatives(input: &mut &str, tokens: &mut Vec<Token>, num_line: u16, errors: &mut Vec<ParserError>) {
    match alt((colon, prefix, source, uri, identifier)).parse_next(input) {
        Ok(mut token) => {
            token.set_num_line(num_line);
            tokens.push(token);
        }

        // Si no es ningún token, se pasa
        Err(ErrMode::Backtrack(_)) => {
            let token_error: Result<&str, ErrMode<ContextError>> = take_while(1, |c: char| c.is_ascii()).parse_next(input);
            if token_error.is_ok() {
                errors.push(ParserError::new(format!("Error léxico: '{}'; en la línea {}", token_error.unwrap().to_string(), num_line)));
            }
        }
        Err(e) => panic!("{}", e),
    }
}