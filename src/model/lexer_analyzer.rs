use winnow::error::{ContextError, ErrMode};
use winnow::prelude::*;
use winnow::token::{take_while, literal};
use winnow::combinator::{alt, delimited};

use super::token::*;

fn prefix(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal("PREFIX").parse_next(input)?;
    Ok(Token::new("PREFIX".to_string(), TokenType::PREFIX))
}

fn source(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal("SOURCE").parse_next(input)?;
    Ok(Token::new("SOURCE".to_string(), TokenType::SOURCE))
} 

fn identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let ident = take_while(1.., |c: char| c.is_alphabetic() || c == '_')
        .parse_next(input)?;

    Ok(Token::new(ident.to_string(), TokenType::IDENT))
}

fn uri(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let uri = delimited('<', take_while(1.., |c: char| c != '>'), '>')
        .parse_next(input)?;

    Ok(Token::new(uri.to_string(), TokenType::URI))
}

pub fn lexer(input: &mut &str) -> Result<Vec<Token>, ErrMode<ContextError>> {
    let mut tokens = Vec::new();

    while !input.is_empty() {
        match alt((prefix, source, uri, identifier)).parse_next(input) {
            Ok(token) => tokens.push(token),
            // Si no es ningún token, se pasa
            Err(ErrMode::Backtrack(_)) => {
                take_while(1, |c: char| c.is_ascii()).parse_next(input)?;
                continue;
            }
            Err(e) => return Err(e),
        }
        let _: Result<&str, ErrMode<ContextError>> = take_while(1.., char::is_whitespace).parse_next(input); // Se ignoran los espacios
    }

    Ok(tokens)
}