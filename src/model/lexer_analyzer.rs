use std::ops::ControlFlow;

use winnow::error::{ContextError, ErrMode};
use winnow::prelude::*;
use winnow::token::{take_while, literal};
use winnow::combinator::{alt, delimited};

use super::parser_error::ParserError;
use super::token::*;

fn prefix(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal("PREFIX").parse_next(input)?;
    Ok(Token::new("PREFIX".to_string(), TokenType::PREFIX))
}

fn colon(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    let _ = literal(":").parse_next(input)?;
    Ok(Token::new(":".to_string(), TokenType::COLON))
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

pub fn lexer(input: &mut &str) -> Result<Vec<Token>, Vec<ParserError>> {
    let mut tokens = Vec::new();
    let mut errors: Vec<ParserError> = Vec::new();

    look_over_input(input, &mut tokens, &mut errors);

    end_lexer(tokens, errors)
}

fn end_lexer(mut tokens: Vec<Token>, errors: Vec<ParserError>) -> Result<Vec<Token>, Vec<ParserError>> {
    if errors.is_empty() {
        tokens.push(Token::create_eof_token());
        Ok(tokens)
    } else {
        Err(errors)
    }
}

fn look_over_input(input: &mut &str, tokens: &mut Vec<Token>, errors: &mut Vec<ParserError>) {
    let mut num_line = 1;

    while !input.is_empty() {
        if let ControlFlow::Break(_) = match_alternatives(input, tokens, num_line, errors) {
            continue;
        }

        let new_line: Result<&str, ErrMode<ContextError>> = take_while(1.., |c: char| c == '\n' || c == '\r').parse_next(input); // Se ignoran los espacios
        if new_line.is_ok() {
            num_line += 1;
        }   

        // let _: Result<&str, ErrMode<ContextError>> = take_while(1.., |c: char| c.is_whitespace() || c == '\t').parse_next(input); // Se ignoran los espacios
    }
}

fn match_alternatives(input: &mut &str, tokens: &mut Vec<Token>, num_line: u16, errors: &mut Vec<ParserError>) -> ControlFlow<()> {
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
            return ControlFlow::Break(());
        }
        Err(e) => panic!("{}", e),
    }
    ControlFlow::Continue(())
}