use winnow::error::{ContextError, ErrMode};
use winnow::prelude::*;
use winnow::token::{literal, take_while};
use winnow::combinator::{alt, preceded};

#[derive(Debug, PartialEq)]
enum TokenType {
    IDENT,
    URI,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    lexeme: String,
    token_type: TokenType,
}

fn identifier(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    take_while(1.., |c: char| c.is_alphabetic() || c == '_')
        .map(|s: &str| Token {
            lexeme: s.to_string(), 
            token_type: TokenType::IDENT,
        })
        .parse_next(input)
}

fn uri(input: &mut &str) -> Result<Token, ErrMode<ContextError>> {
    preceded(
        alt((literal("http://"), literal("https://"))), 
        take_while(1.., |c: char| c.is_ascii()),  
    )
    .map(|s: &str| Token {
        lexeme: s.to_string(),
        token_type: TokenType::URI,
    })
    .parse_next(input)
}

pub fn lexer(input: &mut &str) -> Result<Vec<Token>, ErrMode<ContextError>> {
    let mut tokens = Vec::new();

    while !input.is_empty() {
        let _: Result<&str, ErrMode<ContextError>> = take_while(1.., char::is_whitespace).parse_next(input); // Se ignoran los espacios
        match alt((uri, identifier)).parse_next(input) {
            Ok(token) => tokens.push(token),
            Err(ErrMode::Backtrack(_)) => {
                eprintln!("Error: Token no reconocido en '{}'", input);
                return Err(ErrMode::Backtrack(ContextError::default()));
            }
            Err(e) => return Err(e),
        }
    }

    Ok(tokens)
}