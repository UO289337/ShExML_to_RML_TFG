use chumsky::Parser;
use chumsky::prelude::*;
use chumsky::combinator::*;
use chumsky::primitive::*;

use crate::model::token::*;

use super::ast::*;
use super::token::TokenType;

fn file_parser() -> impl Parser<Token, FileASTNode, Error = Simple<Token>> {
    prefix_parser()
        .then(source_parser())
        .map(|(prefixes, sources)| {
            FileASTNode {
                prefixes,
                sources,
            }
        })
}

fn prefix_parser() -> impl Parser<Token, Vec<PrefixASTNode>, Error = Simple<Token>> {
    prefix_detector()
        .then(identifier_detector(PREFIX.to_string()))        
        .then(colon_detector())             
        .then(uri_detector())              
        .map(|(((_, ident), _), uri)| {
            PrefixASTNode {
                identifier: ident.lexeme.clone(),
                uri: uri.lexeme.clone(),
            }
        })
        .repeated()
        .collect()
}

fn source_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {    
    source_detector()
        .then(identifier_detector(SOURCE.to_string()))        
        .then(uri_detector())        
        .map(|((_, ident), uri)| {
            SourceASTNode {
                identifier: ident.lexeme.clone(),
                uri: uri.lexeme.clone(),
            }
        })
        .repeated()
        .at_least(1)
        .collect()
}

// Detectors

fn prefix_detector() -> Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token> {
    filter(|token: &Token| token.token_type == TokenType::PREFIX)
        .map(|token| token.clone())
}

fn source_detector() -> Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token> {
    filter(|token: &Token| token.token_type == TokenType::SOURCE)
        .map(|token| token.clone())
}

fn identifier_detector(previous_element: String) -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba un identificador después de {previous_element} en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

fn uri_detector() -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba una URI después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

fn colon_detector() -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::COLON)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Faltan los ':' después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

pub fn parser(tokens: Vec<Token>) -> Result<FileASTNode, Vec<Simple<Token>>> {
    let file_parser = file_parser();
    let parsed = file_parser.parse(tokens);

    match parsed {
        Ok(node) => Ok(node),
        Err(e) => Err(e),
    }
}