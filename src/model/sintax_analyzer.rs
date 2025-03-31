use chumsky::Parser;
use chumsky::prelude::*;

use crate::model::token::*;

use super::token::TokenType;
use super::AST::ASTNode;

fn file_parser() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    prefix_parser()
        .chain(source_parser())
        .map(|parsed| {
            let mut prefixes: Vec<ASTNode>= Vec::new();
            let mut sources: Vec<ASTNode> = Vec::new();

            for node in parsed {
                match node {
                    ASTNode::Prefix { .. } => prefixes.push(node),
                    ASTNode::Source { .. } => sources.push(node),
                    _ => (),
                }
            }

            ASTNode::File { prefixes, sources }
        })
}

fn prefix_parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let prefix = filter(|token: &Token| token.token_type == TokenType::PREFIX)
        .map(|token| token.clone());
    
    let identifier = filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba un identificador después de PREFIX en la línea {}", token.found().map(|t| t.num_line).unwrap())));

    let colon = filter(|token: &Token| token.token_type == TokenType::COLON)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Faltan los ':' después del identificador de la línea {}", token.found().map(|t| t.num_line).unwrap())));
    
    let uri = filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba una URI después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())));

    prefix
        .then(identifier)        
        .then(colon)             
        .then(uri)              
        .map(|(((_, ident), _), uri)| {
            ASTNode::Prefix {
                identifier: ident.lexeme.clone(),
                uri: uri.lexeme.clone(),
            }
        })
        .repeated()
        .collect()
}

fn source_parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let source = filter(|token: &Token| token.token_type == TokenType::SOURCE)
        .map(|token| token.clone());
    
    let identifier = filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba un IDENT después de SOURCE en la línea {:?}", token.found().map(|t| t.num_line).unwrap())));

    let uri = filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba una URI después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())));

    source
        .then(identifier)        
        .then(uri)        
        .map(|((_, ident), uri)| {
            ASTNode::Source {
                identifier: ident.lexeme.clone(),
                uri: uri.lexeme.clone(),
            }
        })
        .repeated()
        .at_least(1)
        .collect()
}

pub fn parser(tokens: Vec<Token>) -> Result<ASTNode, Vec<Simple<Token>>> {
    let file_parser = file_parser();
    let parsed = file_parser.parse(tokens);

    match parsed {
        Ok(node) => Ok(node),
        Err(e) => Err(e),
    }
}