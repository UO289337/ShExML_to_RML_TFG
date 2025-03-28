use chumsky::Parser;
use chumsky::prelude::*;

use crate::model::token::*;

use super::sintax_error::SintaxError;
use super::token::TokenType;
use super::AST::ASTNode;

/* fn get_error_message() -> String {
    error_message = format!("Error sintáctico en la línea {}. Lexema: {}",
            self.get_token().get_num_line(),
            self.get_token().get_lexeme());
} */

fn prefix_parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let prefix = filter(|token: &Token| token.token_type == TokenType::PREFIX)
        .map(|token| token.clone());

    let colon = filter(|token: &Token| token.token_type == TokenType::COLON)
        .map(|token| token.clone());
    
    let identifier = filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone());
    
    let uri = filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone());

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

pub fn parser(tokens: Vec<Token>) {
    // TODO Poner el manejo de errores, se puede utilizar recover_with
    let prefix_parser = prefix_parser();
    let parsed = prefix_parser.parse(tokens);

    match parsed {
        Ok(prefix_node) => {
            println!("Nodo AST: {:?}", prefix_node);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}

fn error(message: String) -> SintaxError {
    SintaxError::new(message)
}