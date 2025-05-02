//! Módulo del analizador sintáctico
//! 
//! Realiza el análisis sintáctico del compilador
//! Comprueba que los tokens resultado del analizador léxico se encuentran en el orden esperado y genera el AST

use chumsky::Parser;
use chumsky::prelude::*;
use chumsky::combinator::*;
use chumsky::primitive::*;

use crate::model::token::*;

use super::ast::*;
use super::token::TokenType;

/// Parsea los tokens para generar el nodo File del AST
/// 
/// Realiza el parseo de los tokens de los prefijos y de las fuentes para poder crear el nodo File, que es el nodo raíz del AST
/// 
/// # Retorna
/// Un nodo File del AST
/// 
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
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

/// Parsea los tokens para generar el nodo Prefix del AST
/// 
/// Realiza el parseo de los tokens para detectar la secuencia: PREFIX IDENT: URI
/// 
/// # Retorna
/// Un nodo Prefix del AST
/// 
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
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

/// Parsea los tokens para generar el nodo Source del AST
/// 
/// Realiza el parseo de los tokens para detectar la secuencia: SOURCE IDENT: URI
/// 
/// # Retorna
/// Un nodo Source del AST
/// 
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
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

/// Detecta el token PREFIX en los tokens
/// 
/// # Retorna
/// Un token de tipo Prefix si el token actual es de dicho tipo
/// 
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Prefix
fn prefix_detector() -> Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token> {
    filter(|token: &Token| token.token_type == TokenType::PREFIX)
        .map(|token| token.clone())
}

/// Detecta el token SOURCE en los tokens
/// 
/// # Retorna
/// Un token de tipo Source si el token actual es de dicho tipo
/// 
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source
fn source_detector() -> Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token> {
    filter(|token: &Token| token.token_type == TokenType::SOURCE)
        .map(|token| token.clone())
}

/// Detecta el token IDENT en los tokens
/// 
/// # Argumentos
/// * `previous_token` - El token previo al identificador, que puede ser un PREFIX o un SOURCE
/// 
/// # Retorna
/// Un token de tipo Ident si el token actual es de dicho tipo
/// 
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Ident
fn identifier_detector(previous_token: String) -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::IDENT)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba un identificador después de {previous_token} en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

/// Detecta el token URI en los tokens
/// 
/// # Retorna
/// Un token de tipo Uri si el token actual es de dicho tipo
/// 
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn uri_detector() -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::URI)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Se esperaba una URI después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

/// Detecta el token ':' en los tokens
/// 
/// # Retorna
/// Un token de tipo : si el token actual es de dicho tipo
/// 
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo :
fn colon_detector() -> MapErr<Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>, impl Fn(Simple<Token>) -> Simple<Token>> {
    filter(|token: &Token| token.token_type == TokenType::COLON)
        .map(|token| token.clone())
        .map_err(|token: Simple<Token>| Simple::custom(token.span(), format!("Faltan los ':' después del identificador en la línea {}", token.found().map(|t| t.num_line).unwrap())))
}

/// Parsea el vector de tokens para generar el AST
/// 
/// Toma como entrada el vector de tokens resultado del análisis léxico y genera un árbol AST que tiene un nodo File como raíz
/// 
/// # Argumentos
/// * `tokens` - El vector de tokens resultado del análisis léxico
/// 
/// # Retorna
/// Un nodo File del AST que será el nodo raíz de este
/// 
/// # Errores
/// * `[Vec<Simple<Token>>]` - Un vector con los errores que pueden aparecer al realizar el análisis sintáctico
pub fn parser(tokens: Vec<Token>) -> Result<FileASTNode, Vec<Simple<Token>>> {
    let file_parser = file_parser();
    let parsed = file_parser.parse(tokens);

    match parsed {
        Ok(node) => Ok(node),
        Err(e) => Err(e),
    }
}