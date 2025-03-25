use std::slice::Iter;

use crate::model::AST::*;
use crate::model::lexer_analyzer::Token;

use super::sintax_error::SintaxError;
use super::token::TokenType;
use super::AST::ASTTree;

fn file_node(tokens_iter: Iter) -> FileASTNode {
    let root_node = FileASTNode::new();

    if let Some(nodes) = prefix_node(tokens_iter) {
        for node in nodes {
            root_node.add_child(node)
        }
    }
    root_node
}

fn prefix_node(tokens_iter: Iter) -> Option<Vec<PrefixASTNode>> {
    
}

pub fn grammar(tokens: Vec<Token>) {
    let mut tokens_iter = tokens.iter();
    let ast_root = ASTNode::File(file_node(tokens_iter));
    let mut ast_tree = ASTTree::new(ast_root);
}

fn advance<'a>(tokens_iter: &'a mut Iter<Token>) -> Option<&'a Token> {
    tokens_iter.next()
}

fn error(message: &str) -> SintaxError {
    SintaxError::new(message.to_string())
}

fn match_token(token: Token, token_type: TokenType) {
    if token.get_token_type() == token_type {
        advance(tokens_iter)
    } else {
        error(message);
    }
}