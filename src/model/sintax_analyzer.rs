use crate::model::AST::*;
use crate::model::lexer_analyzer::Token;

use super::AST::ASTTree;

pub fn prefix_node(tokens: Vec<Token>) {

}

pub fn grammar(tokens: Vec<Token>) {
    let ast_root = ASTNode::new(ASTNodeType::Program, " ".to_string());
    let mut ast_tree = ASTTree::new(ast_root);
    let mut tokens_iter = tokens.iter();

    while let Some(token) = tokens_iter.next() {

    }
}