//! Visitor del AST

use crate::model::ast::*;

/// Trait general del visitor para visitar cada uno de los nodos del AST
pub trait Visitor<T> {
    fn visit_file(&mut self, file_node: FileASTNode) -> T;
    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> T;
    // fn visit_source(&mut self, s: SourceASTNode) -> T;
}
