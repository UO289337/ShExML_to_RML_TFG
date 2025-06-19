//! Visitor del AST

use crate::model::{ast::nodes::*, ast::*};

/// Trait general del visitor para visitar cada uno de los nodos del AST
pub trait Visitor<T> {
    fn visit_ast(&mut self, ast: AST) -> T;
    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> T;
    fn visit_source(&mut self, source_node: SourceASTNode) -> T;
    fn visit_query(&mut self, query_node: QueryASTNode) -> T;
    fn visit_iterator(&mut self, iterator_node: IteratorASTNode) -> T;
    fn visit_field(&mut self, field_node: FieldASTNode) -> T;
    fn visit_expression(&mut self, expression_node: ExpressionASTNode) -> T;
    fn visit_shape(&mut self, shape_node: ShapeASTNode) -> T;
    fn visit_shape_tuple(&mut self, shape_tuple_node: ShapeTupleASTNode) -> T;
    fn visit_access(&mut self, access_node: AccessASTNode) -> T;
}
