//! Visitor del AST

use crate::model::{ast::nodes::*, ast::*};

/// Trait general del visitor para visitar cada uno de los nodos del AST
/// Se utiliza &mut con el fin de poder modificar el AST, si así se desea, a medida que se vayan visitando sus nodos
/// Se utiliza el parámetro generalizable T para permitir un mayor uso del Visitor
pub trait Visitor<T> {
    fn visit_ast(&mut self, ast: &mut AST) -> T;
    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> T;
    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> T;
    fn visit_query(&mut self, query_node: &mut QueryASTNode) -> T;
    fn visit_iterator(&mut self, iterator_node: &mut IteratorASTNode) -> T;
    fn visit_field(&mut self, field_node: &mut FieldASTNode) -> T;
    fn visit_expression(&mut self, expression_node: &mut ExpressionASTNode) -> T;
    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> T;
    fn visit_shape_tuple(&mut self, shape_tuple_node: &mut ShapeTupleASTNode) -> T;
    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> T;
}
