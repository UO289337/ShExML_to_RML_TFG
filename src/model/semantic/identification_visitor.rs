use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

use crate::model::{ast::*, ast::nodes::*, compiler_error::CompilerError, lexer::token::Token, visitor::*};

/// Struct para poder realizar las visitas del visitor de la fase de Identificación sobre él
pub struct Identification;

static PREFIXES_SYMBOL_TABLE: Lazy<Mutex<HashMap<String, PrefixASTNode>>> = Lazy::new(|| {
    let map = HashMap::new();
    Mutex::new(map)
});

static SOURCES_SYMBOL_TABLE: Lazy<Mutex<HashMap<String, SourceASTNode>>> = Lazy::new(|| {
    let map = HashMap::new();
    Mutex::new(map)
});

static QUERIES_SYMBOL_TABLE: Lazy<Mutex<HashMap<String, QueryASTNode>>> = Lazy::new(|| {
    let map = HashMap::new();
    Mutex::new(map)
});

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<()> for Identification {

    fn visit_ast(&mut self, ast: AST) {
        // Los Option se obtienen como una referencia (as_ref) y el resto con clone
        ast.get_prefixes().into_iter().for_each(|prefix| {
            self.visit_prefix(prefix.clone());
        });

        ast.get_sources().clone().into_iter().for_each(|source| {
            self.visit_source(source);
        });

        if let Some(queries) = ast.get_queries().as_ref() {
            queries.into_iter().for_each(|query| {
                self.visit_query(query.clone());
            });
        }

        ast.get_expressions().clone().into_iter().for_each(|expression| {
            self.visit_expression(expression);
        });

        ast.get_iterators().clone().into_iter().for_each(|iterator| {
            self.visit_iterator(iterator);
        });

        ast.get_shapes().clone().into_iter().for_each(|shape| {
            self.visit_shape(shape);
        });
    }

    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) {
        if let Some(ident) = prefix_node.get_identifier() {
            PREFIXES_SYMBOL_TABLE.lock().unwrap().insert(ident, prefix_node);   
        }
    }
    
    fn visit_source(&mut self, source_node: SourceASTNode) {
        SOURCES_SYMBOL_TABLE.lock().unwrap().insert(source_node.get_identifier(), source_node);
    }
    
    fn visit_query(&mut self, query_node: QueryASTNode) {
        QUERIES_SYMBOL_TABLE.lock().unwrap().insert(query_node.get_identifier(), query_node);
    }
    
    fn visit_iterator(&mut self, iterator_node: IteratorASTNode) {
        todo!("COMPROBAR QUE, SI TIENE UN IDENT EN EL ACCESO, ESTE SEA UN IDENTIFICADOR DE UNA QUERY")
    }
    
    fn visit_field(&mut self, field_node: FieldASTNode) {
        todo!()
    }
    
    fn visit_expression(&mut self, expression_node: ExpressionASTNode) {
        todo!("COMPROBAR EN LOS ACCESOS QUE EXISTEN LOS SOURCE, ITERATOR Y, EN SU CASO, FIELDS A LOS QUE SE ACCEDE")
    }
    
    fn visit_shape(&mut self, shape_node: ShapeASTNode) {
        todo!("COMPROBAR LOS PREFIJOS")
    }
    
    fn visit_shape_tuple(&mut self, shape_tuple_node: ShapeTupleASTNode) {
        todo!("COMPROBAR LOS PREFIJOS")
    }
    
    fn visit_access(&mut self, access_node: AccessASTNode) {
        todo!("COMPROBAR LOS PREFIJOS Y LOS CAMPOS A LOS QUE SE ACCEDE")
    }
}

fn check_duplicate_identifier(ident: Token) -> Option<CompilerError> {

    None
}