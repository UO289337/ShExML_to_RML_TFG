use std::{collections::HashMap, f64::consts::E, sync::Mutex};

use once_cell::sync::Lazy;

use crate::model::{ast::*, ast::nodes::*, compiler_error::CompilerError, visitor::*};

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
impl Visitor<Vec<Option<CompilerError>>> for Identification {

    fn visit_ast(&mut self, ast: AST) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        // Los Option se obtienen como una referencia (as_ref) y el resto con clone
        ast.get_prefixes().into_iter().for_each(|prefix| {
            self.visit_prefix(prefix.clone());
        });

        ast.get_sources().clone().into_iter().for_each(|source| {
            let error = check_duplicate_identifier(source.get_identifier(), source.get_position());

            if error.is_some() {
                error_vec.push(error);
            } else {
                self.visit_source(source.clone());
            }
        });

        if let Some(queries) = ast.get_queries().as_ref() {
            queries.into_iter().for_each(|query| {
                let error = check_duplicate_identifier(query.get_identifier(), query.get_position());

                if error.is_some() {
                    error_vec.push(error);
                } else {
                    self.visit_query(query.clone());
                }
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

        error_vec
    }

    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> Vec<Option<CompilerError>> {
        if let Some(ident) = prefix_node.get_identifier() {
            PREFIXES_SYMBOL_TABLE.lock().unwrap().insert(ident, prefix_node);   
        }
        vec![None]
    }
    
    fn visit_source(&mut self, source_node: SourceASTNode) -> Vec<Option<CompilerError>> {
        SOURCES_SYMBOL_TABLE.lock().unwrap().insert(source_node.get_identifier(), source_node);
        vec![None]
    }
    
    fn visit_query(&mut self, query_node: QueryASTNode) -> Vec<Option<CompilerError>> {
        QUERIES_SYMBOL_TABLE.lock().unwrap().insert(query_node.get_identifier(), query_node);
        vec![None]
    }
    
    fn visit_iterator(&mut self, iterator_node: IteratorASTNode) -> Vec<Option<CompilerError>> {
        todo!("COMPROBAR QUE, SI TIENE UN IDENT EN EL ACCESO, ESTE SEA UN IDENTIFICADOR DE UNA QUERY");
        vec![None]
    }
    
    fn visit_field(&mut self, field_node: FieldASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }
    
    fn visit_expression(&mut self, expression_node: ExpressionASTNode) -> Vec<Option<CompilerError>> {
        todo!("COMPROBAR EN LOS ACCESOS QUE EXISTEN LOS SOURCE, ITERATOR Y, EN SU CASO, FIELDS A LOS QUE SE ACCEDE");
        vec![None]
    }
    
    fn visit_shape(&mut self, shape_node: ShapeASTNode) -> Vec<Option<CompilerError>> {
        todo!("COMPROBAR LOS PREFIJOS");
        vec![None]
    }
    
    fn visit_shape_tuple(&mut self, shape_tuple_node: ShapeTupleASTNode) -> Vec<Option<CompilerError>> {
        todo!("COMPROBAR LOS PREFIJOS");
        vec![None]
    }
    
    fn visit_access(&mut self, access_node: AccessASTNode) -> Vec<Option<CompilerError>> {
        todo!("COMPROBAR LOS PREFIJOS Y LOS CAMPOS A LOS QUE SE ACCEDE");
        vec![None]
    }
}

fn check_duplicate_identifier(ident: String, position: Position) -> Option<CompilerError> {
    let prefixes_table = PREFIXES_SYMBOL_TABLE.lock().unwrap();
    let sources_table = SOURCES_SYMBOL_TABLE.lock().unwrap();
    let queries_table = QUERIES_SYMBOL_TABLE.lock().unwrap();

    if prefixes_table.contains_key(&ident) {
        let prefix_duplicate = prefixes_table.get(&ident).unwrap();
        return create_error_message(ident, position, prefix_duplicate.get_position());
    } else if sources_table.contains_key(&ident) {
        let source_duplicate = sources_table.get(&ident).unwrap();
        return create_error_message(ident, position, source_duplicate.get_position());
    } else if queries_table.contains_key(&ident) {
        let query_duplicate = queries_table.get(&ident).unwrap();
        return create_error_message(ident, position, query_duplicate.get_position());
    }

    None
}

fn create_error_message(ident: String, position: Position, duplicate_pos: Position) -> Option<CompilerError> {
    Some(CompilerError::new(format!("Identificador duplicado: {ident} en las líneas {} y {}", position.get_num_line(), duplicate_pos.get_num_line())))
}