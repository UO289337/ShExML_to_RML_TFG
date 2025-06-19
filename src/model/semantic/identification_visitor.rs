use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

use crate::model::{ast::nodes::*, ast::*, compiler_error::CompilerError, visitor::*};

/// Enumerador que contiene todos los nodos del AST de la tabla de simbolos
// Se utiliza el enum aqui y no en el AST con el fin de tener mayor control de tipos en el AST
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
enum ASTNodeSymbolTable {
    Prefix(PrefixASTNode),
    Source(SourceASTNode),
    Query(QueryASTNode),
    Iterator(IteratorASTNode),
    Field(FieldASTNode),
    Expression(ExpressionASTNode),
}

impl ManagePosition for ASTNodeSymbolTable {
    fn get_position(&self) -> Position {
        // No se puede evitar el match
        match self {
            ASTNodeSymbolTable::Prefix(prefix) => prefix.get_position(),
            ASTNodeSymbolTable::Source(source) => source.get_position(),
            ASTNodeSymbolTable::Query(query) => query.get_position(),
            ASTNodeSymbolTable::Iterator(iterator) => iterator.get_position(),
            ASTNodeSymbolTable::Field(field) => field.get_position(),
            ASTNodeSymbolTable::Expression(expression) => expression.get_position(),
        }
    }
}

/// Struct para poder realizar las visitas del visitor de la fase de Identificación sobre él
pub struct Identification;

static SYMBOL_TABLE: Lazy<Mutex<HashMap<String, ASTNodeSymbolTable>>> = Lazy::new(|| {
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
            if previous_checking(&mut error_vec, &source) {
                self.visit_source(source.clone());
            }
        });

        if let Some(queries) = ast.get_queries().as_ref() {
            queries.into_iter().for_each(|query| {
                if previous_checking(&mut error_vec, query) {
                    self.visit_query(query.clone());
                }
            });
        }

        ast.get_iterators()
            .clone()
            .into_iter()
            .for_each(|iterator| {
                if previous_checking(&mut error_vec, &iterator) {
                    self.visit_iterator(iterator);
                }
            });

        ast.get_expressions()
            .clone()
            .into_iter()
            .for_each(|expression| {
                self.visit_expression(expression);
            });

        ast.get_shapes().clone().into_iter().for_each(|shape| {
            self.visit_shape(shape);
        });

        error_vec
    }

    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> Vec<Option<CompilerError>> {
        if let Some(ident) = prefix_node.get_identifier() {
            SYMBOL_TABLE
                .lock()
                .unwrap()
                .insert(ident, ASTNodeSymbolTable::Prefix(prefix_node));
        } else {
            // Para el prefijo por defecto (:)
            SYMBOL_TABLE
                .lock()
                .unwrap()
                .insert(String::new(), ASTNodeSymbolTable::Prefix(prefix_node));
        }
        vec![None]
    }

    fn visit_source(&mut self, source_node: SourceASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            source_node.get_identifier(),
            ASTNodeSymbolTable::Source(source_node),
        );
        vec![None]
    }

    fn visit_query(&mut self, query_node: QueryASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            query_node.get_identifier(),
            ASTNodeSymbolTable::Query(query_node),
        );
        vec![None]
    }

    fn visit_iterator(&mut self, mut iterator_node: IteratorASTNode) -> Vec<Option<CompilerError>> {
        let mut table = SYMBOL_TABLE.lock().unwrap();
        table.insert(
            iterator_node.get_identifier(),
            ASTNodeSymbolTable::Iterator(iterator_node.clone()),
        );

        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        match iterator_node.get_iterator_access() {
            IteratorAccess::Ident(ident) => {
                if !table.contains_key(&ident) {
                    error_vec.push(Some(CompilerError::new(format!("No se encuentra el identificador de la Query del acceso del iterador: {ident}"))));
                } else {
                    if let Some(ASTNodeSymbolTable::Query(query)) = table.remove(&ident) {
                        iterator_node.set_query(Some(query));
                    } else {
                        error_vec.push(Some(CompilerError::new(format!("Se esperaba que el identificador {ident} se correspondiera con una consulta SQL"))));
                    }
                }
            }
            IteratorAccess::SqlQuery(_) => (),
            IteratorAccess::CsvPerRow(_) => (),
        }

        iterator_node
            .get_fields()
            .clone()
            .into_iter()
            .for_each(|field| {
                if previous_checking(&mut error_vec, &field) {
                    self.visit_field(field);
                }
            });

        error_vec
    }

    fn visit_field(&mut self, field_node: FieldASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            field_node.get_identifier(),
            ASTNodeSymbolTable::Field(field_node),
        );
        vec![None]
    }

    fn visit_expression(
        &mut self,
        expression_node: ExpressionASTNode,
    ) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            expression_node.clone().get_identifier(),
            ASTNodeSymbolTable::Expression(expression_node.clone()),
        );

        expression_node
            .get_accesses()
            .clone()
            .into_iter()
            .for_each(|access| {
                self.visit_access(access);
            });

        vec![None]
    }

    fn visit_shape(&mut self, shape_node: ShapeASTNode) -> Vec<Option<CompilerError>> {
        let error_vec: Vec<Option<CompilerError>> = Vec::new();

        asociate_prefix_to_shape(
            shape_node.clone(),
            shape_node.get_prefix_ident(),
            error_vec.clone(),
        );
        asociate_prefix_to_shape(
            shape_node.clone(),
            shape_node.get_field_prefix_ident(),
            error_vec.clone(),
        );

        shape_node.get_tuples().into_iter().for_each(|tuple| {
            self.visit_shape_tuple(tuple);
        });

        error_vec
    }

    fn visit_shape_tuple(
        &mut self,
        shape_tuple_node: ShapeTupleASTNode,
    ) -> Vec<Option<CompilerError>> {
        let error_vec: Vec<Option<CompilerError>> = Vec::new();

        asociate_prefix_to_shape(
            shape_tuple_node.clone(),
            shape_tuple_node.get_prefix_ident(),
            error_vec.clone(),
        );
        asociate_prefix_to_shape(
            shape_tuple_node.clone(),
            shape_tuple_node.get_object_prefix_ident(),
            error_vec.clone(),
        );

        let mut object_access = None;

        match shape_tuple_node.get_object() {
            IdentOrAccess::Access(access_node) => object_access = Some(access_node),
            IdentOrAccess::Ident(_) => (),
        }

        if object_access.is_some() {
            self.visit_access(object_access.unwrap());
        }

        error_vec
    }

    fn visit_access(&mut self, mut access_node: AccessASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let mut table = SYMBOL_TABLE.lock().unwrap();

        let ident = access_node.get_identifier();
        let first_access = access_node.get_first_access();
        let second_access = access_node.get_second_access();

        if let Some(ASTNodeSymbolTable::Source(source)) = table.remove(&ident) {
            access_node.set_source_or_expression(Some(SourceOrExpression::Source(source)));
        } else if let Some(ASTNodeSymbolTable::Expression(expression)) = table.remove(&ident) {
            access_node.set_source_or_expression(Some(SourceOrExpression::Expression(expression)));
        } else {
            error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Source o una Expression antes del primer '.', pero se encontró {ident}"))));
        }

        if let Some(ASTNodeSymbolTable::Iterator(iterator)) = table.remove(&first_access) {
            access_node.set_iterator(Some(iterator));
        } else if let Some(ASTNodeSymbolTable::Field(field)) = table.remove(&first_access) {
            access_node.set_field(Some(field));
        } else {
            error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Iterator o Field después del primer '.', pero se encontró {ident}"))));
        }

        if second_access.is_some() {
            if let Some(ASTNodeSymbolTable::Field(field)) = table.remove(&second_access.unwrap()) {
                access_node.set_field(Some(field));
            } else {
                error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Field después del segundo '.', pero se encontró {ident}"))));
            }
        }

        error_vec
    }
}

fn asociate_prefix_to_shape<T>(
    mut node: T,
    prefix: Option<String>,
    mut error_vec: Vec<Option<CompilerError>>,
) where
    T: ManagePrefix,
{
    let mut table = SYMBOL_TABLE.lock().unwrap();

    if prefix.is_some() {
        let ident = prefix.unwrap();
        if let Some(ASTNodeSymbolTable::Prefix(prefix)) = table.remove(&ident) {
            node.set_prefix(Some(prefix));
        } else {
            error_vec.push(Some(CompilerError::new(format!(
                "Se esperaba que el identificador {ident} se correspondiera con un Prefix"
            ))));
        }
    } else {
        // Se comprueba el prefijo por defecto (:)
        if let Some(ASTNodeSymbolTable::Prefix(prefix)) = table.remove(&String::new()) {
            node.set_prefix(Some(prefix));
        } else {
            error_vec.push(Some(CompilerError::new(format!(
                "Se esperaba que existiera el identificador por defecto ':'"
            ))));
        }
    }
}

fn previous_checking<T>(error_vec: &mut Vec<Option<CompilerError>>, node: &T) -> bool
where
    T: ManagePosition + Identifiable,
{
    let error = check_duplicate_identifier(node.get_identifier(), node.get_position());

    if error.is_some() {
        error_vec.push(error);
        return false;
    }

    true
}

fn check_duplicate_identifier(ident: String, position: Position) -> Option<CompilerError> {
    let symbol_table = SYMBOL_TABLE.lock().unwrap();

    if symbol_table.contains_key(&ident) {
        let duplicate: &ASTNodeSymbolTable = symbol_table.get(&ident).unwrap();
        return create_error_message(ident, position, duplicate.get_position());
    }

    None
}

fn create_error_message(
    ident: String,
    position: Position,
    duplicate_pos: Position,
) -> Option<CompilerError> {
    Some(CompilerError::new(format!(
        "Identificador duplicado: {ident} en las líneas {} y {}",
        position.get_num_line(),
        duplicate_pos.get_num_line()
    )))
}
