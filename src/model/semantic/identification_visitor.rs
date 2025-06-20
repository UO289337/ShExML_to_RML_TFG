use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;

use crate::model::{ast::{nodes::*, *}, compiler_error::CompilerError, visitor::*};

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

pub fn reset_state() {
    // Se limpia la tabla de símbolos al finalizar, sobre todo de cara a los tests
    SYMBOL_TABLE.lock().unwrap().clear();
}

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<Vec<Option<CompilerError>>> for Identification {
    fn visit_ast(&mut self, ast: &mut AST) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        ast.get_mut_prefixes().iter_mut().for_each(|prefix| {
            if previous_checking(&mut error_vec, prefix) {
                error_vec.extend(self.visit_prefix(prefix));
            }
        });

        ast.get_mut_sources().iter_mut().for_each(|source| {
            if previous_checking(&mut error_vec, source) {
                error_vec.extend(self.visit_source(source));
            }
        });

        let queries = ast.get_mut_queries();
        if queries.is_some() {
            queries.as_mut().unwrap().iter_mut().for_each(|query| {
                if previous_checking(&mut error_vec, query) {
                    error_vec.extend(self.visit_query(query));
                }
            });
        }
        
        ast.get_mut_iterators()
            .iter_mut()
            .for_each(|iterator| {
                if previous_checking(&mut error_vec, iterator) {
                    error_vec.extend(self.visit_iterator(iterator));
                }
            });

        ast.get_mut_expressions()
            .iter_mut()
            .for_each(|expression| {
                error_vec.extend(self.visit_expression(expression));
            });

        ast.get_mut_shapes().iter_mut().for_each(|shape| {
            error_vec.extend(self.visit_shape(shape));
        });

        // No es objetivo buscar si hay identificadores libres sin utilizar, en este caso no es un problema
        reset_state();
        error_vec
    }

    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> Vec<Option<CompilerError>> {
        let mut table = SYMBOL_TABLE.lock().unwrap();
        if prefix_node.get_identifier() != String::new() {
            table.insert(prefix_node.get_identifier(), ASTNodeSymbolTable::Prefix(prefix_node.clone()));
        } else {
            // Para el prefijo por defecto (:)
            table.insert(String::new(), ASTNodeSymbolTable::Prefix(prefix_node.clone()));
        }
        vec![None]
    }

    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            source_node.get_identifier(),
            ASTNodeSymbolTable::Source(source_node.clone()),
        );
        vec![None]
    }

    fn visit_query(&mut self, query_node: &mut QueryASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            query_node.get_identifier(),
            ASTNodeSymbolTable::Query(query_node.clone()),
        );
        vec![None]
    }

    fn visit_iterator(&mut self, iterator_node: &mut IteratorASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = iterator_node.get_position().get_num_line();

        // Utilizamos un ámbito acotado para la tabla, para que se pueda liberar antes
        {
            let mut table = SYMBOL_TABLE.lock().unwrap();

            match iterator_node.get_iterator_access() {
                IteratorAccess::Ident(ident) => {
                    if !table.contains_key(&ident) {
                        error_vec.push(Some(CompilerError::new(format!("No se encuentra el identificador de la Query del acceso del iterador: {ident}, en la línea {num_line}"))));
                    } else {
                        if let Some(ASTNodeSymbolTable::Query(query)) = table.get(&ident) {
                            iterator_node.set_query(Some(query.clone()));
                        } else {
                            error_vec.push(Some(CompilerError::new(format!("Se esperaba que el identificador {ident} se correspondiera con una consulta SQL en la línea {num_line}"))));
                        }
                    }
                }
                IteratorAccess::SqlQuery(_) => (),
                IteratorAccess::CsvPerRow(_) => (),
            }

            table.insert(
                iterator_node.get_identifier(),
                ASTNodeSymbolTable::Iterator(iterator_node.clone()),
            );
        }

        iterator_node
            .get_mut_fields()
            .iter_mut()
            .for_each(|field| {
                if previous_checking(&mut error_vec, field) {
                    error_vec.extend(self.visit_field(field));
                }
            });

        error_vec
    }

    fn visit_field(&mut self, field_node: &mut FieldASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            field_node.get_identifier(),
            ASTNodeSymbolTable::Field(field_node.clone()),
        );
        vec![None]
    }

    fn visit_expression(
        &mut self,
        expression_node: &mut ExpressionASTNode,
    ) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.lock().unwrap().insert(
            expression_node.clone().get_identifier(),
            ASTNodeSymbolTable::Expression(expression_node.clone()),
        );

        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        expression_node
            .get_mut_accesses()
            .iter_mut()
            .for_each(|access| {
                error_vec.extend(self.visit_access(access));
            });

        error_vec
    }

    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        asociate_prefix_to_shape(
            shape_node,
            shape_node.get_prefix_ident(),
            &mut error_vec,
            false,
        );
        asociate_prefix_to_shape(
            shape_node,
            shape_node.get_field_prefix_ident(),
            &mut error_vec,
            true,
        );

        shape_node.get_mut_tuples().iter_mut().for_each(|tuple| {
            error_vec.extend(self.visit_shape_tuple(tuple));
        });

        error_vec
    }

    fn visit_shape_tuple(
        &mut self,
        shape_tuple_node: &mut ShapeTupleASTNode,
    ) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        asociate_prefix_to_shape(
            shape_tuple_node,
            shape_tuple_node.get_prefix_ident(),
            &mut error_vec,
            false,
        );
        asociate_prefix_to_shape(
            shape_tuple_node,
            shape_tuple_node.get_object_prefix_ident(),
            &mut error_vec,
            true,
        );

        let mut object_access = None;

        match shape_tuple_node.get_object() {
            IdentOrAccess::Access(access_node) => object_access = Some(access_node),
            IdentOrAccess::Ident(_) => (),
        }

        if object_access.is_some() {
            error_vec.extend(self.visit_access(&mut object_access.unwrap()));
        }

        error_vec
    }

    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = access_node.get_position().get_num_line();

        // Utilizamos un ámbito acotado para la tabla, para que se pueda liberar antes
        {
            let table = SYMBOL_TABLE.lock().unwrap();

            let ident = access_node.get_identifier();
            let first_access = access_node.get_first_access();
            let second_access = access_node.get_second_access();

            if let Some(ASTNodeSymbolTable::Source(source)) = table.get(&ident) {
                access_node.set_source_or_expression(Some(SourceOrExpression::Source(source.clone())));
            } else if let Some(ASTNodeSymbolTable::Expression(expression)) = table.get(&ident) {
                access_node.set_source_or_expression(Some(SourceOrExpression::Expression(expression.clone())));
            } else {
                error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Source o una Expression antes del primer '.', pero se encontró {ident} en la línea {num_line}"))));
            }

            if let Some(ASTNodeSymbolTable::Iterator(iterator)) = table.get(&first_access) {
                access_node.set_iterator(Some(iterator.clone()));
            } else if let Some(ASTNodeSymbolTable::Field(field)) = table.get(&first_access) {
                access_node.set_field(Some(field.clone()));
            } else {
                error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Iterator o Field después del primer '.', pero se encontró {first_access} en la línea {num_line}"))));
            }

            if second_access.is_some() {
                if let Some(ASTNodeSymbolTable::Field(field)) = table.get(&second_access.clone().unwrap()) {
                    access_node.set_field(Some(field.clone()));
                } else {
                    error_vec.push(Some(CompilerError::new(format!("Se esperaba el identificador de un Field después del segundo '.', pero se encontró {} en la línea {num_line}", second_access.unwrap()))));
                }
            }
        }

        error_vec
    }
}

fn asociate_prefix_to_shape<T>(
    node: &mut T,
    prefix: Option<String>,
    error_vec: &mut Vec<Option<CompilerError>>,
    object_prefix: bool,
) where
    T: ManagePrefix + ManagePosition,
{
    let table = SYMBOL_TABLE.lock().unwrap();
    let num_line = node.get_position().get_num_line();

    if prefix.is_some() {
        let ident = prefix.unwrap();
        if let Some(ASTNodeSymbolTable::Prefix(prefix)) = table.get(&ident) {
            asociate_prefix(node, object_prefix, prefix);
        } else {
            error_vec.push(Some(CompilerError::new(format!(
                "Se esperaba que el identificador {ident} se correspondiera con un Prefix en la línea {num_line}"
            ))));
        }
    } else {
        // Se comprueba el prefijo por defecto (:)
        if let Some(ASTNodeSymbolTable::Prefix(prefix)) = table.get(&String::new()) {
            asociate_prefix(node, object_prefix, prefix);
        } else if !object_prefix {
            error_vec.push(Some(CompilerError::new(format!(
                "Se esperaba que existiera el identificador por defecto ':' en la línea {num_line}"
            ))));
        }
    }
}

fn asociate_prefix<T>(node: &mut T, object_prefix: bool, prefix: &PrefixASTNode) where
    T: ManagePrefix {
    if !object_prefix {
        node.set_prefix(Some(prefix.clone()));
    } else {
        node.set_object_prefix(Some(prefix.clone()));
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
        duplicate_pos.get_num_line(),
        position.get_num_line()
    )))
}
