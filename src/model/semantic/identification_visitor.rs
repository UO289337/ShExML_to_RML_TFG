//! Módulo de la fase de identificación del analizador semántico; se utiliza el patrón Visitor
//!
//! Comprueba que todos los identificadores utilizados están definidos y que no hay duplicados.
//! También comprueba algunos tipos de identificadores 

use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;
use std::thread_local;

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
    /// Devuelve la posición del nodo AST
    /// 
    /// # Parámetros
    /// * `self` - El propio nodo del AST
    /// 
    /// # Retorna
    /// La posición del nodo
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

// Se utiliza thread_local con el fin de evitar condiciones de carrera al compartir la tabla entre varios tests
thread_local! {
    // La tabla de símbolos en la que se guarda como clave el identificador de cada nodo, que se guarda como valor
    static SYMBOL_TABLE: Lazy<Mutex<HashMap<String, ASTNodeSymbolTable>>> = Lazy::new(|| {
        let map = HashMap::new();
        Mutex::new(map)
    });
}

/// Limpia la tabla de símbolos
pub fn reset_state() {
    // Se limpia la tabla de símbolos al finalizar, sobre todo de cara a los tests
    SYMBOL_TABLE.with(|table| {
        table.lock().unwrap().clear();
    });
}


impl Visitor<Vec<Option<CompilerError>>> for Identification {
    /// Realiza la visita al AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `ast` - El AST que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante esta fase
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

    /// Realiza la visita a un nodo Prefix del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `prefix_node` - El nodo Prefix que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Prefix
    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.with(|t| {
            let mut table = t.lock().unwrap();

            if prefix_node.get_identifier() != String::new() {
                table.insert(prefix_node.get_identifier(), ASTNodeSymbolTable::Prefix(prefix_node.clone()));
            } else {
                // Para el prefijo por defecto (:)
                table.insert(String::new(), ASTNodeSymbolTable::Prefix(prefix_node.clone()));
            }
        });
        vec![None]
    }

    /// Realiza la visita a un nodo Source del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `source_node` - El nodo Source que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Source
    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.with(|table| {
            table.lock().unwrap().insert(
                source_node.get_identifier(),
                ASTNodeSymbolTable::Source(source_node.clone()),
            );
        });
        vec![None]
    }

    /// Realiza la visita a un nodo Query del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `query_node` - El nodo Query que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Query
    fn visit_query(&mut self, query_node: &mut QueryASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.with(|table| {
            table.lock().unwrap().insert(
                query_node.get_identifier(),
                ASTNodeSymbolTable::Query(query_node.clone()),
            );
        });
        vec![None]
    }

    /// Realiza la visita a un nodo Iterator del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `iterator_node` - El nodo Iterator que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Iterator
    fn visit_iterator(&mut self, iterator_node: &mut IteratorASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = iterator_node.get_position().get_num_line();

        SYMBOL_TABLE.with(|t| {
            let mut table = t.lock().unwrap();

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
        });

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

    /// Realiza la visita a un nodo Field del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `field_node` - El nodo Field que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Field
    fn visit_field(&mut self, field_node: &mut FieldASTNode) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.with(|table| {
            table.lock().unwrap().insert(
                field_node.get_identifier(),
                ASTNodeSymbolTable::Field(field_node.clone()),
            );
        });
        vec![None]
    }

    /// Realiza la visita a un nodo Expression del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `expression_node` - El nodo Expression que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Expression
    fn visit_expression(
        &mut self,
        expression_node: &mut ExpressionASTNode,
    ) -> Vec<Option<CompilerError>> {
        SYMBOL_TABLE.with(|table| {
            table.lock().unwrap().insert(
                expression_node.get_identifier(),
                ASTNodeSymbolTable::Expression(expression_node.clone()),
            );
        });

        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        expression_node
            .get_mut_accesses()
            .iter_mut()
            .for_each(|access| {
                error_vec.extend(self.visit_access(access));
            });

        error_vec
    }

    /// Realiza la visita a un nodo Shape del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `shape_node` - El nodo Shape que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la Shape
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

    /// Realiza la visita a un nodo tupla de la Shape del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `shape_tuple_node` - El nodo tupla de la Shape que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la tupla de la Shape
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

    /// Realiza la visita a un nodo Access del AST
    /// 
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Identificación
    /// * `access_node` - El nodo Access que se va a visitar
    /// 
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Access
    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let num_line = access_node.get_position().get_num_line();

        SYMBOL_TABLE.with(|t| {
            let table = t.lock().unwrap();

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
        });

        error_vec
    }
}

/// Asocia un prefijo a una Shape o a una tupla
/// 
/// # Parámetros
/// * `node` - El nodo AST al que se quiere asociar el prefijo
/// * `prefix` - El prefijo que se quiere asociar al nodo AST
/// * `error_vec` - El vector de errores de la fase de identificación
/// * `object_prefix` - Un booleano que indica si es el prefix del objeto o campo (true) o si no (false)
fn asociate_prefix_to_shape<T>(
    node: &mut T,
    prefix: Option<String>,
    error_vec: &mut Vec<Option<CompilerError>>,
    object_prefix: bool,
) where
    T: ManagePrefix + ManagePosition,
{
    SYMBOL_TABLE.with(|t|{
        let table = t.lock().unwrap();
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
    });
}

/// Asocia un nodo Prefix a otro nodo
/// 
/// # Parámetros
/// * `node` - El nodo del AST al cual se le va a asociar el prefijo
/// * `object_prefix` - Un booleano que indica si es el prefix del objeto o campo (true) o si no (false)
/// * `prefix` - El nodo Prefix que se quiere asociar con el nodo del AST
fn asociate_prefix<T>(node: &mut T, object_prefix: bool, prefix: &PrefixASTNode) where
    T: ManagePrefix {
    if !object_prefix {
        node.set_prefix(Some(prefix.clone()));
    } else {
        node.set_object_prefix(Some(prefix.clone()));
    }
}

/// Realiza el chequeo previo de un nodo
/// 
/// Comprueba si el identificador del nodo a chequear ya existe en la tabla de símbolos
/// 
/// # Parámetros
/// * `error_vec` - El vecto de errores de la fase de identificación
/// * `node` - El nodo a chequear
/// 
/// # Retorna
/// true si el nodo pasa el chequeo o false en caso contrario
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

/// Comprueba que el identificador que se le pasa no se encuentra en la tabla de símbolos
/// 
/// # Parámetros
/// * `ident` - El identificador a comprobar si existe en la tabla de símbolos
/// * `position` - La posición del nodo origen; se utiliza para personalizar el mensaje de error
/// 
/// # Retorna
/// Un Option con el posible error creado al detectar que el identificador ya existe en la tabla de símbolos
fn check_duplicate_identifier(ident: String, position: Position) -> Option<CompilerError> {
    SYMBOL_TABLE.with(|t| {
        let table = t.lock().unwrap();
        if table.contains_key(&ident) {
            let duplicate: &ASTNodeSymbolTable = table.get(&ident).unwrap();
            return create_error(ident, position, duplicate.get_position());
        }
        None
    })
}

/// Crea un error con un mensaje que indica que el identificador está duplicado
/// 
/// # Parámetros
/// * `ident` - El identificador duplicado
/// * `position` - La posición del nodo al que pertenece el identificador
/// * `duplicate_pos` - La posición del nodo al que pertenece el identificador que se encuentra en la tabla de símbolos
/// 
/// # Retorna
/// Un Option con el error que indica que hay un identificador duplicado
fn create_error(
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
