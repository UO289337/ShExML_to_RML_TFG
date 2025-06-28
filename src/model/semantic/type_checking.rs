//! Módulo de la fase de chequeo de tipos (type checking) del analizador semántico; se utiliza el patrón Visitor
//!
//! Comprueba que el tipo del Source es correcto y que las bases de datos utilizadas están permitidas
//! También comprueba que los accesos a campos en las tuplas están permitidos

use std::collections::HashMap;

use crate::compiler_error::CompilerError;
use crate::model::{
    ast::{nodes::*, IdentOrAccess, SourceOrExpression, Type, ManageType},
    visitor::Visitor,
};


#[derive(Debug, PartialEq, Clone, Eq)]
enum ASTNodeTypeChecking {
    Source(SourceASTNode),
    Iterator(IteratorASTNode),
}

impl ASTNodeTypeChecking {
    fn get_source(&self) -> Option<SourceASTNode> {
        match self {
            ASTNodeTypeChecking::Source(source_node) => Some(source_node.clone()),
            ASTNodeTypeChecking::Iterator(_) => None,
        }
    }
}

impl ManageType for ASTNodeTypeChecking {
    fn get_type(&self) -> Option<Type> {
        match self {
            ASTNodeTypeChecking::Source(source_node) => source_node.get_type(),
            ASTNodeTypeChecking::Iterator(iterator_node) => iterator_node.get_type(),
        }
    }

    fn set_type(&mut self, node_type: Type) {
        match self {
            ASTNodeTypeChecking::Source(source_node) => source_node.set_type(node_type),
            ASTNodeTypeChecking::Iterator(iterator_node) => iterator_node.set_type(node_type),
        }
    }
}

/// Struct para poder realizar las visitas del visitor de la fase de Type Checking sobre él
pub struct TypeChecking {
    // Sólo se almacenan diccionarios de nodos que tengan tipo
    nodes_with_type: HashMap<String, ASTNodeTypeChecking>,
}

impl TypeChecking {
    pub fn new() -> Self {
        Self {
            nodes_with_type: HashMap::new(),
        }
    }

    fn insert(&mut self, ident: String, node: ASTNodeTypeChecking) {
        self.nodes_with_type.insert(ident, node);
    }

    fn find(&self, ident: String) -> Option<ASTNodeTypeChecking> {
        if self.nodes_with_type.contains_key(&ident) {
            return Some(self.nodes_with_type.get(&ident).unwrap().clone());
        }
        None
    }
}

impl Visitor<Vec<Option<CompilerError>>> for TypeChecking {
    /// Realiza la visita al AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `ast` - El AST que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante esta fase
    fn visit_ast(&mut self, ast: &mut crate::model::ast::AST) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        ast.get_prefixes().iter_mut().for_each(|prefix| {
            error_vec.extend(self.visit_prefix(prefix));
        });

        ast.get_mut_sources().iter_mut().for_each(|source| {
            error_vec.extend(self.visit_source(source));
        });

        let queries = ast.get_mut_queries();
        if queries.is_some() {
            queries.as_mut().unwrap().iter_mut().for_each(|query| {
                error_vec.extend(self.visit_query(query));
            });
        }

        ast.get_mut_iterators().iter_mut().for_each(|iterator| {
            error_vec.extend(self.visit_iterator(iterator));
        });

        ast.get_mut_expressions().iter_mut().for_each(|expression| {
            error_vec.extend(self.visit_expression(expression));
        });

        ast.get_mut_shapes().iter_mut().for_each(|shape| {
            error_vec.extend(self.visit_shape(shape));
        });

        error_vec
    }

    /// Realiza la visita a un nodo Prefix del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `prefix_node` - El nodo Prefix que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Prefix
    fn visit_prefix(&mut self, _prefix_node: &mut PrefixASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    /// Realiza la visita a un nodo Source del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `source_node` - El nodo Source que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Source
    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        match source_node.get_source_definition() {
            crate::model::ast::SourceDefinition::URI(uri) => {
                if check_csv_file_extension(source_node, uri, "La URI", &mut error_vec) {
                    source_node.set_type(Type::CSV);
                }
            }
            crate::model::ast::SourceDefinition::Path(path) => {
                if check_csv_file_extension(source_node, path, "El Path", &mut error_vec) {
                    source_node.set_type(Type::CSV);
                }
            }
            crate::model::ast::SourceDefinition::JdbcURL(jdbc_url) => {
                if check_database(source_node, jdbc_url, &mut error_vec) {
                    source_node.set_type(Type::Database);
                }
            }
        }

        self.insert(source_node.get_identifier(), ASTNodeTypeChecking::Source(source_node.clone()));

        error_vec
    }

    /// Realiza la visita a un nodo Query del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `query_node` - El nodo Query que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Query
    fn visit_query(&mut self, _query_node: &mut QueryASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    /// Realiza la visita a un nodo Iterator del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `iterator_node` - El nodo Iterator que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Iterator
    fn visit_iterator(
        &mut self,
        iterator_node: &mut IteratorASTNode,
    ) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        iterator_node.get_fields().iter_mut().for_each(|field| {
            error_vec.extend(self.visit_field(field));
        });

        match iterator_node.get_iterator_access() {
            crate::model::ast::IteratorAccess::Ident(_) => {
                iterator_node.set_type(Type::Database)
            }
            crate::model::ast::IteratorAccess::SqlQuery(_) => {
                iterator_node.set_type(Type::Database)
            }
            crate::model::ast::IteratorAccess::CsvPerRow(_) => {
                iterator_node.set_type(Type::CSV)
            }
        }

        self.insert(iterator_node.get_identifier(), ASTNodeTypeChecking::Iterator(iterator_node.clone()));

        error_vec
    }

    /// Realiza la visita a un nodo Field del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `field_node` - El nodo Field que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Field
    fn visit_field(&mut self, _field_node: &mut FieldASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    /// Realiza la visita a un nodo Expression del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `expression_node` - El nodo Expression que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Expression
    fn visit_expression(
        &mut self,
        expression_node: &mut ExpressionASTNode,
    ) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
        let mut types: Vec<Option<Type>> = Vec::new();

        expression_node
            .get_mut_accesses()
            .iter_mut()
            .for_each(|access| {
                error_vec.extend(self.visit_access(access));
                types.push(access.get_type());
            });

        error_vec
    }

    /// Realiza la visita a un nodo Shape del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `shape_node` - El nodo Shape que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la Shape
    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec = Vec::new();

        shape_node.get_tuples().iter_mut().for_each(|tuple| {
            error_vec.extend(self.visit_shape_tuple(tuple));
        });

        match shape_node.get_mut_field_identifier() {
            IdentOrAccess::Access(access) => {
                error_vec.extend(self.visit_access(access));
            }
            IdentOrAccess::Ident(_) => (),
        }

        error_vec
    }


    /// Realiza la visita a un nodo tupla de la Shape del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `shape_tuple_node` - El nodo tupla de la Shape que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita a la tupla de la Shape
    fn visit_shape_tuple(
        &mut self,
        shape_tuple_node: &mut ShapeTupleASTNode,
    ) -> Vec<Option<CompilerError>> {
        let mut error_vec = Vec::new();

        match shape_tuple_node.get_mut_object() {
            IdentOrAccess::Access(access_node) => {
                error_vec.extend(self.visit_access(access_node));
            }
            IdentOrAccess::Ident(_) => (),
        }

        error_vec
    }

    /// Realiza la visita a un nodo Access del AST
    ///
    /// # Parámetros
    /// * `self` - El propio struct de la fase de Type Checking
    /// * `access_node` - El nodo Access que se va a visitar
    ///
    /// # Retorna
    /// Un vector con Options que contienen los posibles errores detectados durante la visita al Access
    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec = Vec::new();
        let num_line = access_node.get_position().get_num_line();
        let first_access = access_node.get_source_or_expression().unwrap();

        match first_access {
            SourceOrExpression::Source(source_node) => {
                let iterator = access_node.get_iterator().unwrap();
                let source = self.find(source_node.get_identifier()).unwrap();
                let ite = self.find(iterator.get_identifier()).unwrap();

                // Si el tipo del fichero Source no es CSV o la base de datos no está permitida no habra tipo
                if source.get_type().is_some() {
                    if source.get_type().unwrap() != ite.get_type().unwrap() {
                        error_vec.push(Some(CompilerError::new(format!("El iterador y el Source del acceso de la expresión de la línea {num_line} deben ser del mismo tipo: CSV o base de datos"))));
                    } else {
                        access_node.set_type(source.get_type().unwrap());
                    }
                    // Hay que cambiar el Source que hay por el nuevo, que tiene el tipo
                    access_node.set_source_or_expression(Some(SourceOrExpression::Source(source.get_source().unwrap())));
                }
            }
            SourceOrExpression::Expression(mut expression_node) => {
                error_vec.extend(self.visit_expression(&mut expression_node));
                let field_access = access_node.get_field().unwrap();
                if !expression_node
                    .get_fields()
                    .unwrap()
                    .contains(&field_access)
                {
                    error_vec.push(Some(CompilerError::new(format!(
                        "No se puede acceder al campo '{}' en el acceso de la tupla de la línea {}",
                        field_access.get_access_field_identifier(),
                        access_node.get_position().get_num_line()
                    ))));
                }
                expression_node.get_accesses().iter_mut().for_each(|access| {
                    error_vec.extend(self.visit_access(access));
                });
                // Hay que cambiar el Expression que hay por el nuevo, que tiene el tipo
                access_node.set_source_or_expression(Some(SourceOrExpression::Expression(expression_node)));
            }
        }

        error_vec
    }
}

/// Comprueba que la extensión del fichero utilizado sea CSV
/// 
/// # Parámetros
/// * `source_node` - El nodo Source en el que se encuentra la URI o Path al fichero
/// * `file` - El fichero
/// * `source_definition_type` - El tipo de Source Definition (URI o Path); se utiliza para personalizar el mensaje de error
/// * `error_vec` - El vector de errores de la fase de Type Checking
/// 
/// # Retorna
/// true si el tipo del fichero es CSV o false en caso contrario
fn check_csv_file_extension(
    source_node: &mut SourceASTNode,
    file: String,
    source_definition_type: &str,
    error_vec: &mut Vec<Option<CompilerError>>,
) -> bool {
    if !file.ends_with(".csv") {
        error_vec.push(Some(CompilerError::new(format!(
            "{source_definition_type} del Source de la línea {} no apunta a un fichero CSV",
            source_node.get_position().get_num_line()
        ))));
        return false;
    }
    true
}

/// Comprueba que la base de datos de la JDBC URL está dentro de los permitidos: PostgreSQL, MySQL, SQLite, SQLServer y Oracle
/// 
/// # Parámetros
/// * `source_node` - El nodo Source en el que se encuentra la JDBC URL
/// * `jdbc_url` - La URL JDBC del Source
/// * `error_vec` - El vector de errores de la fase de Type Checking
/// 
/// # Retorna
/// true si la base de datos está dentro de las permitidas o false en caso contrario
fn check_database(
    source_node: &mut SourceASTNode,
    jdbc_url: String,
    error_vec: &mut Vec<Option<CompilerError>>,
) -> bool {
    let database = jdbc_url.strip_prefix("jdbc:");

    if database.is_none() {
        error_vec.push(Some(CompilerError::new(format!(
            "Se ha detectado una JDBC URL incorrecta en la línea {}",
            source_node.get_position().get_num_line()
        ))));
        return false;
    } else {
        let valid_databases = vec!["postgresql", "mysql", "sqlite", "sqlserver", "oracle"];
        let database_type = database.unwrap().split(":").next();

        if database_type.is_none() {
            error_vec.push(Some(CompilerError::new(format!(
                "Se ha detectado una JDBC URL incorrecta en la línea {}",
                source_node.get_position().get_num_line()
            ))));
            return false;
        } else {
            if !valid_databases.contains(&database_type.unwrap()) {
                error_vec.push(Some(CompilerError::new(format!("Se ha detectado una base de datos inválida: {}; sólo se permiten: PostgreSQL, MySQL, SQLite, SQLServer y Oracle", database_type.unwrap()))));
                return false;
            }
        }
    }

    true
}
