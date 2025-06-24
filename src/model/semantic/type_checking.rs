//! Módulo de la fase de chequeo de tipos (type checking) del analizador semántico; se utiliza el patrón Visitor
//!
//! Comprueba que el tipo del Source es correcto y que las bases de datos utilizadas están permitidas

use crate::model::{ast::{nodes::*, SourceType}, compiler_error::CompilerError, visitor::Visitor};

/// Struct para poder realizar las visitas del visitor de la fase de Identificación sobre él
pub struct Identification;

impl Visitor<Vec<Option<CompilerError>>> for Identification {
    fn visit_ast(&mut self, ast: &mut crate::model::ast::AST) -> Vec<Option<CompilerError>> {
        ast.get_prefixes().iter_mut().for_each(|prefix| {
            self.visit_prefix(prefix);
        });
        vec![None]
    }

    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> Vec<Option<CompilerError>> {
        let mut error_vec: Vec<Option<CompilerError>> = Vec::new();

        match source_node.get_source_definition() {
            crate::model::ast::SourceDefinition::URI(uri) => {
                if check_csv_file_extension(source_node, uri, "URI", &mut error_vec) {
                    source_node.set_source_type(SourceType::CSV);
                }
            },
            crate::model::ast::SourceDefinition::Path(path) => {
                if check_csv_file_extension(source_node, path, "Path", &mut error_vec) {
                    source_node.set_source_type(SourceType::CSV);
                }
            },
            crate::model::ast::SourceDefinition::JdbcURL(jdbc_url) => {
                if check_database(source_node, jdbc_url, &mut error_vec) {
                    source_node.set_source_type(SourceType::Database);
                }
            },
        }

        error_vec
    }

    fn visit_query(&mut self, query_node: &mut QueryASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    fn visit_iterator(&mut self, iterator_node: &mut IteratorASTNode) -> Vec<Option<CompilerError>> {
        iterator_node.get_fields().iter_mut().for_each(|field| {
            self.visit_field(field);
        });
        vec![None]
    }

    fn visit_field(&mut self, field_node: &mut FieldASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }

    fn visit_expression(&mut self, expression_node: &mut ExpressionASTNode) -> Vec<Option<CompilerError>> {
        expression_node.get_accesses().iter_mut().for_each(|access| {
            self.visit_access(access);
        });
        vec![None]
    }

    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> Vec<Option<CompilerError>> {
        shape_node.get_tuples().iter_mut().for_each(|tuple| {
            self.visit_shape_tuple(tuple);
        });

        vec![None]
    }

    fn visit_shape_tuple(&mut self, shape_tuple_node: &mut ShapeTupleASTNode) -> Vec<Option<CompilerError>> {
        
        vec![None]
    }

    fn visit_access(&mut self, access_node: &mut AccessASTNode) -> Vec<Option<CompilerError>> {
        vec![None]
    }
}

fn check_csv_file_extension(source_node: &mut SourceASTNode, file: String, source_definition_type: &str, error_vec: &mut Vec<Option<CompilerError>>) -> bool {
    let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
    if !file.ends_with(".csv") {
        error_vec.push(Some(CompilerError::new(format!("El Path {source_definition_type} del Source de la línea {} no apunta a un fichero CSV", source_node.get_position().get_num_line()))));
        return false;
    }
    true
}

fn check_database(source_node: &mut SourceASTNode, jdbc_url: String, error_vec: &mut Vec<Option<CompilerError>>) -> bool {
    let mut error_vec: Vec<Option<CompilerError>> = Vec::new();
    let database = jdbc_url
        .strip_prefix("jdbc:");

    if database.is_none() {
        error_vec.push(Some(CompilerError::new(format!("Se ha detectado una JDBC URL incorrecta en la línea {}", source_node.get_position().get_num_line()))));
        return false;
    } else {
        let valid_databases = vec!["postgresql", "mysql", "sqlite", "sqlserver", "oracle"];
        let database_type = database.unwrap().split(":").next();

        if database_type.is_none() {
            error_vec.push(Some(CompilerError::new(format!("Se ha detectado una JDBC URL incorrecta en la línea {}", source_node.get_position().get_num_line()))));
            return false;
        } else {
            if !valid_databases.contains(&database_type.unwrap()) {
                error_vec.push(Some(CompilerError::new(format!("Se ha detectado un tipo de base de datos inválida: {}; sólo se permiten: PostgreSQL, MySQL, SQLite, SQLServer y Oracle", database_type.unwrap()))));
                return false;
            }
        }
    }

    true
}