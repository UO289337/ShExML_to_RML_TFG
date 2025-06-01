//! Módulo del analizador semántico
//!
//! Realiza el análisis semántico del compilador
//! Comprueba que no se repitan identificadores,

use std::collections::HashSet;

use crate::model::{
    ast::{FileASTNode, PrefixASTNode, QueryASTNode, SourceASTNode},
    compiler_error::CompilerError,
};

/// Realiza el análisis semántico del AST resultado del analizador sintáctico
///
/// Realiza varias comprobaciones a partir de los nodos del AST
///
/// # Argumentos
/// * `node` - La referencia al nodo raíz del AST
///
/// # Retorna
/// El vector con los errores semánticos encontrados durante el análisis; puede estar vacío si no se encontró ninguno
pub fn semantic_analysis(node: &FileASTNode) -> Vec<CompilerError> {
    check_duplicate_identifiers(node)
}

/// Comprueba que no haya identificadores duplicados
///
/// # Argumentos
/// * `node` - La referencia al nodo raíz del AST
///
/// # Retorna
/// El vector con los errores semánticos relacionados de identificadores duplicados
fn check_duplicate_identifiers(node: &FileASTNode) -> Vec<CompilerError> {
    let mut identifiers = Vec::new();
    let mut duplicate_idents_errors = Vec::new();

    identifiers.extend(get_prefix_identifiers(&node.prefixes));
    identifiers.extend(get_source_identifiers(&node.sources));

    if node.queries.is_some() {
        identifiers.extend(get_queries_identifiers(node.queries.as_ref().unwrap()));
    }

    let mut non_duplicates = HashSet::new();

    for identifier in identifiers {
        if !non_duplicates.insert(identifier.clone()) {
            duplicate_idents_errors.push(CompilerError::new(format!(
                "Identificador duplicado: {}",
                identifier
            )));
        }
    }

    duplicate_idents_errors
}

/// Obtiene los identificadores de los PREFIX
///
/// # Argumentos
/// * `prefixes` - La referencia al vector de nodos Prefix del AST
///
/// # Retorna
/// El vector con los identificadores de los PREFIX
fn get_prefix_identifiers(prefixes: &Vec<PrefixASTNode>) -> Vec<String> {
    let mut identifiers = Vec::new();
    for prefix in prefixes {
        identifiers.push(prefix.identifier.clone());
    }
    identifiers
}

/// Obtiene los identificadores de los SOURCE
///
/// # Argumentos
/// * `sources` - La referencia al vector de nodos Source del AST
///
/// # Retorna
/// El vector con los identificadores de los SOURCE
fn get_source_identifiers(sources: &Vec<SourceASTNode>) -> Vec<String> {
    let mut identifiers = Vec::new();
    for source in sources {
        identifiers.push(source.identifier.clone());
    }
    identifiers
}

/// Obtiene los identificadores de los QUERY
///
/// # Argumentos
/// * `queries` - La referencia al vector de nodos Query del AST
///
/// # Retorna
/// El vector con los identificadores de los QUERY
fn get_queries_identifiers(queries: &Vec<QueryASTNode>) -> Vec<String> {
    let mut identifiers = Vec::new();
    for query in queries {
        identifiers.push(query.identifier.clone());
    }
    identifiers
}

// Tests

/// Módulo de los tests del analizador léxico
///
/// Contiene los tests que se encargan de probar que se detectan todos los tokens válidos y se descartan los inválidos
/// Los tests se hacen tanto a nivel de tokens individuales como a nivel de tokens en conjunto
#[cfg(test)]
mod lexer_tests {

    use super::*;

    /// Comprueba que se detectan identificadores duplicados de PREFIX
    #[doc(hidden)]
    #[test]
    fn detect_duplicate_prefix_identifiers() {
        let input = FileASTNode {
            prefixes: vec![
                PrefixASTNode {
                    identifier: "example".to_string(),
                    uri: "https://example.com/".to_string(),
                },
                PrefixASTNode {
                    identifier: "example".to_string(),
                    uri: "http://notexample.es/".to_string(),
                },
            ],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_path: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                query_definition: "SELECT * FROM example;".to_string(),
            }]),
        };

        let actual = check_duplicate_identifiers(&input);
        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual[0],
            CompilerError::new("Identificador duplicado: example".to_string())
        );
    }

    /// Comprueba que se detectan identificadores duplicados de SOURCE
    #[doc(hidden)]
    #[test]
    fn detect_duplicate_source_identifiers() {
        let input = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![
                SourceASTNode {
                    identifier: "films_csv_file".to_string(),
                    source_path: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
                },
                SourceASTNode {
                    identifier: "films_csv_file".to_string(),
                    source_path: "https://another.csv".to_string(),
                },
            ],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                query_definition: "SELECT * FROM example;".to_string(),
            }]),
        };

        let actual = check_duplicate_identifiers(&input);
        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual[0],
            CompilerError::new("Identificador duplicado: films_csv_file".to_string())
        );
    }

    /// Comprueba que se detectan identificadores duplicados de QUERY
    #[doc(hidden)]
    #[test]
    fn detect_duplicate_query_identifiers() {
        let input = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_path: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![
                QueryASTNode {
                    identifier: "query_sql".to_string(),
                    query_definition: "SELECT * FROM example;".to_string(),
                },
                QueryASTNode {
                    identifier: "query_sql".to_string(),
                    query_definition: "SELECT name FROM anothertable;".to_string(),
                },
            ]),
        };

        let actual = check_duplicate_identifiers(&input);
        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual[0],
            CompilerError::new("Identificador duplicado: query_sql".to_string())
        );
    }

    /// Comprueba que se detectan identificadores duplicados entre PREFIX, SOURCE Y QUERY
    #[doc(hidden)]
    #[test]
    fn detect_duplicate_identifiers_between_structures() {
        let input = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "duplicate".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "duplicate".to_string(),
                source_path: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "duplicate".to_string(),
                query_definition: "SELECT * FROM example;".to_string(),
            }]),
        };

        let actual = check_duplicate_identifiers(&input);
        assert_eq!(actual.len(), 2);
        assert_eq!(
            actual[0],
            CompilerError::new("Identificador duplicado: duplicate".to_string())
        );
        assert_eq!(
            actual[1],
            CompilerError::new("Identificador duplicado: duplicate".to_string())
        );
    }
}
