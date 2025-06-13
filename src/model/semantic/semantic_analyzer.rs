//! Módulo del analizador semántico
//!
//! Realiza el análisis semántico del compilador
//! Comprueba que no se repitan identificadores,

use std::collections::HashSet;

use crate::model::{ast::*, compiler_error::CompilerError};

/// Realiza el análisis semántico del AST resultado del analizador sintáctico
///
/// Realiza varias comprobaciones a partir de los nodos del AST
///
/// # Parámetros
/// * `node` - La referencia al nodo raíz del AST
///
/// # Retorna
/// El vector con los errores semánticos encontrados durante el análisis; puede estar vacío si no se encontró ninguno
pub fn semantic_analysis(node: &FileASTNode) -> Vec<CompilerError> {
    // todo!("COMPROBAR LOS TIPOS DE FICHERO QUE SEAN CORRECTOS, LO MISMO CON LAS URLS JDBC");
    check_duplicate_identifiers(node)
}

/// Comprueba que no haya identificadores duplicados
///
/// # Parámetros
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
/// # Parámetros
/// * `prefixes` - La referencia al vector de nodos Prefix del AST
///
/// # Retorna
/// El vector con los identificadores de los PREFIX
fn get_prefix_identifiers(prefixes: &Option<Vec<PrefixASTNode>>) -> Vec<String> {
    let mut identifiers = Vec::new();
    for prefix in prefixes.as_deref().unwrap() {
        identifiers.push(prefix.identifier.clone());
    }
    identifiers
}

/// Obtiene los identificadores de los SOURCE
///
/// # Parámetros
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
/// # Parámetros
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
            prefixes: Some(vec![
                PrefixASTNode {
                    identifier: "example".to_string(),
                    uri: "https://example.com/".to_string(),
                },
                PrefixASTNode {
                    identifier: "example".to_string(),
                    uri: "http://notexample.es/".to_string(),
                },
            ]),
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: None,
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
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
            prefixes: Some(vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![
                SourceASTNode {
                    identifier: "films_csv_file".to_string(),
                    source_definition: "https://shexml.herminiogarcia.com/files/films.csv"
                        .to_string(),
                },
                SourceASTNode {
                    identifier: "films_csv_file".to_string(),
                    source_definition: "https://another.csv".to_string(),
                },
            ],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: None,
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
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
            prefixes: Some(vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![
                QueryASTNode {
                    identifier: "query_sql".to_string(),
                    sql_query: "SELECT * FROM example;".to_string(),
                },
                QueryASTNode {
                    identifier: "query_sql".to_string(),
                    sql_query: "SELECT name FROM anothertable;".to_string(),
                },
            ]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: None,
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
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
            prefixes: Some(vec![PrefixASTNode {
                identifier: "duplicate".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![SourceASTNode {
                identifier: "duplicate".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "duplicate".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: None,
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTuplesASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
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
