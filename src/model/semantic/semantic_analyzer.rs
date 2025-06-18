//! Módulo del analizador semántico
//!
//! Realiza el análisis semántico del compilador
//! Comprueba que no se repitan identificadores,

use std::collections::HashSet;

use crate::model::{ast::*, ast::nodes::*, compiler_error::CompilerError};

/// Realiza el análisis semántico del AST resultado del analizador sintáctico
///
/// Realiza varias comprobaciones a partir de los nodos del AST
///
/// # Parámetros
/// * `node` - La referencia al nodo raíz del AST
///
/// # Retorna
/// El vector con los errores semánticos encontrados durante el análisis; puede estar vacío si no se encontró ninguno
pub fn semantic_analysis(node: &AST) -> Vec<CompilerError> {
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
fn check_duplicate_identifiers(node: &AST) -> Vec<CompilerError> {
    let mut identifiers = Vec::new();
    let mut duplicate_idents_errors = Vec::new();

    identifiers.extend(get_prefix_identifiers(&node.get_prefixes()));
    identifiers.extend(get_source_identifiers(&node.get_sources()));

    if node.get_queries().is_some() {
        identifiers.extend(get_queries_identifiers(node.get_queries().as_ref().unwrap()));
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
fn get_prefix_identifiers(prefixes: &Vec<PrefixASTNode>) -> Vec<String> {
    let mut identifiers = Vec::new();
    for prefix in prefixes {
        if let Some(p) = prefix.get_identifier() {
            identifiers.push(p);
        }
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
        identifiers.push(source.get_identifier());
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
        identifiers.push(query.get_identifier());
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

    /*
    /// Comprueba que se detectan identificadores duplicados de PREFIX
    #[doc(hidden)]
    #[test]
    fn detect_duplicate_prefix_identifiers() {
        let input = AST {
            prefixes: vec![
                PrefixASTNode::new(identifier, uri)
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
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                query: None,
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
            expressions: TestUtilities::create_default_expressions_for_ast(),
            shapes: vec![ShapeASTNode {
                prefix_ident: "example".to_string(),
                identifier: "Films".to_string(),
                field_prefix_ident: "example".to_string(),
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    first_access: "id".to_string(),
                    second_access: None,
                    source_or_expression: None,
                    iterator: None,
                    field: None,
                }),
                prefix: None,
                field_prefix: None,
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "name".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "name".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "year".to_string(),
                        object_prefix_ident: Some("example".to_string()),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "year".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "country".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "country".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "director".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "director".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
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
        let input = AST {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
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
                query: None,
            }],
            expressions: TestUtilities::create_default_expressions_for_ast(),
            shapes: vec![ShapeASTNode {
                prefix_ident: "example".to_string(),
                identifier: "Films".to_string(),
                field_prefix_ident: "example".to_string(),
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    first_access: "id".to_string(),
                    second_access: None,
                    source_or_expression: None,
                    iterator: None,
                    field: None,
                }),
                prefix: None,
                field_prefix: None,
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "name".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "name".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "year".to_string(),
                        object_prefix_ident: Some("example".to_string()),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "year".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "country".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "country".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "director".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "director".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
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
        let input = AST {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
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
                query: None,
            }],
            expressions: TestUtilities::create_default_expressions_for_ast(),
            shapes: vec![ShapeASTNode {
                prefix_ident: "example".to_string(),
                identifier: "Films".to_string(),
                field_prefix_ident: "example".to_string(),
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    first_access: "id".to_string(),
                    second_access: None,
                    source_or_expression: None,
                    iterator: None,
                    field: None,
                }),
                prefix: None,
                field_prefix: None,
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "name".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "name".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "year".to_string(),
                        object_prefix_ident: Some("example".to_string()),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "year".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "country".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "country".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "director".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "director".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
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
        let input = AST {
            prefixes: vec![PrefixASTNode {
                identifier: "duplicate".to_string(),
                uri: "https://example.com/".to_string(),
            }],
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
                query: None,
            }],
            expressions: TestUtilities::create_default_expressions_for_ast(),
            shapes: vec![ShapeASTNode {
                prefix_ident: "example".to_string(),
                identifier: "Films".to_string(),
                field_prefix_ident: "example".to_string(),
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    first_access: "id".to_string(),
                    second_access: None,
                    source_or_expression: None,
                    iterator: None,
                    field: None,
                }),
                prefix: None,
                field_prefix: None,
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "name".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "name".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "year".to_string(),
                        object_prefix_ident: Some("example".to_string()),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "year".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "country".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "country".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
                    },
                    ShapeTupleASTNode {
                        prefix_ident: "example".to_string(),
                        identifier: "director".to_string(),
                        object_prefix_ident: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            first_access: "director".to_string(),
                            second_access: None,
                            source_or_expression: None,
                            iterator: None,
                            field: None,
                        }),
                        prefix: None,
                        object_prefix: None,
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
    */
}
