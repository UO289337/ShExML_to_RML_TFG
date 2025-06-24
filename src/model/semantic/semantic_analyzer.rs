//! Módulo del analizador semántico
//!
//! Realiza el análisis semántico del compilador
//! El análisis semántico se divide en 2 fases: la fase de identificación y la de chequeo de tipos (type checking)

use crate::model::{ast::*, compiler_error::CompilerError, semantic::{identification_visitor::Identification, type_checking::TypeChecking}, visitor::Visitor};

/// Realiza el análisis semántico del AST resultado del analizador sintáctico
///
/// Realiza varias comprobaciones a partir de los nodos del AST
///
/// # Parámetros
/// * `node` - La referencia al nodo raíz del AST
///
/// # Retorna
/// El vector con los errores semánticos encontrados durante el análisis; puede estar vacío si no se encontró ninguno
pub fn semantic_analysis(ast: &mut AST) -> Vec<CompilerError> {
    let mut error_vec = identification_phase(ast);
    if all_errors_none(&error_vec) {
        error_vec.extend(type_checking_phase(ast));
    }
    convert_option_errors_to_compile_errors(error_vec)
}

/// Realiza la llamada al Visitor de la fase de identificación
/// 
/// # Parámetros
/// * `ast` - El AST que se va a visitar
/// 
/// # Retorna
/// Un vector con Options que contienen los posibles errores que se pueden dar en la fase de identificación
fn identification_phase(ast: &mut AST) -> Vec<Option<CompilerError>> {
    let mut identification = Identification::new();
    identification.visit_ast(ast)
}

/// Realiza la llamada al Visitor de la fase de Type Checking
/// 
/// # Parámetros
/// * `ast` - El AST que se va a visitar
/// 
/// # Retorna
/// Un vector con Options que contienen los posibles errores que se pueden dar en la fase de type checking
fn type_checking_phase(ast: &mut AST) -> Vec<Option<CompilerError>> {
    let mut type_checking = TypeChecking;
    type_checking.visit_ast(ast)
}

fn all_errors_none(error_vec: &Vec<Option<CompilerError>>) -> bool {
    for error in error_vec {
        if error.is_some() {
            return false;
        }
    }
    true
}

/// Convierte un vector con Options con errores en un vector con únicamente los errores
/// 
/// # Parámetros
/// * `error_vec` - El vector con los Options que contiene los posibles errores
/// 
/// # Retorna
/// Un vector únicamente con los errores
fn convert_option_errors_to_compile_errors(error_vec: Vec<Option<CompilerError>>) -> Vec<CompilerError> {
    let mut errors: Vec<CompilerError> = Vec::new();
    error_vec.into_iter().for_each(|error| {
        if error.is_some() {
            errors.push(error.unwrap());
        }
    });
    errors
}

// Tests

/// Módulo de los tests de la fase de identificación del analizador semántico
#[cfg(test)]
mod identification_tests {

    use super::*;
    use crate::{model::{ast::nodes::*, lexer::token::{Token, TokenType, CSV_PER_ROW}}, test_utils::TestUtilities};

    /// Comprueba que se pasa la fase de identificación con un identificador de Query en el Iterator y con accesos simples
    #[doc(hidden)]
    #[test]
    fn identification_withouth_errors_with_inline_query_and_simple_access() {
        let prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        let sources = TestUtilities::create_sources_for_ast(
            "films_database",
            SourceDefinition::JdbcURL("jdbc:mysql://localhost:3306/mydb".to_string()),
            2,
        );
        let mut queries =
            TestUtilities::create_queries_for_ast("inline_query", "SELECT * FROM example;", 3);
        let iterators = TestUtilities::create_default_iterators_for_ast(4);
        let expressions = TestUtilities::create_default_expressions_for_ast(10);
        let shapes = TestUtilities::create_default_shapes_for_ast(11);

        let mut ast = AST::new(prefixes, sources, queries.clone(), iterators, expressions, shapes);
        let actual = identification_phase(&mut ast);

        actual.into_iter().for_each(|error| {
            /*
            if error.is_some() {
                println!("{}", error.unwrap().get_message());
            }
            */
            assert!(error.is_none());
        });

        // Las llaves son necesarias para evitar tener que clonar el ast debido a que es &mut
        let mut iterators = ast.get_iterators();
        let iterator = iterators.get_mut(0).unwrap();
        assert_eq!(iterator.get_query().clone().unwrap(), queries.as_mut().unwrap().get(0).unwrap().clone());

        // Comprueba que los accesos de la Expression están asociados al Source y al Iterator
        let mut expressions = ast.get_expressions();
        let accesses = expressions.get_mut(0).unwrap().get_accesses();
        let first_access = accesses.get(0).unwrap();
        let sources = ast.get_sources();
        let source = sources.get(0).unwrap();
        assert_eq!(first_access.get_source_or_expression().unwrap(), SourceOrExpression::Source(source.clone()));
        assert_eq!(first_access.get_iterator().unwrap(), iterator.clone());

        // Comprueba que los accesos de las tuplas de la Shape están asociados a la Expression y a Fields
        // También comprueba los prefijos
        let prefixes = ast.get_prefixes();
        let mut shapes = ast.get_shapes();
        let shape = shapes.get_mut(0).unwrap();
        let tuples = shape.get_tuples();
        tuples.into_iter().for_each(|tuple| {
            assert!(prefixes.contains(&tuple.get_prefix().unwrap()));
            let object_prefix = tuple.get_object_prefix();
            if object_prefix.is_some() {
                assert!(prefixes.contains(&object_prefix.unwrap()));
            }
        });

        // Comprueba que Expression tiene acceso a los campos esperados
        let expression = expressions.get(0).unwrap();
        assert_eq!(expression.get_fields().unwrap().len(), 4);
    }

    /// Comprueba que se pasa la fase de identificación con varios prefijos y accesos a iteradores
    #[doc(hidden)]
    #[test]
    fn identification_withouth_errors_with_various_prefix_and_access_to_iterators() {
        let mut prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        prefixes.extend(TestUtilities::create_prefixes_for_ast("dbr", "http://dbpedia.org/resource/", 2));
        prefixes.extend(TestUtilities::create_prefixes_for_ast("", "http://default.com", 3));
        let sources = TestUtilities::create_sources_for_ast(
            "films_csv_file",
            SourceDefinition::URI("https://shexml.herminiogarcia.com/files/films.csv".to_string()),
            4,
        );
        let fields1 = vec![
            FieldASTNode::new(
                Token::create_test_token("id", 6, TokenType::Ident),
                Token::create_test_token("@id", 6, TokenType::KeyIdentifier),
                Position::new(6),
            ),
            FieldASTNode::new(
                Token::create_test_token("name", 7, TokenType::Ident),
                Token::create_test_token("name", 7, TokenType::Ident),
                Position::new(7),
            ),
            FieldASTNode::new(
                Token::create_test_token("year", 8, TokenType::Ident),
                Token::create_test_token("year", 8, TokenType::Ident),
                Position::new(8),
            )
        ];
        let fields2 = vec![
            FieldASTNode::new(
                Token::create_test_token("id", 10, TokenType::Ident),
                Token::create_test_token("@id", 10, TokenType::KeyIdentifier),
                Position::new(10),
            ),
            FieldASTNode::new(
                Token::create_test_token("name", 11, TokenType::Ident),
                Token::create_test_token("name", 11, TokenType::Ident),
                Position::new(11),
            ),
            FieldASTNode::new(
                Token::create_test_token("country", 12, TokenType::Ident),
                Token::create_test_token("country", 12, TokenType::Ident),
                Position::new(12),
            )
        ];
        let ident1 = Token::create_test_token("films_csv", 5, TokenType::Ident);
        let ident2 = Token::create_test_token("another_films_csv", 9, TokenType::Ident);
        let csvperrow = Token::create_test_token(CSV_PER_ROW, 5, TokenType::CsvPerRow);

        let iterators = vec![IteratorASTNode::new(
            ident1.clone(),
            IteratorAccess::CsvPerRow(csvperrow.get_lexeme()),
            fields1.clone(),
            Position::new(ident1.get_num_line()),
        ),
        IteratorASTNode::new(
            ident2.clone(),
            IteratorAccess::CsvPerRow(csvperrow.get_lexeme()),
            fields2,
            Position::new(ident2.get_num_line()),
        )];

        let identifier = Token::create_test_token("films_csv_file", 13, TokenType::Ident);
        let first_access1 = Token::create_test_token("films_csv", 13, TokenType::Ident);
        let first_access2 = Token::create_test_token("another_films_csv", 13, TokenType::Ident);
        let second_access = Token::create_test_token("id", 13, TokenType::Ident);
        let accesses = vec![AccessASTNode::new(
            identifier.clone(),
            first_access1,
            Some(second_access.clone()),
            Position::new(identifier.get_num_line()),
        ),
        AccessASTNode::new(
            identifier.clone(),
            first_access2,
            Some(second_access),
            Position::new(identifier.get_num_line()),
        )];

        let identifier = Token::create_test_token("films_ids", 13, TokenType::Ident);

        let id_expression = vec![ExpressionASTNode::new(
            identifier.clone(),
            ExpressionType::UNION,
            accesses,
            Position::new(identifier.get_num_line()),
        )];
        
        let identifier = Token::create_test_token("films_csv_file", 14, TokenType::Ident);
        let first_access1 = Token::create_test_token("films_csv", 14, TokenType::Ident);
        let first_access2 = Token::create_test_token("another_films_csv", 14, TokenType::Ident);
        let second_access = Token::create_test_token("name", 14, TokenType::Ident);
        let accesses = vec![AccessASTNode::new(
            identifier.clone(),
            first_access1,
            Some(second_access.clone()),
            Position::new(identifier.get_num_line()),
        ),
        AccessASTNode::new(
            identifier.clone(),
            first_access2,
            Some(second_access),
            Position::new(identifier.get_num_line()),
        )];

        let identifier = Token::create_test_token("films_names", 14, TokenType::Ident);

        let mut expressions = vec![ExpressionASTNode::new(
            identifier.clone(),
            ExpressionType::UNION,
            accesses,
            Position::new(identifier.get_num_line()),
        )];
        expressions.extend(id_expression);

        let prefix_ident = Token::create_test_token("", 15, TokenType::Ident);
        let identifier = Token::create_test_token("Films", 15, TokenType::Ident);
        let field_prefix_ident = Token::create_test_token("dbr", 15, TokenType::Ident);
        let tuples = vec![
            ShapeTupleASTNode::new(
                Some(Token::create_test_token(
                    "example",
                    16,
                    TokenType::Ident,
                )),
                Token::create_test_token("name", 16, TokenType::Ident),
                Some(Token::create_test_token("example", 16, TokenType::Ident)),
                IdentOrAccess::Ident("films_name".to_string()),
                Position::new(16),
            )];

        let shapes = vec![ShapeASTNode::new(
            Some(prefix_ident),
            identifier.clone(),
            Some(field_prefix_ident),
            IdentOrAccess::Ident("films_ids".to_string()),
            tuples,
            Position::new(identifier.get_num_line()),
        )];

        let mut ast = AST::new(prefixes, sources, None, iterators, expressions, shapes);
        let actual = identification_phase(&mut ast);

        actual.into_iter().for_each(|error| {
            if error.is_some() {
                println!("{}", error.unwrap().get_message());
            }
            // assert!(error.is_none());
        });

        // Las llaves son necesarias para evitar tener que clonar el ast debido a que es &mut
        let mut iterators = ast.get_iterators();
        let iterator = iterators.get_mut(0).unwrap();
        assert!(iterator.get_query().is_none());

        // Comprueba que los accesos de la Expression están asociados al Source, al Iterator y a los Field
        let mut expressions = ast.get_expressions();
        let accesses = expressions.get_mut(0).unwrap().get_accesses();
        let first_access = accesses.get(0).unwrap();
        let sources = ast.get_sources();
        let source = sources.get(0).unwrap();
        assert_eq!(first_access.get_source_or_expression().unwrap(), SourceOrExpression::Source(source.clone()));
        assert_eq!(first_access.get_iterator().unwrap(), iterator.clone());
        assert!(iterator.clone().get_fields().contains(&first_access.get_field().unwrap()));

        // Comprueba que los accesos de las tuplas de la Shape están asociados a la Expression y a Fields
        // También comprueba los prefijos
        let prefixes = ast.get_prefixes();
        let mut shapes = ast.get_shapes();
        let shape = shapes.get_mut(0).unwrap();
        let tuples = shape.get_tuples();
        tuples.into_iter().for_each(|tuple| {
            assert!(prefixes.contains(&tuple.get_prefix().unwrap()));
            let object_prefix = tuple.get_object_prefix();
            assert!(prefixes.contains(&object_prefix.unwrap()));
        });

        // Comprueba que Expression tiene acceso a los campos esperados
        let expression = expressions.get(0).unwrap();
        assert_eq!(expression.get_fields().unwrap().len(), 6);
    }

    /// Comprueba que no se pasa la fase de identificación si se utilizan identificadores desconocidos
    #[doc(hidden)]
    #[test]
    fn identification_with_unknown_identifiers() {
        let mut prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        prefixes.extend(TestUtilities::create_prefixes_for_ast("dbr", "http://dbpedia.org/resource/", 2));
        prefixes.extend(TestUtilities::create_prefixes_for_ast("", "http://default.com", 3));
        let sources = TestUtilities::create_sources_for_ast(
            "films_csv_file",
            SourceDefinition::URI("https://shexml.herminiogarcia.com/files/films.csv".to_string()),
            4,
        );
        let fields = vec![
            FieldASTNode::new(
                Token::create_test_token("id", 6, TokenType::Ident),
                Token::create_test_token("@id", 6, TokenType::KeyIdentifier),
                Position::new(6),
            ),
            FieldASTNode::new(
                Token::create_test_token("name", 7, TokenType::Ident),
                Token::create_test_token("name", 7, TokenType::Ident),
                Position::new(7),
            ),
            FieldASTNode::new(
                Token::create_test_token("year", 8, TokenType::Ident),
                Token::create_test_token("year", 8, TokenType::Ident),
                Position::new(8),
            )
        ];
        let ident = Token::create_test_token("films_csv", 5, TokenType::Ident);
        let iterator_access = Token::create_test_token("CSV_PER_ROW", 5, TokenType::Ident);

        let iterators = vec![IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::Ident(iterator_access.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        )];
        
        let identifier = Token::create_test_token("films_csv_file", 9, TokenType::Ident);
        let first_access = Token::create_test_token("films", 9, TokenType::Ident);
        let second_access = Token::create_test_token("director", 9, TokenType::Ident);
        let accesses = vec![AccessASTNode::new(
            identifier.clone(),
            first_access,
            Some(second_access),
            Position::new(identifier.get_num_line()),
        )];

        let identifier = Token::create_test_token("films", 9, TokenType::Ident);

        let expressions = vec![ExpressionASTNode::new(
            identifier.clone(),
            ExpressionType::BASIC,
            accesses,
            Position::new(identifier.get_num_line()),
        )];

        let prefix_ident = Token::create_test_token("", 10, TokenType::Ident);
        let identifier = Token::create_test_token("Films", 10, TokenType::Ident);
        let field_prefix_ident = Token::create_test_token("noexiste", 10, TokenType::Ident);
        let access = AccessASTNode::new(
            Token::create_test_token("films", 10, TokenType::Ident),
            Token::create_test_token("id", 10, TokenType::Ident),
            None,
            Position::new(10),
        );
        let tuples = vec![
            ShapeTupleASTNode::new(
                Some(Token::create_test_token(
                    "example",
                    11,
                    TokenType::Ident,
                )),
                Token::create_test_token("name", 11, TokenType::Ident),
                Some(Token::create_test_token("example", 11, TokenType::Ident)),
                IdentOrAccess::Access(AccessASTNode::new(
                    Token::create_test_token("ejemplo", 11, TokenType::Ident),
                    Token::create_test_token("name", 11, TokenType::Ident),
                    None,
                    Position::new(11),
                )),
                Position::new(11),
            ),
            ShapeTupleASTNode::new(
                Some(Token::create_test_token(
                    "example",
                    12,
                    TokenType::Ident,
                )),
                Token::create_test_token("year", 12, TokenType::Ident),
                Some(Token::create_test_token("dbr", 12, TokenType::Ident)),
                IdentOrAccess::Access(AccessASTNode::new(
                    Token::create_test_token("films", 12, TokenType::Ident),
                    Token::create_test_token("year", 12, TokenType::Ident),
                    None,
                    Position::new(12),
                )),
                Position::new(12),
            )];

        let shapes = vec![ShapeASTNode::new(
            Some(prefix_ident),
            identifier.clone(),
            Some(field_prefix_ident),
            IdentOrAccess::Access(access),
            tuples,
            Position::new(identifier.get_num_line()),
        )];

        let mut ast = AST::new(prefixes, sources, None, iterators, expressions, shapes);
        let actual = identification_phase(&mut ast);

        let mut cont_errors = 0;
        let expected_errors = vec!["No se encuentra el identificador de la Query del acceso del iterador: CSV_PER_ROW, en la línea 5",
            "Se esperaba el identificador de un Iterator o Field después del primer '.', pero se encontró films en la línea 9",
            "Se esperaba el identificador de un Field después del segundo '.', pero se encontró director en la línea 9",
            "Se esperaba que el identificador noexiste se correspondiera con un Prefix en la línea 10",
            "Se esperaba el identificador de un Source o una Expression antes del primer '.', pero se encontró ejemplo en la línea 11"];
        actual.into_iter().for_each(|error| {
            if error.is_some() {
                assert!(expected_errors.contains(&error.unwrap().get_message().as_str()));
                cont_errors += 1;
            }
        });
        assert_eq!(cont_errors, 5);
    }

    /// Comprueba que no se pasa la fase de identificación si se utilizan identificadores de otros elementos
    #[doc(hidden)]
    #[test]
    fn identification_with_identifiers_of_other_structures() {
        let mut prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        prefixes.extend(TestUtilities::create_prefixes_for_ast("dbr", "http://dbpedia.org/resource/", 2));
        prefixes.extend(TestUtilities::create_prefixes_for_ast("", "http://default.com", 3));
        let sources = TestUtilities::create_sources_for_ast(
            "films_csv_file",
            SourceDefinition::URI("https://shexml.herminiogarcia.com/files/films.csv".to_string()),
            4,
        );
        let fields = vec![
            FieldASTNode::new(
                Token::create_test_token("id", 6, TokenType::Ident),
                Token::create_test_token("@id", 6, TokenType::KeyIdentifier),
                Position::new(6),
            ),
            FieldASTNode::new(
                Token::create_test_token("name", 7, TokenType::Ident),
                Token::create_test_token("name", 7, TokenType::Ident),
                Position::new(7),
            ),
            FieldASTNode::new(
                Token::create_test_token("year", 8, TokenType::Ident),
                Token::create_test_token("year", 8, TokenType::Ident),
                Position::new(8),
            )
        ];
        let ident = Token::create_test_token("films_csv", 5, TokenType::Ident);
        let iterator_access = Token::create_test_token("example", 5, TokenType::Ident);

        let iterators = vec![IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::Ident(iterator_access.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        )];
        
        let identifier = Token::create_test_token("films_csv_file", 9, TokenType::Ident);
        let first_access = Token::create_test_token("example", 9, TokenType::Ident);
        let second_access = Token::create_test_token("", 9, TokenType::Ident);
        let accesses = vec![AccessASTNode::new(
            identifier.clone(),
            first_access,
            Some(second_access),
            Position::new(identifier.get_num_line()),
        )];

        let identifier = Token::create_test_token("films", 9, TokenType::Ident);

        let expressions = vec![ExpressionASTNode::new(
            identifier.clone(),
            ExpressionType::BASIC,
            accesses,
            Position::new(identifier.get_num_line()),
        )];

        let prefix_ident = Token::create_test_token("", 10, TokenType::Ident);
        let identifier = Token::create_test_token("Films", 10, TokenType::Ident);
        let field_prefix_ident = Token::create_test_token("dbr", 10, TokenType::Ident);
        let access = AccessASTNode::new(
            Token::create_test_token("films", 10, TokenType::Ident),
            Token::create_test_token("id", 10, TokenType::Ident),
            None,
            Position::new(10),
        );
        let tuples = vec![
            ShapeTupleASTNode::new(
                Some(Token::create_test_token(
                    "example",
                    11,
                    TokenType::Ident,
                )),
                Token::create_test_token("name", 11, TokenType::Ident),
                Some(Token::create_test_token("example", 11, TokenType::Ident)),
                IdentOrAccess::Access(AccessASTNode::new(
                    Token::create_test_token("Films", 11, TokenType::Ident),
                    Token::create_test_token("name", 11, TokenType::Ident),
                    None,
                    Position::new(11),
                )),
                Position::new(11),
            ),
            ShapeTupleASTNode::new(
                Some(Token::create_test_token(
                    "example",
                    12,
                    TokenType::Ident,
                )),
                Token::create_test_token("year", 12, TokenType::Ident),
                Some(Token::create_test_token("dbr", 12, TokenType::Ident)),
                IdentOrAccess::Access(AccessASTNode::new(
                    Token::create_test_token("films", 12, TokenType::Ident),
                    Token::create_test_token("year", 12, TokenType::Ident),
                    None,
                    Position::new(12),
                )),
                Position::new(12),
            )];

        let shapes = vec![ShapeASTNode::new(
            Some(prefix_ident),
            identifier.clone(),
            Some(field_prefix_ident),
            IdentOrAccess::Access(access),
            tuples,
            Position::new(identifier.get_num_line()),
        )];

        let mut ast = AST::new(prefixes, sources, None, iterators, expressions, shapes);
        let actual = identification_phase(&mut ast);

        let mut cont_errors = 0;
        let expected_errors = vec!["Se esperaba que el identificador example se correspondiera con una consulta SQL en la línea 5",
            "Se esperaba el identificador de un Iterator o Field después del primer '.', pero se encontró example en la línea 9",
            "Se esperaba el identificador de un Field después del segundo '.', pero se encontró  en la línea 9",
            "Se esperaba el identificador de un Source o una Expression antes del primer '.', pero se encontró Films en la línea 11"];
        actual.into_iter().for_each(|error| {
            if error.is_some() {
                assert!(expected_errors.contains(&error.unwrap().get_message().as_str()));
                cont_errors += 1;
            }
        });
        assert_eq!(cont_errors, 4);
    }
}

/// Módulo de los tests de la fase de chequeo de tipos del analizador semántico
#[cfg(test)]
mod type_checking_tests {
    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que 
    #[doc(hidden)]
    #[test]
    fn type_checking_withouth_errors() {
        let prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        let sources = TestUtilities::create_sources_for_ast(
            "films_database",
            SourceDefinition::JdbcURL("jdbc:mysql://localhost:3306/mydb".to_string()),
            2,
        );
        let queries =
            TestUtilities::create_queries_for_ast("inline_query", "SELECT * FROM example;", 3);
        let iterators = TestUtilities::create_default_iterators_for_ast(4);
        let expressions = TestUtilities::create_default_expressions_for_ast(10);
        let shapes = TestUtilities::create_default_shapes_for_ast(11);

        let mut ast = AST::new(prefixes, sources, queries, iterators.clone(), expressions, shapes);
        // Es más fácil pasar la fase de identificación directamente
        let _ = identification_phase(&mut ast);
        let actual = type_checking_phase(&mut ast);

        actual.into_iter().for_each(|error| {
            /*
            if error.is_some() {
                println!("{}", error.unwrap().get_message());
            }
            */
            assert!(error.is_none());
        });

        // Comprobamos que se han asignado correctamente los tipos al Iterator y al Source
        let mut iterators = ast.get_iterators();
        let iterator = iterators.get_mut(0).unwrap();
        assert_eq!(iterator.get_type().unwrap(), Type::Database);

        let mut sources = ast.get_sources();
        let source = sources.get_mut(0).unwrap();
        assert_eq!(source.get_type().unwrap(), Type::Database);
    }
}