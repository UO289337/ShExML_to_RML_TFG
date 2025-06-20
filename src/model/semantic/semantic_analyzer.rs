//! Módulo del analizador semántico
//!
//! Realiza el análisis semántico del compilador
//! Comprueba que no se repitan identificadores,

use crate::model::{ast::*, compiler_error::CompilerError, semantic::identification_visitor::{reset_state, Identification}, visitor::Visitor};

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
    // todo!("COMPROBAR LOS TIPOS DE FICHERO QUE SEAN CORRECTOS, LO MISMO CON LAS URLS JDBC");
    let error_vec = identification_phase(ast);

    convert_option_errors_to_compile_errors(error_vec)
}

fn identification_phase(ast: &mut AST) -> Vec<Option<CompilerError>> {
    let mut identification = Identification;
    identification.visit_ast(ast)
}

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

pub fn reset_table() {
    reset_state();
}

/// Módulo de los tests del analizador léxico
///
/// Contiene los tests que se encargan de probar que se detectan todos los tokens válidos y se descartan los inválidos
/// Los tests se hacen tanto a nivel de tokens individuales como a nivel de tokens en conjunto
#[cfg(test)]
mod semantic_tests {
    use super::*;
    use crate::test_utils::TestUtilities;

    /// Comprueba que se pasa la fase de identificación con un identificador de Query en el Iterator y con accesos simples
    #[doc(hidden)]
    #[test]
    fn identification_withouth_errors_with_inline_query_and_simple_access() {
        reset_table();
        let prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        let sources = TestUtilities::create_sources_for_ast(
            "films_csv_file",
            "https://shexml.herminiogarcia.com/files/films.csv",
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
        assert_eq!(first_access.get_souce_or_expression().unwrap(), SourceOrExpression::Source(source.clone()));
        assert_eq!(first_access.get_iterator().unwrap(), iterator.clone());

        // Comprueba que los accesos de las tuplas de la Shape están asociados a la Expression y a Fields
        // También comprueba los prefijos
        let prefixes = ast.get_prefixes();
        let mut shapes = ast.get_shapes();
        let tuples = shapes.get_mut(0).unwrap().get_tuples();
        tuples.into_iter().for_each(|tuple| {
            assert!(prefixes.contains(&tuple.get_prefix().unwrap()));
            let object_prefix = tuple.get_object_prefix();
            if object_prefix.is_some() {
                assert!(prefixes.contains(&object_prefix.unwrap()));
            }
        });
    }
}
