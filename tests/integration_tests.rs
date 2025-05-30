//! Módulo con los tests de integración

use shexml_to_rml_tfg::model;

/// Módulo con los tests de integración del analizador léxico y del analizador sintáctico
#[cfg(test)]
mod integration_lexer_syntax_analyzers_tests {
    use super::*;


    /// Comprueba que la integración entre el analizador léxico y el analizador sintáctico se realiza correctamente
    #[doc(hidden)]
    #[test]
    fn integration_with_valid_input() {
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE films_xml_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        let sintax_result = model::sintax::sintax_analyzer::parser(lexer_result.unwrap());

        assert!(sintax_result.as_ref().is_ok_and(|file| !file.prefixes.is_empty() && !file.sources.is_empty() && file.queries.is_some()));

        let _ = sintax_result.as_ref().map(|file| {
            assert_eq!(file.prefixes.len(), 1);
            assert_eq!(file.sources.len(), 1);
            assert_eq!(file.queries.as_ref().unwrap().len(), 1);
        });
    }

    /// Comprueba que la integración entre el analizador léxico y el analizador sintáctico falla en el caso de que ocurra un error en el análisis léxico
    #[doc(hidden)]
    #[test]
    fn integration_with_lexer_fail() {
        let mut input = "PREFIX example123: <http://example.com/>
            SOURCE films_xml_file <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        assert!(lexer_result.is_err());
    }

    /// Comprueba que la integración entre el analizador léxico y el analizador sintáctico falla en el caso de que ocurra un error en el análisis sintáctico
    #[doc(hidden)]
    #[test]
    fn integration_with_syntax_fail() {
        // Test con un fallo sintáctico
        let mut input = "PREFIX example: <http://example.com/>
            SOURCE <https://shexml.herminiogarcia.com/files/films.csv>
            QUERY query_sql <sql: SELECT * FROM example;>";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        let sintax_result = model::sintax::sintax_analyzer::parser(lexer_result.unwrap());

        assert!(sintax_result.as_ref().is_err());

        let _ = sintax_result.as_ref().map(|file| {
            assert_eq!(file.prefixes.len(), 0);
            assert_eq!(file.sources.len(), 0);
        });
    }
}