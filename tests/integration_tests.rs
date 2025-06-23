//! Módulo con los tests de integración

use shexml_to_rml_tfg::model;

/// Módulo con los tests de integración del analizador léxico, del analizador sintáctico y del analizador semántico
#[cfg(test)]
mod integration_lexer_syntax_analyzers_tests {

    use shexml_to_rml_tfg::model::semantic::semantic_analyzer::reset_table;

    use super::*;

    /// Comprueba que la integración entre el analizador léxico, el analizador sintáctico y el semántico se realiza correctamente
    #[doc(hidden)]
    #[test]
    fn integration_with_valid_input() {
        reset_table();
        let mut input = "PREFIX example: <http://example.com/>
            PREFIX dbr: <http://dbpedia.org/resource/>
            SOURCE films_csv_ast <https://shexml.herminiogarcia.com/asts/films.csv>
            QUERY inline_query <sql: SELECT * FROM example;>
            ITERATOR films_csv <csvperrow> {
                FIELD id <@id>
                FIELD name <name>
                FIELD year <year>
                FIELD country <country>
                FIELD director <director>
            }
            EXPRESSION films <films_csv_ast.films_csv>
            example:Films example:[films.id] {
                example:name [films.name] ;
                example:year [films.year] ;
                example:country dbr:[films.country] ;
                example:director dbr:[films.director] ;
            }";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        let sintax_result = model::syntax::syntax_analyzer::parser(lexer_result.unwrap());
        let sintax_result_for_semantic = sintax_result.clone();
        let semantic_result =
            model::semantic::semantic_analyzer::semantic_analysis(&mut sintax_result_for_semantic.unwrap());

        assert!(semantic_result.is_empty());
        assert!(sintax_result
            .clone()
            .is_ok_and(|ast| !ast.get_prefixes().is_empty()
                && !ast.get_sources().is_empty()
                && ast.get_queries().is_some()
                && !ast.get_iterators().is_empty()
                && !ast.get_expressions().is_empty()
                && !ast.get_shapes().is_empty()));

        let _ = sintax_result.map(|ast| {
            assert_eq!(ast.get_prefixes().len(), 2);
            assert_eq!(ast.get_sources().len(), 1);
            assert_eq!(ast.get_queries().as_ref().unwrap().len(), 1);
            assert_eq!(ast.get_iterators().len(), 1);
            assert_eq!(ast.get_expressions().len(), 1);
            assert_eq!(ast.get_shapes().len(), 1);
        });
        reset_table();
    }

    /// Comprueba que la integración entre los analizadores falla en el caso de que ocurra un error en el análisis léxico
    #[doc(hidden)]
    #[test]
    fn integration_with_lexer_fail() {
        reset_table();
        let mut input = "PREFIX 123example: <http://example.com/>
            PREFIX dbr: <http://dbpedia.org/resource/>
            SOURCE films_csv_ast <https://shexml.herminiogarcia.com/asts/films.csv>
            QUERY inline_query <sql: SELECT * FROM example;>
            ITERATOR films_csv <csvperrow> {
                FIELD id <@id>
                FIELD name <name>
                FIELD year <year>
                FIELD country <country>
                FIELD director <director>
            }
            EXPRESSION films <films_csv_ast.films_csv>
            example:Films example:[films.id] {
                example:name [films.name] ;
                example:year [films.year] ;
                example:country dbr:[films.country] ;
                example:director dbr:[films.director] ;
            }";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        assert!(lexer_result.is_err());
        reset_table();
    }

    /// Comprueba que la integración entre los analizadores falla en el caso de que ocurra un error en el análisis sintáctico
    #[doc(hidden)]
    #[test]
    fn integration_with_syntax_fail() {
        reset_table();
        let mut input = "PREFIX example: <http://example.com/>
            PREFIX dbr: <http://dbpedia.org/resource/>
            SOURCE <https://shexml.herminiogarcia.com/asts/films.csv>
            QUERY inline_query <sql: SELECT * FROM example;>
            ITERATOR films_csv <csvperrow> {
                FIELD id <@id>
                FIELD name <name>
                FIELD year <year>
                FIELD country <country>
                FIELD director <director>
            }
            EXPRESSION films <films_csv_ast.films_csv>
            example: example:[films.id] {
                example:name [films.name] ;
                example:year [films.year] ;
                example:country dbr:[films.country] ;
                example:director dbr:[films.director] ;
            }";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        let sintax_result = model::syntax::syntax_analyzer::parser(lexer_result.unwrap());

        assert!(sintax_result.as_ref().is_err());

        let _ = sintax_result.map(|ast| {
            assert_eq!(ast.get_prefixes().len(), 0);
            assert_eq!(ast.get_sources().len(), 0);
        });
        reset_table();
    }

    /// Comprueba que la integración entre los analizadores falla en el caso de que ocurra un error en el análisis semántico
    #[doc(hidden)]
    #[test]
    fn integration_with_semantic_fail() {
        reset_table();
        let mut input = "PREFIX example: <http://example.com/>
            PREFIX example: <http://dbpedia.org/resource/>
            SOURCE films_csv <https://shexml.herminiogarcia.com/asts/films.csv>
            QUERY inline_query <sql: SELECT * FROM example;>
            ITERATOR films_csv <csvperrow> {
                FIELD id <@id>
                FIELD name <name>
                FIELD year <year>
                FIELD country <country>
                FIELD director <director>
            }
            EXPRESSION films <films_csv_ast.films_csv>
            example:Films example:[films.id] {
                example:name [films.name] ;
                example:year [films.year] ;
                example:country dbr:[films.country] ;
                example:director dbr:[films.director] ;
            }";
        let lexer_result = model::lexer::lexer_analyzer::lexer(&mut input);
        let sintax_result = model::syntax::syntax_analyzer::parser(lexer_result.unwrap());
        let semantic_result =
            model::semantic::semantic_analyzer::semantic_analysis(&mut sintax_result.unwrap());

        /*
        semantic_result.into_iter().for_each(|error| {
            println!("{}", error.get_message());
        });
        */

        // Salen muchos errores porque se coge el primer identificador detectado, por lo que films_csv es un Source y no el Iterator y,
        // por tanto, tampoco se cogen sus campos
        assert_eq!(semantic_result.len(), 10);
        reset_table();
    }
}
