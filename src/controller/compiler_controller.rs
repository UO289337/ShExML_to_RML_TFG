//! Módulo del controlador
//!
//! Módulo intermediario entre el modelo y la vista
//! Su función principal radica en enviar la entrada del usuario de la vista al modelo y de coordinar las distintas fases del compilador: analizador léxico,
//! sintáctico y generación.

use chumsky::error::SimpleReason;

use crate::model;
use crate::model::parser_error::ParserError;
use crate::model::token::Token;
use crate::view;

/// Ejecuta el analizador léxico del compilador
pub fn run_lexer_analyzer() {
    let file_content = view::main_view::input();

    match model::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            run_sintax_analyzer(tokens);
        }
        Err(parser_errors) => {
            show_lexer_errors(parser_errors);
        }
    };
}

/// Ejecuta el analizador sintáctico del compilador
///
/// # Argumentos
/// * `tokens` - El vector de tokens resultado del analizador léxico
fn run_sintax_analyzer(tokens: Vec<Token>) {
    match model::sintax_analyzer::parser(tokens) {
        Ok(node) => {
            model::rml_generator::rml_generator(node);
            view::main_view::show_correct_generation()
        }
        Err(e) => {
            show_sintax_errors(e);
        }
    }
}

/// Muestra los errores léxicos encontrados al realizar el análisis léxico
///
/// # Argumentos
/// * `lexer_errors` - Un vector de ParserError que contiene los errores léxicos encontrados
fn show_lexer_errors(lexer_errors: Vec<ParserError>) {
    for error in lexer_errors {
        eprintln!("Error léxico: {:?}", error)
    }
}

/// Muestra los errores sintácticos encontrados al realizar el análisis sintáctico
///
/// # Argumentos
/// * `sintax_errors` - Un vector de Simple<Token>, de la biblioteca chumsky, que contiene los errores sintácticos encontrados
fn show_sintax_errors(sintax_errors: Vec<chumsky::prelude::Simple<Token>>) {
    for error in sintax_errors {
        let err_message = match error.reason() {
            SimpleReason::Custom(msg) => msg,
            _ => "Error desconocido",
        };
        eprintln!("Error durante el análisis sintáctico: {:?}", err_message);
    }
}
