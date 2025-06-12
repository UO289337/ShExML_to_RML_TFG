//! Módulo del controlador
//!
//! Módulo intermediario entre el modelo y la vista
//! Su función principal radica en enviar la entrada del usuario de la vista al modelo y de coordinar las distintas fases del compilador: analizador léxico,
//! sintáctico y generación.

use chumsky::error::SimpleReason;

use crate::model;
use crate::model::ast::FileASTNode;
use crate::model::compiler_error::CompilerError;
use crate::model::lexer::token::Token;
use crate::view;

/// Ejecuta el analizador léxico del compilador
pub fn run_lexer_analyzer() {
    let file_content = view::main_view::input();

    match model::lexer::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            run_sintax_analyzer(tokens);
        }
        Err(parser_errors) => {
            show_errors(parser_errors);
        }
    };
}

/// Ejecuta el analizador sintáctico del compilador
///
/// # Parámetros
/// * `tokens` - El vector de tokens resultado del analizador léxico
fn run_sintax_analyzer(tokens: Vec<Token>) {
    match model::sintax::sintax_analyzer::parser(tokens) {
        Ok(node) => {
            run_semantic_analyzer(node);
        }
        Err(e) => {
            show_sintax_errors(e);
        }
    }
}

/// Ejecuta el analizador semántico del compilador
///
/// # Parámetros
/// * `node` - El nodo raíz del AST resultado del analizador sintáctico
fn run_semantic_analyzer(node: FileASTNode) {
    let semantic_errors = model::semantic::semantic_analyzer::semantic_analysis(&node);

    if semantic_errors.is_empty() {
        model::generator::rml_generator::rml_generator(node);
        view::main_view::show_correct_generation()
    } else {
        show_errors(semantic_errors);
    }
}

/// Muestra los errores encontrados al realizar los distintos análisis
///
/// # Parámetros
/// * `errors` - Un vector de CompilerError que contiene los errores encontrados
fn show_errors(errors: Vec<CompilerError>) {
    for error in errors {
        eprintln!("{}", error);
    }
}

/// Muestra los errores sintácticos encontrados al realizar el análisis sintáctico
///
/// # Parámetros
/// * `sintax_errors` - Un vector de Simple<Token>, de la biblioteca chumsky, que contiene los errores sintácticos encontrados
fn show_sintax_errors(sintax_errors: Vec<chumsky::prelude::Simple<Token>>) {
    for error in sintax_errors {
        let err_message = match error.reason() {
            SimpleReason::Custom(msg) => msg,
            _ => "Error desconocido",
        };
        eprintln!("Error durante el análisis sintáctico: {}", err_message);
    }
}
