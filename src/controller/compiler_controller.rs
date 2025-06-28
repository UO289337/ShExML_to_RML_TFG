//! Módulo del controlador
//!
//! Módulo intermediario entre el modelo y la vista
//! Su función principal radica en enviar la entrada del usuario de la vista al modelo y de coordinar las distintas fases del compilador: analizador léxico,
//! sintáctico y generación.

use std::env;

use chumsky::error::SimpleReason;

use crate::model;
use crate::model::ast::AST;
use crate::model::compiler_error::CompilerError;
use crate::model::lexer::token::Token;
use crate::view::main_view::ViewOption;
use crate::view;

pub fn run() {
    let args: Vec<String> = env::args().collect();

    let view = view::main_view::select_view(&args[1]);
    let file_content = view.input();

    if file_content.is_some() {
        run_lexer_analyzer(view, file_content.unwrap());
    } else {
        panic!("Ha ocurrido un error al procesar la entrada");
    }
}

/// Ejecuta el analizador léxico del compilador
pub fn run_lexer_analyzer(view: ViewOption, file_content: String) {
    match model::lexer::lexer_analyzer::lexer(&mut file_content.as_str()) {
        Ok(tokens) => {
            run_sintax_analyzer(view, tokens);
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
fn run_sintax_analyzer(view: ViewOption, tokens: Vec<Token>) {
    match model::syntax::syntax_analyzer::parser(tokens) {
        Ok(mut ast) => {
            run_semantic_analyzer(view, &mut ast);
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
fn run_semantic_analyzer(view: ViewOption, ast: &mut AST) {
    let semantic_errors = model::semantic::semantic_analyzer::semantic_analysis(ast);

    if semantic_errors.is_empty() {
        run_rml_generator(view, ast);
    } else {
        show_errors(semantic_errors);
    }
}

fn run_rml_generator(view: ViewOption, ast: &mut AST) {
    let output_file = view.select_output_file();

    match output_file {
        Ok(output_file) => {
            model::generator::rml_generator::rml_generator(ast, output_file);
            view.show_correct_generation();
        },
        Err(error) => panic!("Error: {}", error),
    }
}

/// Muestra los errores encontrados al realizar los distintos análisis
///
/// # Parámetros
/// * `errors` - Un vector de CompilerError que contiene los errores encontrados
fn show_errors(errors: Vec<CompilerError>) {
    errors.into_iter().for_each(|error| {
        println!("Error: {}", error.get_message());
    });
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
