//! Módulo del controlador
//!
//! Módulo intermediario entre el modelo y la vista
//! Su función principal radica en enviar la entrada del usuario de la vista al modelo y de coordinar las distintas fases del compilador: analizador léxico,
//! sintáctico y generación.

use std::env;

use chumsky::error::SimpleReason;

use crate::compiler_error::CompilerError;
use crate::model;
use crate::model::ast::AST;
use crate::model::lexer::token::Token;
use crate::view;
use crate::view::main_view::ViewOption;

/// Ejecuta el compilador
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
///
/// # Parámetros
/// * `view` - La vista utilizada
/// * `file_content` - El contenido del fichero ShExML de entrada
pub fn run_lexer_analyzer(view: ViewOption, file_content: String) {
    match model::lexer::lexer_analyzer::lexer(&mut file_content.as_str()) {
        Ok(tokens) => {
            run_sintax_analyzer(view, tokens);
        }
        Err(parser_errors) => {
            view.show_errors(parser_errors);
        }
    };
}

/// Ejecuta el analizador sintáctico del compilador
///
/// # Parámetros
/// * `view` - La vista utilizada
/// * `tokens` - El vector de tokens resultado del analizador léxico
fn run_sintax_analyzer(view: ViewOption, tokens: Vec<Token>) {
    match model::syntax::syntax_analyzer::parser(tokens) {
        Ok(mut ast) => {
            run_semantic_analyzer(view, &mut ast);
        }
        Err(e) => {
            view.show_errors(trasnform_to_compiler_error(e));
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
        view.show_errors(semantic_errors);
    }
}

/// Ejecuta el generador de RML
///
/// # Parámetros
/// * `view` - La vista utilizada
/// * `astg` - El AST decorado resultado del análisis semántico
fn run_rml_generator(view: ViewOption, ast: &mut AST) {
    let output_file = view.select_output_file();

    match output_file {
        Ok(output_file) => {
            model::generator::rml_generator::rml_generator(ast, output_file);
            view.show_correct_generation();
        }
        Err(error) => panic!("Error: {}", error),
    }
}

/// Transforma errores Simple de chumsky en errores del compilador
///
/// # Parámetros
/// * `sintax_errors` - Errores del analizador sintáctico de chumsky
///
/// # Retorna
/// Un vector con los errores del compilador
fn trasnform_to_compiler_error(
    sintax_errors: Vec<chumsky::prelude::Simple<Token>>,
) -> Vec<CompilerError> {
    let mut errors = Vec::new();

    for error in sintax_errors {
        match error.reason() {
            SimpleReason::Custom(msg) => errors.push(CompilerError::new(msg.clone())),
            _ => errors.push(CompilerError::new(String::from("Error desconocido"))),
        };
    }

    errors
}
