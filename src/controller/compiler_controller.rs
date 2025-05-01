use chumsky::error::SimpleReason;

use crate::view;
use crate::model;
use crate::model::token::Token;
use crate::model::parser_error::ParserError;

pub fn run() {
    let file_content = view::main_view::input();

    match model::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            run_sintax_analyzer(tokens);
        }
        Err(parser_errors) => {
            show_lexer_errors(parser_errors);
        },
    };
}

fn run_sintax_analyzer(tokens: Vec<Token>) {
    match model::sintax_analyzer::parser(tokens) {
        Ok(node) => {
            model::rml_generator::rml_generator(node);
        }
        Err(e) => {
            show_sintax_errors(e);
        }
    }
}

fn show_lexer_errors(parser_errors: Vec<ParserError>) {
    for error in parser_errors {
        eprintln!("Error: {:?}", error)
    }
}

fn show_sintax_errors(e: Vec<chumsky::prelude::Simple<Token>>) {
    for err in e {
        let err_message = match err.reason() {
            SimpleReason::Custom(msg) => msg,
            _ => "Error desconocido",
        };
        eprintln!("Error durante el análisis sintáctico: {:?}", err_message);
    }
}