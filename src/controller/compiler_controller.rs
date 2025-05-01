use chumsky::error::SimpleReason;

use crate::view;
use crate::model;

pub fn run() {
    let file_content = view::main_view::input();

    match model::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            match model::sintax_analyzer::parser(tokens) {
                Ok(node) => {
                    model::rml_generator::rml_generator(node);
                }
                Err(e) => {
                    for err in e {
                        let err_message = match err.reason() {
                            SimpleReason::Custom(msg) => msg,
                            _ => "Error desconocido",
                        };
                        eprintln!("Error durante el análisis sintáctico: {:?}", err_message);
                    }
                }
            }
        }
        Err(parser_errors) => {
            for error in parser_errors {
                eprintln!("Error: {:?}", error)
            }
        },
    };
}