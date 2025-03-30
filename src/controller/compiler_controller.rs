use crate::view;
use crate::model;

pub fn run() {
    let file_content = view::main_view::input();

    let lexer_analysis_result = match model::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }

            // model::sintax_analyzer::parser(tokens);
        }
        Err(parser_errors) => {
            for error in parser_errors {
                eprintln!("Error: {:?}", error)
            }
        },
    };
}