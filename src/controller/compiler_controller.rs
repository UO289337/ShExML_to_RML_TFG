use crate::view;
use crate::model;

pub fn run() {
    let file_content = view::main_view::input();

    let lexer_analysis_result = match model::lexer_analyzer::lexer(&mut file_content.unwrap().as_str()) {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(err) => println!("Error: {:?}", err),
    };
}