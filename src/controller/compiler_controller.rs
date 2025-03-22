use crate::view;
use crate::model;

pub fn run() {
    let file_content = match view::main_view::input_shexml_file() {
        Ok(content) => content,
        Err(error) => panic!("Problem opening the file: {error:?}"),
    };

    let lexer_analysis_result = match model::lexer_analyzer::lexer(&mut file_content.as_str()) {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(err) => println!("Error: {:?}", err),
    };
}