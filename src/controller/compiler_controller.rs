use crate::view;

pub fn run() {
    let file_content = match view::main_view::input_shexml_file() {
        Ok(content) => content,
        Err(error) => panic!("Problem opening the file: {error:?}"),
    };

    
}