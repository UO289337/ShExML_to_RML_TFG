use tinyfiledialogs;
use std::fs;
use std::io::{Error, ErrorKind};
use std::path::Path;

pub fn input() -> Option<String> {
    let file_content = match input_shexml_file() {
        Ok(content) => Some(content),
        Err(error) => match error.kind() {
            ErrorKind::InvalidInput => panic!("{}", error.to_string()),
            ErrorKind::Interrupted => None,
            _ => panic!("Error: {}", error.to_string()),
        },
    };

    check_input(file_content)
}

fn input_shexml_file() -> Result<String, Error> {
    if let Some(file) = tinyfiledialogs::open_file_dialog("Select a ShExML file", "document.shexml", None) {
        let path = Path::new(&file);

        if path.extension().and_then(|ext| ext.to_str()) == Some("shexml") {
            return fs::read_to_string(file);
        } else {
            return Err(Error::new(ErrorKind::InvalidInput, "El fichero seleccionado no tiene la extensión '.shexml'"));

        }
    } else {
        return Err(Error::new(ErrorKind::Interrupted, "Ningún fichero seleccionado, saliendo..."));
    }
}

fn check_input(file_content: Option<String>) -> Option<String> {
    if file_content.is_some() {
        file_content
    } else {
        None
    }
}