//! Módulo de la vista
//!
//! Es la vista principal del compilador, la cual utiliza el usuario para indicar el fichero ShExML de entrada

use std::fs;
use std::io::{Error, ErrorKind};
use std::path::Path;

use crate::compiler_error::CompilerError;
use crate::view::cli_view::CliView;
use crate::view::graphic_view::GraphicView;

pub trait View {
    fn input(&self) -> Option<String>;
    fn input_shexml_file(&self) -> Result<String, Error>;
    fn show_correct_generation(&self);
    fn show_errors(&self, errors: Vec<CompilerError>);
    fn select_output_file(&self) -> Result<String, Error>;
}

pub struct ViewOption{
    view_option: Box<dyn View>,
}

impl ViewOption {
    pub fn new(option: Box<dyn View>) -> Self {
        Self { view_option: option }
    }

    pub fn input(&self) -> Option<String> {
        self.view_option.input()
    }

    pub fn show_correct_generation(&self) {
        self.view_option.show_correct_generation();
    }

    pub fn select_output_file(&self) -> Result<String, Error> {
        self.view_option.select_output_file()
    }

    pub fn show_errors(&self, errors: Vec<CompilerError>) {
        self.view_option.show_errors(errors);
    }
}

pub fn select_view(arg: &String) -> ViewOption {
    let graphic_options = vec![String::from("-g"), String::from("-G"), String::from("-graphic"), String::from("--Graphic")];

    if graphic_options.contains(arg) {
        ViewOption::new(Box::new(GraphicView))
    } else {
        ViewOption::new(Box::new(CliView))
    }
}

/// Comprueba que la extensión del fichero seleccionado por el usuario sea .shexml
///
/// # Parámetros
/// * `file` - El fichero de entrada ShExML indicado por el usuario
///
/// # Retorna
/// El contenido del fichero
///
/// # Errores
/// Devuelve un `[Error]` en el caso de que la extensión del fichero seleccionado no sea .shexml
pub fn check_file_extension(file: &String) -> Result<String, Error> {
    let path = Path::new(&file);

    if path.extension().and_then(|ext| ext.to_str()) == Some("shexml") {
        fs::read_to_string(file)
    } else {
        Err(Error::new(
            ErrorKind::InvalidInput,
            "El fichero seleccionado no tiene la extensión '.shexml'",
        ))
    }
}

pub fn get_errors_message(errors: Vec<CompilerError>) -> String {
    let mut errors_message = String::new();
    errors.into_iter().for_each(|error| {
        let normalized = error.get_message().replace("'", "");
        errors_message.push_str(format!("Error: {normalized}\n").as_str());
    });
    errors_message
}