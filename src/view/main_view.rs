//! Módulo de la vista
//! 
//! Es la vista principal del compilador, la cual utiliza el usuario para indicar el fichero ShExML de entrada

use tinyfiledialogs::{self, message_box_ok};
use std::fs;
use std::io::{Error, ErrorKind};
use std::path::Path;

/// Permite al usuario indicar el fichero ShExML de entrada
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

/// Abre un diálogo del sistema para que el usuario indique el fichero ShExML de entrada
/// 
/// # Errores
/// * `[Error]` - Error en caso de que no se seleccione ningún fichero
fn input_shexml_file() -> Result<String, Error> {
    if let Some(file) = tinyfiledialogs::open_file_dialog("Selecciona un fichero ShExML", "document.shexml", None) {
        check_file_extension(&file)
    } else {
        Err(Error::new(ErrorKind::Interrupted, "Ningún fichero seleccionado, saliendo..."))
    }
}

/// Comprueba que la extensión del fichero seleccionado por el usuario sea .shexml
/// 
/// # Argumentos
/// * `file` - El fichero de entrada ShExML indicado por el usuario
fn check_file_extension(file: &String) -> Result<String, Error> {
    let path = Path::new(&file);

    if path.extension().and_then(|ext| ext.to_str()) == Some("shexml") {
        fs::read_to_string(file)
    } else {
        Err(Error::new(ErrorKind::InvalidInput, "El fichero seleccionado no tiene la extensión '.shexml'"))
    }
}

/// Comprueba que el fichero ShExML no esté vacío
/// 
/// # Argumentos
/// * `file_content` - El contenido del fichero ShExML de entrada seleccionado por el usuario
fn check_input(file_content: Option<String>) -> Option<String> {
    if file_content.is_some() {
        file_content
    } else {
        None
    }
}

/// Muestra un mensaje de Ok por pantalla cuando se genera el fichero RML
pub fn show_correct_generation() {
    message_box_ok(
        "Información",
        "Fichero RML generado con éxito",
        tinyfiledialogs::MessageBoxIcon::Info,
    );
}