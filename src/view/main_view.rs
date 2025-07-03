//! Módulo de la vista
//!
//! Es la vista principal del compilador, la cual utiliza el usuario para indicar el fichero ShExML de entrada

use std::fs;
use std::io::{Error, ErrorKind};
use std::path::Path;

use crate::compiler_error::CompilerError;
use crate::view::cli_view::CliView;
use crate::view::graphic_view::GraphicView;

/// Trait de la vista; patrón Strategy
pub trait View {
    fn input(&self) -> Option<String>;
    fn input_shexml_file(&self) -> Result<String, Error>;
    fn show_correct_generation(&self);
    fn show_errors(&self, errors: Vec<CompilerError>);
    fn select_output_file(&self) -> Result<String, Error>;
}

/// Struct que contiene la opción de vista seleccionada por el usuario
pub struct ViewOption {
    option: Box<dyn View>,
}

impl ViewOption {
    /// Crea una nuevo struct de opción de vista con una opción
    ///
    /// # Parámetros
    /// * `option` - La opción de vista escogida
    ///
    /// # Retorna
    /// Un struct ViewOption
    pub fn new(option: Box<dyn View>) -> Self {
        Self {
            option,
        }
    }

    /// Devuelve la opción de la vista escogida por el usuario
    ///
    /// # Parámetros
    /// * `self` - El propio struct ViewOption
    ///
    /// # Retorna
    /// La vista seleccionada por el usuario
    pub fn get_option(&self) -> &dyn View {
        self.option.as_ref()
    }
}

/// Selecciona una opción de vista dependiendo de la entrada del usuario
///
/// # Parámetros
/// * `arg` - Los argumentos pasados por línea de comandos por el usuario
///
/// # Retorna
/// La opción de vista seleccionada
pub fn select_view(arg: &String) -> ViewOption {
    let graphic_options = vec![
        String::from("-g"),
        String::from("-G"),
        String::from("-graphic"),
        String::from("--Graphic"),
    ];

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

/// Concatena todos los errores de compilación detectados en una cadena
///
/// # Parámetros
/// * `errors` - Vector con los errores de compilación
///
/// # Retorna
/// Una cadena con todos los errores de compilación
pub fn get_errors_message(errors: Vec<CompilerError>) -> String {
    let mut errors_message = String::new();
    errors.into_iter().for_each(|error| {
        let normalized = error.get_message().replace("'", "");
        errors_message.push_str(format!("Error: {normalized}\n").as_str());
    });
    errors_message
}
