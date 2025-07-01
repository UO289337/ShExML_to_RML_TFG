//! Módulo de la vista gráfica

use std::{
    io::{Error, ErrorKind},
    path::Path,
};

use tinyfiledialogs::message_box_ok;

use crate::{compiler_error::CompilerError, view::main_view::*};

/// Struct para la vista gráfica
pub struct GraphicView;

impl View for GraphicView {
    /// Permite seleccionar un fichero ShExML y devuelve su contenido
    ///
    /// # Parámetros
    /// * `self` - La propia vista gráfica
    ///
    /// # Retorna
    /// Un Option que puede contener el contenido del fichero de entrada ShExML
    fn input(&self) -> Option<String> {
        let file_content = match self.input_shexml_file() {
            Ok(content) => Some(content),
            Err(error) => match error.kind() {
                ErrorKind::InvalidInput => panic!("{}", error.to_string()),
                ErrorKind::Interrupted => None,
                _ => panic!("Error: {}", error.to_string()),
            },
        };

        file_content
    }

    /// Permite seleccionar un fichero ShExML mediante CLI
    ///
    /// # Parámetros
    /// * `self` - La propia vista gráfica
    ///
    /// # Retorna
    /// Un Result que puede contener el String con el contenido del fichero o un Error
    fn input_shexml_file(&self) -> Result<String, Error> {
        if let Some(file) = tinyfiledialogs::open_file_dialog(
            "Selecciona un fichero ShExML",
            "document.shexml",
            None,
        ) {
            check_file_extension(&file)
        } else {
            Err(Error::new(
                ErrorKind::Interrupted,
                "Ningún fichero seleccionado, saliendo...",
            ))
        }
    }

    /// Muestra, en un diálogo, que se ha generado correctamente el fichero RML
    ///
    /// # Parámetros
    /// * `self` - La propia vista gráfica
    fn show_correct_generation(&self) {
        message_box_ok(
            "Información",
            "Fichero RML generado con éxito",
            tinyfiledialogs::MessageBoxIcon::Info,
        );
    }

    /// Muestra, en un diálogo, los errores encontrados durante la compilación
    ///
    /// # Parámetros
    /// * `self` - La propia vista gráfica
    /// * `errors` - El vector con los errores de compilación
    fn show_errors(&self, errors: Vec<CompilerError>) {
        message_box_ok(
            "Errores encontrados",
            get_errors_message(errors).as_str(),
            tinyfiledialogs::MessageBoxIcon::Error,
        );
    }

    /// Selecciona un fichero de salida RML
    ///
    /// # Parámetros
    /// * `self` - La propia vista gráfica
    ///
    /// # Retorna
    /// Un Result con el nombre del fichero de salida RML o con un Error
    fn select_output_file(&self) -> Result<String, Error> {
        let title = "Guardar archivo como...";
        let default_name = "generated.rml";

        if let Some(path_str) = tinyfiledialogs::save_file_dialog(title, default_name) {
            let path = Path::new(&path_str);

            if let Some(file_name) = path.file_name() {
                return Ok(file_name.to_string_lossy().to_string());
            } else {
                Err(Error::new(
                    ErrorKind::Other,
                    "Nombre del fichero RML incorrecto",
                ))
            }
        } else {
            Err(Error::new(
                ErrorKind::Other,
                "Error al crear el fichero RML",
            ))
        }
    }
}
