use std::{io::{Error, ErrorKind}, path::Path};

use tinyfiledialogs::message_box_ok;

use crate::{model::compiler_error::CompilerError, view::main_view::*};


pub struct GraphicView;

impl View for GraphicView {
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

    /// Abre un diálogo del sistema para que el usuario indique el fichero ShExML de entrada
    ///
    /// # Retorna
    /// El contenido del fichero
    ///
    /// # Errores
    /// Devuelve un `[Error]` en el caso de que no se seleccione ningún fichero
    fn input_shexml_file(&self) -> Result<String, Error> {
        if let Some(file) =
            tinyfiledialogs::open_file_dialog("Selecciona un fichero ShExML", "document.shexml", None)
        {
            check_file_extension(&file)
        } else {
            Err(Error::new(
                ErrorKind::Interrupted,
                "Ningún fichero seleccionado, saliendo...",
            ))
        }
    }

    fn show_correct_generation(&self) {
        message_box_ok(
            "Información",
            "Fichero RML generado con éxito",
            tinyfiledialogs::MessageBoxIcon::Info,
        );
    }

    fn show_errors(&self, errors: Vec<CompilerError>) {
        let mut errors_message = String::new();
        errors.clone().into_iter().for_each(|error| {
            errors_message.push_str(format!("Error: {}", error.get_message()).as_str());
        });

        message_box_ok(
            "Errores encontrados",
            &get_errors_message(errors),
            tinyfiledialogs::MessageBoxIcon::Error,
        );
    }
    
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