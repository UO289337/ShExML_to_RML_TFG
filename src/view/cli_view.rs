//! Módulo de la vista por línea de comandos (CLI)

use std::{
    env,
    io::{Error, ErrorKind},
};

use crate::{compiler_error::CompilerError, view::main_view::*};

/// Struct para la vista por CLI
pub struct CliView;

impl View for CliView {
    /// Permite seleccionar un fichero ShExML y devuelve su contenido
    ///
    /// # Parámetros
    /// * `self` - La propia vista CLI
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
    /// * `self` - La propia vista CLI
    ///
    /// # Retorna
    /// Un Result que puede contener el String con el contenido del fichero o un Error
    fn input_shexml_file(&self) -> Result<String, Error> {
        let args: Vec<String> = env::args().collect();

        if args.len() < 2 {
            return Err(Error::new(
                ErrorKind::Interrupted,
                "Al utilizar la línea de comandos se debe pasar el nombre del fichero ShExML",
            ));
        } else {
            let arg = args[1].clone();
            if arg != String::from("-f") && arg != String::from("-F") {
                return Err(Error::new(
                    ErrorKind::Interrupted,
                    "Se debe poner un '-f' o '-F' antes del nombre del fichero ShExML",
                ));
            } else {
                check_file_extension(&args[2])
            }
        }
    }

    /// Muestra un mensaje por línea de comandos indicando que se ha generado correctamente el fichero RML
    ///
    /// # Parámetros
    /// * `self` - La propia vista CLI
    fn show_correct_generation(&self) {
        println!("Fichero RML generado con éxito");
    }

    /// Muestra por línea de comandos los errores encontrados durante la compilación
    ///
    /// # Parámetros
    /// * `self` - La propia vista CLI
    /// * `errors` - El vector con los errores de compilación
    fn show_errors(&self, errors: Vec<CompilerError>) {
        println!("{}", get_errors_message(errors));
    }

    /// Selecciona un fichero de salida RML
    ///
    /// # Parámetros
    /// * `self` - La propia vista CLI
    ///
    /// # Retorna
    /// Un Result con el nombre del fichero de salida RML o con un Error
    fn select_output_file(&self) -> Result<String, Error> {
        let args: Vec<String> = env::args().collect();

        if args.len() < 4 {
            return Err(Error::new(
            ErrorKind::Interrupted,
            "Al utilizar la línea de comandos se debe pasar el nombre del fichero RML de salida",
            ));
        } else {
            let arg = args[3].clone();
            if arg != String::from("-o") && arg != String::from("-O") {
                return Err(Error::new(
                    ErrorKind::Interrupted,
                    "Se debe poner un '-o' o '-O' antes del nombre del fichero RML de salida",
                ));
            } else {
                Ok(args[4].clone())
            }
        }
    }
}
