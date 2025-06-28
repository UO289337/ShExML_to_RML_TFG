

use std::{env, io::{Error, ErrorKind}};

use crate::{model::compiler_error::CompilerError, view::main_view::*};


pub struct CliView;

impl View for CliView {
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

    fn show_correct_generation(&self) {
        println!("Fichero RML generado con éxito");
    }

    fn show_errors(&self, errors: Vec<CompilerError>) {
        println!("{}", get_errors_message(errors));
    }
    
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