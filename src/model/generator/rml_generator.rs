//! Módulo del generador del fichero RML
//!
//! A partir del nodo raíz del AST, recorre este para generar el fichero RML equivalente al fichero ShExML inicial

use std::fs;
use std::io::prelude::*;

use crate::model::generator::generator_visitor::Generator;
use crate::model::visitor::Visitor;

use super::super::ast::AST;

const ERR_MESSAGE: &str = "Error durante la escritura del archivo RML";

/// Genera el fichero RML
///
/// Recorre el AST para ir extrayendo la información e ir escribiéndola en el fichero RML en el formato adecuado
///
/// # Parámetros
/// * `ast` - El AST
pub fn rml_generator(ast: &mut AST, file_output: String) {
    let mut generator = Generator::new();
    let mut rml_file = fs::File::create(file_output).unwrap();

    // Contenido general
    writeln!(
        rml_file,
        "@prefix rml:      <http://semweb.mmlab.be/ns/rml#> ."
    )
    .expect(ERR_MESSAGE);
    writeln!(
        rml_file,
        "@prefix rr:       <http://www.w3.org/ns/r2rml#> ."
    )
    .expect(ERR_MESSAGE);
    writeln!(
        rml_file,
        "@prefix d2rq:     <http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#> ."
    )
    .expect(ERR_MESSAGE);
    writeln!(
        rml_file,
        "@prefix ql:       <http://semweb.mmlab.be/ns/ql#> ."
    )
    .expect(ERR_MESSAGE);
    writeln!(
        rml_file,
        "@prefix map:      <http://mapping.example.com/> ."
    )
    .expect(ERR_MESSAGE);

    // Contenido del fichero Shexml
    writeln!(rml_file, "{}", generator.visit_ast(ast)).expect(ERR_MESSAGE);
}
