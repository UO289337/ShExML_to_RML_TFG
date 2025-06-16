use crate::model::{ast::*, visitor::Visitor};

/// Struct para poder realizar las visitas del visitor de la fase de Identificación sobre él
pub struct Identification;

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<()> for Identification {
    fn visit_file(&mut self, file_node: FileASTNode) {
        todo!()
    }

    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) {
        todo!()
    }
}