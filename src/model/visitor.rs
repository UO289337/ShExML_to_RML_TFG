//! Visitor del AST

use super::ast::*;

/// Trait general del visitor para visitar cada uno de los nodos del AST
pub trait Visitor<T> {
    fn visit_file(&mut self, file_node: FileASTNode) -> T;
    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> T;
    // fn visit_source(&mut self, s: SourceASTNode) -> T;
}

/// Struct para poder realizar las visitas del visitor sobre él
pub struct Generator;

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<String> for Generator {
    /// Visita el nodo File
    /// 
    /// # Argumentos
    /// * `self` - El propio generador
    /// * `file_node` - El nodo File del AST
    /// 
    /// # Retorna
    /// Una cadena con el contenido del fichero RML
    fn visit_file(&mut self, file_node: FileASTNode) -> String {
        let mut file_generation = String::new();
        for prefix in file_node.prefixes {
            file_generation.push_str(&format!("@prefix {} .\n",  self.visit_prefix(prefix)));
        }
        file_generation
        
        // Todavía no es necesario meter el source al RML
    }

    /// Visita el nodo Prefix
    /// 
    /// # Argumentos
    /// * `self` - El propio generador
    /// * `prefix_node` - El nodo Prefix del AST
    /// 
    /// # Retorna
    /// Una cadena con el formato RML equivalente del prefix de ShExML
    fn visit_prefix(&mut self, prefix_node: PrefixASTNode) -> String {
        let mut prefix_generation = String::new();
        prefix_generation.push_str(format!("{}:     ", prefix_node.identifier).as_str());
        prefix_generation.push_str(format!("<{}>", prefix_node.uri).as_str());
        prefix_generation
    }

    /* fn visit_source(&mut self, s: SourceASTNode) -> String {
        todo!()
    } */
}