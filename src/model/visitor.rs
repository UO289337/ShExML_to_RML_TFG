use super::ast::*;

pub trait Visitor<T> {
    fn visit_file(&mut self, f: FileASTNode) -> T;
    fn visit_prefix(&mut self, p: PrefixASTNode) -> T;
    // fn visit_source(&mut self, s: SourceASTNode) -> T;
}

pub struct Generator;

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<String> for Generator {
    fn visit_file(&mut self, f: FileASTNode) -> String {
        let mut file_generation = String::new();
        for prefix in f.prefixes {
            file_generation.push_str(&format!("@prefix {} .\n",  self.visit_prefix(prefix)));
        }
        file_generation
        
        // Todavía no es necesario meter el source al RML
    }

    fn visit_prefix(&mut self, p: PrefixASTNode) -> String {
        let mut prefix_generation = String::new();
        prefix_generation.push_str(format!("{}:     ", p.identifier).as_str());
        prefix_generation.push_str(format!("<{}>", p.uri).as_str());
        prefix_generation
    }

    /* fn visit_source(&mut self, s: SourceASTNode) -> String {
        todo!()
    } */
}