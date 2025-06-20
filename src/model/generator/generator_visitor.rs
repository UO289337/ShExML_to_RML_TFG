use crate::model::{ast::nodes::*, ast::*, visitor::Visitor};

/// Struct para poder realizar las visitas del visitor sobre él
pub struct Generator;

// No se utiliza &str porque no se podria devolver el valor al tener la propiedad
impl Visitor<String> for Generator {
    /// Visita el nodo File
    ///
    /// # Parámetros
    /// * `self` - El propio generador
    /// * `file_node` - El nodo File del AST
    ///
    /// # Retorna
    /// Una cadena con el contenido del fichero RML
    fn visit_ast(&mut self, ast: &mut AST) -> String {
        let mut file_generation = String::new();
        for mut prefix in ast.get_prefixes() {
            file_generation.push_str(&format!("@prefix {} .\n", self.visit_prefix(&mut prefix)));
        }
        file_generation

        // Todavía no es necesario meter el source al RML
    }

    /// Visita el nodo Prefix
    ///
    /// # Parámetros
    /// * `self` - El propio generador
    /// * `prefix_node` - El nodo Prefix del AST
    ///
    /// # Retorna
    /// Una cadena con el formato RML equivalente del prefix de ShExML
    fn visit_prefix(&mut self, prefix_node: &mut PrefixASTNode) -> String {
        let mut prefix_generation = String::new();
        let prefix;

        if let Some(p) = prefix_node.get_identifier() {
            prefix = p;
        } else {
            prefix = String::new();
        }

        prefix_generation.push_str(&format!("{}:     ", prefix.as_str()));
        prefix_generation.push_str(format!("<{}>", prefix_node.get_uri()).as_str());
        prefix_generation
    }

    fn visit_source(&mut self, _source_node: &mut SourceASTNode) -> String {
        todo!()
    }

    fn visit_query(&mut self, _query_node: &mut QueryASTNode) -> String {
        todo!()
    }

    fn visit_iterator(&mut self, _iterator_node: &mut IteratorASTNode) -> String {
        todo!()
    }

    fn visit_field(&mut self, _field_node: &mut FieldASTNode) -> String {
        todo!()
    }

    fn visit_expression(&mut self, _expression_node: &mut ExpressionASTNode) -> String {
        todo!()
    }

    fn visit_shape(&mut self, _shape_node: &mut ShapeASTNode) -> String {
        todo!()
    }

    fn visit_shape_tuple(&mut self, _shape_tuple_node: &mut ShapeTupleASTNode) -> String {
        todo!()
    }

    fn visit_access(&mut self, _access_node: &mut AccessASTNode) -> String {
        todo!()
    }
}
