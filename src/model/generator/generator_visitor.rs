use crate::model::{ast::nodes::*, ast::*, visitor::Visitor};

/// Struct para poder realizar las visitas del visitor sobre él
pub struct Generator {
    source_number: u16,
    database_number: u16,

    unused_identifiers: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        Generator { source_number: 0, database_number: 0, unused_identifiers: Vec::new() }
    }

    fn generate_next_identifier(&mut self, identifier_prefix: &str) -> String {
        match identifier_prefix {
            "source" => {
                self.source_number += 1;
                let ident = format!("l_{}", self.source_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            "database" => {
                self.database_number += 1;
                let ident = format!("db_{}", self.source_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            _ => String::new(),
        }
    }

    fn find_last_unused_identifier(&mut self, identifier: String) -> String {
        let mut index = 0;
        if let Some(pos) = self.unused_identifiers.iter().position(|x| x.starts_with(&identifier)) {
            index = pos;
        }
        self.unused_identifiers.remove(index)
    }
}

const LOGICAL_SOURCE: &str = "rml:LogicalSource";
const REFERENCE_FORMULATION: &str = "rml:referenceFormulation";
const SOURCE: &str = "rml:source";
const QUERY: &str = "rml:query";
const SQL_VERSION: &str = "rr:sqlVersion";
const JDBC_DSN: &str = "d2rq:jdbcDSN";
const JDBC_DRIVER: &str = "d2rq:jdbcDriver";
const PASSWORD: &str = "d2rq:password";
const USERNAME: &str = "d2rq:username";

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
        ast.get_prefixes().iter_mut().for_each(|prefix| {
            file_generation.push_str(&format!("@prefix {} .\n", self.visit_prefix(prefix)));
        });
        file_generation.push_str("\n");

        // No se hace aquí la visita a los Source porque solamente se meten los que se utilizan, es decir, los que están referenciados desde un acceso
        ast.get_sources().iter_mut().for_each(|source| {
            file_generation.push_str(&format!("{}", self.visit_source(source)));
        });

        ast.get_iterators().iter_mut().for_each(|iterator| {
            file_generation.push_str(&format!("{}", self.visit_iterator(iterator)));
            check_query_iterator(&mut file_generation, iterator);
        });

        file_generation
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
        let prefix = prefix_node.get_identifier();

        prefix_generation.push_str(format!("{}:\t", prefix.as_str()).as_str());
        prefix_generation.push_str(format!("<{}>", prefix_node.get_uri()).as_str());
        prefix_generation
    }

    fn visit_source(&mut self, source_node: &mut SourceASTNode) -> String {
        let mut source_generation = String::new();
        source_generation.push_str(format!("map:{}", self.generate_next_identifier("source")).as_str());
        source_generation.push_str("  a");
        source_generation.push_str(format!("\t\t\t\t\t{LOGICAL_SOURCE} ;\n").as_str());

        match source_node.get_type().unwrap() {
            Type::CSV => source_generation.push_str(generate_csv_logical_source(source_node).as_str()),
            Type::Database => source_generation.push_str(generate_database_logical_source(source_node, self).as_str()),
        }

        source_generation
    }

    fn visit_query(&mut self, _query_node: &mut QueryASTNode) -> String {
        String::new()
    }

    fn visit_iterator(&mut self, _iterator_node: &mut IteratorASTNode) -> String {
        String::new()
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

fn generate_csv_logical_source(source_node: &mut SourceASTNode) -> String {
    let mut source_generation = String::new();
    source_generation.push_str(format!("\t{REFERENCE_FORMULATION}\t").as_str());
    source_generation.push_str(format!("ql:{} ; \n", source_node.get_type().unwrap().to_string()).as_str());

    source_generation.push_str(format!("\t{SOURCE}\t\t\t\t").as_str());
    source_generation.push_str(format!("{}", source_node.get_source_definition().to_string()).as_str());
    source_generation
}

fn generate_database_logical_source(source_node: &mut SourceASTNode, generator: &mut Generator) -> String {
    let mut source_generation = String::new();
    source_generation.push_str(format!("\t{QUERY} ; \n").as_str());
    
    source_generation.push_str(format!("\t{SOURCE}\t\t\t\t").as_str());
    source_generation.push_str(format!("map:{} ;\n", generator.generate_next_identifier("database")).as_str());

    source_generation.push_str(format!("\t{SQL_VERSION}\t\t\trr:SQL2008 .\n\n").as_str());
    source_generation.push_str(generate_database_declaration(source_node, generator).as_str());
    source_generation
}

fn generate_database_declaration(source_node: &mut SourceASTNode, generator: &mut Generator) -> String {
    let mut database_generation = String::new();
    database_generation.push_str(format!("map:{}  a\t\t\t\"http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#Database\" ;\n", generator.find_last_unused_identifier("db".to_string())).as_str());

    database_generation.push_str(format!("\t{JDBC_DSN}\t\t{} ;\n", source_node.get_source_definition().to_string()).as_str());
    database_generation.push_str(format!("\t{JDBC_DRIVER}\t\t{} ;\n", generate_database_driver(source_node)).as_str());
    database_generation.push_str(format!("\t{PASSWORD}\t\t\"\" ;\n").as_str());
    database_generation.push_str(format!("\t{USERNAME}\t\t\"\" .\n").as_str());
    database_generation
}

fn generate_database_driver(source_node: &mut SourceASTNode) -> String {
    let mut database_driver_generation = String::new();
    let source_definition = source_node.get_source_definition().to_string();
    let database = source_definition.strip_prefix("jdbc:").unwrap().split(":").next().unwrap();

    match database {
        "postgresql" => database_driver_generation = String::from("org.postgresql.Driver"),
        "mysql" => database_driver_generation = String::from("com.mysql.cj.jdbc.Driver"), 
        "sqlite" => database_driver_generation = String::from("org.sqlite.JDBC"), 
        "sqlserver" => database_driver_generation = String::from("com.microsoft.sqlserver.jdbc.SQLServerDriver"), 
        "oracle" => database_driver_generation = String::from("oracle.jdbc.OracleDriver"),
        _ => (),
    }

    database_driver_generation
}

fn check_query_iterator(file_generation: &mut String, iterator: &mut IteratorASTNode) {
    let possible_query = iterator.get_query();
    if possible_query.is_some() {
        set_query_in_logical_source(file_generation, possible_query.unwrap().get_sql_query().to_string());
    }

    let iterator_access = iterator.get_iterator_access();
    match iterator_access {
        IteratorAccess::SqlQuery(sql_query) =>  set_query_in_logical_source(file_generation, sql_query),
        IteratorAccess::Ident(_) => (),
        IteratorAccess::CsvPerRow(_) => (),
    }
}

fn set_query_in_logical_source(file_generation: &mut String, query: String) {
    if let Some(pos) = file_generation.find("rml:query") {
        let insert_pos = pos + "rml:query".len();
        file_generation.insert_str(insert_pos, format!("\t\t\t\t\"{}\"", query).as_str());
    }
}