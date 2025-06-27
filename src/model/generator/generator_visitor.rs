use crate::model::{ast::nodes::*, ast::*, visitor::Visitor};

/// Struct para poder realizar las visitas del visitor sobre él
pub struct Generator {
    logical_source_number: u16,
    database_number: u16,
    subject_map_number: u16,
    predicate_map_number: u16,
    object_map_number: u16,
    predicate_object_number: u16,
    triples_map_number: u16,

    unused_identifiers: Vec<String>,
}

impl Generator {
    pub fn new() -> Self {
        Generator { logical_source_number: 0, database_number: 0, subject_map_number: 0, predicate_map_number: 0, object_map_number: 0, predicate_object_number: 0, triples_map_number: 0, unused_identifiers: Vec::new() }
    }

    fn generate_next_identifier(&mut self, identifier_prefix: &str) -> String {
        match identifier_prefix {
            LOGICAL_SOURCE => {
                self.logical_source_number += 1;
                let ident = format!("{LOGICAL_SOURCE}_{}", self.logical_source_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            DATABASE => {
                self.database_number += 1;
                let ident = format!("{DATABASE}_{}", self.database_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            PREDICATE_MAP => {
                self.predicate_map_number += 1;
                let ident = format!("{PREDICATE_MAP}_{}", self.predicate_map_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            SUBJECT_MAP => {
                self.subject_map_number += 1;
                let ident = format!("{SUBJECT_MAP}_{}", self.subject_map_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            OBJECT_MAP => {
                self.object_map_number += 1;
                let ident = format!("{OBJECT_MAP}_{}", self.object_map_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            PREDICATE_OBJECT_MAP => {
                self.predicate_object_number += 1;
                let ident = format!("{PREDICATE_OBJECT_MAP}_{}", self.predicate_object_number);
                self.unused_identifiers.push(ident.clone());
                ident
            },
            TRIPLES_MAP => {
                self.triples_map_number += 1;
                let ident = format!("{TRIPLES_MAP}_{}", self.triples_map_number);
                self.unused_identifiers.push(ident.clone());
                ident
            }
            _ => String::new(),
        }
    }

    fn find_last_unused_identifier(&mut self, identifier: String) -> String {
        if let Some(pos) = self.unused_identifiers.iter().position(|x| x.starts_with(&identifier)) {
            return self.unused_identifiers.remove(pos);
        } else {
            String::new()
        }
    }

    fn generate_database_logical_source(&mut self, source_node: &mut SourceASTNode) -> String {
        let mut source_generation = String::new();
        source_generation.push_str(format!("\t{RML_QUERY} ; \n").as_str());
        
        source_generation.push_str(format!("\t{RML_SOURCE}\t\t\t").as_str());
        source_generation.push_str(format!("map:{} ;\n", self.generate_next_identifier(DATABASE)).as_str());

        source_generation.push_str(format!("\t{RR_SQL_VERSION}\t\trr:SQL2008 .\n\n").as_str());
        source_generation.push_str(self.generate_database_declaration(source_node).as_str());
        source_generation
    }

    fn generate_database_declaration(&mut self, source_node: &mut SourceASTNode) -> String {
        let mut database_generation = String::new();
        database_generation.push_str(format!("map:{}  a\t\t\t\t\"http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#Database\" ;\n", self.find_last_unused_identifier(String::from(DATABASE))).as_str());

        database_generation.push_str(format!("\t{D2RQ_JDBC_DSN}\t\t{} ;\n", source_node.get_source_definition().to_string()).as_str());
        database_generation.push_str(format!("\t{D2RQ_JDBC_DRIVER}\t\t{} ;\n", generate_database_driver(source_node)).as_str());
        database_generation.push_str(format!("\t{D2RQ_PASSWORD}\t\t\"\" ;\n").as_str());
        database_generation.push_str(format!("\t{D2RQ_USERNAME}\t\t\"\" .\n\n").as_str());
        database_generation
    }
    
    fn predicate_map_generation(&mut self, shape_tuple_node: &mut ShapeTupleASTNode, shape_tuple_generation: &mut String) {
        shape_tuple_generation.push_str(format!("map:{}", self.generate_next_identifier(OBJECT_MAP)).as_str());
        shape_tuple_generation.push_str("  a");
        shape_tuple_generation.push_str(format!("\t\t\t\t{RR_PREDICATE_MAP} ;\n").as_str());
    
        let possible_prefix_ident = shape_tuple_node.get_prefix_ident();
        let prefix_ident;
    
        if possible_prefix_ident.is_some() {
            prefix_ident = possible_prefix_ident.unwrap();
        } else {
            prefix_ident = String::from(":");
        }
    
        shape_tuple_generation.push_str(format!("\t{RR_CONSTANT}\t\t\t{prefix_ident}{} .\n\n", shape_tuple_node.get_identifier()).as_str());
    }

    fn object_map_generation(&mut self, shape_tuple_node: &mut ShapeTupleASTNode, shape_tuple_generation: &mut String) {
        shape_tuple_generation.push_str(format!("map:{}", self.generate_next_identifier(PREDICATE_MAP)).as_str());
        shape_tuple_generation.push_str("  a");
        shape_tuple_generation.push_str(format!("\t\t\t\t{RR_OBJECT_MAP} ;\n").as_str());
    
        let possible_object_prefix= shape_tuple_node.get_object_prefix();
        let mut object_type = String::from("Literal");
        let mut template = String::new();
    
        if possible_object_prefix.is_some() {
            template = possible_object_prefix.unwrap().get_uri();
            object_type = String::from("IRI");
        }

        let object;

        match shape_tuple_node.get_object() {
            IdentOrAccess::Access(access_node) => {
                object = access_node.get_field().unwrap().get_access_field_identifier();
            },
            IdentOrAccess::Ident(_) => todo!(),
        }

        template.push_str(format!("{{{}}}", object).as_str());
    
        shape_tuple_generation.push_str(format!("\t{RR_TEMPLATE}\t\t\t{template} ;\n").as_str());
        shape_tuple_generation.push_str(format!("\t{RR_TERM_TYPE}\t\t\trr:{object_type} .\n\n").as_str());
    }
    
    fn generate_sources(&mut self, expression: Option<ExpressionASTNode>, sources_generation: &mut String) {
        let mut sources_visited = Vec::new();
    
        expression.unwrap().get_mut_accesses().iter_mut().for_each(|access| {
            match access.get_source_or_expression().unwrap() {
                SourceOrExpression::Source(mut source_node) => {
                    if !sources_visited.contains(&source_node.get_identifier()) {
                        sources_generation.push_str(self.visit_source(&mut source_node).as_str());
                        sources_visited.push(source_node.get_identifier());
                    }
                },
                SourceOrExpression::Expression(_) => (),
            }
        });
    }

    fn generate_predicate_object_map(&mut self) -> String {
        let mut predicate_object_map_generation = String::new();
        let flag = true;

        while flag {
            let object_map_ident = self.find_last_unused_identifier(String::from(OBJECT_MAP));
            let predicate_map_ident = self.find_last_unused_identifier(String::from(PREDICATE_MAP));

            if object_map_ident == String::new() || predicate_map_ident == String::new() {
                break;
            }

            let ident = self.generate_next_identifier(PREDICATE_OBJECT_MAP);
            predicate_object_map_generation.push_str(format!("map:{}", ident).as_str());
            predicate_object_map_generation.push_str(" a");
            predicate_object_map_generation.push_str(format!("\t\t\t\t{RR_PREDICATE_OBJECT_MAP} ;\n").as_str());

            predicate_object_map_generation.push_str(format!("\t{RR_OBJECT_MAP}\t\tmap:{object_map_ident} ;\n").as_str());
            predicate_object_map_generation.push_str(format!("\t{RR_PREDICATE_MAP}\t\tmap:{predicate_map_ident} .\n\n").as_str());
        }
        predicate_object_map_generation
    }

    fn generate_triples_map(&mut self) -> String {
        let mut predicate_object_map_generation = String::new();
        let ident = self.generate_next_identifier(TRIPLES_MAP);
        predicate_object_map_generation.push_str(format!("map:{}\t", ident).as_str());
        predicate_object_map_generation.push_str(" a");
        predicate_object_map_generation.push_str(format!("\t\t\t\t\t{RR_TRIPLES_MAP} ;\n").as_str());

        predicate_object_map_generation.push_str(format!("\t{RML_LOGICAL_SOURCE}\t\tmap:{} ;\n", self.find_last_unused_identifier(String::from(LOGICAL_SOURCE))).as_str());
        predicate_object_map_generation.push_str(format!("\t{RR_PREDICATE_OBJECT_MAP}\t").as_str());

        // Vector para los identificadores de los predicateObject
        let mut ident_vec = Vec::new();
        let mut flag = true;

        while flag {
            let unused_ident = self.find_last_unused_identifier(String::from(PREDICATE_OBJECT_MAP));
            if unused_ident == String::new() {
                flag = false;
            } else {
                ident_vec.push(unused_ident);
            }
        }

        let len = ident_vec.len();

        if len == 1 {
            predicate_object_map_generation.push_str(format!("map:{}", ident_vec.get(0).unwrap()).as_str());
        } else if len != 0 {
            for i in 0..len - 1 {
                predicate_object_map_generation.push_str(format!("map:{}, ", ident_vec.get(i).unwrap()).as_str());
            }
            predicate_object_map_generation.push_str(format!("map:{}", ident_vec.get(len-1).unwrap()).as_str());
        }

        predicate_object_map_generation.push_str(" ;\n");
        predicate_object_map_generation.push_str(format!("\t{RR_SUBJECT_MAP}\t\t\tmap:{} .\n\n", self.find_last_unused_identifier(String::from(SUBJECT_MAP))).as_str());

        predicate_object_map_generation
    }
}

const LOGICAL_SOURCE: &str = "l";
const DATABASE: &str = "db";
const SUBJECT_MAP: &str = "s";
const PREDICATE_MAP: &str = "r";    // Se usa r y no p para que no haya problemas en el find con po
const OBJECT_MAP: &str = "o";
const PREDICATE_OBJECT_MAP: &str = "po";
const TRIPLES_MAP: &str = "m";

const RML_LOGICAL_SOURCE: &str = "rml:LogicalSource";
const RML_REFERENCE_FORMULATION: &str = "rml:referenceFormulation";
const RML_SOURCE: &str = "rml:source";
const RML_QUERY: &str = "rml:query";

const RR_SQL_VERSION: &str = "rr:sqlVersion";
const RR_PREDICATE_MAP: &str = "rr:predicateMap";
const RR_SUBJECT_MAP: &str = "rr:SubjectMap";
const RR_OBJECT_MAP: &str = "rr:objectMap";
const RR_PREDICATE_OBJECT_MAP: &str = "rr:predicateObjectMap";
const RR_TRIPLES_MAP: &str = "rr:TriplesMap";
const RR_TEMPLATE: &str = "rr:template";
const RR_TERM_TYPE: &str = "rr:termType";
const RR_CONSTANT: &str = "rr:constant";

const D2RQ_JDBC_DSN: &str = "d2rq:jdbcDSN";
const D2RQ_JDBC_DRIVER: &str = "d2rq:jdbcDriver";
const D2RQ_PASSWORD: &str = "d2rq:password";
const D2RQ_USERNAME: &str = "d2rq:username";

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

        ast.get_mut_iterators().iter_mut().for_each(|iterator| {
            file_generation.push_str(&format!("{}", self.visit_iterator(iterator)));
            check_query_iterator(&mut file_generation, iterator);
        });

        ast.get_mut_shapes().iter_mut().for_each(|shape| {
            file_generation.push_str(&format!("{}", self.visit_shape(shape)));
        });

        file_generation.push_str(self.generate_predicate_object_map().as_str());
        file_generation.push_str(self.generate_triples_map().as_str());

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
        source_generation.push_str(format!("map:{}", self.generate_next_identifier(LOGICAL_SOURCE)).as_str());
        source_generation.push_str("  a");
        source_generation.push_str(format!("\t\t\t\t{RML_LOGICAL_SOURCE} ;\n").as_str());

        match source_node.get_type().unwrap() {
            Type::CSV => source_generation.push_str(generate_csv_logical_source(source_node).as_str()),
            Type::Database => source_generation.push_str(self.generate_database_logical_source(source_node).as_str()),
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
        String::new()
    }

    fn visit_expression(&mut self, _expression_node: &mut ExpressionASTNode) -> String {
        todo!()
    }

    fn visit_shape(&mut self, shape_node: &mut ShapeASTNode) -> String {
        let mut shape_generation = String::new();
        let mut expression = None;

        match shape_node.get_field_identifier() {
            IdentOrAccess::Access(access_node) => {
                shape_generation.push_str(format!("map:{}", self.generate_next_identifier(SUBJECT_MAP)).as_str());
                shape_generation.push_str("  a");
                shape_generation.push_str(format!("\t\t\t\t{RR_SUBJECT_MAP} ;\n").as_str());

                shape_generation.push_str(format!("\t{RR_TEMPLATE}\t\t\t").as_str());

                let field_prefix = shape_node.get_field_prefix().unwrap().get_uri();
                let field_identifier = access_node.get_field().unwrap().get_access_field_identifier();
                shape_generation.push_str(format!("\"{field_prefix}{{{field_identifier}}}\" .\n\n").as_str());

                match access_node.get_source_or_expression().unwrap() {
                    SourceOrExpression::Expression(expression_node) => expression = Some(expression_node),
                    SourceOrExpression::Source(_) => (),
                }
            },
            IdentOrAccess::Ident(_) => ()
        }

        self.generate_sources(expression, &mut shape_generation);

        shape_node.get_mut_tuples().iter_mut().for_each(|tuple| {
            shape_generation.push_str(self.visit_shape_tuple(tuple).as_str());
        });

        shape_generation
    }

    fn visit_shape_tuple(&mut self, shape_tuple_node: &mut ShapeTupleASTNode) -> String {
        let mut shape_tuple_generation = String::new();
        self.predicate_map_generation(shape_tuple_node, &mut shape_tuple_generation);
        self.object_map_generation(shape_tuple_node, &mut shape_tuple_generation);
        shape_tuple_generation
    }

    fn visit_access(&mut self, _access_node: &mut AccessASTNode) -> String {
        todo!()
    }
}

fn generate_csv_logical_source(source_node: &mut SourceASTNode) -> String {
    let mut source_generation = String::new();
    source_generation.push_str(format!("\t{RML_REFERENCE_FORMULATION}\t").as_str());
    source_generation.push_str(format!("ql:{} ; \n", source_node.get_type().unwrap().to_string()).as_str());

    source_generation.push_str(format!("\t{RML_SOURCE}\t\t\t").as_str());
    source_generation.push_str(format!("{} .\n\n", source_node.get_source_definition().to_string()).as_str());
    source_generation
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
        file_generation.insert_str(insert_pos, format!("\t\t\t\"{}\"", query).as_str());
    }
}