use super::ast::ASTNode;
use std::fs;
use std::io::prelude::*;

const RML_FILE_NAME: &str = "generated.rml";

pub fn rml_generator(node: ASTNode) {
    if let ASTNode::File { prefixes, sources } = &node {      
        let mut rml_file = fs::File::create(RML_FILE_NAME).unwrap();

        for prefix in prefixes {
            if let ASTNode::Prefix { identifier, uri } = &prefix {
                writeln!(rml_file, "@prefix {}:   <{}> .", identifier, uri);
            }
        }

        writeln!(rml_file, "@prefix schema:   <http://schema.org/> .");
        writeln!(rml_file, "@prefix rml:      <http://semweb.mmlab.be/ns/rml#> .");
        writeln!(rml_file, "@prefix rr:       <http://www.w3.org/ns/r2rml#> .");
        writeln!(rml_file, "@prefix d2rq:     <http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#> .");
        writeln!(rml_file, "@prefix ql:       <http://semweb.mmlab.be/ns/ql#> .");
        writeln!(rml_file, "@prefix map:      <http://mapping.example.com/> .");

        for source in sources {
            println!("{:?}", source);
        }
    }
}