#[derive(Debug)]
pub enum ASTNode {
    File {
        prefixes: Vec<ASTNode>,
        sources: Vec<ASTNode>,
    },
    Prefix {
        identifier: String,
        uri: String,
    },
    Source {
        identifier: String,
        uri: String,
    },
}