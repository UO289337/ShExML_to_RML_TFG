#[derive(Debug)]
pub enum ASTNode {
    File(),
    Prefix {
        identifier: String,
        uri: String,
    },
    Source {
        identifier: String,
        uri: String,
    },
}