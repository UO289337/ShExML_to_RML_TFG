#[derive(Debug)]
pub struct FileASTNode {
    pub prefixes: Vec<PrefixASTNode>,
    pub sources: Vec<SourceASTNode>,
}

#[derive(Debug)]
pub struct PrefixASTNode {
    pub identifier: String,
    pub uri: String,
}


#[derive(Debug)]
pub struct SourceASTNode {
    pub identifier: String,
    pub uri: String,
}