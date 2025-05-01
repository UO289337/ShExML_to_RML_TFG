// Utilizar Box<T> siempre que se quiera utilizar recursividad, por ejemplo: Add(Box<Expr>, Box<Expr>)

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