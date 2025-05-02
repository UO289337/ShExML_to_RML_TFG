//! Estructura del AST (Abstract Syntax Tree) del compilador

/// Nodo de tipo File del AST
/// 
/// Es la raíz del árbol AST y contiene vectores de prefijos (prefixes) y de fuentes (sources) como hijos
#[derive(Debug)]
pub struct FileASTNode {
    pub prefixes: Vec<PrefixASTNode>,
    pub sources: Vec<SourceASTNode>,
}

/// Nodo de tipo Prefix del AST
/// 
/// Se corresponde con los Prefix de ShEXMl; contiene un identificador y una URI
#[derive(Debug)]
pub struct PrefixASTNode {
    pub identifier: String,
    pub uri: String,
}

/// Nodo de tipo Source del AST
/// 
/// Se corresponde con los Source de SheXMl; contiene un identificador y una URI
#[derive(Debug)]
pub struct SourceASTNode {
    pub identifier: String,
    pub uri: String,
}