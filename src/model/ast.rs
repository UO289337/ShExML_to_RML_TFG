//! Estructura del AST (Abstract Syntax Tree) del compilador

/// Nodo de tipo File del AST
///
/// Es la raíz del árbol AST y contiene vectores de prefijos (prefixes) y de fuentes (sources) como hijos
#[derive(Debug, PartialEq)]
pub struct FileASTNode {
    pub prefixes: Vec<PrefixASTNode>,
    pub sources: Vec<SourceASTNode>,
    pub queries: Option<Vec<QueryASTNode>>,
    pub iterators: Option<Vec<IteratorASTNode>>,
}

/// Nodo de tipo Prefix del AST
///
/// Se corresponde con los Prefix de ShEXMl; contiene un identificador y una URI
#[derive(Debug, PartialEq)]
pub struct PrefixASTNode {
    pub identifier: String,
    pub uri: String,
}

/// Nodo de tipo Source del AST
///
/// Se corresponde con los Source de SheXMl; contiene un identificador y un path o URL
#[derive(Debug, PartialEq)]
pub struct SourceASTNode {
    pub identifier: String,
    pub source_definition: String,
}

/// Nodo de tipo Query del AST
///
/// Se corresponde con los Query de SheXMl; contiene un identificador y una consulta SQL
#[derive(Debug, PartialEq)]
pub struct QueryASTNode {
    pub identifier: String,
    pub sql_query: String,
}

/// Nodo de tipo Iterator del AST
///
/// Se corresponde con los Iterator de SheXMl; contiene un identificador, como se accede (identificador, csvperrow o consulta SQL) y un vector de fields
#[derive(Debug, PartialEq)]
pub struct IteratorASTNode {
    pub identifier: String,
    pub iterator_access: String,
    pub fields: Vec<FieldASTNode>
}

/// Nodo de tipo Field del AST
///
/// Se corresponde con los Field de un ITERATOR de SheXMl; contiene un identificador para el field y otro para el campo accedido
#[derive(Debug, PartialEq)]
pub struct FieldASTNode {
    pub field_identifier: String,
    pub access_field_identifier: String,
}