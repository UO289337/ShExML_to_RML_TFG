//! Estructura del AST (Abstract Syntax Tree) del compilador

use crate::model::lexer::token::{Token, TokenType};

/// Tipos de expresiones
///
/// Enumerador que contiene todos los tipos de expresiones que puede haber en el compilador
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpressionType {
    BASIC,
    UNION,
    JOIN,
}

/// Ident o Access
///
/// Enumerador que contiene el identificador o el nodo de acceso a un field de un Shape
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentOrAccess {
    Ident(String),
    Access(AccessASTNode),
}

impl ExpressionType {
    /// Obtiene el tipo de una expresión a partir de un token
    ///
    /// A partir de un token y de su tipo, obtiene el tipo que debe ser la expresión
    ///
    /// # Retorna
    /// El tipo de la expresión
    pub fn from(token: Token) -> ExpressionType {
        match token.token_type {
            TokenType::Union => ExpressionType::UNION,
            TokenType::Join => ExpressionType::JOIN,
            _ => ExpressionType::BASIC,
        }
    }
}

/// Nodo de tipo File del AST
///
/// Es la raíz del árbol AST y contiene vectores de prefijos (prefixes) y de fuentes (sources) como hijos
#[derive(Debug, PartialEq)]
pub struct FileASTNode {
    pub prefixes: Option<Vec<PrefixASTNode>>,
    pub sources: Vec<SourceASTNode>,
    pub queries: Option<Vec<QueryASTNode>>,
    pub iterators: Vec<IteratorASTNode>,
    pub expressions: Option<Vec<ExpressionASTNode>>,
    pub shapes: Vec<ShapeASTNode>,
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
    pub fields: Vec<FieldASTNode>,
}

/// Nodo de tipo Field del AST
///
/// Se corresponde con los Field de un ITERATOR de SheXMl; contiene un identificador para el field y otro para el campo accedido
#[derive(Debug, PartialEq)]
pub struct FieldASTNode {
    pub field_identifier: String,
    pub access_field_identifier: String,
}

/// Nodo de tipo Expression del AST
///
/// Se corresponde con los Expression de SheXMl; contiene un identificador, el tipo de la expresión y los accesos a iteradores y/o campos que se realizan
#[derive(Debug, PartialEq)]
pub struct ExpressionASTNode {
    pub identifier: String,
    pub expression_type: ExpressionType,
    pub accesses: Vec<AccessASTNode>,
}

/// Nodo de tipo Access del AST
///
/// Se corresponde con los accesos a iteradores y/o campos de estos en las expresiones
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct AccessASTNode {
    pub identifier: String,
    pub iterator_accessed: String,
    pub field_accessed: Option<String>,
}

/// Nodo de tipo Shape del AST
///
/// Se corresponde con los Shape de ShExML
#[derive(Debug, PartialEq)]
pub struct ShapeASTNode {
    pub prefix: String,
    pub identifier: String,
    pub field_prefix: String,
    pub field_identifier: IdentOrAccess,
    pub tuples: Vec<ShapeTupleASTNode>,
}

/// Nodo de tipo ShapeTuples del AST
///
/// Se corresponde con las tuplas de los Shape de ShExML
#[derive(Debug, PartialEq)]
pub struct ShapeTupleASTNode {
    pub prefix: String,
    pub identifier: String,
    pub object_prefix: Option<String>,
    pub object: IdentOrAccess,
}
