//! Estructura del AST (Abstract Syntax Tree) del compilador
//! 
//! En los nodos del AST no se utilizan referencias dado que los struct son dueños de los datos

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
    Ident(Token),
    Access(AccessASTNode),
}

/// Source o Expression
///
/// Enumerador que contiene el nodo Source o Expression que puede haber en un acceso en un Shape
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourceOrExpression {
    Source(SourceASTNode),
    Expression(ExpressionASTNode),
}


impl ExpressionType {
    /// Obtiene el tipo de una expresión a partir de un token
    ///
    /// A partir de un token y de su tipo, obtiene el tipo que debe ser la expresión
    ///
    /// # Retorna
    /// El tipo de la expresión
    pub fn from(token: Token) -> ExpressionType {
        match token.get_token_type() {
            TokenType::Union => ExpressionType::UNION,
            TokenType::Join => ExpressionType::JOIN,
            _ => ExpressionType::BASIC,
        }
    }
}

/// El AST
///
/// Representa el AST; contiene los prefijos, sources, consultas, iteradores, expresiones y Shapes.
#[derive(Debug, PartialEq)]
pub struct AST {
    prefixes: Vec<PrefixASTNode>,
    sources: Vec<SourceASTNode>,
    queries: Option<Vec<QueryASTNode>>,
    iterators: Vec<IteratorASTNode>,
    expressions: Vec<ExpressionASTNode>,
    shapes: Vec<ShapeASTNode>,
}

impl AST {
    /// Crea un nuevo AST
    /// 
    /// # Argumentos
    /// * `prefixes` - El vector con los nodos Prefix del AST
    /// * `sources` - El vector con los nodos Source del AST
    /// * `queries` - Un Option con el vector con los nodos Query del AST
    /// * `iterators` - Un vector con los nodos Iterator del AST
    /// * `expressions` - Un vector con los nodos Expression del AST
    /// * `shapes` - Un vector con los nodos Shape del AST
    /// 
    /// # Retorna
    /// Un AST creado con todos sus componentes
    pub fn new(prefixes: Vec<PrefixASTNode>,
    sources: Vec<SourceASTNode>,
    queries: Option<Vec<QueryASTNode>>,
    iterators: Vec<IteratorASTNode>,
    expressions: Vec<ExpressionASTNode>,
    shapes: Vec<ShapeASTNode>) -> Self {
        AST { prefixes, sources, queries, iterators, expressions, shapes }
    }
    /// Devuelve el vector de nodos Prefix del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Prefix del AST
    pub fn get_prefixes(&self) -> Vec<PrefixASTNode> {
        self.prefixes.clone()
    }

    /// Devuelve el vector de nodos Source del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Source del AST
    pub fn get_sources(&self) -> Vec<SourceASTNode> {
        self.sources.clone()
    }

    /// Devuelve el vector de nodos Query del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Query del AST
    pub fn get_queries(&self) -> Option<Vec<QueryASTNode>> {
        self.queries.clone()
    }

    /// Devuelve el vector de nodos Iterator del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Iterator del AST
    pub fn get_iterators(&self) -> Vec<IteratorASTNode> {
        self.iterators.clone()
    }

    /// Devuelve el vector de nodos Expression del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Expression del AST
    pub fn get_expressions(&self) -> Vec<ExpressionASTNode> {
        self.expressions.clone()
    }

    /// Devuelve el vector de nodos Shape del AST
    /// 
    /// # Argumentos
    /// * `self` - El propio AST
    /// 
    /// # Retorna
    /// El vector de nodos Shape del AST
    pub fn get_shapes(&self) -> Vec<ShapeASTNode> {
        self.shapes.clone()
    }
}

/// Nodo de tipo Prefix del AST
///
/// Se corresponde con los Prefix de ShEXMl; contiene un identificador y una URI
#[derive(Debug, PartialEq, Clone)]
pub struct PrefixASTNode {
    identifier: Option<Token>,    // El prefijo por defecto (:) no lleva identificador
    uri: Token,
}

impl PrefixASTNode {
    /// Crea un nodo Prefix del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Prefix
    /// * `uri` - La URI del nodo Prefix
    /// 
    /// # Retorna
    /// Un nodo Prefix del AST
    pub fn new(identifier: Option<Token>,
    uri: Token,) -> Self {
        PrefixASTNode { identifier, uri }
    }

    /// Devuelve el Option con el token del identificador del Prefix
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Prefix
    /// 
    /// # Retorna
    /// Un Option con el token del identificador del Prefix
    pub fn get_identifier(&self) -> Option<Token> {
        self.identifier.clone()
    }

    /// Devuelve el token de la URI del Prefix
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Prefix
    /// 
    /// # Retorna
    /// El token de la URI del Prefix
    pub fn get_uri(&self) -> Token {
        self.uri.clone()
    }
}

/// Nodo de tipo Source del AST
///
/// Se corresponde con los Source de SheXMl; contiene un identificador y un path o URL
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct SourceASTNode {
    identifier: Token,
    source_definition: Token,
}

impl SourceASTNode {
    /// Crea un nodo Source del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Source
    /// * `source_definition` - La definición del nodo Source
    /// 
    /// # Retorna
    /// Un nodo Source del AST
    pub fn new(identifier: Token,
    source_definition: Token,) -> Self {
        SourceASTNode { identifier, source_definition }
    }

    /// Devuelve el token del identificador del Source
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Source
    /// 
    /// # Retorna
    /// El token del identificador del Source
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el token de la definición del Source
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Source
    /// 
    /// # Retorna
    /// El token de la definición del Source
    pub fn get_source_definition(&self) -> Token {
        self.source_definition.clone()
    }
}

/// Nodo de tipo Query del AST
///
/// Se corresponde con los Query de SheXMl; contiene un identificador y una consulta SQL
#[derive(Debug, PartialEq,Clone, Eq, Hash)]
pub struct QueryASTNode {
    identifier: Token,
    sql_query: Token,
}

impl QueryASTNode {
    /// Crea un nodo Query del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Query
    /// * `sql_query` - La consulta SQL del nodo Query
    /// 
    /// # Retorna
    /// Un nodo Query del AST
    pub fn new(identifier: Token,
    sql_query: Token) -> Self {
        QueryASTNode { identifier, sql_query }
    }

    /// Devuelve el token del identificador de la Query
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Query
    /// 
    /// # Retorna
    /// El token del identificador de la Query
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el token de la definición de la Query
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Query
    /// 
    /// # Retorna
    /// El token de la consulta SQL de la Query
    pub fn get_sql_query(&self) -> Token {
        self.sql_query.clone()
    }
}

/// Nodo de tipo Iterator del AST
///
/// Se corresponde con los Iterator de SheXMl; contiene un identificador, como se accede (identificador, csvperrow o consulta SQL) y un vector de fields
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct IteratorASTNode {
    identifier: Token,
    iterator_access: Token,
    fields: Vec<FieldASTNode>,

    // Fase identificación
    query: Option<QueryASTNode>,
}

impl IteratorASTNode {
    /// Crea un nodo Iterator del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Iterator
    /// * `iterator_access` - El acceso al Iterator
    /// * `fields` - El vector con los campos del Iterator
    /// 
    /// # Retorna
    /// Un nodo Iterator del AST
    pub fn new(identifier: Token,
    iterator_access: Token,
    fields: Vec<FieldASTNode>) -> Self {
        IteratorASTNode { identifier, iterator_access, fields, query: None }
    }

    /// Devuelve el token del identificador del Iterator
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Iterator
    /// 
    /// # Retorna
    /// El token del identificador del Iterator
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el token del acceso al Iterator
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Iterator
    /// 
    /// # Retorna
    /// El token del acceso al Iterator
    pub fn get_iterator_access(&self) -> Token {
        self.iterator_access.clone()
    }

    /// Devuelve el vector de nodos Field del Iterator
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Iterator
    /// 
    /// # Retorna
    /// El vector de nodos Field del Iterator
    pub fn get_fields(&self) -> Vec<FieldASTNode> {
        self.fields.clone()
    }

    /// Devuelve el Option de la consulta SQL del Iterator
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Iterator
    /// 
    /// # Retorna
    /// El Option con la consulta SQL del Iterator
    pub fn get_query(&self) -> Option<QueryASTNode> {
        self.query.clone()
    }
}

/// Nodo de tipo Field del AST
///
/// Se corresponde con los Field de un ITERATOR de SheXMl; contiene un identificador para el field y otro para el campo accedido
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct FieldASTNode {
    identifier: Token,
    access_field_identifier: Token,
}

impl FieldASTNode {
    /// Crea un nodo Field del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Field
    /// * `access_field_identifier` - El identificador del acceso al campo del nodo Fied
    /// 
    /// # Retorna
    /// Un nodo Field del AST
    pub fn new(identifier: Token,
    access_field_identifier: Token) -> Self {
        FieldASTNode { identifier, access_field_identifier }
    }

    /// Devuelve el token del identificador del Field
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Field
    /// 
    /// # Retorna
    /// El token con el identificador del Field
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el token del identificador del acceso del Field
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Field
    /// 
    /// # Retorna
    /// El token con el identificador del acceso del Field
    pub fn get_access_field_identifier(&self) -> Token {
        self.access_field_identifier.clone()
    }
}

/// Nodo de tipo Expression del AST
///
/// Se corresponde con los Expression de SheXMl; contiene un identificador, el tipo de la expresión y los accesos a iteradores y/o campos que se realizan
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ExpressionASTNode {
    identifier: Token,
    expression_type: ExpressionType,
    accesses: Vec<AccessASTNode>,
}

impl ExpressionASTNode {
    /// Crea un nodo Expression del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Expression
    /// * `expression_type` - El tipo de la Expression
    /// * `accesses` - El vector con los accesos realizados en la Expression
    /// 
    /// # Retorna
    /// Un nodo Expression del AST
    pub fn new(identifier: Token,
    expression_type: ExpressionType,
    accesses: Vec<AccessASTNode>,) -> Self {
        ExpressionASTNode { identifier, expression_type, accesses }
    }
    
    /// Devuelve el token del identificador de la Expression
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Expression
    /// 
    /// # Retorna
    /// El token con el identificador de la Expression
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el tipo de la Expression
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Expression
    /// 
    /// # Retorna
    /// El tipo de la Expression
    pub fn get_expression_type(&self) -> ExpressionType {
        self.expression_type.clone()
    }

    /// Devuelve el vector con los accesos que se realizan en la Expression
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Expression
    /// 
    /// # Retorna
    /// El vector con los accesos que se realizan en la Expression
    pub fn get_accesses(&self) -> Vec<AccessASTNode> {
        self.accesses.clone()
    }
}

/// Nodo de tipo Access del AST
///
/// Se corresponde con los accesos a iteradores y/o campos de estos en las expresiones
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct AccessASTNode {
    identifier: Token,
    first_access: Token,
    second_access: Option<Token>,

    // Fase identificación
    source_or_expression: Option<SourceOrExpression>,
    iterator: Option<IteratorASTNode>,
    field: Option<FieldASTNode>,
}

impl AccessASTNode {
    /// Crea un nodo Access del AST
    /// 
    /// # Argumentos
    /// * `identifier` - El identificador del nodo Access
    /// * `first_access` - El primer acceso del nodo Access
    /// * `second_access` - El segundo acceso del nodo Access
    /// 
    /// # Retorna
    /// Un nodo Access del AST
    pub fn new(identifier: Token,
    first_access: Token,
    second_access: Option<Token>) -> Self {
        AccessASTNode { identifier, first_access, second_access, source_or_expression: None, iterator: None, field: None }
    }

    /// Devuelve el token del identificador del acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El token del identificador del acceso
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el token del primer acceso del acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El token del primer acceso del acceso
    pub fn get_first_access(&self) -> Token {
        self.first_access.clone()
    }

    /// Devuelve el Option del token del segundo acceso del acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El Option del token del  segundo acceso del acceso
    pub fn get_second_access(&self) -> Option<Token> {
        self.second_access.clone()
    }

    /// Devuelve el nodo Source o Expression del acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El nodo Source o Expression del acceso
    pub fn get_souce_or_expression(&self) -> Option<SourceOrExpression> {
        self.source_or_expression.clone()
    }

    /// Devuelve el nodo del Iterator accedido en el acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El nodo del Iterator accedido en el acceso
    pub fn get_iterator(&self) -> Option<IteratorASTNode> {
        self.iterator.clone()
    }

    /// Devuelve el Option del nodo del Field accedido en el acceso
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Access
    /// 
    /// # Retorna
    /// El Option del nodo del Field accedido en el acceso
    pub fn get_field(&self) -> Option<FieldASTNode> {
        self.field.clone()
    }
}

/// Nodo de tipo Shape del AST
///
/// Se corresponde con las Shape de ShExML; contiene el identificador de los Prefix de la Shape y del campo de acceso a esta, el identificador de
/// la propia Shape y del campo de acceso, las tuplas de la Shape y los nodos Prefix asociados con el nodo
#[derive(Debug, PartialEq, Clone)]
pub struct ShapeASTNode {
    prefix_ident: Option<Token>,
    identifier: Token,
    field_prefix_ident: Option<Token>,
    field_identifier: IdentOrAccess,
    tuples: Vec<ShapeTupleASTNode>,

    // Fase Identificación
    prefix: Option<PrefixASTNode>,
    field_prefix: Option<PrefixASTNode>,
}

impl ShapeASTNode {
    /// Crea un nodo Shape del AST
    /// 
    /// # Argumentos
    /// * `prefix_ident` - El identificador del prefix del nodo Shape
    /// * `identifier` - El identificador del nodo Shape
    /// * `field_prefix_ident` - El identificador el prefix del campo del nodo Shape
    /// * `field_identifier` - El identificador del campo del nodo Shape
    /// * `tuples` - El vector con las tuplas de la Shape
    /// * `prefix` - El Option con el nodo Prefix asociado con la Shape
    /// * `field_prefix` - El Option con el nodo Prefix asociado con el campo de la Shape
    /// 
    /// # Retorna
    /// Un nodo Shape del AST
    pub fn new(prefix_ident: Option<Token>,
    identifier: Token,
    field_prefix_ident: Option<Token>,
    field_identifier: IdentOrAccess,
    tuples: Vec<ShapeTupleASTNode>) -> Self {
        ShapeASTNode { prefix_ident, identifier, field_prefix_ident, field_identifier, tuples, prefix: None, field_prefix: None }
    }

    /// Devuelve el Option del token del identificador del Prefix de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El Option del token del identificador del Prefix de la Shape
    pub fn get_prefix_ident(&self) -> Option<Token> {
        self.prefix_ident.clone()
    }

    /// Devuelve el token del identificador de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El token del identificador de la Shape
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el Option del token del identificador del Prefix del campo de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El Option del token del identificador del Prefix del campo de la Shape
    pub fn get_field_prefix_ident(&self) -> Option<Token> {
        self.field_prefix_ident.clone()
    }

    /// Devuelve el identificador o acceso del campo de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El identificador o acceso del campo de la Shape
    pub fn get_field_identifier(&self) -> IdentOrAccess {
        self.field_identifier.clone()
    }

    /// Devuelve las tuplas de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El vector con las tuplas de la Shape
    pub fn get_tuples(&self) -> Vec<ShapeTupleASTNode>{
        self.tuples.clone()
    }

    /// Devuelve el nodo Prefix asociado con la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El nodo Prefix asociado con la Shape
    pub fn get_prefix(&self) -> Option<PrefixASTNode> {
        self.prefix.clone()
    }

    /// Devuelve el nodo Prefix del campo de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo Shape
    /// 
    /// # Retorna
    /// El nodo Prefix del campo de la Shape
    pub fn get_field_prefix(&self) -> Option<PrefixASTNode> {
        self.field_prefix.clone()
    }
}

/// Nodo de tipo ShapeTuples del AST
///
/// Se corresponde con las tuplas de los Shape de ShExML; contiene el identificador de los Prefix de la tupla y del objeto de esta, el identificador de la
/// propia tupla y del objeto y los nodos Prefix asociados con la tupla y el objeto
#[derive(Debug, PartialEq, Clone)]
pub struct ShapeTupleASTNode {
    prefix_ident: Option<Token>,
    identifier: Token,
    object_prefix_ident: Option<Token>,
    object: IdentOrAccess,

    // Fase Identificación
    prefix: Option<PrefixASTNode>,
    object_prefix: Option<PrefixASTNode>,
}

impl ShapeTupleASTNode {
    /// Crea un nodo ShapeTuple del AST
    /// 
    /// # Argumentos
    /// * `prefix_ident` - El identificador del nodo ShapeTuple
    /// * `identifier` - El identificador de la tupla
    /// * `object_prefix_ident` - El Option con el identificador del objeto del nodo Shape
    /// * `object` - El identificador o acceso del objeto de la Shape
    /// * `prefix` - El Option con el nodo Prefix asociado con la tupla de la Shape
    /// * `object_prefix` - El Option con el nodo Prefix asociado con el objeto de la Shape
    /// 
    /// # Retorna
    /// Un nodo Access del AST
    pub fn new(prefix_ident: Option<Token>,
    identifier: Token,
    object_prefix_ident: Option<Token>,
    object: IdentOrAccess) -> Self {
        ShapeTupleASTNode { prefix_ident, identifier, object_prefix_ident, object, prefix: None, object_prefix: None }
    }

    /// Devuelve el Option del token del identificador del Prefix de la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El Option del token del identificador del Prefix de la tupla de la Shape
    pub fn get_prefix_ident(&self) -> Option<Token> {
        self.prefix_ident.clone()
    }

    /// Devuelve el token del identificador de la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El token del identificador de la tupla de la Shape
    pub fn get_identifier(&self) -> Token {
        self.identifier.clone()
    }

    /// Devuelve el Option del token del identificador del Prefix del objeto de la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El Option del token del identificador del Prefix del objeto de la tupla de la Shape
    pub fn get_object_prefix_ident(&self) -> Option<Token> {
        self.object_prefix_ident.clone()
    }

    /// Devuelve el identificador o acceso del objeto de la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El identificador o acceso del objeto de la tupla de la Shape
    pub fn get_object(&self) -> IdentOrAccess {
        self.object.clone()
    }

    /// Devuelve el Option del nodo Prefix asociado con la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El Option del nodo Prefix asociado con la tupla de la Shape
    pub fn get_prefix(&self) -> Option<PrefixASTNode> {
        self.prefix.clone()
    }

    /// Devuelve el Option del nodo Prefix asociado con el objeto de la tupla de la Shape
    /// 
    /// # Argumentos
    /// * `self` - El propio nodo ShapeTuple
    /// 
    /// # Retorna
    /// El Option del nodo Prefix asociado con el objeto de la tupla de la Shape
    pub fn get_object_prefix(&self) -> Option<PrefixASTNode> {
        self.object_prefix.clone()
    }
}
