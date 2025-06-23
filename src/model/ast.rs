//! Estructura del AST (Abstract Syntax Tree) del compilador
//!
//! En los nodos del AST no se utilizan referencias dado que los struct son dueños de los datos

use crate::model::lexer::token::{Token, TokenType};
use nodes::*;

/// Tipos de SourceDefinition
///
/// Enumerador que contiene todos los tipos de SourceDefiniton que hay: Uri, FilePath, Path, JDBC
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum SourceDefinition {
    URI(String),
    Path(String),
    JdbcURL(String),
}

/// Tipos de expresiones
///
/// Enumerador que contiene todos los tipos de expresiones que puede haber en el compilador
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpressionType {
    BASIC,
    UNION,
    JOIN,
}

impl ExpressionType {
    /// Obtiene el tipo de una expresión a partir de un String
    ///
    /// A partir de un String y de su tipo, obtiene el tipo que debe ser la expresión
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

/// Ident o Access
///
/// Enumerador que contiene el identificador o el nodo de acceso a un field de un Shape
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentOrAccess {
    Ident(String),
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

/// Acceso del iterador
///
/// Enumerador que contiene la cadena de acceso del iterador, que puede ser un identificador, una consulta SQL o un csvperrow
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IteratorAccess {
    Ident(String),
    SqlQuery(String),
    CsvPerRow(String),
}

/// La posición de un nodo
///
/// Contiene el número de línea de la entrada donde se encuentra el nodo
// Se ha utilizado un struct y no el campo directamente en los struct para poder añadir posteriormente más indicadores de posición como la columna
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    num_line: u16,
}

impl Position {
    /// Crea una posición con el número de línea
    ///
    /// # Argumentos
    /// * `num_line` - El número de línea
    ///
    /// # Retorna
    /// Una posición
    pub fn new(num_line: u16) -> Self {
        Position { num_line }
    }

    /// Devuelve el número de línea de la posición
    ///
    /// # Argumentos
    /// * `self` - La propia posición
    ///
    /// # Retorna
    /// El número de línea de la posición
    pub fn get_num_line(&self) -> u16 {
        self.num_line
    }
}

pub mod nodes {
    use super::*;

    /// Trait para el manejo de la posición de los nodos
    pub trait ManagePosition {
        fn get_position(&self) -> Position;
    }

    /// Trait para la fase de identificación
    pub trait Identifiable {
        fn get_identifier(&self) -> String;
    }

    /// Trait para el manejo de prefijos
    pub trait ManagePrefix {
        fn set_prefix(&mut self, prefix: Option<PrefixASTNode>);
        fn set_object_prefix(&mut self, prefix: Option<PrefixASTNode>);
    }

    /// Nodo de tipo Prefix del AST
    ///
    /// Se corresponde con los Prefix de ShEXMl; contiene un identificador y una URI
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct PrefixASTNode {
        identifier: Option<String>, // El prefijo por defecto (:) no lleva identificador
        uri: String,
        position: Position,
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
        pub fn new(identifier: Option<Token>, uri: Token, position: Position) -> Self {
            let ident = get_lexeme_of_option(identifier);
            PrefixASTNode {
                identifier: ident,
                uri: uri.get_lexeme(),
                position,
            }
        }

        /// Devuelve el String de la URI del Prefix
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Prefix
        ///
        /// # Retorna
        /// El String de la URI del Prefix
        pub fn get_uri(&self) -> String {
            self.uri.clone()
        }
    }

    impl Identifiable for PrefixASTNode {
        /// Devuelve el String del identificador del Prefix
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Prefix
        ///
        /// # Retorna
        /// El String del identificador del Prefix
        fn get_identifier(&self) -> String {
            if self.identifier.is_some() {
                return self.identifier.as_ref().unwrap().to_string()
            }
            String::new()
        }
    }

    impl ManagePosition for PrefixASTNode {
        /// Devuelve la posición del nodo Prefix
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Prefix del AST
        /// 
        /// # Retorna
        /// La posición del nodo Prefix con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    /// Nodo de tipo Source del AST
    ///
    /// Se corresponde con los Source de SheXMl; contiene un identificador y un path o URL
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct SourceASTNode {
        identifier: String,
        source_definition: SourceDefinition,
        position: Position,
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
        pub fn new(
            identifier: Token,
            source_definition: SourceDefinition,
            position: Position,
        ) -> Self {
            SourceASTNode {
                identifier: identifier.get_lexeme(),
                source_definition,
                position,
            }
        }

        /// Devuelve el String de la definición del Source
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Source
        ///
        /// # Retorna
        /// El String de la definición del Source
        pub fn get_source_definition(&self) -> SourceDefinition {
            self.source_definition.clone()
        }
    }

    impl ManagePosition for SourceASTNode {
        /// Devuelve la posición del nodo Source
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Source del AST
        /// 
        /// # Retorna
        /// La posición del nodo Source con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    impl Identifiable for SourceASTNode {
        /// Devuelve el String del identificador del Source
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Source
        ///
        /// # Retorna
        /// El String del identificador del Source
        fn get_identifier(&self) -> String {
            self.identifier.clone()
        }
    }

    /// Nodo de tipo Query del AST
    ///
    /// Se corresponde con los Query de SheXMl; contiene un identificador y una consulta SQL
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct QueryASTNode {
        identifier: String,
        sql_query: String,
        position: Position,
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
        pub fn new(identifier: Token, sql_query: Token, position: Position) -> Self {
            QueryASTNode {
                identifier: identifier.get_lexeme(),
                sql_query: sql_query.get_lexeme(),
                position,
            }
        }

        /// Devuelve el String de la definición de la Query
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Query
        ///
        /// # Retorna
        /// El String de la consulta SQL de la Query
        pub fn get_sql_query(&self) -> String {
            self.sql_query.clone()
        }
    }

    impl ManagePosition for QueryASTNode {
        /// Devuelve la posición del nodo Query
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Query del AST
        /// 
        /// # Retorna
        /// La posición del nodo Query con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    impl Identifiable for QueryASTNode {
        /// Devuelve el String del identificador del Source
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Source
        ///
        /// # Retorna
        /// El String del identificador del Source
        fn get_identifier(&self) -> String {
            self.identifier.clone()
        }
    }

    /// Nodo de tipo Iterator del AST
    ///
    /// Se corresponde con los Iterator de SheXMl; contiene un identificador, como se accede (identificador, csvperrow o consulta SQL) y un vector de fields
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct IteratorASTNode {
        identifier: String,
        iterator_access: IteratorAccess,
        fields: Vec<FieldASTNode>,
        position: Position,

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
        pub fn new(
            identifier: Token,
            iterator_access: IteratorAccess,
            fields: Vec<FieldASTNode>,
            position: Position,
        ) -> Self {
            IteratorASTNode {
                identifier: identifier.get_lexeme(),
                iterator_access,
                fields,
                query: None,
                position,
            }
        }

        /// Devuelve el acceso al Iterator
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Iterator
        ///
        /// # Retorna
        /// El acceso al Iterator
        pub fn get_iterator_access(&self) -> IteratorAccess {
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

        /// Devuelve el vector de nodos Field mutable del Iterator
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Iterator
        ///
        /// # Retorna
        /// El vector de nodos Field mutable del Iterator
        pub fn get_mut_fields(&mut self) -> &mut Vec<FieldASTNode> {
            &mut self.fields
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

        /// Devuelve el Option mutable de la consulta SQL table del Iterator
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Iterator
        ///
        /// # Retorna
        /// El Option mutable con la consulta SQL del Iterator
        pub fn get_mut_query(&mut self) -> &mut Option<QueryASTNode> {
            &mut self.query
        }

        /// Asocia un nodo Query con el Iterator
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Iterator
        /// * `query` - El Option que contiene el nodo Query del AST que se quiere asociar al iterador
        pub fn set_query(&mut self, query: Option<QueryASTNode>) {
            self.query = query.clone();
        }
    }

    impl ManagePosition for IteratorASTNode {
        /// Devuelve la posición del nodo Iterator
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Iterator del AST
        /// 
        /// # Retorna
        /// La posición del nodo Iterator con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    impl Identifiable for IteratorASTNode {
        /// Devuelve el String del identificador del Source
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Source
        ///
        /// # Retorna
        /// El String del identificador del Source
        fn get_identifier(&self) -> String {
            self.identifier.clone()
        }
    }

    /// Nodo de tipo Field del AST
    ///
    /// Se corresponde con los Field de un ITERATOR de SheXMl; contiene un identificador para el field y otro para el campo accedido
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct FieldASTNode {
        identifier: String,
        access_field_identifier: String,
        position: Position,
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
        pub fn new(identifier: Token, access_field_identifier: Token, position: Position) -> Self {
            FieldASTNode {
                identifier: identifier.get_lexeme(),
                access_field_identifier: access_field_identifier.get_lexeme(),
                position,
            }
        }

        /// Devuelve el String del identificador del acceso del Field
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Field
        ///
        /// # Retorna
        /// El String con el identificador del acceso del Field
        pub fn get_access_field_identifier(&self) -> String {
            self.access_field_identifier.clone()
        }
    }

    impl Identifiable for FieldASTNode {
        /// Devuelve el String del identificador del Field
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Field
        ///
        /// # Retorna
        /// El String con el identificador del Field
        fn get_identifier(&self) -> String {
            self.identifier.clone()
        }
    }

    impl ManagePosition for FieldASTNode {
        /// Devuelve la posición del nodo Field
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Field del AST
        /// 
        /// # Retorna
        /// La posición del nodo Field con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    /// Nodo de tipo Expression del AST
    ///
    /// Se corresponde con los Expression de SheXMl; contiene un identificador, el tipo de la expresión y los accesos a iteradores y/o campos que se realizan
    #[derive(Debug, PartialEq, Eq, Clone, Hash)]
    pub struct ExpressionASTNode {
        identifier: String,
        expression_type: ExpressionType,
        accesses: Vec<AccessASTNode>,
        position: Position,
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
        pub fn new(
            identifier: Token,
            expression_type: ExpressionType,
            accesses: Vec<AccessASTNode>,
            position: Position,
        ) -> Self {
            ExpressionASTNode {
                identifier: identifier.get_lexeme(),
                expression_type,
                accesses,
                position,
            }
        }

        /// Devuelve el String del identificador de la Expression
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Expression
        ///
        /// # Retorna
        /// El String con el identificador de la Expression
        pub fn get_identifier(&self) -> String {
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

        /// Devuelve el vector mutable con los accesos que se realizan en la Expression
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Expression
        ///
        /// # Retorna
        /// El vector mutable con los accesos que se realizan en la Expression
        pub fn get_mut_accesses(&mut self) -> &mut Vec<AccessASTNode> {
            &mut self.accesses
        }
    }

    impl ManagePosition for ExpressionASTNode {
        /// Devuelve la posición del nodo Expression
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Expression del AST
        /// 
        /// # Retorna
        /// La posición del nodo Expression con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    /// Nodo de tipo Access del AST
    ///
    /// Se corresponde con los accesos a iteradores y/o campos de estos en las expresiones
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct AccessASTNode {
        identifier: String,
        first_access: String,
        second_access: Option<String>,
        position: Position,

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
        pub fn new(
            identifier: Token,
            first_access: Token,
            second_access: Option<Token>,
            position: Position,
        ) -> Self {
            let access = get_lexeme_of_option(second_access);
            AccessASTNode {
                identifier: identifier.get_lexeme(),
                first_access: first_access.get_lexeme(),
                second_access: access,
                source_or_expression: None,
                iterator: None,
                field: None,
                position,
            }
        }

        /// Devuelve el String del identificador del acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        ///
        /// # Retorna
        /// El String del identificador del acceso
        pub fn get_identifier(&self) -> String {
            self.identifier.clone()
        }

        /// Devuelve el String del primer acceso del acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        ///
        /// # Retorna
        /// El String del primer acceso del acceso
        pub fn get_first_access(&self) -> String {
            self.first_access.clone()
        }

        /// Devuelve el Option del String del segundo acceso del acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        ///
        /// # Retorna
        /// El Option del String del  segundo acceso del acceso
        pub fn get_second_access(&self) -> Option<String> {
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

        /// Asocia un nodo Source o un nodo Expressio con el acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        /// * `query` - El Option que contiene el el SourceOrExpression
        pub fn set_source_or_expression(
            &mut self,
            source_or_expression: Option<SourceOrExpression>,
        ) {
            self.source_or_expression = source_or_expression;
        }

        /// Asocia un nodo Iterator con la tupla de acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        /// * `query` - El Option que contiene el nodo Iterator del AST que se quiere asociar con el Access
        pub fn set_iterator(&mut self, iterator: Option<IteratorASTNode>) {
            self.iterator = iterator;
        }

        /// Asocia un nodo Field con la tupla de acceso
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Access
        /// * `query` - El Option que contiene el nodo Field del AST que se quiere asociar con el Access
        pub fn set_field(&mut self, field: Option<FieldASTNode>) {
            self.field = field;
        }
    }

    impl ManagePosition for AccessASTNode {
        /// Devuelve la posición del nodo Access
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Access del AST
        /// 
        /// # Retorna
        /// La posición del nodo Access con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    /// Nodo de tipo Shape del AST
    ///
    /// Se corresponde con las Shape de ShExML; contiene el identificador de los Prefix de la Shape y del campo de acceso a esta, el identificador de
    /// la propia Shape y del campo de acceso, las tuplas de la Shape y los nodos Prefix asociados con el nodo
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct ShapeASTNode {
        prefix_ident: Option<String>,
        identifier: String,
        field_prefix_ident: Option<String>,
        field_identifier: IdentOrAccess,
        tuples: Vec<ShapeTupleASTNode>,
        position: Position,

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
        pub fn new(
            prefix_ident: Option<Token>,
            identifier: Token,
            field_prefix_ident: Option<Token>,
            field_identifier: IdentOrAccess,
            tuples: Vec<ShapeTupleASTNode>,
            position: Position,
        ) -> Self {
            let prefix = get_lexeme_of_option(prefix_ident);
            let field_prefix = get_lexeme_of_option(field_prefix_ident);
            ShapeASTNode {
                prefix_ident: prefix,
                identifier: identifier.get_lexeme(),
                field_prefix_ident: field_prefix,
                field_identifier,
                tuples,
                prefix: None,
                field_prefix: None,
                position,
            }
        }

        /// Devuelve el Option del String del identificador del Prefix de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        ///
        /// # Retorna
        /// El Option del String del identificador del Prefix de la Shape
        pub fn get_prefix_ident(&self) -> Option<String> {
            self.prefix_ident.clone()
        }

        /// Devuelve el String del identificador de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        ///
        /// # Retorna
        /// El String del identificador de la Shape
        pub fn get_identifier(&self) -> String {
            self.identifier.clone()
        }

        /// Devuelve el Option del String del identificador del Prefix del campo de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        ///
        /// # Retorna
        /// El Option del String del identificador del Prefix del campo de la Shape
        pub fn get_field_prefix_ident(&self) -> Option<String> {
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

        /// Devuelve las tuplas mutables de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        ///
        /// # Retorna
        /// El vector con las tuplas de la Shape
        pub fn get_tuples(&self) -> Vec<ShapeTupleASTNode> {
            self.tuples.clone()
        }

        /// Devuelve las tuplas de la Shape en un vector mutable
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        ///
        /// # Retorna
        /// El vector mutable con las tuplas de la Shape
        pub fn get_mut_tuples(&mut self) -> &mut Vec<ShapeTupleASTNode> {
            &mut self.tuples
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

    impl ManagePosition for ShapeASTNode {
        /// Devuelve la posición del nodo Shape
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo Shape del AST
        /// 
        /// # Retorna
        /// La posición del nodo Shape con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    impl ManagePrefix for ShapeASTNode {
        /// Asocia un nodo Prefix con la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        /// * `query` - El Option que contiene el nodo Prefix del AST que se quiere asociar a la Shape
        fn set_prefix(&mut self, prefix: Option<PrefixASTNode>) {
            self.prefix = prefix;
        }
        
        /// Asocia un nodo Prefix con el campo de la Shape
        /// 
        /// # Argumentos
        /// * `self` - El propio nodo Shape
        /// * `prefix` - Un Option con el nodo Prefix del AST que se quiere asociar al campo de la Shape
        fn set_object_prefix(&mut self, prefix: Option<PrefixASTNode>) {
            self.field_prefix = prefix;
        }
    }

    /// Nodo de tipo ShapeTuples del AST
    ///
    /// Se corresponde con las tuplas de los Shape de ShExML; contiene el identificador de los Prefix de la tupla y del objeto de esta, el identificador de la
    /// propia tupla y del objeto y los nodos Prefix asociados con la tupla y el objeto
    #[derive(Debug, PartialEq, Clone, Eq, Hash)]
    pub struct ShapeTupleASTNode {
        prefix_ident: Option<String>,
        identifier: String,
        object_prefix_ident: Option<String>,
        object: IdentOrAccess,
        position: Position,

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
        pub fn new(
            prefix_ident: Option<Token>,
            identifier: Token,
            object_prefix_ident: Option<Token>,
            object: IdentOrAccess,
            position: Position,
        ) -> Self {
            let prefix = get_lexeme_of_option(prefix_ident);
            let object_prefix = get_lexeme_of_option(object_prefix_ident);
            ShapeTupleASTNode {
                prefix_ident: prefix,
                identifier: identifier.get_lexeme(),
                object_prefix_ident: object_prefix,
                object,
                prefix: None,
                object_prefix: None,
                position,
            }
        }

        /// Devuelve el Option del String del identificador del Prefix de la tupla de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo ShapeTuple
        ///
        /// # Retorna
        /// El Option del String del identificador del Prefix de la tupla de la Shape
        pub fn get_prefix_ident(&self) -> Option<String> {
            self.prefix_ident.clone()
        }

        /// Devuelve el String del identificador de la tupla de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo ShapeTuple
        ///
        /// # Retorna
        /// El String del identificador de la tupla de la Shape
        pub fn get_identifier(&self) -> String {
            self.identifier.clone()
        }

        /// Devuelve el Option del String del identificador del Prefix del objeto de la tupla de la Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo ShapeTuple
        ///
        /// # Retorna
        /// El Option del String del identificador del Prefix del objeto de la tupla de la Shape
        pub fn get_object_prefix_ident(&self) -> Option<String> {
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

    impl ManagePosition for ShapeTupleASTNode {
        /// Devuelve la posición del nodo ShapeTuple
        /// 
        /// # Parámetros
        /// * `self` - El propio nodo ShapeTuple del AST
        /// 
        /// # Retorna
        /// La posición del nodo ShapeTuple con respecto a la entrada
        fn get_position(&self) -> Position {
            self.position.clone()
        }
    }

    impl ManagePrefix for ShapeTupleASTNode {
        /// Asocia un nodo Prefix con la tupla de Shape
        ///
        /// # Argumentos
        /// * `self` - El propio nodo ShapeTuple
        /// * `query` - El Option que contiene el nodo Prefix del AST que se quiere asociar a la tupla de Shape
        fn set_prefix(&mut self, prefix: Option<PrefixASTNode>) {
            self.prefix = prefix;
        }
        
        /// Asocia un nodo Prefix con el objeto de la tupla de la Shape
        /// 
        /// # Argumentos
        /// * `self` - El propio nodo ShapeTuple
        /// * `prefix` - El Option con el nodo Prefix del AST que se quiere asociar al objeto de la tupla de la Shape
        fn set_object_prefix(&mut self, prefix: Option<PrefixASTNode>) {
            self.object_prefix = prefix;
        }
    }

    /// Obtiene el lexema de un Token dentro de un Option
    ///
    /// # Argumentos
    /// * `option` - El Option que contiene el token
    ///
    /// # Retorna
    /// El Option que contiene el lexema
    fn get_lexeme_of_option(option: Option<Token>) -> Option<String> {
        if let Some(second) = option {
            Some(second.get_lexeme())
        } else {
            None
        }
    }
}

/// El AST
///
/// Representa el AST; contiene los prefijos, sources, consultas, iteradores, expresiones y Shapes.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub fn new(
        prefixes: Vec<PrefixASTNode>,
        sources: Vec<SourceASTNode>,
        queries: Option<Vec<QueryASTNode>>,
        iterators: Vec<IteratorASTNode>,
        expressions: Vec<ExpressionASTNode>,
        shapes: Vec<ShapeASTNode>,
    ) -> Self {
        AST {
            prefixes,
            sources,
            queries,
            iterators,
            expressions,
            shapes,
        }
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

    /// Devuelve el vector mutable de nodos Prefix del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector mutable de nodos Prefix del AST
    pub fn get_mut_prefixes(&self) -> Vec<PrefixASTNode> {
        self.prefixes.clone()
    }

    /// Devuelve el vector de nodos mutable Source del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector de nodos mutable Source del AST
    pub fn get_sources(&self) -> Vec<SourceASTNode> {
        self.sources.clone()
    }

    /// Devuelve el vector de nodos Source del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector de nodos Source del AST
    pub fn get_mut_sources(&mut self) -> &mut Vec<SourceASTNode> {
        &mut self.sources
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

    /// Devuelve el vector mutable de nodos Query del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector mutable de nodos Query del AST
    pub fn get_mut_queries(&mut self) -> &mut Option<Vec<QueryASTNode>> {
        &mut self.queries
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

    /// Devuelve el vector mutable de nodos Iterator del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector mutable de nodos Iterator del AST
    pub fn get_mut_iterators(&mut self) -> &mut Vec<IteratorASTNode> {
        &mut self.iterators
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

    /// Devuelve el vector mutable de nodos Expression del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector mutable de nodos Expression del AST
    pub fn get_mut_expressions(&mut self) -> &mut Vec<ExpressionASTNode> {
        &mut self.expressions
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

    /// Devuelve el vector mutable de nodos Shape del AST
    ///
    /// # Argumentos
    /// * `self` - El propio AST
    ///
    /// # Retorna
    /// El vector mutable de nodos Shape del AST
    pub fn get_mut_shapes(&mut self) -> &mut Vec<ShapeASTNode> {
        &mut self.shapes
    }
}
