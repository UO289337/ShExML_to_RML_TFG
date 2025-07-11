//! Módulo del analizador sintáctico
//!
//! Realiza el análisis sintáctico del compilador
//! Comprueba que los tokens resultado del analizador léxico se encuentran en el orden esperado y genera el AST

use chumsky::combinator::*;
use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::Parser;

use crate::model::ast::nodes::*;
use crate::model::lexer::token::*;

use super::super::ast::*;
use super::super::lexer::token::TokenType;

/// Parsea los tokens para generar el AST
///
/// Realiza el parseo de los tokens de los prefijos y de las fuentes para poder crear el AST
///
/// # Retorna
/// El AST creado
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn ast_parser() -> impl Parser<Token, AST, Error = Simple<Token>> {
    prefix_parser()
        .then(source_parser())
        .then(query_parser().or_not())
        .then(iterator_parser())
        .then(expression_parser())
        .then(shape_parser())
        .then_ignore(eof_parser())
        .map(
            |(((((prefixes, sources), queries), iterators), expressions), shapes)| {
                AST::new(prefixes, sources, queries, iterators, expressions, shapes)
            },
        )
}

/// Parsea los tokens para generar un vector de nodos Prefix del AST
///
/// Realiza el parseo de los Prefix y crea los nodos PrefixASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Prefix del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn prefix_parser() -> impl Parser<Token, Vec<PrefixASTNode>, Error = Simple<Token>> {
    single_prefix_parser()
        .map(|((_, ident), uri)| create_prefix_node(ident, uri))
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Prefix del AST
///
/// Realiza el parseo de los tokens con la secuencia: Prefix Ident? Colon LeftAngleBrackey Uri RightAngleBracket
///
/// # Retorna
/// Un nodo Prefix del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_prefix_parser() -> Then<
    Map<
        Then<
            Then<
                MapErr<
                    Map<
                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                        impl Fn(Token) -> Token,
                        Token,
                    >,
                    impl Fn(Simple<Token>) -> Simple<Token>,
                >,
                OrNot<
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                >,
            >,
            MapErr<
                Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
                impl Fn(Simple<Token>) -> Simple<Token>,
            >,
        >,
        fn(((Token, Option<Token>), Token)) -> (Token, Option<Token>),
        ((Token, Option<Token>), Token),
    >,
    impl Parser<Token, Token, Error = Simple<Token>>,
> {
    prefix_token_parser()
        .then(identifier_parser(PREFIX).or_not())
        .then_ignore(colon_parser("después del identificador"))
        .then(uri_with_angle_brackets_parser())
}

/// Crea un nodo Prefix del AST
///
/// # Argumentos
/// * `ident` - El Option con el identificador del Prefix
/// * `uri` - La URI del Prefix
///
/// # Retorna
/// Un nodo Prefix del AST
fn create_prefix_node(ident: Option<Token>, uri: Token) -> PrefixASTNode {
    if let Some(identifier) = ident {
        return PrefixASTNode::new(
            Some(identifier.clone()),
            uri,
            Position::new(identifier.get_num_line()),
        );
    }

    PrefixASTNode::new(None, uri.clone(), Position::new(uri.get_num_line()))
}

/// Parsea los tokens para generar un vector de nodos Source del AST
///
/// Realiza el parseo de los Source y crea los nodos SourceASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Source del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn source_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {
    single_source_parser()
        .map(|((_, identifier), source_definition)| {
            SourceASTNode::new(
                identifier.clone(),
                source_definition,
                Position::new(identifier.get_num_line()),
            )
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Source del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia: Source Ident SourceDefinition RightAngleBracket
///
/// # Retorna
/// Un nodo Source del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_source_parser() -> Map<
    Then<
        Then<
            Map<
                Then<
                    Then<
                        MapErr<
                            Map<
                                Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                impl Fn(Token) -> Token,
                                Token,
                            >,
                            impl Fn(Simple<Token>) -> Simple<Token>,
                        >,
                        MapErr<
                            Map<
                                Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                impl Fn(Token) -> Token,
                                Token,
                            >,
                            impl Fn(Simple<Token>) -> Simple<Token>,
                        >,
                    >,
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                >,
                fn(((Token, Token), Token)) -> (Token, Token),
                ((Token, Token), Token),
            >,
            Or<
                Or<
                    Or<
                        Map<
                            MapErr<
                                Map<
                                    Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                    impl Fn(Token) -> Token,
                                    Token,
                                >,
                                impl Fn(Simple<Token>) -> Simple<Token>,
                            >,
                            impl Fn(Token) -> SourceDefinition,
                            Token,
                        >,
                        Map<
                            MapErr<
                                Map<
                                    Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                    impl Fn(Token) -> Token,
                                    Token,
                                >,
                                impl Fn(Simple<Token>) -> Simple<Token>,
                            >,
                            impl Fn(Token) -> SourceDefinition,
                            Token,
                        >,
                    >,
                    Map<
                        impl Parser<Token, AccessASTNode, Error = Simple<Token>>,
                        impl Fn(AccessASTNode) -> SourceDefinition,
                        AccessASTNode,
                    >,
                >,
                Map<
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                    impl Fn(Token) -> SourceDefinition,
                    Token,
                >,
            >,
        >,
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
    >,
    fn((((Token, Token), SourceDefinition), Token)) -> ((Token, Token), SourceDefinition),
    (((Token, Token), SourceDefinition), Token),
> {
    source_token_parser()
        .then(identifier_parser(SOURCE))
        .then_ignore(left_angle_bracket_parser("la URL o ruta"))
        .then(source_definition_parser())
        .then_ignore(right_angle_bracket_parser("la URL o ruta"))
}

/// Parsea los tokens de la definición del Source
///
/// Realiza el parseo de los tokens para parsear la secuencia: (Uri|JdbcUrl|FilePath|Path)
///
/// # Retorna
/// El parser de la definición de Source
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn source_definition_parser() -> Or<
    Or<
        Or<
            Map<
                MapErr<
                    Map<
                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                        impl Fn(Token) -> Token,
                        Token,
                    >,
                    impl Fn(Simple<Token>) -> Simple<Token>,
                >,
                impl Fn(Token) -> SourceDefinition,
                Token,
            >,
            Map<
                MapErr<
                    Map<
                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                        impl Fn(Token) -> Token,
                        Token,
                    >,
                    impl Fn(Simple<Token>) -> Simple<Token>,
                >,
                impl Fn(Token) -> SourceDefinition,
                Token,
            >,
        >,
        Map<
            impl Parser<Token, AccessASTNode, Error = Simple<Token>>,
            impl Fn(AccessASTNode) -> SourceDefinition,
            AccessASTNode,
        >,
    >,
    Map<
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
        impl Fn(Token) -> SourceDefinition,
        Token,
    >,
> {
    uri_parser()
        .map(|uri| SourceDefinition::URI(uri.get_lexeme()))
        .or(path_parser().map(|path| SourceDefinition::Path(path.get_lexeme())))
        .or(access_parser(LEFT_ANGLE_BRACKET).map(|access| {
            // Se puede detectar una ruta relativa fichero.tipo como acceso
            let path = format!(
                "{}{ACCESS_DOT}{}",
                access.get_identifier(),
                access.get_first_access()
            );
            SourceDefinition::Path(path)
        }))
        .or(jdbc_url_parser().map(|jdbc_url| SourceDefinition::JdbcURL(jdbc_url.get_lexeme())))
}

/// Parsea los tokens para generar un vector de nodos Query del AST
///
/// Realiza el parseo de los Query y crea los nodos QueryASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Query del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn query_parser() -> impl Parser<Token, Vec<QueryASTNode>, Error = Simple<Token>> {
    single_query_parser()
        .map(|((_, identifier), sql_query)| {
            QueryASTNode::new(
                identifier.clone(),
                sql_query,
                Position::new(identifier.get_num_line()),
            )
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Query del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia: Query Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
///
/// # Retorna
/// Un vector con nodos Query del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_query_parser() -> Map<
    Then<
        Then<
            Map<
                Then<
                    Map<
                        Then<
                            Then<
                                MapErr<
                                    Map<
                                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                        impl Fn(Token) -> Token,
                                        Token,
                                    >,
                                    impl Fn(Simple<Token>) -> Simple<Token>,
                                >,
                                MapErr<
                                    Map<
                                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                        impl Fn(Token) -> Token,
                                        Token,
                                    >,
                                    impl Fn(Simple<Token>) -> Simple<Token>,
                                >,
                            >,
                            MapErr<
                                Map<
                                    Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                    impl Fn(Token) -> Token,
                                    Token,
                                >,
                                impl Fn(Simple<Token>) -> Simple<Token>,
                            >,
                        >,
                        fn(((Token, Token), Token)) -> (Token, Token),
                        ((Token, Token), Token),
                    >,
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                >,
                fn(((Token, Token), Token)) -> (Token, Token),
                ((Token, Token), Token),
            >,
            MapErr<
                Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
                impl Fn(Simple<Token>) -> Simple<Token>,
            >,
        >,
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
    >,
    fn((((Token, Token), Token), Token)) -> ((Token, Token), Token),
    (((Token, Token), Token), Token),
> {
    query_token_parser()
        .then(identifier_parser(QUERY))
        .then_ignore(left_angle_bracket_parser("la consulta SQL"))
        .then_ignore(sql_type_parser())
        .then(sql_query_token_parser())
        .then_ignore(right_angle_bracket_parser("la consulta SQL"))
}

/// Parsea los tokens para generar un vector de nodos Iterator del AST
///
/// Realiza el parseo de los Iterator y crea los nodos IteratorASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Iterator del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn iterator_parser() -> impl Parser<Token, Vec<IteratorASTNode>, Error = Simple<Token>> {
    single_iterator_parser().repeated().at_least(1).collect()
}

/// Parsea los tokens para generar un nodo Iterator del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Iterator Ident LeftAngleBracket IteratorAccess RightAngleBracket Fields
///
/// # Retorna
/// Un vector con nodos Query del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_iterator_parser() -> impl Parser<Token, IteratorASTNode, Error = Simple<Token>> {
    iterator_token_parser()
        .then(identifier_parser(ITERATOR))
        .then_ignore(left_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow",
        ))
        .then(iterator_access_parser())
        .then_ignore(right_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow",
        ))
        .then(iterator_fields_parser())
        .map(|(((_, ident), iterator_access), fields)| {
            create_iterator_ast_node(ident, iterator_access, fields)
        })
}

/// Parsea los tokens del acceso de un Iterator
///
/// Realiza el parseo de los tokens para parsear la secuencia: (CsvPerRow|Ident|SqlType SqlQuery)
///
/// # Retorna
/// Una tupla con el token del identificador, csvperrow o :sql y un Option con la consulta SQL en el caso de que el primer token sea :sql
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn iterator_access_parser() -> impl Parser<Token, (Token, Option<Token>), Error = Simple<Token>> {
    identifier_parser(LEFT_ANGLE_BRACKET)
        .map(|token| (token, None))
        .or(csv_per_row_parser().map(|token| (token, None)))
        .or(sql_type_parser()
            .then(sql_query_token_parser())
            .map(|(sql_type, sql_query)| (sql_type, Some(sql_query))))
}

/// Parsea los tokens del cuerpo de los campos (fields) de un Iterator
///
/// Realiza el parseo de los tokens para parsear la secuencia: OpeningCurlyBrace Fields CLosingCurlyBrace
///
/// # Retorna
/// El parser del acceso de un Iterator
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn iterator_fields_parser() -> impl Parser<Token, Vec<FieldASTNode>, Error = Simple<Token>> {
    opening_curly_brace_parser("los campos")
        .ignore_then(field_parser())
        .then_ignore(closing_curly_brace_parser("los campos"))
}

/// Crea un nodo Iterator del AST
///
/// Crea, a partir de los datos del parser de Iterator, el nodo AST de este
///
/// # Parámetros
/// * `identifier` - El token identificador del Iterator
/// * `iterator_access` - Una tupla con el token Ident o CsvPerRow o con los tokens SqlType y SqlQuery
/// * `fields` - El vector de fields del Iterator
///
/// # Retorna
/// Un nodo Iterator del AST
fn create_iterator_ast_node(
    identifier: Token,
    iterator_access: (Token, Option<Token>),
    fields: Vec<FieldASTNode>,
) -> IteratorASTNode {
    let (token1, token2): (Token, Option<Token>) = iterator_access;

    if token2.is_none() && token1.get_lexeme() == CSV_PER_ROW {
        IteratorASTNode::new(
            identifier.clone(),
            IteratorAccess::CsvPerRow(token1.get_lexeme()),
            fields,
            Position::new(identifier.get_num_line()),
        )
    } else if token2.is_none() && token1.get_lexeme() != CSV_PER_ROW {
        IteratorASTNode::new(
            identifier.clone(),
            IteratorAccess::Ident(token1.get_lexeme()),
            fields,
            Position::new(identifier.get_num_line()),
        )
    } else {
        IteratorASTNode::new(
            identifier.clone(),
            IteratorAccess::SqlQuery(token2.unwrap().get_lexeme()),
            fields,
            Position::new(identifier.get_num_line()),
        )
    }
}

/// Parsea los tokens para generar un vector de nodos Field del AST
///
/// Realiza el parseo de los Field y crea los nodos FieldASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Field del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn field_parser() -> impl Parser<Token, Vec<FieldASTNode>, Error = Simple<Token>> {
    single_field_parser()
        .map(|((_, field_identifier), access_field_identifier)| {
            FieldASTNode::new(
                field_identifier.clone(),
                access_field_identifier,
                Position::new(field_identifier.get_num_line()),
            )
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar el nodo Field del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Field identifier LeftAngleBracket Identifier RightAngleBracket
///
/// # Retorna
/// Un vector con nodos Field del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_field_parser() -> Map<
    Then<
        Then<
            Map<
                Then<
                    Then<
                        MapErr<
                            Map<
                                Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                impl Fn(Token) -> Token,
                                Token,
                            >,
                            impl Fn(Simple<Token>) -> Simple<Token>,
                        >,
                        MapErr<
                            Map<
                                Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                                impl Fn(Token) -> Token,
                                Token,
                            >,
                            impl Fn(Simple<Token>) -> Simple<Token>,
                        >,
                    >,
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                >,
                fn(((Token, Token), Token)) -> (Token, Token),
                ((Token, Token), Token),
            >,
            MapErr<
                Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
                impl Fn(Simple<Token>) -> Simple<Token>,
            >,
        >,
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
    >,
    fn((((Token, Token), Token), Token)) -> ((Token, Token), Token),
    (((Token, Token), Token), Token),
> {
    field_token_parser()
        .then(identifier_parser(FIELD))
        .then_ignore(left_angle_bracket_parser("el identificador"))
        .then(identifier_parser(LEFT_ANGLE_BRACKET))
        .then_ignore(right_angle_bracket_parser("el identificador"))
}

/// Parsea los tokens para generar un vector de nodos Expression del AST
///
/// Realiza el parseo de los Expression y crea los nodos ExpressionASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Expression del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn expression_parser() -> impl Parser<Token, Vec<ExpressionASTNode>, Error = Simple<Token>> {
    single_expression_parser().repeated().at_least(1).collect()
}

/// Parsea los tokens para generar un nodo Expression del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Expression Ident LeftAngleBracket Access (UNION|JOIN) Access (Substituting Access|On Access Equal Access) RightAngleBracket
///
/// # Retorna
/// Un nodo Expression del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_expression_parser() -> impl Parser<Token, ExpressionASTNode, Error = Simple<Token>> {
    expression_token_parser()
        .then(identifier_parser(EXPRESSION))
        .then_ignore(left_angle_bracket_parser("el acceso"))
        .then(access_parser(LEFT_ANGLE_BRACKET))
        .then(union_access_parser().or_not())
        .then_ignore(right_angle_bracket_parser("el acceso"))
        .map(|(((_, ident), iterator_access), union_access)| {
            create_expression_node(ident, iterator_access, union_access)
        })
}

/// Parsea los tokens del Access con UNION
///
/// Realiza el parseo de los tokens para parsear la secuencia: Union Access
///
/// # Retorna
/// Una tupla con: el token de Union, el token de acceso, y 2 None
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn union_access_parser() -> impl Parser<Token, AccessASTNode, Error = Simple<Token>> {
    union_parser()
        .ignore_then(access_parser(UNION))
        .map(|access| access)
}

/// Crea un nodo Expression del AST
///
/// A partir de la información sacada del parser crea un nodo expresión del AST
///
/// # Parámetros
/// * `identifier` - El identificado de la expresión
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `union_access` - Un Option con el acceso al UNION
///
/// # Retorna
/// Un nodo ExpressionASTNode
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn create_expression_node(
    identifier: Token,
    iterator_access: AccessASTNode,
    union_access: Option<AccessASTNode>,
) -> ExpressionASTNode {
    let basic_expression = union_access.is_none();
    let accesses = create_vec_of_accesses(iterator_access, union_access);
    let position = Position::new(identifier.get_num_line());

    if basic_expression {
        return ExpressionASTNode::new(identifier, ExpressionType::BASIC, accesses, position);
    }
    ExpressionASTNode::new(identifier, ExpressionType::UNION, accesses, position)
}

/// Crea un vector de nodos AccessASTNode
///
/// A partir de los accesos de una expresión, crea su nodo de vectores Access del AST
///
/// # Parámetros
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `union_access` - Un Option con el acceso del UNION
///
/// # Retorna
/// Una tupla con un Option con el UNION y un vector con todos los accesos de una expresión
fn create_vec_of_accesses(
    iterator_access: AccessASTNode,
    union_access: Option<AccessASTNode>,
) -> Vec<AccessASTNode> {
    let mut accesses = vec![iterator_access];

    if union_access.is_some() {
        accesses.push(union_access.unwrap());
    }

    accesses
}

/// Parsea los tokens para generar un nodo Access del AST
///
/// Realiza el parseo de los tokens con la secuencia: Ident AccessDot Ident (AccessDot Ident)?
///
/// # Retorna
/// Un nodo Access del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn access_parser(previous_token: &str) -> impl Parser<Token, AccessASTNode, Error = Simple<Token>> {
    identifier_parser(previous_token)
        .then_ignore(access_dot_parser())
        .then(identifier_parser(ACCESS_DOT))
        .then(third_access_parser())
        .map(|((ident, iterator_accessed), field_accessed)| {
            create_access_node(ident, iterator_accessed, field_accessed)
        })
}

/// Parsea los tokens del tercer acceso del Access
///
/// Realiza el parseo de los tokens con la secuencia opcional: (AccessDot Ident)?
///
/// # Retorna
/// El parser del tercer acceso de Access
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn third_access_parser() -> OrNot<
    Then<
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
    >,
> {
    access_dot_parser()
        .then(identifier_parser(ACCESS_DOT))
        .or_not()
}

/// Crea un nodo Access del AST
///
/// A partir de una serie de identificadores y puntos de acceso (.), crea un nodo AccessASTNode
///
/// # Parámetros
/// * `identifier` - El identificador de un SOURCE
/// * `iterator_accessed` - El identificador del iterador accedido
/// * `field_accesed` - El identificador del campo del iterador accedido
///
/// # Retorna
/// Un nodo Access del AST
fn create_access_node(
    identifier: Token,
    iterator_accessed: Token,
    field_accessed: Option<(Token, Token)>,
) -> AccessASTNode {
    if field_accessed.is_some() {
        // El 1 apunta al identificador; el 0 al punto ('.')
        return AccessASTNode::new(
            identifier.clone(),
            iterator_accessed,
            Some(field_accessed.unwrap().1),
            Position::new(identifier.get_num_line()),
        );
    }

    AccessASTNode::new(
        identifier.clone(),
        iterator_accessed,
        None,
        Position::new(identifier.get_num_line()),
    )
}

/// Parsea los tokens para generar un vector de nodos Shape del AST
///
/// Realiza el parseo de las Shape y crea los nodos ShapeASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos Shape del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn shape_parser() -> impl Parser<Token, Vec<ShapeASTNode>, Error = Simple<Token>> {
    single_shape_parser().repeated().at_least(1).collect()
}

/// Parsea los tokens para generar el nodo Shape del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// PrefixOrUri Ident FieldPrefixOrUri LeftBracket FieldIdent RightBracket ShapeBody
///
/// # Retorna
/// Un nodo Shape del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_shape_parser() -> impl Parser<Token, ShapeASTNode, Error = Simple<Token>> {
    prefix_with_ident_parser()
        .then(prefix_shape_parser(
            "en la Shape, después del identificador del Prefix del acceso",
        ))
        .then_ignore(left_bracket_parser("el identificador"))
        .then(ident_or_access_parser())
        .then_ignore(right_bracket_parser("el identificador"))
        .then(shape_body_parser())
        .map(|(((shape_prefix, shape_uri), field_ident), shape_tuples)| {
            create_shape_node(shape_prefix, shape_uri, field_ident, shape_tuples)
        })
}

/// Parsea los tokens del cuerpo de la Shape
///
/// Realiza el parseo de los tokens con la secuencia: OpeningCurlyBrackets ShapeTuple+ ClosingCurlyBrackets
///
/// # Retorna
/// El parser del cuerpo del Shape: '{' ShapeTuple+ '}'
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn shape_body_parser() -> impl Parser<Token, Vec<ShapeTupleASTNode>, Error = Simple<Token>> {
    opening_curly_brace_parser("las tuplas de la Shape")
        .ignore_then(shape_tuple_parser())
        .then_ignore(closing_curly_brace_parser("las tuplas de la Shape"))
}

/// Crea un nodo Shape del AST
///
/// # Retorna
/// Un nodo ShapeASTNode del AST
///
/// # Parámetros
/// * `shape_prefix` - Una tupla con el Token del Prefix y del Ident después de este
/// * `field_prefix_ident` - El Ident del Prefix del field
/// * `field_ident` - Una tupla con 2 Option, uno con el token del Ident y otro con el token del nodo Access
/// * `shape_tuples` - Un vector con las tuplas de la Shape
fn create_shape_node(
    shape_prefix: (Option<Token>, Token),
    field_prefix_ident: Option<Token>,
    field_ident: (Option<Token>, Option<AccessASTNode>),
    shape_tuples: Vec<ShapeTupleASTNode>,
) -> ShapeASTNode {
    let (prefix, identifier) = shape_prefix;
    let (shape_field_ident, shape_field_access) = field_ident;

    let field_ident;

    if shape_field_ident.is_some() {
        field_ident = IdentOrAccess::Ident(shape_field_ident.unwrap().get_lexeme());
    } else {
        field_ident = IdentOrAccess::Access(shape_field_access.unwrap());
    }

    ShapeASTNode::new(
        prefix,
        identifier.clone(),
        field_prefix_ident,
        field_ident,
        shape_tuples,
        Position::new(identifier.get_num_line()),
    )
}

/// Parsea los tokens para generar un vector de nodos ShapeTuple del AST
///
/// Realiza el parseo de las ShapeTuple y crea los nodos ShapeTupleASTNode que se introducen en un vector
///
/// # Retorna
/// Un vector con nodos ShapeTuple del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn shape_tuple_parser() -> impl Parser<Token, Vec<ShapeTupleASTNode>, Error = Simple<Token>> {
    single_shape_tuple_parser().repeated().at_least(1).collect()
}

/// Parsea los tokens para generar un nodo ShapeTuples del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Prefix Ident Prefix? LeftBracket IdentOrAccess RightBracket Semicolon
///
/// # Retorna
/// Un nodo ShapeTuple del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn single_shape_tuple_parser() -> impl Parser<Token, ShapeTupleASTNode, Error = Simple<Token>> {
    prefix_with_ident_parser()
        .then(
            prefix_shape_parser("en la tupla del Shape, después del identificador del Prefix")
                .or_not(),
        )
        .then_ignore(left_bracket_parser("el identificador"))
        .then(ident_or_access_parser())
        .then_ignore(right_bracket_parser("el identificador"))
        .then_ignore(semicolon_parser())
        .map(|((tuple_prefix, object_prefix), tuple_object)| {
            create_shape_tuple_node(tuple_prefix, object_prefix, tuple_object)
        })
}

/// Crea un nodo ShapeTuple del AST
///
/// # Retorna
/// Un nodo ShapeTuplesASTNode del AST
///
/// # Parámetros
/// * `tuple_prefix` - Una tupla con el Token del Prefix y con el Ident después de este
/// * `object_prefix` - Un Option con el token del Prefix
/// * `tuple_object` - Una tupla con 2 Option, uno con el token del Ident y otro con el token del nodo Access
fn create_shape_tuple_node(
    tuple_prefix: (Option<Token>, Token),
    object_prefix: Option<Option<Token>>,
    tuple_object: (Option<Token>, Option<AccessASTNode>),
) -> ShapeTupleASTNode {
    let (prefix, identifier) = tuple_prefix;
    let (tuple_object_ident, tuple_field_access) = tuple_object;

    let object_ident;

    if tuple_object_ident.is_some() {
        object_ident = IdentOrAccess::Ident(tuple_object_ident.unwrap().get_lexeme());
    } else {
        object_ident = IdentOrAccess::Access(tuple_field_access.unwrap());
    }

    let object_prefix_ident;

    if object_prefix.is_some() {
        object_prefix_ident = object_prefix.unwrap();
    } else {
        object_prefix_ident = None;
    }

    ShapeTupleASTNode::new(
        prefix,
        identifier.clone(),
        object_prefix_ident,
        object_ident,
        Position::new(identifier.get_num_line()),
    )
}

// Parsers auxiliares

/// Parsea los tokens del ident o Access
///
/// Realiza el parseo de los tokens con la secuencial: (Ident|Access)
///
/// # Retorna
/// El parser del Ident o Access
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn ident_or_access_parser(
) -> impl Parser<Token, (Option<Token>, Option<AccessASTNode>), Error = Simple<Token>> {
    access_parser(LEFT_BRACKET)
        .map(|access| (None, Some(access)))
        .or(identifier_parser(LEFT_BRACKET).map(|token| (Some(token), None)))
}

/// Realiza el parseo de un Prefix o Uri, junto con el Ident que acompaña a Prefix y la URI que acompaña a esta
///
/// # Retorna
/// Un Parser con una tupla con el identificador del Prefix y con la URI o el identificador después del Prefix, dependiendo de lo que se encuentre
fn prefix_with_ident_parser() -> Map<
    Then<
        Map<
            Then<
                OrNot<
                    MapErr<
                        Map<
                            Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                            impl Fn(Token) -> Token,
                            Token,
                        >,
                        impl Fn(Simple<Token>) -> Simple<Token>,
                    >,
                >,
                MapErr<
                    Map<
                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                        impl Fn(Token) -> Token,
                        Token,
                    >,
                    impl Fn(Simple<Token>) -> Simple<Token>,
                >,
            >,
            fn((Option<Token>, Token)) -> Option<Token>,
            (Option<Token>, Token),
        >,
        MapErr<
            Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
            impl Fn(Simple<Token>) -> Simple<Token>,
        >,
    >,
    impl Fn((Option<Token>, Token)) -> (Option<Token>, Token),
    (Option<Token>, Token),
> {
    identifier_parser("la expresión o Shape, al principio de una Shape")
        .or_not()
        .then_ignore(colon_parser("antes del identificador en la Shape"))
        .then(identifier_parser(COLON))
        .map(|(colon_ident, ident)| (colon_ident, ident))
}

/// Parsea los tokens del Prefix o Uri del field de Shape
///
/// Realiza el parseo de los tokens con la secuencia: (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)
///
/// # Retorna
/// El parser del Prefix o de la URI del field de Shape
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn prefix_shape_parser(
    message: &str,
) -> Map<
    Map<
        Then<
            OrNot<
                MapErr<
                    Map<
                        Filter<impl Fn(&Token) -> bool, Simple<Token>>,
                        impl Fn(Token) -> Token,
                        Token,
                    >,
                    impl Fn(Simple<Token>) -> Simple<Token>,
                >,
            >,
            MapErr<
                Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
                impl Fn(Simple<Token>) -> Simple<Token>,
            >,
        >,
        fn((Option<Token>, Token)) -> Option<Token>,
        (Option<Token>, Token),
    >,
    impl Fn(Option<Token>) -> Option<Token>,
    Option<Token>,
> {
    identifier_parser(COLON)
        .or_not()
        .then_ignore(colon_parser(message))
        .map(|ident| ident)
}

/// Realiza el parseo de una URI con '<' y '>' a ambos lados
///
/// # Retorna
/// Un Parser con un Token Uri
fn uri_with_angle_brackets_parser() -> impl Parser<Token, Token, Error = Simple<Token>> {
    left_angle_bracket_parser("la URI")
        .ignore_then(uri_parser())
        .then_ignore(right_angle_bracket_parser("la URI"))
        .map(|uri| uri)
}

// Parsers particulares

/// Parsea el token Prefix en los tokens
///
/// # Retorna
/// Un token de tipo Prefix si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Prefix
fn prefix_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Prefix,
        format!("Se esperaba un PREFIX en la línea"),
    )
}

/// Parsea el token Source en los tokens
///
/// # Retorna
/// Un token de tipo Source si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Source
fn source_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    // Se indica que 'Se esperaba un PREFIX' porque el analizador no tiene manera de saber si lo que hay en la línea del error
    // es un PREFIX o un SOURCE en el caso, por ejemplo, de que encuentre SOURC
    general_parser(
        TokenType::Source,
        format!("Se esperaba un PREFIX o un SOURCE en la línea"),
    )
}

/// Parsea el token Query en los tokens
///
/// # Retorna
/// Un token de tipo Query si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Query
fn query_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Query,
        format!("Se esperaba un QUERY en la línea"),
    )
}

/// Parsea el token Iterator en los tokens
///
/// # Retorna
/// Un token de tipo Iterator si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Iterator
fn iterator_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Iterator,
        format!("Se esperaba un ITERATOR en la línea"),
    )
}

/// Parsea el token Field en los tokens
///
/// # Retorna
/// Un token de tipo Field si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Field
fn field_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Field,
        format!("Se esperaba un FIELD en la línea"),
    )
}

/// Parsea el token Expression en los tokens
///
/// # Retorna
/// Un token de tipo Expression si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Expression
fn expression_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Expression,
        format!("Se esperaba un EXPRESSION en la línea"),
    )
}

/// Parsea el token Union en los tokens
///
/// # Retorna
/// Un token de tipo Union si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Union
fn union_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Union,
        format!("Se esperaba un UNION en la expresión de la línea"),
    )
}

/// Parsea el token CsvPerRow en los tokens
///
/// # Retorna
/// Un token de tipo CsvPerRow si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo CsvPerRow
fn csv_per_row_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::CsvPerRow,
        format!("Se esperaba un csvperrow en la línea"),
    )
}

/// Parsea el token SqlType en los tokens
///
/// # Retorna
/// Un token de tipo SqlType si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo SqlType
fn sql_type_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::SqlType,
        format!("Se esperaba 'sql:' después de '<' en la línea"),
    )
}

/// Parsea el token Ident en los tokens
///
/// # Parámetros
/// * `previous_token` - El token previo al identificador, que puede ser un PREFIX o un SOURCE
///
/// # Retorna
/// Un token de tipo Ident si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Ident
fn identifier_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Ident,
        format!("Se esperaba un identificador después de {previous_token} en la línea"),
    )
}

/// Parsea el token URI en los tokens
///
/// # Retorna
/// Un token de tipo Uri si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn uri_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Uri,
        format!("Se esperaba una URI entre '<' y '>' en la línea"),
    )
}

/// Parsea el token JdbcUrl en los tokens
///
/// # Retorna
/// Un token de tipo JdbcUrl si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo JdbcUrl
fn jdbc_url_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::JdbcUrl,
        format!("Se esperaba una URL JDBC entre '<' y '>' en la línea"),
    )
}

/// Parsea el token Path en los tokens
///
/// # Retorna
/// Un token de tipo Path si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Path
fn path_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Path,
        format!("Se esperaba una ruta entre '<' y '>' en la línea"),
    )
}

/// Parsea el token SqlQuery en los tokens
///
/// # Retorna
/// Un token de tipo SqlQuery si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Uri
fn sql_query_token_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::SqlQuery,
        format!("Se esperaba una consulta SQL entre '<' y '>' en la línea"),
    )
}

/// Parsea el token ':' en los tokens
///
/// # Retorna
/// Un token de tipo : si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo :
fn colon_parser(
    message: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Colon,
        format!("Faltan los ':' {message} en la línea"),
    )
}

/// Parsea el token ';' en los tokens
///
/// # Retorna
/// Un token de tipo ; si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo ;
fn semicolon_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::SemiColon,
        format!("Falta el ';' después del ']' en la línea"),
    )
}

/// Parsea el token '.' en los tokens
///
/// # Retorna
/// Un token de tipo . si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo .
fn access_dot_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::AccessDot,
        format!("Falta un '.' después del identificador en la línea"),
    )
}

/// Parsea el token '<' en los tokens
///
/// # Retorna
/// Un token de tipo < si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo <
fn left_angle_bracket_parser(
    next_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::LeftAngleBracket,
        format!("Se esperaba un '<' antes de {next_token} en la línea"),
    )
}

/// Parsea el token '>' en los tokens
///
/// # Retorna
/// Un token de tipo > si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo >
fn right_angle_bracket_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::RightAngleBracket,
        format!("Se esperaba un '>' después de {previous_token} en la línea"),
    )
}

/// Parsea el token '{' en los tokens
///
/// # Retorna
/// Un token de tipo { si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo {
fn opening_curly_brace_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    // Duplicar las llaves implica escapar una
    general_parser(
        TokenType::OpeningCurlyBrace,
        format!("Se esperaba un '{{' antes de {previous_token} en la línea"),
    )
}

/// Parsea el token '}' en los tokens
///
/// # Retorna
/// Un token de tipo } si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo }
fn closing_curly_brace_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    // Duplicar las llaves implica escapar una
    general_parser(
        TokenType::ClosingCurlyBrace,
        format!("Se esperaba un '}}' después de {previous_token} en la línea"),
    )
}

/// Parsea el token '[' en los tokens
///
/// # Retorna
/// Un token de tipo [ si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo [
fn left_bracket_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::LeftBracket,
        format!("Se esperaba un '[' antes de {previous_token} en la línea"),
    )
}

/// Parsea el token ']' en los tokens
///
/// # Retorna
/// Un token de tipo ] si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo ]
fn right_bracket_parser(
    previous_token: &str,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::RightBracket,
        format!("Se esperaba un ']' después de {previous_token} en la línea"),
    )
}

/// Parsea el token EOF en los tokens
///
/// # Retorna
/// Un token de tipo EOF si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo >
fn eof_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::EOF,
        format!(
            "Se ha encontrado una cadena donde debería estar el final del fichero, en la línea"
        ),
    )
}

/// Parsea cualquier token válido
///
/// /// # Parámetros
/// * `token_type` - El tipo de token esperado
/// * `message` - El mensaje de error que se muestra en el caso de que el tipo del token no sea el esperado
///
/// # Retorna
/// El token reconocido
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea del tipo esperado
fn general_parser(
    token_type: TokenType,
    message: String,
) -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    filter(move |token: &Token| token.get_token_type() == token_type)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            let line = token.found().map(|t| t.get_num_line()).unwrap_or(0);
            Simple::custom(token.span(), format!("{message} {}", line))
        })
}

/// Parsea el vector de tokens para generar el AST
///
/// Toma como entrada el vector de tokens resultado del análisis léxico y genera un árbol AST que tiene un AST como raíz
///
/// # Parámetros
/// * `tokens` - El vector de tokens resultado del análisis léxico
///
/// # Retorna
/// Un AST del AST que será el nodo raíz de este
///
/// # Errores
/// * `[Vec<Simple<Token>>]` - Un vector con los errores que pueden aparecer al realizar el análisis sintáctico
pub fn parser(tokens: Vec<Token>) -> Result<AST, Vec<Simple<Token>>> {
    let ast_parser = ast_parser();
    let parsed = ast_parser.parse(tokens);

    match parsed {
        Ok(node) => Ok(node),
        Err(e) => Err(e),
    }
}

// Tests

/// Módulo de los tests de los parsers analizador sintáctico
///
/// Contiene los tests de los parsers que se encargan de parsear los tokens provenientes del análisis léxico
#[cfg(test)]
mod sintax_parsers_tests {

    use super::*;

    /// Comprueba que se parsean los tokens Prefix
    #[doc(hidden)]
    #[test]
    fn parse_valid_prefix() {
        let expected_token = Token::create_test_token(PREFIX, 1, TokenType::Prefix);
        let actual = prefix_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Prefix aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_prefix() {
        let actual =
            prefix_token_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Source
    #[doc(hidden)]
    #[test]
    fn parse_valid_source() {
        let expected_token = Token::create_test_token(SOURCE, 1, TokenType::Source);
        let actual = source_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Source aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_source() {
        let actual =
            source_token_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Query
    #[doc(hidden)]
    #[test]
    fn parse_valid_query() {
        let expected_token = Token::create_test_token(QUERY, 1, TokenType::Query);
        let actual = query_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Query aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_query() {
        let actual =
            query_token_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Iterator
    #[doc(hidden)]
    #[test]
    fn parse_valid_iterator() {
        let expected_token = Token::create_test_token(ITERATOR, 1, TokenType::Iterator);
        let actual = iterator_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Iterator aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_iterator() {
        let actual = iterator_token_parser().parse(vec![Token::create_test_token(
            FIELD,
            1,
            TokenType::Field,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Field
    #[doc(hidden)]
    #[test]
    fn parse_valid_field() {
        let expected_token = Token::create_test_token(FIELD, 1, TokenType::Field);
        let actual = field_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Field aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_field() {
        let actual =
            field_token_parser().parse(vec![Token::create_test_token("FELD", 1, TokenType::Ident)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Expression
    #[doc(hidden)]
    #[test]
    fn parse_valid_expression() {
        let expected_token = Token::create_test_token(EXPRESSION, 1, TokenType::Expression);
        let actual = expression_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Expression aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_expression() {
        let actual = expression_token_parser().parse(vec![Token::create_test_token(
            FIELD,
            1,
            TokenType::Field,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Union
    #[doc(hidden)]
    #[test]
    fn parse_valid_union() {
        let expected_token = Token::create_test_token(UNION, 1, TokenType::Union);
        let actual = union_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Union aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_union() {
        let actual = union_parser().parse(vec![Token::create_test_token(
            SEMICOLON,
            1,
            TokenType::SemiColon,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens CsvPerRow
    #[doc(hidden)]
    #[test]
    fn parse_valid_csv_per_row() {
        let expected_token = Token::create_test_token(CSV_PER_ROW, 1, TokenType::CsvPerRow);
        let actual = csv_per_row_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens CsvPerRow aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_csv_per_row() {
        let actual = csv_per_row_parser().parse(vec![Token::create_test_token(
            "csvperrow",
            1,
            TokenType::Ident,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens SqlType
    #[doc(hidden)]
    #[test]
    fn parse_valid_sql_type() {
        let expected_token = Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType);
        let actual = sql_type_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens SqlType aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_sql_type() {
        let actual =
            sql_type_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Ident
    #[doc(hidden)]
    #[test]
    fn parse_valid_identifier() {
        let expected_token = Token::create_test_token("ident", 1, TokenType::Ident);
        let actual = identifier_parser("PREFIX").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Ident aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_identifier() {
        let actual = identifier_parser("SOURCE").parse(vec![Token::create_test_token(
            COLON,
            1,
            TokenType::Colon,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens URI
    #[doc(hidden)]
    #[test]
    fn parse_valid_uri() {
        let expected_token = Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri);
        let actual = uri_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens URI aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_uri() {
        let actual = uri_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens JDBC URL
    #[doc(hidden)]
    #[test]
    fn parse_valid_jdbc_url() {
        let expected_token =
            Token::create_test_token("jdbc:mysql://localhost:3306/mydb", 1, TokenType::JdbcUrl);
        let actual = jdbc_url_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens JDBC URL aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_jdbc_url() {
        let actual = jdbc_url_parser().parse(vec![Token::create_test_token(
            "https://ejemplo.com",
            1,
            TokenType::Uri,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Path
    #[doc(hidden)]
    #[test]
    fn parse_valid_path() {
        let expected_token = Token::create_test_token("ejemplo/fichero.csv", 1, TokenType::Path);
        let actual = path_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Path aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_path() {
        let actual = path_parser().parse(vec![Token::create_test_token(
            "ejemplo",
            1,
            TokenType::Ident,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens SqlQuery
    #[doc(hidden)]
    #[test]
    fn parse_valid_sql_query() {
        let expected_token =
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery);
        let actual = sql_query_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens SqlQuery aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_sql_query() {
        let actual = sql_query_token_parser().parse(vec![Token::create_test_token(
            COLON,
            1,
            TokenType::Colon,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Colon (:)
    #[doc(hidden)]
    #[test]
    fn parse_valid_colon() {
        let expected_token = Token::create_test_token(COLON, 1, TokenType::Colon);
        let actual = colon_parser("antes del identificador").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Colon (:) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_colon() {
        let actual = colon_parser("antes del identificador").parse(vec![Token::create_test_token(
            "ident",
            1,
            TokenType::Ident,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Semicolon (;)
    #[doc(hidden)]
    #[test]
    fn parse_valid_semicolon() {
        let expected_token = Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon);
        let actual = semicolon_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Semicolon (;) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_semicolon() {
        let actual =
            semicolon_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens AccessDot (.)
    #[doc(hidden)]
    #[test]
    fn parse_valid_access_dot_parser() {
        let expected_token = Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot);
        let actual = access_dot_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens AccessDot (.) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_access_dot() {
        let actual =
            access_dot_parser().parse(vec![Token::create_test_token("ident", 1, TokenType::Ident)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens LeftAngleBracket (<)
    #[doc(hidden)]
    #[test]
    fn parse_valid_left_angle_bracket() {
        let expected_token =
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket);
        let actual = left_angle_bracket_parser("URI").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens LeftAngleBracket (<) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_left_angle_bracket() {
        let actual = left_angle_bracket_parser("URI").parse(vec![Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            1,
            TokenType::RightAngleBracket,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens RightAngleBracket (>)
    #[doc(hidden)]
    #[test]
    fn parse_valid_right_angle_bracket() {
        let expected_token =
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket);
        let actual = right_angle_bracket_parser("URI").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens RightAngleBracket (>) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_right_angle_bracket() {
        let actual = right_angle_bracket_parser("URI").parse(vec![Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            1,
            TokenType::LeftAngleBracket,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens OpeningCurlyBrackets ({)
    #[doc(hidden)]
    #[test]
    fn parse_valid_opening_curly_bracket() {
        let expected_token =
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace);
        let actual = opening_curly_brace_parser("ITERATOR").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens OpeningCurlyBracket ({) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_opening_curly_bracket() {
        let actual = opening_curly_brace_parser("ITERATOR").parse(vec![Token::create_test_token(
            CLOSING_CURLY_BRACE,
            1,
            TokenType::ClosingCurlyBrace,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens ClosingCurlyBrackets (})
    #[doc(hidden)]
    #[test]
    fn parse_valid_closing_curly_bracket() {
        let expected_token =
            Token::create_test_token(CLOSING_CURLY_BRACE, 1, TokenType::ClosingCurlyBrace);
        let actual = closing_curly_brace_parser("ITERATOR").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens ClosingCurlyBrackets (}) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_closing_curly_bracket() {
        let actual = closing_curly_brace_parser("ITERATOR").parse(vec![Token::create_test_token(
            OPENING_CURLY_BRACE,
            1,
            TokenType::OpeningCurlyBrace,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens LeftBracket ([)
    #[doc(hidden)]
    #[test]
    fn parse_valid_left_bracket() {
        let expected_token = Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket);
        let actual = left_bracket_parser("tupla").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens LeftBracket ([) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_left_bracket() {
        let actual = left_bracket_parser("tupla").parse(vec![Token::create_test_token(
            RIGHT_BRACKET,
            1,
            TokenType::RightBracket,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens RightBracket (])
    #[doc(hidden)]
    #[test]
    fn parse_valid_right_bracket() {
        let expected_token = Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket);
        let actual = right_bracket_parser("tupla").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens LeftBracket ([) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_right_bracket() {
        let actual = right_bracket_parser("tupla").parse(vec![Token::create_test_token(
            LEFT_BRACKET,
            1,
            TokenType::LeftBracket,
        )]);
        check_error(actual);
    }

    /// Comprueba que no se parsean como tokens ClosingCurlyBrackets (}) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_closing_opening_curly_bracket() {
        let actual = closing_curly_brace_parser("ITERATOR").parse(vec![Token::create_test_token(
            OPENING_CURLY_BRACE,
            1,
            TokenType::OpeningCurlyBrace,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsea el token EOF
    #[doc(hidden)]
    #[test]
    fn parse_eof() {
        let expected_token = Token::create_test_token(EOF, 1, TokenType::EOF);
        let actual = eof_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que el resultado actual del test es igual al esperado
    ///
    /// # Parámetros
    /// * `expected` - El token esperado
    /// * `actual` - El token parseado real
    fn check_ok(expected: Token, actual: Result<Token, Vec<Simple<Token>>>) {
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el resultado actual del test es un error
    ///
    /// # Parámetros
    /// * `actual` - Un Result con el error esperado
    fn check_error(actual: Result<Token, Vec<Simple<Token>>>) {
        assert!(
            actual.is_err(),
            "Se esperaba un error, pero se obtuvo: {:?}",
            actual
        );
    }
}

/// Módulo para los tests del analizador sintáctico
///
/// Contiene los tests que se encargan de comprobar que los diferentes parsers del analizador sintáctico funcionan correctamente
#[cfg(test)]
mod sintax_tests {
    use std::vec;

    use chumsky::error::SimpleReason;

    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que el parser general del AST es capaz de generarlo
    #[doc(hidden)]
    #[test]
    fn ast_parser_with_valid_sintax() {
        let mut tokens_vector: Vec<Token> = TestUtilities::create_valid_prefix_test(1);
        tokens_vector.append(&mut TestUtilities::create_valid_source_test(2));
        tokens_vector.append(&mut TestUtilities::create_valid_query_test(3));
        tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(4));
        tokens_vector.append(&mut TestUtilities::create_valid_expression_test(10));
        tokens_vector.append(&mut TestUtilities::create_valid_shape_test(11));
        tokens_vector.append(&mut vec![Token::create_test_token(EOF, 17, TokenType::EOF)]);

        let prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        let sources = TestUtilities::create_sources_for_ast(
            "films_database",
            SourceDefinition::JdbcURL("jdbc:mysql://localhost:3306/mydb".to_string()),
            2,
        );
        let queries =
            TestUtilities::create_queries_for_ast("inline_query", "SELECT * FROM example;", 3);
        let iterators = TestUtilities::create_default_iterators_for_ast(4);
        let expressions = TestUtilities::create_default_expressions_for_ast(10);
        let shapes = TestUtilities::create_default_shapes_for_ast(11);

        let expected = AST::new(prefixes, sources, queries, iterators, expressions, shapes);

        let actual = ast_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general del AST es capaz de generar el nodo raíz del AST si no hay query y no hay errores sintácticos
    #[doc(hidden)]
    #[test]
    fn ast_parser_with_valid_sintax_and_withouth_query() {
        let mut tokens_vector: Vec<Token> = TestUtilities::create_valid_prefix_test(1);
        tokens_vector.append(&mut TestUtilities::create_valid_source_test(2));
        tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(3));
        tokens_vector.append(&mut TestUtilities::create_valid_expression_test(9));
        tokens_vector.append(&mut TestUtilities::create_valid_shape_test(10));
        tokens_vector.append(&mut vec![Token::create_test_token(EOF, 14, TokenType::EOF)]);

        let prefixes = TestUtilities::create_prefixes_for_ast("example", "https://example.com/", 1);
        let sources = TestUtilities::create_sources_for_ast(
            "films_database",
            SourceDefinition::JdbcURL("jdbc:mysql://localhost:3306/mydb".to_string()),
            2,
        );
        let iterators = TestUtilities::create_default_iterators_for_ast(3);
        let expressions = TestUtilities::create_default_expressions_for_ast(9);
        let shapes = TestUtilities::create_default_shapes_for_ast(10);

        let expected = AST::new(prefixes, sources, None, iterators, expressions, shapes);

        let actual = ast_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general del AST lo genera si no hay prefixes
    #[doc(hidden)]
    #[test]
    fn ast_parser_withouth_prefixes() {
        let mut fail_tokens_vector: Vec<Token> = TestUtilities::create_valid_source_test(1);
        fail_tokens_vector.append(&mut TestUtilities::create_valid_query_test(2));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(3));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_expression_test(9));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_shape_test(10));
        fail_tokens_vector.append(&mut vec![Token::create_test_token(EOF, 16, TokenType::EOF)]);

        check_parser_error::<AST>(
            ast_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX en la línea 1",
        );
    }

    /// Comprueba que el parser general del AST no genera el nodo raíz del AST si no hay sources
    #[doc(hidden)]
    #[test]
    fn ast_parser_withouth_sources() {
        let mut fail_tokens_vector: Vec<Token> = TestUtilities::create_valid_prefix_test(1);
        fail_tokens_vector.append(&mut TestUtilities::create_valid_query_test(2));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(3));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_expression_test(9));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_shape_test(10));
        fail_tokens_vector.append(&mut vec![Token::create_test_token(EOF, 16, TokenType::EOF)]);

        check_parser_error::<AST>(
            ast_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX o un SOURCE en la línea 2",
        );
    }

    /// Comprueba que el parser general del AST no lo genera si no hay expressions
    #[doc(hidden)]
    #[test]
    fn ast_parser_withouth_expressions() {
        let mut fail_tokens_vector: Vec<Token> = TestUtilities::create_valid_prefix_test(1);
        fail_tokens_vector.append(&mut TestUtilities::create_valid_source_test(2));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_query_test(3));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(4));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_shape_test(10));
        fail_tokens_vector.append(&mut vec![Token::create_test_token(EOF, 16, TokenType::EOF)]);

        check_parser_error::<AST>(
            ast_parser(),
            fail_tokens_vector,
            "Se esperaba un EXPRESSION en la línea 10",
        );
    }

    /// Comprueba que el parser general del AST no lo genera si no hay Shapes
    #[doc(hidden)]
    #[test]
    fn ast_parser_withouth_shapes() {
        let mut fail_tokens_vector: Vec<Token> = TestUtilities::create_valid_prefix_test(1);
        fail_tokens_vector.append(&mut TestUtilities::create_valid_source_test(2));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_query_test(3));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_iterator_test(4));
        fail_tokens_vector.append(&mut TestUtilities::create_valid_expression_test(10));
        fail_tokens_vector.append(&mut vec![Token::create_test_token(EOF, 11, TokenType::EOF)]);

        check_parser_error::<AST>(
            ast_parser(),
            fail_tokens_vector,
            "Faltan los ':' antes del identificador en la Shape en la línea 11",
        );
    }

    /// Comprueba que el parser de Prefix parsea la secuencia de tokens: Prefix Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_prefix_sintax() {
        let prefix_ident_1 = Token::create_test_token("ident", 1, TokenType::Ident);
        let prefix_uri_1 = Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri);

        let mut tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            prefix_ident_1.clone(),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            prefix_uri_1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = PrefixASTNode::new(
            Some(prefix_ident_1),
            prefix_uri_1.clone(),
            Position::new(prefix_uri_1.get_num_line()),
        );
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más PREFIX
        let prefix_ident_2 = Token::create_test_token("ident2", 2, TokenType::Ident);
        let prefix_uri_2 = Token::create_test_token("https://ejemplo2.com", 2, TokenType::Uri);

        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(PREFIX, 2, TokenType::Prefix));
        tokens_vector.push(prefix_ident_2.clone());
        tokens_vector.push(Token::create_test_token(COLON, 2, TokenType::Colon));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(prefix_uri_2.clone());
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = PrefixASTNode::new(
            Some(prefix_ident_2),
            prefix_uri_2.clone(),
            Position::new(prefix_uri_2.get_num_line()),
        );

        let expected_vector = vec![expected, expected2];
        let actual = prefix_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Prefix parsea la secuencia de tokens: Prefix Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_identifier() {
        let uri = Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri);
        let tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            uri.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = PrefixASTNode::new(None, uri.clone(), Position::new(uri.get_num_line()));
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_prefix() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident LeftAngleBracket Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_colon() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Faltan los ':' después del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la URI en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_uri() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba una URI entre '<' y '>' en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de la URI en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la URI en la línea 1",
        );
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_uri() {
        let ident1 = Token::create_test_token("ident", 1, TokenType::Ident);
        let uri1 = Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri);

        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            ident1.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            uri1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode::new(
            ident1.clone(),
            SourceDefinition::URI(uri1.get_lexeme()),
            Position::new(ident1.get_num_line()),
        );
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let ident2 = Token::create_test_token("ident2", 2, TokenType::Ident);
        let uri2 = Token::create_test_token("https://ejemplo2.com/fichero.csv", 2, TokenType::Uri);

        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(ident2.clone());
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(uri2.clone());
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode::new(
            ident2.clone(),
            SourceDefinition::URI(uri2.get_lexeme()),
            Position::new(ident2.get_num_line()),
        );

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket JdbcUrl RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_jdbc_url() {
        let ident1 = Token::create_test_token("ident", 1, TokenType::Ident);
        let jdbc_url1 =
            Token::create_test_token("jdbc:mysql://localhost:3306/mydb", 1, TokenType::JdbcUrl);

        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            ident1.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            jdbc_url1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode::new(
            ident1.clone(),
            SourceDefinition::JdbcURL(jdbc_url1.get_lexeme()),
            Position::new(ident1.get_num_line()),
        );
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let ident2 = Token::create_test_token("ident2", 2, TokenType::Ident);
        let jdbc_url2 = Token::create_test_token(
            "jdbc:mysql://localhost:3356/anotherdb",
            2,
            TokenType::JdbcUrl,
        );

        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(ident2.clone());
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(jdbc_url2.clone());
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode::new(
            ident2.clone(),
            SourceDefinition::JdbcURL(jdbc_url2.get_lexeme()),
            Position::new(ident2.get_num_line()),
        );

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket Path RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_path() {
        let ident1 = Token::create_test_token("ident", 1, TokenType::Ident);
        let path1 = Token::create_test_token("file://ejemplo/fichero.csv", 1, TokenType::Path);

        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            ident1.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            path1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode::new(
            ident1.clone(),
            SourceDefinition::Path(path1.get_lexeme()),
            Position::new(ident1.get_num_line()),
        );
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let ident2 = Token::create_test_token("ident2", 2, TokenType::Ident);
        let path2 = Token::create_test_token(
            "file://C:\\ejemplo\\path\\a\\fichero\\fichero.csv",
            2,
            TokenType::Path,
        );

        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(ident2.clone());
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(path2.clone());
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode::new(
            ident2.clone(),
            SourceDefinition::Path(path2.get_lexeme()),
            Position::new(ident2.get_num_line()),
        );

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_source() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX o un SOURCE en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de SOURCE en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_url_or_path() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba una URI entre '<' y '>' en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<SourceASTNode>>(
            source_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Query parsea la secuencia de tokens: Query Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_query_sintax() {
        let ident1 = Token::create_test_token("ident", 1, TokenType::Ident);
        let sql_query1 = Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery);

        let mut tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            ident1.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            sql_query1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = QueryASTNode::new(
            ident1.clone(),
            sql_query1,
            Position::new(ident1.get_num_line()),
        );
        let actual = query_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más QUERY
        let ident2 = Token::create_test_token("ident2", 2, TokenType::Ident);
        let sql_query2 = Token::create_test_token(
            "SELECT * FROM example WHERE id = '1' LIMIT 100;",
            2,
            TokenType::SqlQuery,
        );

        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(QUERY, 2, TokenType::Query));
        tokens_vector.push(ident2.clone());
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(SQL_TYPE, 2, TokenType::SqlType));
        tokens_vector.push(sql_query2.clone());
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = QueryASTNode::new(
            ident2.clone(),
            sql_query2,
            Position::new(ident2.get_num_line()),
        );

        let expected_vector = vec![expected, expected2];
        let actual = query_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_query() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba un QUERY en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de QUERY en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_type() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba 'sql:' después de '<' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_query() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba una consulta SQL entre '<' y '>' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<QueryASTNode>>(
            query_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Field parsea la secuencia de tokens: Field Ident LeftAngleBracket Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_field_sintax() {
        let field = Token::create_test_token("field", 1, TokenType::Ident);
        let access_field = Token::create_test_token("field", 1, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            field.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            access_field.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = FieldASTNode::new(
            field.clone(),
            access_field,
            Position::new(field.get_num_line()),
        );
        let actual = field_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un FIELD en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field LeftAngleBracket Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de FIELD en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_access_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de < en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident LeftAngleBracket Ident
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident Ident
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_sql_query() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let sql_query = Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery);
        let field_ident = Token::create_test_token("field", 2, TokenType::Ident);
        let field_access = Token::create_test_token("field", 2, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            sql_query.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            field_ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            field_access.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let fields = vec![FieldASTNode::new(
            field_ident.clone(),
            field_access,
            Position::new(field_ident.get_num_line()),
        )];
        let expected = IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::SqlQuery(sql_query.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        );

        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    /// con más de un field
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_more_than_one_field() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let sql_query = Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery);
        let field_ident1 = Token::create_test_token("field", 2, TokenType::Ident);
        let field_access1 = Token::create_test_token("field", 2, TokenType::Ident);
        let field_ident2 = Token::create_test_token("field2", 3, TokenType::Ident);
        let field_access2 = Token::create_test_token("field", 3, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            sql_query.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            field_ident1.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            field_access1.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 3, TokenType::Field),
            field_ident2.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            field_access2.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 4, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 4, TokenType::EOF),
        ];

        let fields = vec![
            FieldASTNode::new(
                field_ident1.clone(),
                field_access1,
                Position::new(field_ident1.get_num_line()),
            ),
            FieldASTNode::new(
                field_ident2.clone(),
                field_access2,
                Position::new(field_ident2.get_num_line()),
            ),
        ];

        let expected = IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::SqlQuery(sql_query.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        );
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_ident() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let access_ident = Token::create_test_token("inline_query", 1, TokenType::Ident);
        let field_ident = Token::create_test_token("field", 2, TokenType::Ident);
        let field_access = Token::create_test_token("field", 2, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            access_ident.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            field_ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            field_access.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let fields = vec![FieldASTNode::new(
            field_ident.clone(),
            field_access,
            Position::new(field_ident.get_num_line()),
        )];
        let expected = IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::Ident(access_ident.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        );
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_csv_per_row() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let csvperrow = Token::create_test_token(CSV_PER_ROW, 1, TokenType::CsvPerRow);
        let field_ident = Token::create_test_token("field", 2, TokenType::Ident);
        let field_access = Token::create_test_token("field", 2, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            csvperrow.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            field_ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            field_access.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let fields = vec![FieldASTNode::new(
            field_ident.clone(),
            field_access,
            Position::new(field_ident.get_num_line()),
        )];
        let expected = IteratorASTNode::new(
            ident.clone(),
            IteratorAccess::CsvPerRow(csvperrow.get_lexeme()),
            fields,
            Position::new(ident.get_num_line()),
        );
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_iterator() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un ITERATOR en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de ITERATOR en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la consulta SQL, identificador o csvperrow en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de < en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(iterator_parser(), fail_tokens_vector, "Se esperaba un '>' después de la consulta SQL, identificador o csvperrow en la línea 1");
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_opening_curly_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '{' antes de los campos en la línea 2",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_fields() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(CLOSING_CURLY_BRACE, 2, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 2, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un FIELD en la línea 2",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field*
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_closing_curly_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '}' después de los campos en la línea 3",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident (CsvperRow|Ident|SqlType SqlQuery) OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de la consulta SQL, identificador o csvperrow en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket Field*
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_curly_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '{' antes de los campos en la línea 2",
        );
    }

    /// Comprueba que el parser de Access parsea la secuencia de tokens: Ident AccessDot
    #[doc(hidden)]
    #[test]
    fn valid_access_sintax_with_iterator_access() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator = Token::create_test_token("iterator", 1, TokenType::Ident);

        let tokens_vector = vec![
            ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            iterator.clone(),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = AccessASTNode::new(
            ident.clone(),
            iterator,
            None,
            Position::new(ident.get_num_line()),
        );
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Access parsea la secuencia de tokens: Ident AccessDot Ident AccessDot
    #[doc(hidden)]
    #[test]
    fn valid_access_sintax_with_field_access() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator = Token::create_test_token("iterator", 1, TokenType::Ident);
        let field = Token::create_test_token("field", 1, TokenType::Ident);

        let tokens_vector = vec![
            ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            iterator.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            field.clone(),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = AccessASTNode::new(
            ident.clone(),
            iterator,
            Some(field),
            Position::new(ident.get_num_line()),
        );
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: AccessDot Ident AccessDot Ident
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_source_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<AccessASTNode>(
            access_parser(LEFT_ANGLE_BRACKET),
            fail_tokens_vector,
            "Se esperaba un identificador después de < en la línea 1",
        );
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident Ident AccessDot Ident
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_dot_between_source_and_iterator() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<AccessASTNode>(
            access_parser(LEFT_ANGLE_BRACKET),
            fail_tokens_vector,
            "Falta un '.' después del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident AccessDot AccessDot Ident
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_iterator_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<AccessASTNode>(
            access_parser(LEFT_ANGLE_BRACKET),
            fail_tokens_vector,
            "Se esperaba un identificador después de . en la línea 1",
        );
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident AccessDot Ident Ident
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_dot_between_iterator_and_field() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator = Token::create_test_token("iterator", 1, TokenType::Ident);

        let tokens_vector = vec![
            ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            iterator.clone(),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        // No dará error en este parser pero si en el siguiente que se ejecute al estar un ident suelto
        let expected = AccessASTNode::new(
            ident.clone(),
            iterator,
            None,
            Position::new(ident.get_num_line()),
        );
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident AccessDot Ident AccessDot  
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_field_identifier() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator = Token::create_test_token("iterator", 1, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        // No dará error en este parser pero si en el siguiente que se ejecute al estar un '.' suelto
        let expected = AccessASTNode::new(
            ident.clone(),
            iterator,
            None,
            Position::new(ident.get_num_line()),
        );
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Expression parsea la secuencia de tokens:
    /// Expression Ident LeftAngleBracket Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_basic_expression_sintax() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator = Token::create_test_token("iterator", 1, TokenType::Ident);
        let access_ident = Token::create_test_token("id", 1, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            iterator.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            access_ident.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let accesses = vec![AccessASTNode::new(
            iterator.clone(),
            access_ident,
            None,
            Position::new(iterator.get_num_line()),
        )];
        let expected = ExpressionASTNode::new(
            ident.clone(),
            ExpressionType::BASIC,
            accesses,
            Position::new(ident.get_num_line()),
        );

        let actual = expression_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Expression parsea la secuencia de tokens:
    /// Expression Ident LeftAngleBracket Access Union Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_union_expression_sintax() {
        let ident = Token::create_test_token("ident", 1, TokenType::Ident);
        let iterator1 = Token::create_test_token("iterator1", 1, TokenType::Ident);
        let access_ident1 = Token::create_test_token("id1", 1, TokenType::Ident);
        let iterator2 = Token::create_test_token("iterator2", 1, TokenType::Ident);
        let access_ident2 = Token::create_test_token("id2", 1, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            ident.clone(),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            iterator1.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            access_ident1.clone(),
            Token::create_test_token(UNION, 1, TokenType::Union),
            iterator2.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            access_ident2.clone(),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let accesses = vec![
            AccessASTNode::new(
                iterator1.clone(),
                access_ident1,
                None,
                Position::new(iterator1.get_num_line()),
            ),
            AccessASTNode::new(
                iterator2.clone(),
                access_ident2,
                None,
                Position::new(iterator2.get_num_line()),
            ),
        ];
        let expected = ExpressionASTNode::new(
            ident.clone(),
            ExpressionType::UNION,
            accesses,
            Position::new(ident.get_num_line()),
        );

        let actual = expression_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Ident LeftAngleBracket Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_expression() {
        let fail_tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un EXPRESSION en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de EXPRESSION en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de el acceso en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de < en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident LeftAngleBracket Access
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de el acceso en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression básicas no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident Access
    #[doc(hidden)]
    #[test]
    fn expression_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un '<' antes de el acceso en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Union no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident LeftAngleBracket Access Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn union_expression_sintax_withouth_union() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de el acceso en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Union no parsea como tales aquellas secuencias de tokens que son:
    /// Expression Ident LeftAngleBracket Access Union RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn union_expression_sintax_withouth_second_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(UNION, 1, TokenType::Union),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de UNION en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// Ident Colon Ident (Ident Colon Ident)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_ident_prefix() {
        let prefix_ident = Token::create_test_token("example", 1, TokenType::Ident);
        let ident = Token::create_test_token("name", 1, TokenType::Ident);
        let access_ident = Token::create_test_token("films", 1, TokenType::Ident);
        let first_access = Token::create_test_token("name", 1, TokenType::Ident);

        let tokens_vector = vec![
            prefix_ident.clone(),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            ident.clone(),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            access_ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            first_access.clone(),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let object = IdentOrAccess::Access(AccessASTNode::new(
            access_ident.clone(),
            first_access,
            None,
            Position::new(access_ident.get_num_line()),
        ));
        let expected = ShapeTupleASTNode::new(
            Some(prefix_ident),
            ident.clone(),
            None,
            object,
            Position::new(ident.get_num_line()),
        );

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// (Ident Colon Ident) Prefix Colon LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_field_prefix() {
        let prefix_ident = Token::create_test_token("example", 1, TokenType::Ident);
        let ident = Token::create_test_token("name", 1, TokenType::Ident);
        let object_ident = Token::create_test_token("example", 1, TokenType::Ident);
        let access_ident = Token::create_test_token("films", 1, TokenType::Ident);
        let first_access = Token::create_test_token("name", 1, TokenType::Ident);

        let tokens_vector = vec![
            prefix_ident.clone(),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            ident.clone(),
            object_ident.clone(),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            access_ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            first_access.clone(),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let object = IdentOrAccess::Access(AccessASTNode::new(
            access_ident.clone(),
            first_access,
            None,
            Position::new(access_ident.get_num_line()),
        ));
        let expected = ShapeTupleASTNode::new(
            Some(prefix_ident),
            ident.clone(),
            Some(object_ident),
            object,
            Position::new(ident.get_num_line()),
        );

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Ident Colon Ident)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_ident_prefix() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de : en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Ident Colon Ident) (Ident Colon)? (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_left_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un '[' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Ident Colon Ident) (Ident Colon)? LeftBracket RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_ident_or_access_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de [ en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Ident Colon Ident) (Ident Colon)? LeftBracket (Ident|Access) Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_right_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un ']' después de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// Ident Colon Ident (Ident Colon)? LeftBracket (Ident|Access) RightBracket
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_semicolon() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Falta el ';' después del ']' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// Ident Colon Ident (Ident Colon)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_sintax_with_ident_prefix() {
        let ident = Token::create_test_token("Films", 1, TokenType::Ident);
        let access_ident = Token::create_test_token("films", 1, TokenType::Ident);
        let first_access = Token::create_test_token("id", 1, TokenType::Ident);
        let tuple_ident = Token::create_test_token("example", 1, TokenType::Ident);
        let tuple_access = Token::create_test_token("name", 1, TokenType::Ident);
        let tuple_object = Token::create_test_token("films_name", 1, TokenType::Ident);

        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            ident.clone(),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            access_ident.clone(),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            first_access.clone(),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            tuple_ident.clone(),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            tuple_access.clone(),
            Token::create_test_token(LEFT_BRACKET, 2, TokenType::LeftBracket),
            tuple_object.clone(),
            Token::create_test_token(RIGHT_BRACKET, 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let field_identifier = IdentOrAccess::Access(AccessASTNode::new(
            access_ident.clone(),
            first_access,
            None,
            Position::new(access_ident.get_num_line()),
        ));
        let tuples = vec![ShapeTupleASTNode::new(
            Some(tuple_ident),
            tuple_access.clone(),
            None,
            IdentOrAccess::Ident(tuple_object.get_lexeme()),
            Position::new(tuple_access.get_num_line()),
        )];
        let expected = ShapeASTNode::new(
            None,
            ident.clone(),
            None,
            field_identifier,
            tuples,
            Position::new(ident.get_num_line()),
        );

        let actual = shape_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Ident Colon)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_ident_prefix() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            Token::create_test_token("name", 2, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 2, TokenType::LeftBracket),
            Token::create_test_token("films_name", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeASTNode>>(
            shape_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de : en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// Ident Colon Ident (Ident Colon)? (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_left_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            Token::create_test_token("name", 2, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 2, TokenType::LeftBracket),
            Token::create_test_token("films_name", 2, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un '[' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// Ident Colon Ident (Ident Colon)? LeftBracket RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_ident_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            Token::create_test_token("name", 2, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 2, TokenType::LeftBracket),
            Token::create_test_token(RIGHT_BRACKET, 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeASTNode>>(
            shape_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de [ en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// Ident Colon Ident (Ident Colon)? LeftBracket (Ident|Access) Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_right_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 2, TokenType::Colon),
            Token::create_test_token("name", 2, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 2, TokenType::LeftBracket),
            Token::create_test_token("films_name", 2, TokenType::Ident),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeASTNode>>(
            shape_parser(),
            fail_tokens_vector,
            "Se esperaba un ']' después de el identificador en la línea 1",
        );
    }

    /// Realiza una comprobación general de un error de un parser
    ///
    /// #Parámetros
    /// * `fail_tokens_vector` - El vector con los tokens que se quiere ver que dan error en el parser
    /// * `error_message` - El mensaje de error esperado
    fn check_parser_error<T>(
        parser: impl Parser<Token, T, Error = Simple<Token>>,
        fail_tokens_vector: Vec<Token>,
        error_message: &str,
    ) {
        let actual = parser.parse(fail_tokens_vector);
        check_error(actual, error_message);
    }

    /// Comprueba que el resultado actual del test es un error y que el mensaje de este concuerda con el esperado
    ///
    /// Utiliza como tipo genérico el tipo de nodo del AST que se esté testeando
    ///
    /// #Parámetros
    /// * `actual` - El Result con un vector con posibles errores o con nodos
    /// * `error_message` - El mensaje de error esperado
    fn check_error<T>(actual: Result<T, Vec<Simple<Token>>>, expected_error_message: &str) {
        assert!(actual.is_err(), "Se esperaba un error");

        let _ = actual.map_err(|e| {
            let mut error_message_find = false;

            for error in e {
                let actual_error = match error.reason() {
                    SimpleReason::Custom(msg) => msg,
                    _ => "Otro error",
                };

                println!("{}", actual_error);
                if actual_error == expected_error_message {
                    error_message_find = true;
                    break;
                }
            }

            assert!(error_message_find);
        });
    }
}
