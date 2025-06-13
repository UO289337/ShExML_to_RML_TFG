//! Módulo del analizador sintáctico
//!
//! Realiza el análisis sintáctico del compilador
//! Comprueba que los tokens resultado del analizador léxico se encuentran en el orden esperado y genera el AST

use chumsky::combinator::*;
use chumsky::prelude::*;
use chumsky::primitive::*;
use chumsky::Parser;

use crate::model::lexer::token::*;

use super::super::ast::*;
use super::super::lexer::token::TokenType;

/// Parsea los tokens para generar el nodo File del AST
///
/// Realiza el parseo de los tokens de los prefijos y de las fuentes para poder crear el nodo File, que es el nodo raíz del AST
///
/// # Retorna
/// Un nodo File del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn file_parser() -> impl Parser<Token, FileASTNode, Error = Simple<Token>> {
    prefix_parser()
        .or_not()
        .then(source_parser())
        .then(query_parser().or_not())
        .then(iterator_parser())
        .then(expression_parser().or_not())
        .then(shape_parser())
        .then_ignore(eof_parser())
        .map(
            |(((((prefixes, sources), queries), iterators), expressions), shapes)| FileASTNode {
                prefixes,
                sources,
                queries,
                iterators,
                expressions,
                shapes,
            },
        )
}

/// Parsea los tokens para generar un nodo Prefix del AST
///
/// Realiza el parseo de los tokens con la secuencia: Prefix Ident Colon LeftAngleBrackey Uri RightAngleBracket
///
/// # Retorna
/// Un vector con nodos Prefix del AST
///
/// # Errores
/// Devuelve un  `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn prefix_parser() -> impl Parser<Token, Vec<PrefixASTNode>, Error = Simple<Token>> {
    prefix_token_parser()
        .then(identifier_parser(PREFIX))
        .then_ignore(colon_parser())
        .then(uri_with_angle_brackets_parser())
        .map(|((_, ident), uri)| PrefixASTNode {
            identifier: ident.lexeme.clone(),
            uri: uri.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Source del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia: Source Ident (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
///
/// # Retorna
/// Un vector con nodos Source del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn source_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {
    source_token_parser()
        .then(identifier_parser(SOURCE))
        .then_ignore(left_angle_bracket_parser("la URL o ruta"))
        .then(
            uri_parser()
                .or(file_path_parser())
                .or(path_parser())
                .or(jdbc_url_parser()),
        )
        .then_ignore(right_angle_bracket_parser("la URL o ruta"))
        .map(|((_, ident), source_definition)| SourceASTNode {
            identifier: ident.lexeme.clone(),
            source_definition: source_definition.lexeme.clone(),
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
fn query_parser() -> impl Parser<Token, Vec<QueryASTNode>, Error = Simple<Token>> {
    query_token_parser()
        .then(identifier_parser(QUERY))
        .then_ignore(left_angle_bracket_parser("la consulta SQL"))
        .then_ignore(sql_type_parser())
        .then(sql_query_token_parser())
        .then_ignore(right_angle_bracket_parser("la consulta SQL"))
        .map(|((_, ident), sql_query)| QueryASTNode {
            identifier: ident.lexeme.clone(),
            sql_query: sql_query.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Iterator del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Iterator Ident LeftAngleBracket (CsvPerRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBrace Fields CLosingCurlyBrace
///
/// # Retorna
/// Un vector con nodos Query del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn iterator_parser() -> impl Parser<Token, Vec<IteratorASTNode>, Error = Simple<Token>> {
    iterator_token_parser()
        .then(identifier_parser(ITERATOR))
        .then_ignore(left_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow",
        ))
        .then(
            // Es necesario utilizar tuplas para poner concatenar el SqlType y el SqlQuery
            identifier_parser(LEFT_ANGLE_BRACKET)
                .map(|token| (token, None))
                .or(csv_per_row_parser().map(|token| (token, None)))
                .or(sql_type_parser()
                    .then(sql_query_token_parser())
                    .map(|(sql_type, sql_query)| (sql_type, Some(sql_query)))),
        )
        .then_ignore(right_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow",
        ))
        .then_ignore(opening_curly_brace_parser("los campos"))
        .then(field_parser())
        .then_ignore(closing_curly_brace_parser("los campos"))
        .map(|(((_, ident), iterator_access), fields)| {
            create_iterator_ast_node(ident, iterator_access, fields)
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Crea un nodo Iterator del AST
///
/// Crea, a partir de los datos del parser de Iterator, el nodo AST de este
///
/// # Parámetros
/// * `ident` - El token identificador del Iterator
/// * `iterator_access` - Una tupla con el token Ident o CsvPerRow o con los tokens SqlType y SqlQuery
/// * `fields` - El vector de fields del Iterator
///
/// # Retorna
/// Un nodo Iterator del AST
fn create_iterator_ast_node(
    ident: Token,
    iterator_access: (Token, Option<Token>),
    fields: Vec<FieldASTNode>,
) -> IteratorASTNode {
    let (token1, token2): (Token, Option<Token>) = iterator_access;

    if token2.is_none() {
        IteratorASTNode {
            identifier: ident.lexeme.clone(),
            iterator_access: token1.lexeme.clone(),
            fields: fields,
        }
    } else {
        IteratorASTNode {
            identifier: ident.lexeme.clone(),
            iterator_access: token2.unwrap().lexeme.clone().to_string(),
            fields: fields,
        }
    }
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
fn field_parser() -> impl Parser<Token, Vec<FieldASTNode>, Error = Simple<Token>> {
    field_token_parser()
        .then(identifier_parser(FIELD))
        .then_ignore(left_angle_bracket_parser("el identificador"))
        .then(identifier_parser(LEFT_ANGLE_BRACKET).or(key_identifier_parser()))
        .then_ignore(right_angle_bracket_parser("el identificador"))
        .map(|((_, ident), access_ident)| FieldASTNode {
            field_identifier: ident.lexeme.clone(),
            access_field_identifier: access_ident.lexeme.clone(),
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Parsea los tokens para generar un nodo Expression del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// Expression Ident LeftAngleBracket Access (UNION|JOIN) Access (Substituting Access|On Access Equal Access) RightAngleBracket
///
/// # Retorna
/// Un vector con nodos Expression del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn expression_parser() -> impl Parser<Token, Vec<ExpressionASTNode>, Error = Simple<Token>> {
    expression_token_parser()
        .then(identifier_parser(EXPRESSION))
        .then_ignore(left_angle_bracket_parser("el acceso"))
        .then(access_parser(LEFT_ANGLE_BRACKET))
        .then(
            union_parser()
                .then(access_parser(UNION))
                .map(|(union, access)| (union, access, None, None))
                .or(join_parser()
                    .then(access_parser(JOIN))
                    .then_ignore(on_parser())
                    .then(access_parser(ON))
                    .then_ignore(equal_parser())
                    .then(access_parser(EQUAL))
                    .map(|(((join, access_join), access_on), access_equal)| {
                        (join, access_join, Some(access_on), Some(access_equal))
                    }))
                .or_not(),
        )
        .then_ignore(right_angle_bracket_parser("el acceso"))
        .map(|(((_, ident), iterator_access), more_accesses)| {
            create_expression_node(ident, iterator_access, more_accesses)
        })
        .repeated()
        .at_least(1)
        .collect()
}

/// Crea un nodo Expression del AST
///
/// A partir de la información sacada del parser crea un nodo expresión del AST
///
/// # Parámetros
/// * `ident` - El identificado de la expresión
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `union_or_join` - Un Option con el token UNION, JOIN o ninguno indicando el tipo de la expresión
/// * `more_accesses` - Un Option con los posibles accesos a: el JOIN O UNION, el ON y el que puede haber después del =
///
/// # Retorna
/// Un nodo ExpressionASTNode
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn create_expression_node(
    ident: Token,
    iterator_access: AccessASTNode,
    more_accesses: Option<(
        Token,
        AccessASTNode,
        Option<AccessASTNode>,
        Option<AccessASTNode>,
    )>,
) -> ExpressionASTNode {
    let basic_expression = more_accesses.is_none();
    let (join_or_union_token, accesses) = create_vec_of_accesses(iterator_access, more_accesses);

    if basic_expression {
        return ExpressionASTNode {
            identifier: ident.lexeme.clone(),
            expression_type: ExpressionType::BASIC,
            accesses,
        };
    }

    let expression_type = ExpressionType::from(join_or_union_token.unwrap());

    if expression_type == ExpressionType::UNION {
        ExpressionASTNode {
            identifier: ident.lexeme.clone(),
            expression_type: ExpressionType::UNION,
            accesses,
        }
    } else {
        ExpressionASTNode {
            identifier: ident.lexeme.clone(),
            expression_type: ExpressionType::JOIN,
            accesses,
        }
    }
}

/// Crea un vector de nodos AccessASTNode
///
/// A partir de los accesos de una expresión, crea su nodo de vectores Access del AST
///
/// # Parámetros
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `more_accesses` - Un Option con los posibles accesos a: el JOIN O UNION, el ON y el que puede haber después del =
///
/// # Retorna
/// Una tupla con un Option con el JOIN o UNION y un vector con todos los accesos de una expresión
fn create_vec_of_accesses(
    iterator_access: AccessASTNode,
    more_accesses: Option<(
        Token,
        AccessASTNode,
        Option<AccessASTNode>,
        Option<AccessASTNode>,
    )>,
) -> (Option<Token>, Vec<AccessASTNode>) {
    let mut accesses = vec![iterator_access];
    let mut token: Option<Token> = None;

    if more_accesses.is_some() {
        let (tok, field_access, on_access, equal_access) = more_accesses.unwrap();
        token = Some(tok);
        accesses.push(field_access);

        if on_access.is_some() && equal_access.is_some() {
            accesses.push(on_access.unwrap());
            accesses.push(equal_access.unwrap());
        }
    }

    (token, accesses)
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
        .then(
            access_dot_parser()
                .then(identifier_parser(ACCESS_DOT))
                .or_not(),
        )
        .map(|((ident, iterator_accessed), field_accessed)| {
            create_access_node(ident, iterator_accessed, field_accessed)
        })
}

/// Crea un nodo Access del AST
///
/// A partir de una serie de identificadores y puntos de acceso (.), crea un nodo AccessASTNode
///
/// # Parámetros
/// * `ident` - El identificador de un SOURCE
/// * `iterator_accessed` - El identificador del iterador accedido
/// * `field_accesed` - El identificador del campo del iterador accedido
///
/// # Retorna
/// Un nodo Access del AST
fn create_access_node(
    ident: Token,
    iterator_accessed: Token,
    field_accessed: Option<(Token, Token)>,
) -> AccessASTNode {
    if field_accessed.is_some() {
        return AccessASTNode {
            identifier: ident.lexeme.clone(),
            iterator_accessed: iterator_accessed.lexeme.clone(),
            field_accessed: Some(field_accessed.unwrap().1.lexeme.clone()), // El 1 apunta al identificador; el 0 al punto ('.')
        };
    }

    AccessASTNode {
        identifier: ident.lexeme.clone(),
        iterator_accessed: iterator_accessed.lexeme.clone(),
        field_accessed: None,
    }
}

/// Parsea los tokens para generar el nodo Shape del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// (Prefix Colon|LeftAngelBracket Uri RightAngleBracket) Ident (Prefix Colon|LeftAngelBracket Uri RightAngleBracket) LeftBracket (Ident|Access) RightBracket OpeningCurlyBrackets ShapeTuples ClosingCurlyBrackets
///
/// # Retorna
/// Un vector con nodos Shape del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn shape_parser() -> impl Parser<Token, Vec<ShapeASTNode>, Error = Simple<Token>> {
    prefix_or_uri_parser()
        .then(
            colon_parser()
                .map(|_| None)
                .or(uri_with_angle_brackets_parser().map(|uri| Some(uri))),
        )
        .then_ignore(left_bracket_parser("identificador"))
        .then(
            identifier_parser(LEFT_BRACKET)
                .map(|token| (Some(token), None))
                .or(access_parser(LEFT_BRACKET).map(|access| (None, Some(access)))),
        )
        .then_ignore(right_bracket_parser("identificador"))
        .then_ignore(opening_curly_brace_parser("las tuplas de la Shape"))
        .then(shape_tuple_parser())
        .then_ignore(closing_curly_brace_parser("las tuplas de la Shape"))
        .map(
            |(((shape_prefix_or_uri, shape_field_prefix_or_uri), field_ident), shape_tuples)| {
                create_shape_node(
                    shape_prefix_or_uri,
                    shape_field_prefix_or_uri,
                    field_ident,
                    shape_tuples,
                )
            },
        )
        .repeated()
        .at_least(1)
        .collect()
}

/// Crea un nodo Shape del AST
///
/// # Retorna
/// Un nodo ShapeASTNode del AST
///
/// # Parámetros
/// * `shape_prefix_or_uri` - Una tupla con el Token del Prefix y el Option de la URI
/// * `shape_field_prefix_or_uri` - Un Option con la Uri del field de la Shape
/// * `field_ident` - Una tupla con 2 Option, uno con el token del Ident y otro con el token del nodo Access
/// * `shape_tuples` - Un vector con las tuplas de la Shape
fn create_shape_node(
    shape_prefix_or_uri: (Token, Option<Token>),
    shape_field_prefix_or_uri: Option<Token>,
    field_ident: (Option<Token>, Option<AccessASTNode>),
    shape_tuples: Vec<ShapeTupleASTNode>,
) -> ShapeASTNode {
    let (shape_ident, shape_uri) = shape_prefix_or_uri;
    let (shape_field_ident, shape_field_access) = field_ident;
    let mut shape_prefix_or_uri = PrefixOrURI::Prefix;
    let mut sh_field_prefix_or_uri = PrefixOrURI::Prefix;

    if shape_uri.is_some() {
        shape_prefix_or_uri = PrefixOrURI::URI(shape_uri.unwrap().lexeme);

        if shape_field_prefix_or_uri.is_some() {
            sh_field_prefix_or_uri = PrefixOrURI::URI(shape_field_prefix_or_uri.unwrap().lexeme)
        }
    }

    let field_ident;

    if shape_field_ident.is_some() {
        field_ident = IdentOrAccess::Ident(shape_field_ident.unwrap().lexeme);
    } else {
        field_ident = IdentOrAccess::Access(shape_field_access.unwrap());
    }

    ShapeASTNode {
        prefix_or_uri: shape_prefix_or_uri,
        identifier: shape_ident.lexeme,
        field_prefix_or_uri: sh_field_prefix_or_uri,
        field_identifier: field_ident,
        tuples: shape_tuples,
    }
}

/// Parsea los tokens para generar el nodo ShapeTuples del AST
///
/// Realiza el parseo de los tokens para parsear la secuencia:
/// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
///
/// # Retorna
/// Un vector con nodos Shape del AST
///
/// # Errores
/// Devuelve un `Simple<Token>` si ocurre un error durante el parseo de los tokens
fn shape_tuple_parser() -> impl Parser<Token, Vec<ShapeTupleASTNode>, Error = Simple<Token>> {
    prefix_or_uri_parser()
        .then(
            colon_parser()
                .map(|_| None)
                .or(uri_with_angle_brackets_parser().map(|uri| Some(uri)))
                .or_not(),
        )
        .then_ignore(left_bracket_parser("identificador"))
        .then(
            identifier_parser(LEFT_BRACKET)
                .map(|token| (Some(token), None))
                .or(access_parser(LEFT_BRACKET).map(|access| (None, Some(access)))),
        )
        .then_ignore(right_bracket_parser("identificador"))
        .then_ignore(semicolon_parser())
        .map(
            |((tuple_prefix_or_uri, object_prefix_or_uri), tuple_object)| {
                create_shape_tuple_node(tuple_prefix_or_uri, object_prefix_or_uri, tuple_object)
            },
        )
        .repeated()
        .at_least(1)
        .collect()
}

/// Crea un nodo ShapeTuple del AST
///
/// # Retorna
/// Un nodo ShapeTuplesASTNode del AST
///
/// # Parámetros
/// * `tuple_prefix_or_uri` - Una tupla con el Token del Prefix y el Option de la URI
/// * `object_prefix_or_uri` - Un Option con otro Option con el token de la URI; el primer Option indica la opcionalidad del uso de Prefix o URI
/// * `tuple_object` - Una tupla con 2 Option, uno con el token del Ident y otro con el token del nodo Access
fn create_shape_tuple_node(
    tuple_prefix_or_uri: (Token, Option<Token>),
    object_prefix_or_uri: Option<Option<Token>>,
    tuple_object: (Option<Token>, Option<AccessASTNode>),
) -> ShapeTupleASTNode {
    let (tuple_ident, tuple_uri) = tuple_prefix_or_uri;
    let (tuple_object_ident, tuple_field_access) = tuple_object;
    let mut tuple_prefix_or_uri = PrefixOrURI::Prefix;
    let mut tuple_object_prefix_or_uri = Some(PrefixOrURI::Prefix);
    let is_object_prefix_or_uri = object_prefix_or_uri.is_some();

    if tuple_uri.is_some() {
        tuple_prefix_or_uri = PrefixOrURI::URI(tuple_uri.unwrap().lexeme);

        if is_object_prefix_or_uri && object_prefix_or_uri.as_ref().unwrap().is_some() {
            tuple_object_prefix_or_uri = Some(PrefixOrURI::URI(
                object_prefix_or_uri.unwrap().unwrap().lexeme,
            ));
        } else if !is_object_prefix_or_uri {
            tuple_object_prefix_or_uri = None;
        }
    }

    let object_ident;

    if tuple_object_ident.is_some() {
        object_ident = IdentOrAccess::Ident(tuple_object_ident.unwrap().lexeme);
    } else {
        object_ident = IdentOrAccess::Access(tuple_field_access.unwrap());
    }

    ShapeTupleASTNode {
        prefix_or_uri: tuple_prefix_or_uri,
        identifier: tuple_ident.lexeme,
        object_prefix_or_uri: tuple_object_prefix_or_uri,
        object: object_ident,
    }
}

// Parsers auxiliares

/// Realiza el parseo de un Prefix o Uri, junto con el Ident que acompaña a Prefix y la URI que acompaña a esta
///
/// # Retorna
/// Un Parser con una tupla con el identificador del Prefix y con un Option con la URI, dependiendo de lo que se encuentre
fn prefix_or_uri_parser() -> Or<
    Map<
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
        impl Fn((Token, Token)) -> (Token, Option<Token>),
        (Token, Token),
    >,
    Map<
        Then<
            impl Parser<Token, Token, Error = Simple<Token>>,
            MapErr<
                Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
                impl Fn(Simple<Token>) -> Simple<Token>,
            >,
        >,
        impl Fn((Token, Token)) -> (Token, Option<Token>),
        (Token, Token),
    >,
> {
    (colon_parser()
        .then(identifier_parser(COLON))
        .map(|(_, ident)| (ident, None)))
    .or(uri_with_angle_brackets_parser()
        .then(identifier_parser(LEFT_ANGLE_BRACKET))
        .map(|(uri, ident)| (ident, Some(uri))))
}

/// Realiza el parseo de una URI con '<' y '>' a ambos lados
///
/// # Retorna
/// Un Parser con un Token Uri
fn uri_with_angle_brackets_parser() -> impl Parser<Token, Token, Error = Simple<Token>> {
    left_angle_bracket_parser("la URI")
        .then(uri_parser())
        .then_ignore(right_angle_bracket_parser("la URI"))
        .map(|(_, uri)| uri)
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

/// Parsea el token Join en los tokens
///
/// # Retorna
/// Un token de tipo Join si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Join
fn join_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Join,
        format!("Se esperaba un JOIN en la expresión de la línea"),
    )
}

/// Parsea el token On en los tokens
///
/// # Retorna
/// Un token de tipo On si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo On
fn on_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::On,
        format!("Se esperaba un ON después del segundo acceso en la expresión de la línea"),
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
        format!("Se esperaba un identificador después de '{previous_token}' en la línea"),
    )
}

/// Parsea el token KeyIdentifier en los tokens
///
/// # Retorna
/// Un token de tipo KeyIdentifier si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Ident
fn key_identifier_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::KeyIdentifier,
        format!("Se esperaba un identificador clave después de '<' en la línea"),
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

/// Parsea el token FilePath en los tokens
///
/// # Retorna
/// Un token de tipo FilePath si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo FilePath
fn file_path_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::FilePath,
        format!("Se esperaba una ruta con file entre '<' y '>' en la línea"),
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
fn colon_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Colon,
        format!("Faltan los ':' después del identificador en la línea"),
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
        format!("Faltan los ';' después del ']' en la línea"),
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

/// Parsea el token '=' en los tokens
///
/// # Retorna
/// Un token de tipo = si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo =
fn equal_parser() -> MapErr<
    Map<Filter<impl Fn(&Token) -> bool, Simple<Token>>, impl Fn(Token) -> Token, Token>,
    impl Fn(Simple<Token>) -> Simple<Token>,
> {
    general_parser(
        TokenType::Equal,
        format!(
            "Falta un '=' después del identificador siguiente al ON de la expresión de la línea"
        ),
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
        format!("Se esperaba un '[[' después de {previous_token} en la línea"),
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
        format!("Se esperaba un ']]' después de {previous_token} en la línea"),
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
    filter(move |token: &Token| token.token_type == token_type)
        .map(|token| token.clone())
        .map_err(move |token: Simple<Token>| {
            let line = token.found().map(|t| t.num_line).unwrap_or(0);
            Simple::custom(token.span(), format!("{message} {}", line))
        })
}

/// Parsea el vector de tokens para generar el AST
///
/// Toma como entrada el vector de tokens resultado del análisis léxico y genera un árbol AST que tiene un nodo File como raíz
///
/// # Parámetros
/// * `tokens` - El vector de tokens resultado del análisis léxico
///
/// # Retorna
/// Un nodo File del AST que será el nodo raíz de este
///
/// # Errores
/// * `[Vec<Simple<Token>>]` - Un vector con los errores que pueden aparecer al realizar el análisis sintáctico
pub fn parser(tokens: Vec<Token>) -> Result<FileASTNode, Vec<Simple<Token>>> {
    let file_parser = file_parser();
    let parsed = file_parser.parse(tokens);

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
        let actual = union_parser().parse(vec![Token::create_test_token(JOIN, 1, TokenType::Join)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Join
    #[doc(hidden)]
    #[test]
    fn parse_valid_join() {
        let expected_token = Token::create_test_token(JOIN, 1, TokenType::Join);
        let actual = join_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Join aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_join() {
        let actual =
            join_parser().parse(vec![Token::create_test_token(UNION, 1, TokenType::Union)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens On
    #[doc(hidden)]
    #[test]
    fn parse_valid_on() {
        let expected_token = Token::create_test_token(ON, 1, TokenType::On);
        let actual = on_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens On aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_on() {
        let actual = on_parser().parse(vec![Token::create_test_token(UNION, 1, TokenType::Union)]);
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

    /// Comprueba que se parsean los tokens KeyIdentifier
    #[doc(hidden)]
    #[test]
    fn parse_valid_key_identifier() {
        let expected_token = Token::create_test_token("@ident", 1, TokenType::KeyIdentifier);
        let actual = key_identifier_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens KeyIdentifier aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_key_identifier() {
        let actual = key_identifier_parser().parse(vec![Token::create_test_token(
            "invalid",
            1,
            TokenType::Ident,
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

    /// Comprueba que se parsean los tokens FilePath
    #[doc(hidden)]
    #[test]
    fn parse_valid_file_path() {
        let expected_token = Token::create_test_token(
            "file:///ejemplo/path/a/fichero/fichero.csv",
            1,
            TokenType::FilePath,
        );
        let actual = file_path_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens FilePath aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_file_path() {
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
            "file:///ejemplo/path/a/fichero/fichero.csv",
            1,
            TokenType::FilePath,
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
        let actual = colon_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Colon (:) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_colon() {
        let actual =
            colon_parser().parse(vec![Token::create_test_token("ident", 1, TokenType::Ident)]);
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

    /// Comprueba que se parsean los tokens Equal (=)
    #[doc(hidden)]
    #[test]
    fn parse_valid_equal() {
        let expected_token = Token::create_test_token(EQUAL, 1, TokenType::Equal);
        let actual = equal_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Equal (=) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_equal() {
        let actual =
            equal_parser().parse(vec![Token::create_test_token(COLON, 1, TokenType::Colon)]);
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
        let actual = left_bracket_parser("tupla").parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens LeftBracket ([) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_right_bracket() {
        let actual = left_bracket_parser("tupla").parse(vec![Token::create_test_token(
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
    use chumsky::error::SimpleReason;

    use super::*;

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax() {
        let tokens_vector: Vec<Token> = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com/", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(SOURCE, 2, TokenType::Source),
            Token::create_test_token("films_csv_file", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "https://shexml.herminiogarcia.com/files/films.csv",
                2,
                TokenType::Uri,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(QUERY, 3, TokenType::Query),
            Token::create_test_token("query_sql", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 3, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 3, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            Token::create_test_token(ITERATOR, 4, TokenType::Iterator),
            Token::create_test_token("film_csv", 4, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 4, TokenType::LeftAngleBracket),
            Token::create_test_token("query_sql", 4, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 4, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 4, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 5, TokenType::Field),
            Token::create_test_token("id", 5, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 5, TokenType::LeftAngleBracket),
            Token::create_test_token("@id", 5, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 5, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 6, TokenType::Field),
            Token::create_test_token("name", 6, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 6, TokenType::LeftAngleBracket),
            Token::create_test_token("name", 6, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 6, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 7, TokenType::Field),
            Token::create_test_token("year", 7, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 7, TokenType::LeftAngleBracket),
            Token::create_test_token("year", 7, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 7, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 8, TokenType::Field),
            Token::create_test_token("country", 8, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 8, TokenType::LeftAngleBracket),
            Token::create_test_token("country", 8, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 8, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 9, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EXPRESSION, 10, TokenType::Expression),
            Token::create_test_token("films", 10, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 10, TokenType::LeftAngleBracket),
            Token::create_test_token("films_csv_file", 10, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 10, TokenType::AccessDot),
            Token::create_test_token("film_csv", 10, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 10, TokenType::RightAngleBracket),

            Token::create_test_token(COLON, 11, TokenType::Colon),
            Token::create_test_token("Films", 11, TokenType::Ident),
            Token::create_test_token(COLON, 11, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 11, TokenType::LeftBracket),
            Token::create_test_token("films", 11, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 11, TokenType::AccessDot),
            Token::create_test_token("id", 11, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 11, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 11, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 12, TokenType::Colon),
            Token::create_test_token("name", 12, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 12, TokenType::LeftBracket),
            Token::create_test_token("films", 12, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 12, TokenType::AccessDot),
            Token::create_test_token("name", 12, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 12, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 12, TokenType::SemiColon),
            Token::create_test_token(COLON, 13, TokenType::Colon),
            Token::create_test_token("year", 13, TokenType::Ident),
            Token::create_test_token(COLON, 13, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 13, TokenType::LeftBracket),
            Token::create_test_token("films", 13, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 13, TokenType::AccessDot),
            Token::create_test_token("year", 13, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 13, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 13, TokenType::SemiColon),
            Token::create_test_token(COLON, 14, TokenType::Colon),
            Token::create_test_token("country", 14, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 14, TokenType::LeftBracket),
            Token::create_test_token("films", 14, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 14, TokenType::AccessDot),
            Token::create_test_token("country", 14, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 14, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 14, TokenType::SemiColon),
            Token::create_test_token(COLON, 15, TokenType::Colon),
            Token::create_test_token("director", 15, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 15, TokenType::LeftBracket),
            Token::create_test_token("films", 15, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 15, TokenType::AccessDot),
            Token::create_test_token("director", 15, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 15, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 15, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 16, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 16, TokenType::EOF),
        ];

        let expected = FileASTNode {
            prefixes: Some(vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: Some(vec![ExpressionASTNode {
                identifier: "films".to_string(),
                expression_type: ExpressionType::BASIC,
                accesses: vec![AccessASTNode {
                    identifier: "films_csv_file".to_string(),
                    iterator_accessed: "film_csv".to_string(),
                    field_accessed: None,
                }],
            }]),
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
        };

        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST si no hay query y no hay errores sintácticos
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax_and_withouth_query() {
        let tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com/", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(SOURCE, 2, TokenType::Source),
            Token::create_test_token("films_csv_file", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "https://shexml.herminiogarcia.com/files/films.csv",
                2,
                TokenType::Uri,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(ITERATOR, 3, TokenType::Iterator),
            Token::create_test_token("film_csv", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token("query_sql", 3, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 3, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 4, TokenType::Field),
            Token::create_test_token("id", 4, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 4, TokenType::LeftAngleBracket),
            Token::create_test_token("@id", 4, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 4, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 5, TokenType::Field),
            Token::create_test_token("name", 5, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 5, TokenType::LeftAngleBracket),
            Token::create_test_token("name", 5, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 5, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 6, TokenType::Field),
            Token::create_test_token("year", 6, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 6, TokenType::LeftAngleBracket),
            Token::create_test_token("year", 6, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 6, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 7, TokenType::Field),
            Token::create_test_token("country", 7, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 7, TokenType::LeftAngleBracket),
            Token::create_test_token("country", 7, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 7, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 8, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EXPRESSION, 9, TokenType::Expression),
            Token::create_test_token("films", 9, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 9, TokenType::LeftAngleBracket),
            Token::create_test_token("films_csv_file", 9, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 9, TokenType::AccessDot),
            Token::create_test_token("film_csv", 9, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 9, TokenType::RightAngleBracket),
            Token::create_test_token(COLON, 10, TokenType::Colon),
            Token::create_test_token("Films", 10, TokenType::Ident),
            Token::create_test_token(COLON, 10, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 10, TokenType::LeftBracket),
            Token::create_test_token("films", 10, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 10, TokenType::AccessDot),
            Token::create_test_token("id", 10, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 10, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 10, TokenType::OpeningCurlyBrace),
            Token::create_test_token(COLON, 11, TokenType::Colon),
            Token::create_test_token("name", 11, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 11, TokenType::LeftBracket),
            Token::create_test_token("films", 11, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 11, TokenType::AccessDot),
            Token::create_test_token("name", 11, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 11, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 11, TokenType::SemiColon),
            Token::create_test_token(COLON, 12, TokenType::Colon),
            Token::create_test_token("year", 12, TokenType::Ident),
            Token::create_test_token(COLON, 12, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 12, TokenType::LeftBracket),
            Token::create_test_token("films", 12, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 12, TokenType::AccessDot),
            Token::create_test_token("year", 12, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 12, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 12, TokenType::SemiColon),
            Token::create_test_token(COLON, 13, TokenType::Colon),
            Token::create_test_token("country", 13, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 13, TokenType::LeftBracket),
            Token::create_test_token("films", 13, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 13, TokenType::AccessDot),
            Token::create_test_token("country", 13, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 13, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 13, TokenType::SemiColon),
            Token::create_test_token(COLON, 14, TokenType::Colon),
            Token::create_test_token("director", 14, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 14, TokenType::LeftBracket),
            Token::create_test_token("films", 14, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 14, TokenType::AccessDot),
            Token::create_test_token("director", 14, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 14, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 14, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 15, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 15, TokenType::EOF),
        ];

        let expected = FileASTNode {
            prefixes: Some(vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: None,
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: Some(vec![ExpressionASTNode {
                identifier: "films".to_string(),
                expression_type: ExpressionType::BASIC,
                accesses: vec![AccessASTNode {
                    identifier: "films_csv_file".to_string(),
                    iterator_accessed: "film_csv".to_string(),
                    field_accessed: None,
                }],
            }]),
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
        };

        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST si no hay Expression y no hay errores sintácticos
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax_and_withouth_expression() {
        let tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("example", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com/", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(SOURCE, 2, TokenType::Source),
            Token::create_test_token("films_csv_file", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "https://shexml.herminiogarcia.com/files/films.csv",
                2,
                TokenType::Uri,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(QUERY, 3, TokenType::Query),
            Token::create_test_token("query_sql", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 3, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 3, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            Token::create_test_token(ITERATOR, 4, TokenType::Iterator),
            Token::create_test_token("film_csv", 4, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 4, TokenType::LeftAngleBracket),
            Token::create_test_token("query_sql", 4, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 4, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 4, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 5, TokenType::Field),
            Token::create_test_token("id", 5, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 5, TokenType::LeftAngleBracket),
            Token::create_test_token("@id", 5, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 5, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 6, TokenType::Field),
            Token::create_test_token("name", 6, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 6, TokenType::LeftAngleBracket),
            Token::create_test_token("name", 6, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 6, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 7, TokenType::Field),
            Token::create_test_token("year", 7, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 7, TokenType::LeftAngleBracket),
            Token::create_test_token("year", 7, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 7, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 8, TokenType::Field),
            Token::create_test_token("country", 8, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 8, TokenType::LeftAngleBracket),
            Token::create_test_token("country", 8, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 8, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 9, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 9, TokenType::EOF),
        ];

        let expected = FileASTNode {
            prefixes: Some(vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }]),
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: vec![IteratorASTNode {
                identifier: "film_csv".to_string(),
                iterator_access: "query_sql".to_string(),
                fields: vec![
                    FieldASTNode {
                        field_identifier: "id".to_string(),
                        access_field_identifier: "@id".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "name".to_string(),
                        access_field_identifier: "name".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "year".to_string(),
                        access_field_identifier: "year".to_string(),
                    },
                    FieldASTNode {
                        field_identifier: "country".to_string(),
                        access_field_identifier: "country".to_string(),
                    },
                ],
            }],
            expressions: None,
            shapes: vec![ShapeASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "Films".to_string(),
                field_prefix_or_uri: PrefixOrURI::Prefix,
                field_identifier: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "id".to_string(),
                    field_accessed: None,
                }),
                tuples: vec![
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "name".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "name".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "year".to_string(),
                        object_prefix_or_uri: Some(PrefixOrURI::Prefix),
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "year".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "country".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "country".to_string(),
                            field_accessed: None,
                        }),
                    },
                    ShapeTupleASTNode {
                        prefix_or_uri: PrefixOrURI::Prefix,
                        identifier: "director".to_string(),
                        object_prefix_or_uri: None,
                        object: IdentOrAccess::Access(AccessASTNode {
                            identifier: "films".to_string(),
                            iterator_accessed: "director".to_string(),
                            field_accessed: None,
                        }),
                    },
                ],
            }],
        };

        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay prefixes
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_prefixes() {
        let fail_tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<FileASTNode>(
            file_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX en la línea 1",
        );
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay sources
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_sources() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<FileASTNode>(
            file_parser(),
            fail_tokens_vector,
            "Se esperaba un PREFIX o un SOURCE en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix parsea la secuencia de tokens: Prefix Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_prefix_sintax() {
        let mut tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = PrefixASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más PREFIX
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(PREFIX, 2, TokenType::Prefix));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(COLON, 2, TokenType::Colon));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(
            "https://ejemplo2.com",
            2,
            TokenType::Uri,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = PrefixASTNode {
            identifier: "ident2".to_string(),
            uri: "https://ejemplo2.com".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = prefix_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
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

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            Token::create_test_token(PREFIX, 1, TokenType::Prefix),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<PrefixASTNode>>(
            prefix_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de 'PREFIX' en la línea 1",
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
        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://ejemplo.com/fichero.csv", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "https://ejemplo.com/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(
            "https://ejemplo2.com/fichero.csv",
            2,
            TokenType::Uri,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "https://ejemplo2.com/fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket JdbcUrl RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_jdbc_url() {
        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("jdbc:mysql://localhost:3306/mydb", 1, TokenType::JdbcUrl),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "jdbc:mysql://localhost:3306/mydb".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(
            "jdbc:mysql://localhost:3356/anotherdb",
            2,
            TokenType::JdbcUrl,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "jdbc:mysql://localhost:3356/anotherdb".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket FilePath RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_file_path() {
        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "file:///ejemplo/path/a/fichero/fichero.csv",
                1,
                TokenType::FilePath,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "file:///ejemplo/path/a/fichero/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(
            "file:///otroejemplo/path/a/fichero/otrofichero.csv",
            2,
            TokenType::FilePath,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "file:///otroejemplo/path/a/fichero/otrofichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = source_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket Path RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_path() {
        let mut tokens_vector = vec![
            Token::create_test_token(SOURCE, 1, TokenType::Source),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("ejemplo/fichero.csv", 1, TokenType::Path),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "ejemplo/fichero.csv".to_string(),
        };
        let actual = source_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(SOURCE, 2, TokenType::Source));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(
            "C:\\ejemplo\\path\\a\\fichero\\fichero.csv",
            2,
            TokenType::Path,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "C:\\ejemplo\\path\\a\\fichero\\fichero.csv".to_string(),
        };

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
            "Se esperaba un identificador después de 'SOURCE' en la línea 1",
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
        let mut tokens_vector = vec![
            Token::create_test_token(QUERY, 1, TokenType::Query),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, 1, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", 1, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = QueryASTNode {
            identifier: "ident".to_string(),
            sql_query: "SELECT * FROM example;".to_string(),
        };
        let actual = query_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más QUERY
        let eof_node = tokens_vector.pop();
        tokens_vector.push(Token::create_test_token(QUERY, 2, TokenType::Query));
        tokens_vector.push(Token::create_test_token("ident2", 2, TokenType::Ident));
        tokens_vector.push(Token::create_test_token(
            LEFT_ANGLE_BRACKET,
            2,
            TokenType::LeftAngleBracket,
        ));
        tokens_vector.push(Token::create_test_token(SQL_TYPE, 2, TokenType::SqlType));
        tokens_vector.push(Token::create_test_token(
            "SELECT * FROM example;",
            2,
            TokenType::SqlQuery,
        ));
        tokens_vector.push(Token::create_test_token(
            RIGHT_ANGLE_BRACKET,
            2,
            TokenType::RightAngleBracket,
        ));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = QueryASTNode {
            identifier: "ident2".to_string(),
            sql_query: "SELECT * FROM example;".to_string(),
        };

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
            "Se esperaba un identificador después de 'QUERY' en la línea 1",
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
        let tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = FieldASTNode {
            field_identifier: "field".to_string(),
            access_field_identifier: "@field".to_string(),
        };
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
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<FieldASTNode>>(
            field_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de 'FIELD' en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(FIELD, 1, TokenType::Field),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
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
            "Se esperaba un identificador después de '<' en la línea 1",
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
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 1, TokenType::KeyIdentifier),
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
        let tokens_vector = vec![
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let expected = IteratorASTNode {
            identifier: "ident".to_string(),
            iterator_access: "SELECT * FROM example;".to_string(),
            fields: vec![FieldASTNode {
                field_identifier: "field".to_string(),
                access_field_identifier: "@field".to_string(),
            }],
        };
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    /// con más de un field
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_more_than_one_field() {
        let tokens_vector = vec![
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(FIELD, 3, TokenType::Field),
            Token::create_test_token("field2", 3, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 3, TokenType::LeftAngleBracket),
            Token::create_test_token("field", 3, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 3, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 4, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 4, TokenType::EOF),
        ];

        let expected = IteratorASTNode {
            identifier: "ident".to_string(),
            iterator_access: "SELECT * FROM example;".to_string(),
            fields: vec![
                FieldASTNode {
                    field_identifier: "field".to_string(),
                    access_field_identifier: "@field".to_string(),
                },
                FieldASTNode {
                    field_identifier: "field2".to_string(),
                    access_field_identifier: "field".to_string(),
                },
            ],
        };
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_ident() {
        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("inline_query", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let expected = IteratorASTNode {
            identifier: "ident".to_string(),
            iterator_access: "inline_query".to_string(),
            fields: vec![FieldASTNode {
                field_identifier: "field".to_string(),
                access_field_identifier: "@field".to_string(),
            }],
        };
        let actual = iterator_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_csv_per_row() {
        let tokens_vector = vec![
            Token::create_test_token(ITERATOR, 1, TokenType::Iterator),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token(CSV_PER_ROW, 1, TokenType::CsvPerRow),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, 1, TokenType::OpeningCurlyBrace),
            Token::create_test_token(FIELD, 2, TokenType::Field),
            Token::create_test_token("field", 2, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 2, TokenType::LeftAngleBracket),
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        let expected = IteratorASTNode {
            identifier: "ident".to_string(),
            iterator_access: "csvperrow".to_string(),
            fields: vec![FieldASTNode {
                field_identifier: "field".to_string(),
                access_field_identifier: "@field".to_string(),
            }],
        };
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de 'ITERATOR' en la línea 1",
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de '<' en la línea 1",
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '{' antes de los fields en la línea 2",
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '}' después de los fields en la línea 3",
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
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
            Token::create_test_token("@field", 2, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 2, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<IteratorASTNode>>(
            iterator_parser(),
            fail_tokens_vector,
            "Se esperaba un '{' antes de los fields en la línea 2",
        );
    }

    /// Comprueba que el parser de Access parsea la secuencia de tokens: Ident AccessDot
    #[doc(hidden)]
    #[test]
    fn valid_access_sintax_with_iterator_access() {
        let tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = AccessASTNode {
            identifier: "ident".to_string(),
            iterator_accessed: "iterator".to_string(),
            field_accessed: None,
        };
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Access parsea la secuencia de tokens: Ident AccessDot Ident AccessDot
    #[doc(hidden)]
    #[test]
    fn valid_access_sintax_with_field_access() {
        let tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = AccessASTNode {
            identifier: "ident".to_string(),
            iterator_accessed: "iterator".to_string(),
            field_accessed: Some("field".to_string()),
        };
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
            "Se esperaba un identificador después de '<' en la línea 1",
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
            "Se esperaba un identificador después de '.' en la línea 1",
        );
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident AccessDot Ident Ident
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_dot_between_iterator_and_field() {
        let tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token("field", 1, TokenType::Ident),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        // No dará error en este parser pero si en el siguiente que se ejecute al estar un ident suelto
        let expected = AccessASTNode {
            identifier: "ident".to_string(),
            iterator_accessed: "iterator".to_string(),
            field_accessed: None,
        };
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Access no parsea como tales aquellas secuencias de tokens que son: Ident AccessDot Ident AccessDot  
    #[doc(hidden)]
    #[test]
    fn access_sintax_withouth_field_identifier() {
        let tokens_vector = vec![
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        // No dará error en este parser pero si en el siguiente que se ejecute al estar un '.' suelto
        let expected = AccessASTNode {
            identifier: "ident".to_string(),
            iterator_accessed: "iterator".to_string(),
            field_accessed: None,
        };
        let actual = access_parser(LEFT_ANGLE_BRACKET).parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser de Expression parsea la secuencia de tokens:
    /// Expression Ident LeftAngleBracket Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_basic_expression_sintax() {
        let tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ExpressionASTNode {
            identifier: "ident".to_string(),
            expression_type: ExpressionType::BASIC,
            accesses: vec![AccessASTNode {
                identifier: "id".to_string(),
                iterator_accessed: "iterator".to_string(),
                field_accessed: None,
            }],
        };

        let actual = expression_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Expression parsea la secuencia de tokens:
    /// Expression Ident LeftAngleBracket Access Union Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_union_expression_sintax() {
        let tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(UNION, 1, TokenType::Union),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ExpressionASTNode {
            identifier: "ident".to_string(),
            expression_type: ExpressionType::UNION,
            accesses: vec![
                AccessASTNode {
                    identifier: "id1".to_string(),
                    iterator_accessed: "iterator1".to_string(),
                    field_accessed: None,
                },
                AccessASTNode {
                    identifier: "id2".to_string(),
                    iterator_accessed: "iterator2".to_string(),
                    field_accessed: None,
                },
            ],
        };

        let actual = expression_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de Expression parsea la secuencia de tokens:
    /// Expression Ident LeftAngleBracket Access Union Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_join_expression_sintax() {
        let tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ExpressionASTNode {
            identifier: "ident".to_string(),
            expression_type: ExpressionType::JOIN,
            accesses: vec![
                AccessASTNode {
                    identifier: "id1".to_string(),
                    iterator_accessed: "iterator1".to_string(),
                    field_accessed: None,
                },
                AccessASTNode {
                    identifier: "id2".to_string(),
                    iterator_accessed: "iterator2".to_string(),
                    field_accessed: None,
                },
                AccessASTNode {
                    identifier: "id3".to_string(),
                    iterator_accessed: "iterator3".to_string(),
                    field_accessed: Some("field3".to_string()),
                },
                AccessASTNode {
                    identifier: "id4".to_string(),
                    iterator_accessed: "iterator4".to_string(),
                    field_accessed: Some("field4".to_string()),
                },
            ],
        };

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
            "Se esperaba un identificador después de 'EXPRESSION' en la línea 1",
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
            "Se esperaba un identificador después de '<' en la línea 1",
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
            "Se esperaba un identificador después de 'UNION' en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Access On Access Equal Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_join() {
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
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un '>' después de el acceso en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Join On Access Equal Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_second_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de 'JOIN' en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Join Access Access Equal Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_on() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un ON después del segundo acceso en la expresión de la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Join Access On Equal Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_third_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de 'ON' en la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Join Access On Access Access RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_equal() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token("id4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator4", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field4", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Falta un '=' después del identificador siguiente al ON de la expresión de la línea 1",
        );
    }

    /// Comprueba que el parser de Expression Join no parsea como tales aquellas secuencias de tokens que son:
    /// Expression LeftAngleBracket Access Join Access On Access Equal RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn join_expression_sintax_withouth_fourth_access() {
        let fail_tokens_vector = vec![
            Token::create_test_token(EXPRESSION, 1, TokenType::Expression),
            Token::create_test_token("ident", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("id1", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator1", 1, TokenType::Ident),
            Token::create_test_token(JOIN, 1, TokenType::Join),
            Token::create_test_token("id2", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator2", 1, TokenType::Ident),
            Token::create_test_token(ON, 1, TokenType::On),
            Token::create_test_token("id3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("iterator3", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("field3", 1, TokenType::Ident),
            Token::create_test_token(EQUAL, 1, TokenType::Equal),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ExpressionASTNode>>(
            expression_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de '=' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// Prefix Colon Ident (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_ident_prefix() {
        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ShapeTupleASTNode {
            prefix_or_uri: PrefixOrURI::Prefix,
            identifier: "name".to_string(),
            object_prefix_or_uri: None,
            object: IdentOrAccess::Access(AccessASTNode {
                identifier: "films".to_string(),
                iterator_accessed: "name".to_string(),
                field_accessed: None,
            }),
        };

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// LeftAngelBracket Uri RightAngleBracket Ident (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_ident_uri() {
        let tokens_vector = vec![
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ShapeTupleASTNode {
            prefix_or_uri: PrefixOrURI::URI("https://example.com".to_string()),
            identifier: "name".to_string(),
            object_prefix_or_uri: None,
            object: IdentOrAccess::Access(AccessASTNode {
                identifier: "films".to_string(),
                iterator_accessed: "name".to_string(),
                field_accessed: None,
            }),
        };

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) Prefix Colon LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_field_prefix() {
        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films", 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, 1, TokenType::AccessDot),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ShapeTupleASTNode {
            prefix_or_uri: PrefixOrURI::Prefix,
            identifier: "name".to_string(),
            object_prefix_or_uri: Some(PrefixOrURI::Prefix),
            object: IdentOrAccess::Access(AccessASTNode {
                identifier: "films".to_string(),
                iterator_accessed: "name".to_string(),
                field_accessed: None,
            }),
        };

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) LeftAngelBracket Uri RightAngleBracket LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_tuple_sintax_with_field_uri() {
        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        let expected = ShapeTupleASTNode {
            prefix_or_uri: PrefixOrURI::Prefix,
            identifier: "name".to_string(),
            object_prefix_or_uri: Some(PrefixOrURI::URI("https://example.com".to_string())),
            object: IdentOrAccess::Access(AccessASTNode {
                identifier: "films".to_string(),
                iterator_accessed: "name".to_string(),
                field_accessed: None,
            }),
        };

        let actual = shape_tuple_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_ident_prefix_or_uri() {
        let fail_tokens_vector = vec![
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un ':' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_left_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un '[' antes del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_ident_or_access_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de '[' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_right_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(SEMICOLON, 1, TokenType::SemiColon),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un ']' después del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket
    #[doc(hidden)]
    #[test]
    fn shape_tuple_sintax_withouth_semicolon() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("name", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token(LEFT_BRACKET, 1, TokenType::LeftBracket),
            Token::create_test_token("films_name", 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, 1, TokenType::RightBracket),
            Token::create_test_token(EOF, 1, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeTupleASTNode>>(
            shape_tuple_parser(),
            fail_tokens_vector,
            "Se esperaba un ';' después de ']' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// Prefix Colon Ident (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_sintax_with_ident_prefix() {
        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
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

        let expected = ShapeASTNode {
            prefix_or_uri: PrefixOrURI::Prefix,
            identifier: "Films".to_string(),
            field_prefix_or_uri: PrefixOrURI::Prefix,
            field_identifier: IdentOrAccess::Access(AccessASTNode { 
                identifier: "films".to_string(), 
                iterator_accessed: "id".to_string(), 
                field_accessed: None, 
            }),
            tuples: vec![ShapeTupleASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "name".to_string(),
                object_prefix_or_uri: None,
                object: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "name".to_string(),
                    field_accessed: None,
                })
            }],
        };

        let actual = shape_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// LeftAngelBracket Uri RightAngleBracket Ident (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_sintax_with_ident_uri() {
        let tokens_vector = vec![
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
            Token::create_test_token("Films", 1, TokenType::Ident),
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

        let expected = ShapeASTNode {
            prefix_or_uri: PrefixOrURI::URI("https://example.com".to_string()),
            identifier: "Films".to_string(),
            field_prefix_or_uri: PrefixOrURI::Prefix,
            field_identifier: IdentOrAccess::Access(AccessASTNode { 
                identifier: "films".to_string(), 
                iterator_accessed: "id".to_string(), 
                field_accessed: None, 
            }),
            tuples: vec![ShapeTupleASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "name".to_string(),
                object_prefix_or_uri: None,
                object: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "name".to_string(),
                    field_accessed: None,
                })
            }],
        };

        let actual = shape_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple parsea la secuencia de tokens:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) LeftAngelBracket Uri RightAngleBracket LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn valid_shape_sintax_with_field_uri() {
        let tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, 1, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com", 1, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, 1, TokenType::RightAngleBracket),
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

        let expected = ShapeASTNode {
            prefix_or_uri: PrefixOrURI::Prefix,
            identifier: "Films".to_string(),
            field_prefix_or_uri: PrefixOrURI::URI("https://example.com".to_string()),
            field_identifier: IdentOrAccess::Access(AccessASTNode { 
                identifier: "films".to_string(), 
                iterator_accessed: "id".to_string(), 
                field_accessed: None, 
            }),
            tuples: vec![ShapeTupleASTNode {
                prefix_or_uri: PrefixOrURI::Prefix,
                identifier: "name".to_string(),
                object_prefix_or_uri: None,
                object: IdentOrAccess::Access(AccessASTNode {
                    identifier: "films".to_string(),
                    iterator_accessed: "name".to_string(),
                    field_accessed: None,
                })
            }],
        };

        let actual = shape_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_ident_prefix_or_uri() {
        let fail_tokens_vector = vec![
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
            "Se esperaba un ':' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? (Ident|Access) RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_left_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
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
            "Se esperaba un '[' antes del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket RightBracket Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_ident_field() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
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
            Token::create_test_token(RIGHT_BRACKET, 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeASTNode>>(
            shape_parser(),
            fail_tokens_vector,
            "Se esperaba un identificador después de '[' en la línea 1",
        );
    }

    /// Comprueba que el parser de ShapeTuple no parsea como tales aquellas secuencias de tokens que son:
    /// (Prefix Colon Ident|LeftAngelBracket Uri RightAngleBracket Ident) (Prefix Colon|LeftAngelBracket Uri RightAngleBracket)? LeftBracket (Ident|Access) Semicolon
    #[doc(hidden)]
    #[test]
    fn shape_sintax_withouth_right_bracket() {
        let fail_tokens_vector = vec![
            Token::create_test_token(COLON, 1, TokenType::Colon),
            Token::create_test_token("Films", 1, TokenType::Ident),
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
            Token::create_test_token(SEMICOLON, 2, TokenType::SemiColon),
            Token::create_test_token(CLOSING_CURLY_BRACE, 3, TokenType::ClosingCurlyBrace),
            Token::create_test_token(EOF, 3, TokenType::EOF),
        ];

        check_parser_error::<Vec<ShapeASTNode>>(
            shape_parser(),
            fail_tokens_vector,
            "Se esperaba un ']' después del identificador en la línea 1",
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
