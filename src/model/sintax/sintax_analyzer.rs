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
        .then(sources_parser())
        .then(query_parser().or_not())
        .then(iterator_parser().or_not())
        .then(expression_parser().or_not())
        .then_ignore(eof_parser())
        .map(
            |((((prefixes, sources), queries), iterators), expressions)| FileASTNode {
                prefixes,
                sources,
                queries,
                iterators,
                expressions,
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
        .then(identifier_parser(PREFIX.to_string()))
        .then_ignore(colon_parser())
        .then_ignore(left_angle_bracket_parser("la URI".to_string()))
        .then(uri_parser())
        .then_ignore(right_angle_bracket_parser("la URI".to_string()))
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
fn sources_parser() -> impl Parser<Token, Vec<SourceASTNode>, Error = Simple<Token>> {
    source_parser()
        .then(identifier_parser(SOURCE.to_string()))
        .then_ignore(left_angle_bracket_parser("la URL o ruta".to_string()))
        .then(
            uri_parser()
                .or(file_path_parser())
                .or(path_parser())
                .or(jdbc_url_parser()),
        )
        .then_ignore(right_angle_bracket_parser("la URL o ruta".to_string()))
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
        .then(identifier_parser(QUERY.to_string()))
        .then_ignore(left_angle_bracket_parser("la consulta SQL".to_string()))
        .then_ignore(sql_type_parser())
        .then(sql_query_token_parser())
        .then_ignore(right_angle_bracket_parser("la consulta SQL".to_string()))
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
        .then(identifier_parser(ITERATOR.to_string()))
        .then_ignore(left_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow".to_string(),
        ))
        .then(
            // Es necesario utilizar tuplas para poner concatenar el SqlType y el SqlQuery
            identifier_parser("<".to_string())
                .map(|token| (token, None))
                .or(csv_per_row_parser().map(|token| (token, None)))
                .or(sql_type_parser()
                    .then(sql_query_token_parser())
                    .map(|(sql_type, sql_query)| (sql_type, Some(sql_query)))),
        )
        .then_ignore(right_angle_bracket_parser(
            "la consulta SQL, identificador o csvperrow".to_string(),
        ))
        .then_ignore(opening_curly_brace_parser("los fields".to_string()))
        .then(field_parser())
        .then_ignore(closing_curly_brace_parser("los fields".to_string()))
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
/// # Argumentos
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
        .then(identifier_parser(FIELD.to_string()))
        .then_ignore(left_angle_bracket_parser("el identificador".to_string()))
        .then(identifier_parser("<".to_string()).or(key_identifier_parser()))
        .then_ignore(right_angle_bracket_parser("el identificador".to_string()))
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
        .then(identifier_parser(EXPRESSION.to_string()))
        .then_ignore(left_angle_bracket_parser("acceso".to_string()))
        .then(access_parser())
        .then(union_parser().or(join_parser()).or_not())
        .then(
            // Es necesario utilizar tuplas para poner concatenar el Substituting y el Access y el On Access Equal Access
            access_parser()
                .then(
                    on_parser()
                        .then(access_parser())
                        .then_ignore(equal_parser())
                        .then(access_parser())
                        .map(|((_, access_on), access_equal)| (access_on, access_equal))
                        .or_not(),
                )
                .map(|(access, access_on)| match access_on {
                    Some((access_on, access_equal)) => {
                        (access, Some(access_on), Some(access_equal))
                    }
                    None => (access, None, None),
                })
                .or_not(),
        )
        .then_ignore(right_angle_bracket_parser("acceso".to_string()))
        .map(
            |((((_, ident), iterator_access), union_or_join), more_accesses)| {
                create_expression_node(ident, iterator_access, union_or_join, more_accesses)
            },
        )
        .repeated()
        .at_least(1)
        .collect()
}

/// Crea un nodo Expression del AST
///
/// A partir de la información sacada del parser crea un nodo expresión del AST
///
/// # Argumentos
/// * `ident` - El identificado de la expresión
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `union_or_join` - Un Option con el token UNION, JOIN o ninguno indicando el tipo de la expresión
/// * `more_accesses` - Un Option con los posibles accesos a: el JOIN O UNION, el ON y el que puede haber después del =
///
/// # Retorna
/// Un nodo ExpressionASTNode
fn create_expression_node(
    ident: Token,
    iterator_access: AccessASTNode,
    union_or_join: Option<Token>,
    more_accesses: Option<(AccessASTNode, Option<AccessASTNode>, Option<AccessASTNode>)>,
) -> ExpressionASTNode {
    let accesses = create_vec_of_accesses(iterator_access, more_accesses);

    if union_or_join.is_none() {
        return ExpressionASTNode {
            identifier: ident.lexeme.clone(),
            expression_type: ExpressionType::BASIC,
            accesses,
        };
    }

    let expression_type = ExpressionType::from(union_or_join.unwrap());

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
/// # Argumentos
/// * `iterator_access` - El nodo Access del AST de acceso al iterador
/// * `more_accesses` - Un Option con los posibles accesos a: el JOIN O UNION, el ON y el que puede haber después del =
///
/// # Retorna
/// Un vector con todos los accesos de una expresión
fn create_vec_of_accesses(
    iterator_access: AccessASTNode,
    more_accesses: Option<(AccessASTNode, Option<AccessASTNode>, Option<AccessASTNode>)>,
) -> Vec<AccessASTNode> {
    let mut accesses = vec![iterator_access];

    if more_accesses.is_some() {
        let (field_access, on_access, equal_access) = more_accesses.unwrap();
        accesses.push(field_access);

        if on_access.is_some() && equal_access.is_some() {
            accesses.push(on_access.unwrap());
            accesses.push(equal_access.unwrap());
        }
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
fn access_parser() -> impl Parser<Token, AccessASTNode, Error = Simple<Token>> {
    identifier_parser(LEFT_ANGLE_BRACKET.to_string())
        .then(access_dot_parser())
        .then(identifier_parser(ACCESS_DOT.to_string()))
        .then(
            access_dot_parser()
                .then(identifier_parser(ACCESS_DOT.to_string()))
                .or_not(),
        )
        .map(|(((ident, _), iterator_accessed), field_accessed)| {
            create_access_node(ident, iterator_accessed, field_accessed)
        })
}

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
fn source_parser() -> MapErr<
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
        format!("Se esperaba un ON en la expresión de la línea"),
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
/// # Argumentos
/// * `previous_token` - El token previo al identificador, que puede ser un PREFIX o un SOURCE
///
/// # Retorna
/// Un token de tipo Ident si el token actual es de dicho tipo
///
/// # Errores
/// Devuelve un `[Simple<Token>]` en el caso de que el token actual no sea de tipo Ident
fn identifier_parser(
    previous_token: String,
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
    next_token: String,
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
    previous_token: String,
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
    previous_token: String,
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
    previous_token: String,
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
/// /// # Argumentos
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
/// # Argumentos
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
    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que se parsean los tokens Prefix
    #[doc(hidden)]
    #[test]
    fn parse_valid_prefix() {
        let expected_token = TestUtilities::prefix_test_token(1);
        let actual = prefix_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Prefix aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_prefix() {
        let actual = prefix_token_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Source
    #[doc(hidden)]
    #[test]
    fn parse_valid_source() {
        let expected_token = TestUtilities::source_test_token(1);
        let actual = source_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Source aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_source() {
        let actual = source_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Query
    #[doc(hidden)]
    #[test]
    fn parse_valid_query() {
        let expected_token = TestUtilities::query_test_token(1);
        let actual = query_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Query aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_query() {
        let actual = query_token_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Iterator
    #[doc(hidden)]
    #[test]
    fn parse_valid_iterator() {
        let expected_token = TestUtilities::iterator_test_token(1);
        let actual = iterator_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Iterator aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_iterator() {
        let actual = iterator_token_parser().parse(vec![TestUtilities::field_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Field
    #[doc(hidden)]
    #[test]
    fn parse_valid_field() {
        let expected_token = TestUtilities::field_test_token(1);
        let actual = field_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Field aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_field() {
        let actual = field_token_parser().parse(vec![TestUtilities::ident_test_token("FIELD", 1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens CsvPerRow
    #[doc(hidden)]
    #[test]
    fn parse_valid_csv_per_row() {
        let expected_token = TestUtilities::csv_per_row_test_token(1);
        let actual = csv_per_row_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens CsvPerRow aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_csv_per_row() {
        let actual =
            csv_per_row_parser().parse(vec![TestUtilities::ident_test_token("csvperrow", 1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens SqlType
    #[doc(hidden)]
    #[test]
    fn parse_valid_sql_type() {
        let expected_token = TestUtilities::sql_type_test_token(1);
        let actual = sql_type_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens SqlType aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_sql_type() {
        let actual = sql_type_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Ident
    #[doc(hidden)]
    #[test]
    fn parse_valid_identifier() {
        let expected_token = TestUtilities::ident_test_token("ident", 1);
        let actual = identifier_parser("PREFIX".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Ident aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_identifier() {
        let actual =
            identifier_parser("SOURCE".to_string()).parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens KeyIdentifier
    #[doc(hidden)]
    #[test]
    fn parse_valid_key_identifier() {
        let expected_token = TestUtilities::key_identifier_test_token("@ident", 1);
        let actual = key_identifier_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens KeyIdentifier aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_key_identifier() {
        let actual =
            key_identifier_parser().parse(vec![TestUtilities::ident_test_token("invalid", 1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens URI
    #[doc(hidden)]
    #[test]
    fn parse_valid_uri() {
        let expected_token = TestUtilities::uri_test_token("https://ejemplo.com", 1);
        let actual = uri_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens URI aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_uri() {
        let actual = uri_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens JDBC URL
    #[doc(hidden)]
    #[test]
    fn parse_valid_jdbc_url() {
        let expected_token =
            TestUtilities::jdbc_url_test_token("jdbc:mysql://localhost:3306/mydb", 1);
        let actual = jdbc_url_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens JDBC URL aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_jdbc_url() {
        let actual = jdbc_url_parser().parse(vec![TestUtilities::uri_test_token(
            "https://ejemplo.com",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens FilePath
    #[doc(hidden)]
    #[test]
    fn parse_valid_file_path() {
        let expected_token =
            TestUtilities::file_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 1);
        let actual = file_path_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens FilePath aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_file_path() {
        let actual = jdbc_url_parser().parse(vec![TestUtilities::uri_test_token(
            "https://ejemplo.com",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Path
    #[doc(hidden)]
    #[test]
    fn parse_valid_path() {
        let expected_token = TestUtilities::path_test_token("ejemplo/fichero.csv", 1);
        let actual = path_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Path aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_path() {
        let actual = path_parser().parse(vec![TestUtilities::file_path_test_token(
            "file:///ejemplo/path/a/fichero/fichero.csv",
            1,
        )]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens SqlQuery
    #[doc(hidden)]
    #[test]
    fn parse_valid_sql_query() {
        let expected_token = TestUtilities::sql_query_test_token("SELECT * FROM example;", 1);
        let actual = sql_query_token_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens SqlQuery aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_sql_query() {
        let actual = sql_query_token_parser().parse(vec![TestUtilities::colon_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens Colon (:)
    #[doc(hidden)]
    #[test]
    fn parse_valid_colon() {
        let expected_token = TestUtilities::colon_test_token(1);
        let actual = colon_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens Colon (:) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_colon() {
        let actual = colon_parser().parse(vec![TestUtilities::ident_test_token("ident", 1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens LeftAngleBracket (<)
    #[doc(hidden)]
    #[test]
    fn parse_valid_left_angle_bracket() {
        let expected_token = TestUtilities::left_angle_bracket_test_token(1);
        let actual =
            left_angle_bracket_parser("URI".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens LeftAngleBracket (<) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_left_angle_bracket() {
        let actual = left_angle_bracket_parser("URI".to_string())
            .parse(vec![TestUtilities::right_angle_bracket_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens RightAngleBracket (>)
    #[doc(hidden)]
    #[test]
    fn parse_valid_right_angle_bracket() {
        let expected_token = TestUtilities::right_angle_bracket_test_token(1);
        let actual =
            right_angle_bracket_parser("URI".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens RightAngleBracket (>) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_right_angle_bracket() {
        let actual = right_angle_bracket_parser("URI".to_string())
            .parse(vec![TestUtilities::left_angle_bracket_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens OpeningCurlyBrackets ({)
    #[doc(hidden)]
    #[test]
    fn parse_valid_opening_curly_bracket() {
        let expected_token = TestUtilities::opening_curly_brace_test_token(1);
        let actual =
            opening_curly_brace_parser("ITERATOR".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens OpeningCurlyBracket ({) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_invalid_opening_curly_bracket() {
        let actual = opening_curly_brace_parser("ITERATOR".to_string())
            .parse(vec![TestUtilities::closing_curly_brace_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsean los tokens ClosingCurlyBrackets (})
    #[doc(hidden)]
    #[test]
    fn parse_valid_closing_curly_bracket() {
        let expected_token = TestUtilities::closing_curly_brace_test_token(1);
        let actual =
            closing_curly_brace_parser("ITERATOR".to_string()).parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que no se parsean como tokens ClosingCurlyBrackets (}) aquellos que lo son
    #[doc(hidden)]
    #[test]
    fn not_parse_closing_opening_curly_bracket() {
        let actual = closing_curly_brace_parser("ITERATOR".to_string())
            .parse(vec![TestUtilities::opening_curly_brace_test_token(1)]);
        check_error(actual);
    }

    /// Comprueba que se parsea el token EOF
    #[doc(hidden)]
    #[test]
    fn parse_eof() {
        let expected_token = TestUtilities::eof_test_token(1);
        let actual = eof_parser().parse(vec![expected_token.clone()]);
        check_ok(expected_token, actual);
    }

    /// Comprueba que el resultado actual del test es igual al esperado
    ///
    /// # Argumentos
    /// * `expected` - El token esperado
    /// * `actual` - El token parseado real
    fn check_ok(expected: Token, actual: Result<Token, Vec<Simple<Token>>>) {
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el resultado actual del test es un error
    ///
    /// # Argumentos
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

    use crate::test_utils::TestUtilities;

    use super::*;

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax() {
        let tokens_vector: Vec<Token> = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("example", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://example.com/", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::source_test_token(2),
            TestUtilities::ident_test_token("films_csv_file", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::uri_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::query_test_token(3),
            TestUtilities::ident_test_token("query_sql", 3),
            TestUtilities::left_angle_bracket_test_token(3),
            TestUtilities::sql_type_test_token(3),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 3),
            TestUtilities::right_angle_bracket_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: Some(vec![QueryASTNode {
                identifier: "query_sql".to_string(),
                sql_query: "SELECT * FROM example;".to_string(),
            }]),
            iterators: None,
            expressions: None,
        };

        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file es capaz de generar el nodo raíz del AST si no hay query y no hay errores sintácticos
    #[doc(hidden)]
    #[test]
    fn file_parser_with_valid_sintax_and_withouth_query() {
        let tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("example", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://example.com/", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::source_test_token(2),
            TestUtilities::ident_test_token("films_csv_file", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::uri_test_token("https://shexml.herminiogarcia.com/files/films.csv", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::eof_test_token(3),
        ];

        let expected = FileASTNode {
            prefixes: vec![PrefixASTNode {
                identifier: "example".to_string(),
                uri: "https://example.com/".to_string(),
            }],
            sources: vec![SourceASTNode {
                identifier: "films_csv_file".to_string(),
                source_definition: "https://shexml.herminiogarcia.com/files/films.csv".to_string(),
            }],
            queries: None,
            iterators: None,
            expressions: None,
        };

        let actual = file_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap());
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay prefixes
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_prefixes() {
        let tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    /// Comprueba que el parser general de file no genera el nodo raíz del AST si no hay sources
    #[doc(hidden)]
    #[test]
    fn file_parser_withouth_sources() {
        let tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = file_parser().parse(tokens_vector.clone());
        // Es necesario crear el Result con el error dentro porque es lo que espera check_error
        let actual = Err(actual.unwrap_err());
        check_error::<FileASTNode>(actual, "Se esperaba un PREFIX o un SOURCE en la línea 1");
    }

    /// Comprueba que el parser de Prefix parsea la secuencia de tokens: Prefix Ident Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_prefix_sintax() {
        let mut tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = PrefixASTNode {
            identifier: "ident".to_string(),
            uri: "https://ejemplo.com".to_string(),
        };
        let actual = prefix_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más PREFIX
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::prefix_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::colon_test_token(2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::uri_test_token("https://ejemplo2.com", 2));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
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
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un PREFIX en la línea 1");
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Colon LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(
            actual,
            "Se esperaba un identificador después de 'PREFIX' en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident LeftAngleBracket Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_colon() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(
            actual,
            "Faltan los ':' después del identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Uri RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '<' antes de la URI en la línea 1");
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_uri() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba una URI entre '<' y '>' en la línea 1");
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon LeftAngleBracket Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '>' después de la URI en la línea 1");
    }

    /// Comprueba que el parser de Prefix no parsea como tales aquellas secuencias de tokens que son: Prefix Ident Colon Uri
    #[doc(hidden)]
    #[test]
    fn prefix_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::prefix_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::colon_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = prefix_parser().parse(fail_tokens_vector);
        check_error::<PrefixASTNode>(actual, "Se esperaba un '<' antes de la URI en la línea 1");
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket URI RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_uri() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "https://ejemplo.com/fichero.csv".to_string(),
        };
        let actual = sources_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::uri_test_token(
            "https://ejemplo2.com/fichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "https://ejemplo2.com/fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = sources_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket JdbcUrl RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_jdbc_url() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::jdbc_url_test_token("jdbc:mysql://localhost:3306/mydb", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "jdbc:mysql://localhost:3306/mydb".to_string(),
        };
        let actual = sources_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::jdbc_url_test_token(
            "jdbc:mysql://localhost:3356/anotherdb",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "jdbc:mysql://localhost:3356/anotherdb".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = sources_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket FilePath RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_file_path() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::file_path_test_token("file:///ejemplo/path/a/fichero/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "file:///ejemplo/path/a/fichero/fichero.csv".to_string(),
        };
        let actual = sources_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::file_path_test_token(
            "file:///otroejemplo/path/a/fichero/otrofichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "file:///otroejemplo/path/a/fichero/otrofichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = sources_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source parsea la secuencia de tokens: Source Ident LeftAngleBracket Path RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_source_sintax_with_path() {
        let mut tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::path_test_token("ejemplo/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = SourceASTNode {
            identifier: "ident".to_string(),
            source_definition: "ejemplo/fichero.csv".to_string(),
        };
        let actual = sources_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más SOURCE
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::source_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::path_test_token(
            "C:\\ejemplo\\path\\a\\fichero\\fichero.csv",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
        tokens_vector.push(eof_node.unwrap());

        let expected2 = SourceASTNode {
            identifier: "ident2".to_string(),
            source_definition: "C:\\ejemplo\\path\\a\\fichero\\fichero.csv".to_string(),
        };

        let expected_vector = vec![expected, expected2];
        let actual = sources_parser().parse(tokens_vector);
        assert_eq!(expected_vector, actual.unwrap());
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_source() {
        let fail_tokens_vector = vec![
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba un PREFIX o un SOURCE en la línea 1");
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source LeftAngleBracket (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un identificador después de 'SOURCE' en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path) RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_url_or_path() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(actual, "Se esperaba una URI entre '<' y '>' en la línea 1");
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident LeftAngleBracket (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '>' después de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Source no parsea como tales aquellas secuencias de tokens que son: Source Ident (Uri|JdbcUrl|FilePath|Path)
    #[doc(hidden)]
    #[test]
    fn source_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::source_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::uri_test_token("https://ejemplo.com/fichero.csv", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = sources_parser().parse(fail_tokens_vector);
        check_error::<SourceASTNode>(
            actual,
            "Se esperaba un '<' antes de la URL o ruta en la línea 1",
        );
    }

    /// Comprueba que el parser de Query parsea la secuencia de tokens: Query Ident LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_query_sintax() {
        let mut tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let expected = QueryASTNode {
            identifier: "ident".to_string(),
            sql_query: "SELECT * FROM example;".to_string(),
        };
        let actual = query_parser().parse(tokens_vector.clone());
        assert_eq!(expected, actual.unwrap()[0]);

        // Se añaden más QUERY
        let eof_node = tokens_vector.pop();
        tokens_vector.push(TestUtilities::query_test_token(2));
        tokens_vector.push(TestUtilities::ident_test_token("ident2", 2));
        tokens_vector.push(TestUtilities::left_angle_bracket_test_token(2));
        tokens_vector.push(TestUtilities::sql_type_test_token(2));
        tokens_vector.push(TestUtilities::sql_query_test_token(
            "SELECT * FROM example;",
            2,
        ));
        tokens_vector.push(TestUtilities::right_angle_bracket_test_token(2));
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
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba un QUERY en la línea 1");
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query LeftAngleBracket SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un identificador después de 'QUERY' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlQuery RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_type() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(actual, "Se esperaba 'sql:' después de '<' en la línea 1");
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_sql_query() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba una consulta SQL entre '<' y '>' en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident LeftAngleBracket SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '>' después de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Query no parsea como tales aquellas secuencias de tokens que son: Query Ident SqlType SqlQuery
    #[doc(hidden)]
    #[test]
    fn query_sintax_withouth_right_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::query_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = query_parser().parse(fail_tokens_vector);
        check_error::<QueryASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL en la línea 1",
        );
    }

    /// Comprueba que el parser de Field parsea la secuencia de tokens: Field Ident LeftAngleBracket Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn valid_field_sintax() {
        let tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
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
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(actual, "Se esperaba un FIELD en la línea 1");
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field LeftAngleBracket Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(
            actual,
            "Se esperaba un identificador después de 'FIELD' en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident Ident RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(
            actual,
            "Se esperaba un '<' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident LeftAngleBracket RightAngleBracket
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_access_field() {
        let fail_tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(
            actual,
            "Se esperaba un identificador después de '<' en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident LeftAngleBracket Ident
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(
            actual,
            "Se esperaba un '>' después de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Field no parsea como tales aquellas secuencias de tokens que son: Field Ident Ident
    #[doc(hidden)]
    #[test]
    fn field_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::field_test_token(1),
            TestUtilities::ident_test_token("field", 1),
            TestUtilities::key_identifier_test_token("@field", 1),
            TestUtilities::eof_test_token(1),
        ];

        let actual = field_parser().parse(fail_tokens_vector);
        check_error::<FieldASTNode>(
            actual,
            "Se esperaba un '<' antes de el identificador en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator parsea la secuencia de tokens:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn valid_iterator_sintax_with_sql_query() {
        let tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
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
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::field_test_token(3),
            TestUtilities::ident_test_token("field2", 3),
            TestUtilities::left_angle_bracket_test_token(3),
            TestUtilities::ident_test_token("field", 3),
            TestUtilities::right_angle_bracket_test_token(3),
            TestUtilities::closing_curly_brace_test_token(4),
            TestUtilities::eof_test_token(4),
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
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::ident_test_token("inline_query", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
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
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::csv_per_row_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
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
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(actual, "Se esperaba un ITERATOR en la línea 1");
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_identifier() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un identificador después de 'ITERATOR' en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_left_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL, identificador o csvperrow en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket RightAngleBracket OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_access() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un identificador después de '<' en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_right_angle_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '>' después de la consulta SQL, identificador o csvperrow en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_opening_curly_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '{' antes de los fields en la línea 2",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_fields() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::closing_curly_brace_test_token(2),
            TestUtilities::eof_test_token(2),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(actual, "Se esperaba un FIELD en la línea 2");
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket OpeningCurlyBracket Field*
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_closing_curly_bracket() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '}' después de los fields en la línea 3",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident (CsvperRow|Ident|SqlType SqlQuery) OpeningCurlyBracket Field* ClosingCurlyBracket
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_angle_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::opening_curly_brace_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::closing_curly_brace_test_token(3),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '<' antes de la consulta SQL, identificador o csvperrow en la línea 1",
        );
    }

    /// Comprueba que el parser de Iterator no parsea como tales aquellas secuencias de tokens que son:
    /// Iterator Ident LeftAngleBracket (CsvperRow|Ident|SqlType SqlQuery) RightAngleBracket Field*
    #[doc(hidden)]
    #[test]
    fn iterator_sintax_withouth_curly_brackets() {
        let fail_tokens_vector = vec![
            TestUtilities::iterator_test_token(1),
            TestUtilities::ident_test_token("ident", 1),
            TestUtilities::left_angle_bracket_test_token(1),
            TestUtilities::sql_type_test_token(1),
            TestUtilities::sql_query_test_token("SELECT * FROM example;", 1),
            TestUtilities::right_angle_bracket_test_token(1),
            TestUtilities::field_test_token(2),
            TestUtilities::ident_test_token("field", 2),
            TestUtilities::left_angle_bracket_test_token(2),
            TestUtilities::key_identifier_test_token("@field", 2),
            TestUtilities::right_angle_bracket_test_token(2),
            TestUtilities::eof_test_token(3),
        ];

        let actual = iterator_parser().parse(fail_tokens_vector);
        check_error::<IteratorASTNode>(
            actual,
            "Se esperaba un '{' antes de los fields en la línea 2",
        );
    }

    /// Comprueba que el resultado actual del test es un error y que el mensaje de este concuerda con el esperado
    ///
    /// Utiliza como tipo genérico el tipo de nodo del AST que se esté testeando
    ///
    /// #Argumentos
    /// * `actual` - El Result con el error
    /// * `error_message` - El mensaje de error esperado
    fn check_error<T>(actual: Result<Vec<T>, Vec<Simple<Token>>>, expected_error_message: &str) {
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
