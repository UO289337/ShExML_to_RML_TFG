use crate::model::{ast::*, ast::nodes::*, lexer::token::*};

pub struct TestUtilities;

impl TestUtilities {
    // Test Utilities de los tokens

    /// Crea los tokens necesarios para tener un Prefix correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Prefix
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_prefix_test(num_line: u16) -> Vec<Token> {
        vec![
            Token::create_test_token(PREFIX, num_line, TokenType::Prefix),
            Token::create_test_token("example", num_line, TokenType::Ident),
            Token::create_test_token(COLON, num_line, TokenType::Colon),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token("https://example.com/", num_line, TokenType::Uri),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
        ]
    }

    /// Crea los tokens necesarios para tener un Source correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Source
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_source_test(num_line: u16) -> Vec<Token> {
        vec![
            Token::create_test_token(SOURCE, num_line, TokenType::Source),
            Token::create_test_token("films_csv_file", num_line, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token(
                "https://shexml.herminiogarcia.com/files/films.csv",
                num_line,
                TokenType::Uri,
            ),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
        ]
    }

    /// Crea los tokens necesarios para tener un Query correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Query
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_query_test(num_line: u16) -> Vec<Token> {
        vec![
            Token::create_test_token(QUERY, num_line, TokenType::Query),
            Token::create_test_token("inline_query", num_line, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token(SQL_TYPE, num_line, TokenType::SqlType),
            Token::create_test_token("SELECT * FROM example;", num_line, TokenType::SqlQuery),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
        ]
    }

    /// Crea los tokens necesarios para tener un Iterator correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Iterator
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_iterator_test(num_line: u16) -> Vec<Token> {
        let mut iterator = vec![
            Token::create_test_token(ITERATOR, num_line, TokenType::Iterator),
            Token::create_test_token("films_csv", num_line, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token("inline_query", num_line, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, num_line, TokenType::OpeningCurlyBrace),
        ];

        iterator.append(&mut Self::create_valid_field_key_identifier_test(
            num_line + 1,
        ));
        iterator.append(&mut Self::create_valid_field_test(num_line + 2, "name", false));
        iterator.append(&mut Self::create_valid_field_test(num_line + 3, "year", false));
        iterator.append(&mut Self::create_valid_field_test(num_line + 4, "country", false));

        iterator.append(&mut vec![Token::create_test_token(
            CLOSING_CURLY_BRACE,
            num_line + 5,
            TokenType::ClosingCurlyBrace,
        )]);

        iterator
    }

    /// Crea los tokens necesarios para tener un Field con un KeyIdentifier correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Field
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_field_key_identifier_test(num_line: u16) -> Vec<Token> {
        vec![
            Token::create_test_token(FIELD, num_line, TokenType::Field),
            Token::create_test_token("id", num_line, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token("@id", num_line, TokenType::KeyIdentifier),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
        ]
    }

    /// Crea los tokens necesarios para tener un Field correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Field
    /// * `ident` - El identificador que se utilizará en el campo y en el acceso a este
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_field_test(num_line: u16, ident: &str, key: bool) -> Vec<Token> {
        if key {
            vec![
                Token::create_test_token(FIELD, num_line, TokenType::Field),
                Token::create_test_token(ident, num_line, TokenType::Ident),
                Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
                Token::create_test_token(ident, num_line, TokenType::KeyIdentifier),
                Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
            ]
        } else {
            vec![
                Token::create_test_token(FIELD, num_line, TokenType::Field),
                Token::create_test_token(ident, num_line, TokenType::Ident),
                Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
                Token::create_test_token(ident, num_line, TokenType::Ident),
                Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
            ]
        }
    }

    /// Crea los tokens necesarios para tener un Expression correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Expression
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_expression_test(num_line: u16) -> Vec<Token> {
        vec![
            Token::create_test_token(EXPRESSION, num_line, TokenType::Expression),
            Token::create_test_token("films", num_line, TokenType::Ident),
            Token::create_test_token(LEFT_ANGLE_BRACKET, num_line, TokenType::LeftAngleBracket),
            Token::create_test_token("films_csv_file", num_line, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, num_line, TokenType::AccessDot),
            Token::create_test_token("films_csv", num_line, TokenType::Ident),
            Token::create_test_token(RIGHT_ANGLE_BRACKET, num_line, TokenType::RightAngleBracket),
        ]
    }

    /// Crea los tokens necesarios para tener un Shape correcto desde el punto de vista sintáctico
    ///
    /// # Parámetros
    /// * `num_line` - El número de línea donde está el Shape
    ///
    /// # Retorna
    /// Un vector de tokens
    pub fn create_valid_shape_test(num_line: u16) -> Vec<Token> {
        // Se crea junto con las Shape Tuples
        vec![
            Token::create_test_token("example", num_line, TokenType::Ident),
            Token::create_test_token(COLON, num_line, TokenType::Colon),
            Token::create_test_token("Films", num_line, TokenType::Ident),
            Token::create_test_token("example", num_line, TokenType::Ident),
            Token::create_test_token(COLON, num_line, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, num_line, TokenType::LeftBracket),
            Token::create_test_token("films", num_line, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, num_line, TokenType::AccessDot),
            Token::create_test_token("id", num_line, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, num_line, TokenType::RightBracket),
            Token::create_test_token(OPENING_CURLY_BRACE, num_line, TokenType::OpeningCurlyBrace),
            Token::create_test_token("example", num_line + 1, TokenType::Ident),
            Token::create_test_token(COLON, num_line + 1, TokenType::Colon),
            Token::create_test_token("name", num_line + 1, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, num_line + 1, TokenType::LeftBracket),
            Token::create_test_token("films", num_line + 1, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, num_line + 1, TokenType::AccessDot),
            Token::create_test_token("name", num_line + 1, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, num_line + 1, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, num_line + 1, TokenType::SemiColon),
            Token::create_test_token("example", num_line + 2, TokenType::Ident),
            Token::create_test_token(COLON, num_line + 2, TokenType::Colon),
            Token::create_test_token("year", num_line + 2, TokenType::Ident),
            Token::create_test_token(COLON, num_line + 2, TokenType::Colon),
            Token::create_test_token(LEFT_BRACKET, num_line + 2, TokenType::LeftBracket),
            Token::create_test_token("films", num_line + 2, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, num_line + 2, TokenType::AccessDot),
            Token::create_test_token("year", num_line + 2, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, num_line + 2, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, num_line + 2, TokenType::SemiColon),
            Token::create_test_token("example", num_line + 3, TokenType::Ident),
            Token::create_test_token(COLON, num_line + 3, TokenType::Colon),
            Token::create_test_token("country", num_line + 3, TokenType::Ident),
            Token::create_test_token(LEFT_BRACKET, num_line + 3, TokenType::LeftBracket),
            Token::create_test_token("films", num_line + 3, TokenType::Ident),
            Token::create_test_token(ACCESS_DOT, num_line + 3, TokenType::AccessDot),
            Token::create_test_token("country", num_line + 3, TokenType::Ident),
            Token::create_test_token(RIGHT_BRACKET, num_line + 3, TokenType::RightBracket),
            Token::create_test_token(SEMICOLON, num_line + 3, TokenType::SemiColon),
            Token::create_test_token(
                CLOSING_CURLY_BRACE,
                num_line + 5,
                TokenType::ClosingCurlyBrace,
            ),
        ]
    }

    // TestUtilities de los nodos del AST

    /// Crea un nodo Prefix del AST para ser utilizado en el AST
    ///
    /// # Parámetros
    /// `ident` - El identificador del Prefix
    /// `uri` - La URI del Prefix
    ///
    /// # Retorna
    /// Un Option con un vector de nodos Prefix que unicamente contiene un nodo Prefix
    pub fn create_prefixes_for_ast(ident: &str, uri: &str, num_line: u16) -> Vec<PrefixASTNode> {
        let identifier = Token::create_test_token(ident, num_line, TokenType::Ident);
        let uri = Token::create_test_token(uri, num_line, TokenType::Uri);

        vec![PrefixASTNode::new(Some(identifier), uri.clone(), Position::new(uri.get_num_line()))]
    }

    /// Crea un nodo Source del AST para ser utilizado en el AST
    ///
    /// # Parámetros
    /// `ident` - El identificador del Source
    /// `source_definition` - La definición del Source (fichero, URI, path)
    ///
    /// # Retorna
    /// Un Option con un vector de nodos Source que unicamente contiene un nodo Source
    pub fn create_sources_for_ast(
        ident: &str,
        uri: &str, num_line: u16
    ) -> Vec<SourceASTNode> {
        let identifier = Token::create_test_token(ident, num_line, TokenType::Ident);
        let source_definition = SourceDefinition::URI(uri.to_string());

        vec![SourceASTNode::new(identifier.clone(), source_definition, Position::new(identifier.get_num_line()))]
    }

    /// Crea un nodo Query del AST para ser utilizado en el AST
    ///
    /// # Parámetros
    /// `ident` - El identificador del Query
    /// `sql_query` - La consulta SQL
    ///
    /// # Retorna
    /// Un Option con un vector de nodos Query que unicamente contiene un nodo Query
    pub fn create_queries_for_ast(ident: &str, sql_query: &str, num_line: u16) -> Option<Vec<QueryASTNode>> {
        let identifier = Token::create_test_token(ident, num_line, TokenType::Ident);
        let query: Token = Token::create_test_token(sql_query, num_line, TokenType::SqlQuery);

        Some(vec![QueryASTNode::new(identifier.clone(), query, Position::new(identifier.get_num_line()))])
    }

    /// Crea un nodo Iterator del AST que tiene unos valores por defecto
    ///
    /// # Retorna
    /// Un vector de nodos Iterator del AST que contiene un nodo Iterator
    pub fn create_default_iterators_for_ast(num_line: u16) -> Vec<IteratorASTNode> {
        let fields = vec![
            FieldASTNode::new(Token::create_test_token("id", num_line + 1, TokenType::Ident), 
                Token::create_test_token("@id", num_line + 1, TokenType::KeyIdentifier), Position::new(num_line + 1)), 
            FieldASTNode::new(Token::create_test_token("name", num_line + 2, TokenType::Ident), 
                Token::create_test_token("name", num_line + 2, TokenType::Ident), Position::new(num_line + 2)), 
            FieldASTNode::new(Token::create_test_token("year", num_line + 3, TokenType::Ident), 
                Token::create_test_token("year", num_line + 3, TokenType::Ident), Position::new(num_line + 3)), 
            FieldASTNode::new(Token::create_test_token("country", num_line + 4, TokenType::Ident), 
                Token::create_test_token("country", num_line + 4, TokenType::Ident), Position::new(num_line + 4))];
        let ident = Token::create_test_token("films_csv", num_line, TokenType::Ident);
        let inline_query = Token::create_test_token("inline_query", num_line, TokenType::Ident);
        
        vec![IteratorASTNode::new(ident.clone(), inline_query, fields, Position::new(ident.get_num_line()))]
    }

    /// Crea un nodo Expression del AST que tiene unos valores por defecto
    ///
    /// # Retorna
    /// Un vector de nodos Expression del AST que contiene un nodo Expression
    pub fn create_default_expressions_for_ast(num_line: u16) -> Vec<ExpressionASTNode> {
        let identifier = Token::create_test_token("films_csv_file", num_line, TokenType::Ident);
        let first_access = Token::create_test_token("films_csv", num_line, TokenType::Ident);
        let accesses = vec![AccessASTNode::new(identifier.clone(), first_access, None, Position::new(identifier.get_num_line()))];

        let identifier = Token::create_test_token("films", num_line, TokenType::Ident);

        vec![ExpressionASTNode::new(identifier.clone(), ExpressionType::BASIC, accesses, Position::new(identifier.get_num_line()))]
    }

    /// Crea un nodo Shape del AST que tiene unos valores por defecto
    ///
    /// El nodo Shape incluye las Shape Tuples
    ///
    /// # Retorna
    /// Un vector de nodos Shape del AST que contiene un nodo Shape
    pub fn create_default_shapes_for_ast(num_line: u16) -> Vec<ShapeASTNode> {
        let prefix_ident = Token::create_test_token("example", num_line, TokenType::Ident);
        let identifier = Token::create_test_token("Films", num_line, TokenType::Ident);
        let field_prefix_ident = Token::create_test_token("example", num_line, TokenType::Ident);
        let access = AccessASTNode::new(Token::create_test_token("films", num_line, TokenType::Ident), 
            Token::create_test_token("id", num_line, TokenType::Ident), None, Position::new(num_line));
        let tuples = vec![ShapeTupleASTNode::new(
            Some(Token::create_test_token("example", num_line + 1, TokenType::Ident)), Token::create_test_token("name", num_line + 1, TokenType::Ident), None, 
            IdentOrAccess::Access(AccessASTNode::new(Token::create_test_token("films", num_line + 1, TokenType::Ident), 
            Token::create_test_token("name", num_line + 1, TokenType::Ident), None, Position::new(num_line + 1))), Position::new(num_line + 1)),
            ShapeTupleASTNode::new(
            Some(Token::create_test_token("example", num_line + 2, TokenType::Ident)), Token::create_test_token("year", num_line + 2, TokenType::Ident), None, 
            IdentOrAccess::Access(AccessASTNode::new(Token::create_test_token("films", num_line + 2, TokenType::Ident), 
            Token::create_test_token("year", num_line + 2, TokenType::Ident), None, Position::new(num_line + 2))), Position::new(num_line + 2)),
            ShapeTupleASTNode::new(
            Some(Token::create_test_token("example", num_line + 3, TokenType::Ident)), Token::create_test_token("country", num_line + 3, TokenType::Ident), None, 
            IdentOrAccess::Access(AccessASTNode::new(Token::create_test_token("films", num_line + 3, TokenType::Ident), Token::create_test_token("country", num_line + 3, TokenType::Ident), None, Position::new(num_line + 3))), Position::new(num_line + 3))];

        vec![ShapeASTNode::new(Some(prefix_ident), identifier.clone(), Some(field_prefix_ident), IdentOrAccess::Access(access), tuples, Position::new(identifier.get_num_line()))]
    }
}
