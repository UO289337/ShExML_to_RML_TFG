use winnow;

enum token_type {
    // TODO: Poner tipos de tokens
}

struct token {
    lexeme: &str,
    token_type: token_type,
}