#pragma once

enum TokenKind
{
    EQUALITY,
    EQUALS,
    LESS_THAN_EQUALS,
    MORE_THAN_EQUALS,
    LESS_THAN,
    MORE_THAN,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    DOT,
    COMMA,
    SEMICOLON,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,

    IF,
    ELSE,
    WHILE,
    FOR,
    FOREACH,
    FUNC,
    PROC,
    RETURN,
    BREAK,
    CONTINUE,
    MATCH,
    ENUM,
    UNION,
    STRUCT,
    TUPLE,
    CONST,
    STATIC,

    USIZE,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,

    BOOL,
    STRING,
    CHAR,
    TRUE,
    FALSE,

    IDENTIFIER,
    STRING_LITERAL,
    CHAR_LITERAL,
    INTEGER_LITERAL,
    FLOAT_LITERAL,
};

char *String(char *text)
{
    size_t len = strlen(text) - 2;
    char *result = malloc(sizeof(char) * len);
    strncpy(result, text + 1, len);
    return result;
}
