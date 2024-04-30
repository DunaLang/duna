%{
#include <stdbool.h>
#include <stdint.h>
#include "tokens.h"

union {
    int64_t ival;
    uint64_t uval;
    double fval;

    bool bval;
    char* sval;
    char cval;

    char* identifier;
} yylval;

int charPos=1;

#define ADJ (charPos+= yyleng)

%}

%option nounput
%option noinput
%option noyywrap

intLiteral    [0-9]+
floatLiteral  [0-9]+.[0-9]+
stringLiteral \"[^\n"]+\"
identifier    [@a-zA-Z][a-zA-Z0-9]*

%%

[ \t\n]

"==" {
    ADJ; printf("EQUALITY\n"); return EQUALITY; }
"=" {
    ADJ; printf("EQUALS\n"); return EQUALS; }
"<=" {
    ADJ; printf("LESS_THAN_EQUALS\n"); return LESS_THAN_EQUALS; }
">=" {
    ADJ; printf("MORE_THAN_EQUALS\n"); return MORE_THAN_EQUALS; }
"<" {
    ADJ; printf("LESS_THAN\n"); return LESS_THAN; }
">" {
    ADJ; printf("MORE_THAN\n"); return MORE_THAN; }
"(" {
    ADJ; printf("LEFT_PAREN\n"); return LEFT_PAREN; }
")" {
    ADJ; printf("RIGHT_PAREN\n"); return RIGHT_PAREN; }
"[" {
    ADJ; printf("LEFT_BRACE\n"); return LEFT_BRACE; }
"]" {
    ADJ; printf("RIGHT_BRACE\n"); return RIGHT_BRACE; }
"{" {
    ADJ; printf("LEFT_BRACKET\n"); return LEFT_BRACKET; }
"}" {
    ADJ; printf("RIGHT_BRACKET\n"); return RIGHT_BRACKET; }
"." {
    ADJ; printf("DOT\n"); return DOT; }
"," {
    ADJ; printf("COMMA\n"); return COMMA; }
";" {
    ADJ; printf("SEMICOLON\n"); return SEMICOLON; }
"+" {
    ADJ; printf("PLUS\n"); return PLUS; }
"-" {
    ADJ; printf("MINUS\n"); return MINUS; }
"*" {
    ADJ; printf("ASTERISK\n"); return ASTERISK; }
"/" {
    ADJ; printf("SLASH\n"); return SLASH; }

"if" {
    ADJ; printf("IF\n"); return IF; }
"else" {
    ADJ; printf("ELSE\n"); return ELSE; }
"while" {
    ADJ; printf("WHILE\n"); return WHILE; }
"for" {
    ADJ; printf("FOR\n"); return FOR; }
"foreach" {
    ADJ; printf("FOREACH\n"); return FOREACH; }
"func" {
    ADJ; printf("FUNC\n"); return FUNC; }
"proc" {
    ADJ; printf("PROC\n"); return PROC; }
"return" {
    ADJ; printf("RETURN\n"); return RETURN; }
"break" {
    ADJ; printf("BREAK\n"); return BREAK; }
"continue" {
    ADJ; printf("CONTINUE\n"); return CONTINUE; }
"match" {
    ADJ; printf("MATCH\n"); return MATCH; }
"enum" {
    ADJ; printf("ENUM\n"); return ENUM; }
"union" {
    ADJ; printf("UNION\n"); return UNION; }
"struct" {
    ADJ; printf("STRUCT\n"); return STRUCT; }
"tuple" {
    ADJ; printf("TUPLE\n"); return TUPLE; }
"const" {
    ADJ; printf("CONST\n"); return CONST; }
"static" {
    ADJ; printf("STATIC\n"); return STATIC; }
"usize" {
    ADJ; printf("USIZE\n"); return USIZE; }
"u8" {
    ADJ; printf("U8\n"); return U8; }
"u16" {
    ADJ; printf("U16\n"); return U16; }
"u32" {
    ADJ; printf("U32\n"); return U32; }
"u64" {
    ADJ; printf("U64\n"); return U64; }
"i8" {
    ADJ; printf("I8\n"); return I8; }
"i16" {
    ADJ; printf("I16\n"); return I16; }
"i32" {
    ADJ; printf("I32\n"); return I32; }
"i64" {
    ADJ; printf("I64\n"); return I64; }
"f32" {
    ADJ; printf("F32\n"); return F32; }
"f64" {
    ADJ; printf("F64\n"); return F64; }
"bool" {
    ADJ; printf("BOOL\n"); return BOOL; }
"string" {
    ADJ; printf("STRING\n"); return STRING; }
"char" {
    ADJ; printf("CHAR\n"); return CHAR; }
"true" {
    ADJ; printf("TRUE\n"); return TRUE; }
"false" {
    ADJ; printf("FALSE\n"); return FALSE; }

{intLiteral}    { ADJ; yylval.ival=atoi(yytext); printf("int(%ld)\n", yylval.ival); return INTEGER_LITERAL; }
{floatLiteral}  { ADJ; yylval.fval=atof(yytext); printf("float(%f)\n", yylval.fval); return FLOAT_LITERAL; }
{stringLiteral} { ADJ; yylval.sval=String(yytext); printf("str(%s)\n", yylval.sval); return STRING_LITERAL; }
{identifier}    { ADJ; yylval.identifier=yytext; printf("id(%s)\n", yylval.identifier); return IDENTIFIER; }

<<EOF>> { printf("EOF\n"); yyterminate(); }

. { printf("Unrecognized character: %s\n", yytext); }

%%

int main(int argc, char const *argv[])
{
    argv++, argc--;

    if (argc != 1)
    {
        printf("Usage:\n\tdune <file_path>\n");
        return -1;
    }

    yyin = fopen(argv[0], "r");

    while (yywrap())
    {
        if (yylex() == END_OF_FILE)
        {
            break;
        }
    }

    return 0;
}
