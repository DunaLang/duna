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
    ADJ; printf("==\n"); return EQUALITY; }
"=" {
    ADJ; printf("=\n"); return EQUALS; }
"<=" {
    ADJ; printf("<=\n"); return LESS_THAN_EQUALS; }
">=" {
    ADJ; printf(">=\n"); return MORE_THAN_EQUALS; }
"<" {
    ADJ; printf("<\n"); return LESS_THAN; }
">" {
    ADJ; printf(">\n"); return MORE_THAN; }
"(" {
    ADJ; printf("(\n"); return LEFT_PAREN; }
")" {
    ADJ; printf(")\n"); return RIGHT_PAREN; }
"[" {
    ADJ; printf("[\n"); return LEFT_BRACE; }
"]" {
    ADJ; printf("]\n"); return RIGHT_BRACE; }
"{" {
    ADJ; printf("{\n"); return LEFT_BRACKET; }
"}" {
    ADJ; printf("}\n"); return RIGHT_BRACKET; }
"." {
    ADJ; printf(".\n"); return DOT; }
"," {
    ADJ; printf(",\n"); return COMMA; }
";" {
    ADJ; printf(";\n"); return SEMICOLON; }
"+" {
    ADJ; printf("+\n"); return PLUS; }
"-" {
    ADJ; printf("-\n"); return MINUS; }
"*" {
    ADJ; printf("*\n"); return ASTERISK; }
"/" {
    ADJ; printf("/\n"); return SLASH; }

"if" {
    ADJ; printf("if\n"); return IF; }
"else" {
    ADJ; printf("else\n"); return ELSE; }
"while" {
    ADJ; printf("while\n"); return WHILE; }
"for" {
    ADJ; printf("for\n"); return FOR; }
"foreach" {
    ADJ; printf("foreach\n"); return FOREACH; }
"func" {
    ADJ; printf("func\n"); return FUNC; }
"proc" {
    ADJ; printf("proc\n"); return PROC; }
"return" {
    ADJ; printf("return\n"); return RETURN; }
"break" {
    ADJ; printf("break\n"); return BREAK; }
"continue" {
    ADJ; printf("continue\n"); return CONTINUE; }
"match" {
    ADJ; printf("match\n"); return MATCH; }
"enum" {
    ADJ; printf("enum\n"); return ENUM; }
"union" {
    ADJ; printf("union\n"); return UNION; }
"struct" {
    ADJ; printf("struct\n"); return STRUCT; }
"tuple" {
    ADJ; printf("tuple\n"); return TUPLE; }
"const" {
    ADJ; printf("const\n"); return CONST; }
"static" {
    ADJ; printf("static\n"); return STATIC; }
"usize" {
    ADJ; printf("usize\n"); return USIZE; }
"u8" {
    ADJ; printf("u8\n"); return U8; }
"u16" {
    ADJ; printf("u16\n"); return U16; }
"u32" {
    ADJ; printf("u32\n"); return U32; }
"u64" {
    ADJ; printf("u64\n"); return U64; }
"i8" {
    ADJ; printf("i8\n"); return I8; }
"i16" {
    ADJ; printf("i16\n"); return I16; }
"i32" {
    ADJ; printf("i32\n"); return I32; }
"i64" {
    ADJ; printf("i64\n"); return I64; }
"f32" {
    ADJ; printf("f32\n"); return F32; }
"f64" {
    ADJ; printf("f64\n"); return F64; }
"bool" {
    ADJ; printf("bool\n"); return BOOL; }
"string" {
    ADJ; printf("string\n"); return STRING; }
"char" {
    ADJ; printf("char\n"); return CHAR; }
"true" {
    ADJ; printf("true\n"); return TRUE; }
"false" {
    ADJ; printf("false\n"); return FALSE; }

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
