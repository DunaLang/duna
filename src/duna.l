%{
#include <stdio.h>
%}

intLiteral    [0-9]+
floatLiteral  [0-9]+.[0-9]+
stringLiteral \"[^\n"]+\"
charLiteral   \'[^\\']\'
identifier    [@a-zA-Z][a-zA-Z0-9]*

%%

[ \t\n] ;

"==" { printf("EQUALITY\n"); }
"<=" { printf("LESS_THAN_EQUALS\n"); }
">=" { printf("MORE_THAN_EQUALS\n"); }
"<"  { printf("LESS_THAN\n"); }
">"  { printf("MORE_THAN\n"); }
"="  { printf("ASSIGN\n"); }
"("  { printf("LEFT_PAREN\n"); }
")"  { printf("RIGHT_PAREN\n"); }
"["  { printf("LEFT_BRACE\n"); }
"]"  { printf("RIGHT_BRACE\n"); }
"{"  { printf("LEFT_BRACKET\n"); }
"}"  { printf("RIGHT_BRACKET\n"); }
"."  { printf("DOT\n"); }
","  { printf("COMMA\n"); }
";"  { printf("SEMICOLON\n"); }
"+"  { printf("PLUS\n"); }
"-"  { printf("MINUS\n"); }
"*"  { printf("ASTERISK\n"); }
"/"  { printf("SLASH\n"); }

"if"       { printf("IF\n"); }
"else"     { printf("ELSE\n"); }
"while"    { printf("WHILE\n"); }
"for"      { printf("FOR\n"); }
"foreach"  { printf("FOREACH\n"); }
"func"     { printf("FUNC\n"); }
"proc"     { printf("PROC\n"); }
"return"   { printf("RETURN\n"); }
"break"    { printf("BREAK\n"); }
"continue" { printf("CONTINUE\n"); }
"match"    { printf("MATCH\n"); }
"default"  { printf("DEFAULT\n"); }
"enum"     { printf("ENUM\n"); }
"union"    { printf("UNION\n"); }
"struct"   { printf("STRUCT\n"); }
"tuple"    { printf("TUPLE\n"); }
"const"    { printf("CONST\n"); }
"static"   { printf("STATIC\n"); }
"usize"    { printf("USIZE\n"); }
"u8"       { printf("U8\n"); }
"u16"      { printf("U16\n"); }
"u32"      { printf("U32\n"); }
"u64"      { printf("U64\n"); }
"i8"       { printf("I8\n"); }
"i16"      { printf("I16\n"); }
"i32"      { printf("I32\n"); }
"i64"      { printf("I64\n"); }
"f32"      { printf("F32\n"); }
"f64"      { printf("F64\n"); }
"bool"     { printf("BOOL\n"); }
"string"   { printf("STRING\n"); }
"char"     { printf("CHAR\n"); }
"true"     { printf("TRUE\n"); }
"false"    { printf("FALSE\n"); }
"not"      { printf("NOT\n"); }
"and"      { printf("AND\n"); }
"or"       { printf("OR\n"); }
"typedef"  { printf("TYPEDEF\n"); }
"new"      { printf("NEW\n"); }
"free"     { printf("FREE\n"); }
"print"    { printf("PRINT\n"); }

{intLiteral}    { printf("INTEGER(%s)\n", yytext); }
{floatLiteral}  { printf("FLOAT(%s)\n", yytext); }
{stringLiteral} { printf("STRING(%s)\n", yytext); }
{charLiteral}   { printf("CHAR(%s)\n", yytext); }
{identifier}    { printf("IDENT(%s)\n", yytext); }

"//".*\n ;
(\/\*([^*]|[\r\n]|(\*+([^*\/]|[\r\n])))*\*+\/)|(\/\/.*) ;

. { printf("Unrecognized character: %s\n", yytext); }

%%

#ifndef yywrap
    int yywrap(void) {return 1;}
#endif

int main(int argc, char const *argv[])
{
    argv++, argc--;

    if (argc != 1)
    {
        printf("Usage:\n\tdune <file_path>\n");
        return -1;
    }

    yyin = fopen(argv[0], "r");

    yylex();
    return 0;
}
