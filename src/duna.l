%{
  #include <stdio.h>
  #include "y.tab.h"
  extern int yyerror (char *msg);
  int count_newlines(char* text);
  int process_bool_literal(char* text);
%}


booleanLiteral ("true"|"false")
intLiteral     [0-9]+
floatLiteral   [0-9]+.[0-9]+
stringLiteral  \"[^\n"]+\"
charLiteral    \'[^\\']\'
identifier     [@a-zA-Z_][a-zA-Z0-9_]*
commentary     (\/\*([^*]|[\r\n]|(\*+([^*\/]|[\r\n])))*\*+\/)|(\/\/.*)

%%

[ \t] {}
[\n]  { yylineno++; }

"++" { return CONCAT; }
"==" { return EQUALITY; }
"!=" { return INEQUALITY; }
"+=" { return ADD_ASSIGN; }
"-=" { return SUB_ASSIGN; }
"*=" { return MULT_ASSIGN; }
"/=" { return DIV_ASSIGN; }
"="  { return ASSIGN; }
"<=" { return LESS_THAN_EQUALS; }
">=" { return MORE_THAN_EQUALS; }
"<"  { return LESS_THAN; }
">"  { return MORE_THAN; }
"+"  { return PLUS; }
"-"  { return MINUS; }
"*"  { return ASTERISK; }
"/"  { return SLASH; }
"::" { return DOUBLE_COLON; }
"=>" { return EQUALS_ARROW; }
"&"  { return AMPERSAND; }
"#"  { return HASHTAG; }
"%"  { return PERCENTAGE; }
"("  { return yytext[0]; }
")"  { return yytext[0]; }
"["  { return yytext[0]; }
"]"  { return yytext[0]; }
"{"  { return yytext[0]; }
"}"  { return yytext[0]; }
"."  { return yytext[0]; }
","  { return yytext[0]; }
";"  { return yytext[0]; }
"_"  { return yytext[0]; }
":"  { return yytext[0]; }
"|"  { return yytext[0]; }

"if"       { return IF; }
"else"     { return ELSE; }
"while"    { return WHILE; }
"for"      { return FOR; }
"foreach"  { return FOREACH; }
"func"     { return FUNC; }
"proc"     { return PROC; }
"return"   { return RETURN; }
"break"    { return BREAK; }
"continue" { return CONTINUE; }
"match"    { return MATCH; }
"enum"     { return ENUM; }
"union"    { return UNION; }
"struct"   { return STRUCT; }
"const"    { return CONST; }
"static"   { return STATIC; }
"usize"    { return USIZE; }
"u8"       { return U8; }
"u16"      { return U16; }
"u32"      { return U32; }
"u64"      { return U64; }
"i8"       { return I8; }
"i16"      { return I16; }
"i32"      { return I32; }
"i64"      { return I64; }
"f32"      { return F32; }
"f64"      { return F64; }
"bool"     { return BOOL; }
"string"   { return STRING; }
"char"     { return CHAR; }
"not"      { return NOT; }
"and"      { return AND; }
"or"       { return OR; }
"new"      { return NEW; }
"delete"   { return DELETE; }
"print"    { return PRINT; }
"cast"     { return CAST; }
"typedef"  { return TYPEDEF; }
"null"     { return T_NULL; }

{booleanLiteral} { yylval.sValue = strdup(yytext); return BOOLEAN_LITERAL; }
{intLiteral}     { yylval.sValue = strdup(yytext); return INT_LITERAL; }
{floatLiteral}   { yylval.sValue = strdup(yytext); return FLOAT_LITERAL; }
{stringLiteral}  { yylval.sValue = strdup(yytext); return STRING_LITERAL; }
{charLiteral}    { yylval.cValue = yytext[1]; return CHAR_LITERAL; }
{identifier}     { yylval.sValue = strdup(yytext); return IDENTIFIER;}
{commentary}     { yylineno += count_newlines(yytext); }

. {yyerror("Unrecognized character"); }

%%

#ifndef yywrap
    int yywrap(void) {return 1;}
#endif

int count_newlines(char *text)
{
    int count = 0;
    while (*text != '\0')
    {
        if (*text == '\n')
        {
            count++;
        }
        text++;
    }
    return count;
}
