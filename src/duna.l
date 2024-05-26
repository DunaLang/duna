%{
  #include <stdio.h>
  #include "y.tab.h"
  // Para tipos é melhor: {yylval.sValue = strdup("u8"); return TYPE;} ?
  extern int yyerror (char *msg);
  int count_newlines(char* text);
  int process_bool_literal(char* text);
%}


booleanLiteral ("true"|"false")
intLiteral    [0-9]+
floatLiteral  [0-9]+.[0-9]+
stringLiteral \"[^\n"]+\"
charLiteral   \'[^\\']\'
identifier    [@a-zA-Z][a-zA-Z0-9]*
commentary       (\/\*([^*]|[\r\n]|(\*+([^*\/]|[\r\n])))*\*+\/)|(\/\/.*) 

%%

[ \t]   {}
[\n]    {yylineno++;}

"=="   { printf("TOKEN(%s)\n", yytext); return(EQUALITY);}
"!="   { printf("TOKEN(%s)\n", yytext); return(INEQUALITY);}
"="   { printf("TOKEN(%s)\n", yytext); return(ASSIGN);}
"<="   { printf("TOKEN(%s)\n", yytext); return(LESS_THAN_EQUALS);}
">="   { printf("TOKEN(%s)\n", yytext); return(MORE_THAN_EQUALS);}
"<"   { printf("TOKEN(%s)\n", yytext); return(LESS_THAN);}
">"   { printf("TOKEN(%s)\n", yytext); return(MORE_THAN);}
"+"   { printf("TOKEN(%s)\n", yytext); return(PLUS);}
"-"   { printf("TOKEN(%s)\n", yytext); return(MINUS);}
"*"   { printf("TOKEN(%s)\n", yytext); return(ASTERISK);}
"/"   { printf("TOKEN(%s)\n", yytext); return(SLASH);}
"++"   { printf("TOKEN(%s)\n", yytext); return(INCREMENT);}
"--"   { printf("TOKEN(%s)\n", yytext); return(DECREMENT);}
"::"   { printf("TOKEN(%s)\n", yytext); return(DOUBLE_COLON);}
"=>"   { printf("TOKEN(%s)\n", yytext); return(EQUALS_ARROW);}
"&"   { printf("TOKEN(%s)\n", yytext); return(AMPERSAND);}
"#"   { printf("TOKEN(%s)\n", yytext); return(HASHTAG);}
"%"   { printf("TOKEN(%s)\n", yytext); return(PERCENTAGE);}
"("   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
")"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"["   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"]"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"{"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"}"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"."   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
","   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
";"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
"_"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}
":"   { printf("TOKEN(%s)\n", yytext); return(yytext[0]);}

"if"          { printf("TOKEN(%s)\n", yytext); return(IF); }
"else"          { printf("TOKEN(%s)\n", yytext); return(ELSE); }
"while"          { printf("TOKEN(%s)\n", yytext); return(WHILE); }
"for"          { printf("TOKEN(%s)\n", yytext); return(FOR); }
"foreach"          { printf("TOKEN(%s)\n", yytext); return(FOREACH); }
"func"          { printf("TOKEN(%s)\n", yytext); return(FUNC); }
"proc"          { printf("TOKEN(%s)\n", yytext); return(PROC); }
"return"          { printf("TOKEN(%s)\n", yytext); return(RETURN); }
"break"          { printf("TOKEN(%s)\n", yytext); return(BREAK); }
"continue"          { printf("TOKEN(%s)\n", yytext); return(CONTINUE); }
"match"          { printf("TOKEN(%s)\n", yytext); return(MATCH); }
"enum"          { printf("TOKEN(%s)\n", yytext); return(ENUM); }
"union"          { printf("TOKEN(%s)\n", yytext); return(UNION); }
"struct"          { printf("TOKEN(%s)\n", yytext); return(STRUCT); }
"tuple"          { printf("TOKEN(%s)\n", yytext); return(TUPLE); }
"const"          { printf("TOKEN(%s)\n", yytext); return(CONST); }
"static"          { printf("TOKEN(%s)\n", yytext); return(STATIC); }
"usize"          { printf("TOKEN(%s)\n", yytext); return(USIZE); }
"u8"          { printf("TOKEN(%s)\n", yytext); return(U8); }
"u16"          { printf("TOKEN(%s)\n", yytext); return(U16); }
"u32"          { printf("TOKEN(%s)\n", yytext); return(U32); }
"u64"          { printf("TOKEN(%s)\n", yytext); return(U64); }
"i8"          { printf("TOKEN(%s)\n", yytext); return(I8); }
"i16"          { printf("TOKEN(%s)\n", yytext); return(I16); }
"i32"          { printf("TOKEN(%s)\n", yytext); return(I32); }
"i64"          { printf("TOKEN(%s)\n", yytext); return(I64); }
"f32"          { printf("TOKEN(%s)\n", yytext); return(F32); }
"f64"          { printf("TOKEN(%s)\n", yytext); return(F64); }
"bool"          { printf("TOKEN(%s)\n", yytext); return(BOOL); }
"string"          { printf("TOKEN(%s)\n", yytext); return(STRING); }
"char"          { printf("TOKEN(%s)\n", yytext); return(CHAR); }
"not"          { printf("TOKEN(%s)\n", yytext); return(NOT); }
"and"          { printf("TOKEN(%s)\n", yytext); return(AND); }
"or"          { printf("TOKEN(%s)\n", yytext); return(OR); }
"new"          { printf("TOKEN(%s)\n", yytext); return(NEW); }
"delete"          { printf("TOKEN(%s)\n", yytext); return(DELETE); }
"print"          { printf("TOKEN(%s)\n", yytext); return(PRINT); }
"cast"          { printf("TOKEN(%s)\n", yytext); return(CAST); }

{booleanLiteral}  {printf("LITERAL(%s)\n", yytext); yylval.iValue = process_bool_literal(yytext); return BOOLEAN_LITERAL;}
{intLiteral}      {printf("LITERAL(%s)\n", yytext); yylval.iValue = atoi(yytext); return INT_LITERAL; }
{floatLiteral}    {printf("LITERAL(%s)\n", yytext); yylval.fValue = atof(yytext); return FLOAT_LITERAL; }
{stringLiteral}   {printf("LITERAL(%s)\n", yytext); yylval.sValue = strdup(yytext); return STRING_LITERAL; }
{charLiteral}     {printf("LITERAL(%s)\n", yytext); yylval.cValue = yytext[1]; return CHAR_LITERAL; }
{identifier}      {printf("ID(%s)\n", yytext); yylval.sValue = strdup(yytext); return IDENTIFIER;}
{commentary}      {yylineno += count_newlines(yytext);}

. {yyerror("Unrecognized character"); }

%%

#ifndef yywrap
    int yywrap(void) {return 1;}
#endif

int count_newlines(char *text) {
    int count = 0;
    while (*text != '\0') {
        if (*text == '\n') {
            count++;
        }
        text++;
    }
    return count;
}

int process_bool_literal(char* text) {
   if (strcmp(text, "true") == 0) {
        return 1;
    } else {
        return 0;
    }
}