%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex(void);
int yyerror(char *s);
extern int yylineno;
extern char * yytext;
extern FILE * yyin;
%}

%union {
    int iValue;    
    float fValue;
    char* sValue;
    char cValue;
};


// Tokens terminais
%token <iValue> INT_LITERAL
%token <fValue> FLOAT_LITERAL
%token <sValue> STRING_LITERAL
%token <cValue> CHAR_LITERAL
%token <iValue> BOOLEAN_LITERAL
%token IF ELSE WHILE FOR FOREACH FUNC PROC RETURN BREAK CONTINUE 
%token MATCH ENUM UNION STRUCT TUPLE CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR TYPEDEF
%token NOT AND OR NEW DELETE PRINT
%token EQUALITY DIFFERENT ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS
%token <sValue> IDENTIFIER

%start program

%%
program : statements {}
  ;

statements : statement    {}
  | statements statement {}
  ;

statement : varDecl ';' {}
  | assignment ';'      {}
  | typedef ';'         {}
  | break ';'           {}
  | continue ';'        {}
  | return ';'          {}
  | if                  {}
  | while               {}
  | foreach             {}
  | struct ';'          {}
  | union ';'           {}
  | proc                {}
  | func                {}
  ;

break : BREAK;
continue : CONTINUE;
return : RETURN | RETURN expr;

fields : field ';'
  | fields field ';'
  ;

params : field
  | params ',' field
  ;

field : type id;

func : FUNC id '(' ')' ':' type block                     {printf(">>> Func v1\n");}
  | FUNC id '<' types '>' '(' ')' ':' type block          {printf(">>> Func v2 (precisa disso?)\n");}
  | FUNC id '(' params ')' ':' type block                 {printf(">>> Func v3\n");}
  | FUNC id '<' types '>' '(' params ')' ':' type block   {printf(">>> Func v4\n");}
  ;

proc : PROC id '(' ')' block                          {printf(">>> Proc v1\n");}
  | PROC id '(' params ')' block                      {printf(">>> Proc v2\n");}
  | PROC id '<' types '>' '(' params ')' block        {printf(">>> Proc v3\n");}
  ;

union : UNION id '{' fields '}'                       {printf(">>> Union\n");}
  ;

struct : STRUCT id '{' fields '}'                     {printf(">>> Struct v1\n");}
  | STRUCT id '<' types '>' '{' fields '}'            {printf(">>> Struct v2\n");}
  ;

foreach : FOREACH '(' type id ':' id ')' block        {printf(">>> Foreach v1\n");}
  | FOREACH '('typequalifier type id ':' id ')' block {printf(">>> Foreach v2\n");}
  ;

while : WHILE '(' expr ')' block            {printf(">>> While\n");}
  ;

if : IF '(' expr ')' block                   {printf(">>> If v1\n");}
  | IF '(' expr ')' block elseifs            {printf(">>> If v2\n");}
  | IF '(' expr ')' block ELSE block         {printf(">>> If v3\n");}
  | IF '(' expr ')' block elseifs ELSE block {printf(">>> If v4\n");}
  ;

elseifs : elseif
  | elseifs elseif 
  ;

elseif : ELSE IF '(' expr ')' block
  ;

varDecl : type id                  {printf(">>> Var declaration v1\n");}
  | type assignment                {printf(">>> Var declaration v2\n");}
  | typequalifiers type id         {printf(">>> Var declaration v3\n");}
  | typequalifiers type assignment {printf(">>> Var declaration v4\n");}

assignment : id '=' literal        {printf(">>> Assignment v1\n");}
  | id '=' id                      {printf(">>> Assignment v2\n");}
  /* | id '=' expr                    {printf(">>> Assignment v3\n");} */
  ;

typequalifiers : typequalifier
  | typequalifier typequalifiers
  ;

typequalifier : CONST
  | STATIC
  ;

typedef : TYPEDEF type id 
  ;

types : type
  | types ',' type
  ;

type : USIZE 
  | U8 {}
  | U16 {}
  | U32 {}
  | U64 {}
  | I8 {}
  | I16 {}
  | I32 {}
  | I64 {}
  | F32 {}
  | F64 {}
  | BOOL {}
  | STRING {}
  | CHAR {}
  | type '[' ']'
  | type '*'
  | id {}
  ;

expr : BOOLEAN_LITERAL;

literal : INT_LITERAL {}
  | FLOAT_LITERAL {}
  | STRING_LITERAL {}
  | CHAR_LITERAL {}
  | BOOLEAN_LITERAL {}
  ;

block : '{' '}'
  | '{' statements '}'
  ;
id : IDENTIFIER {};
%%

int yyerror(char* msg) {
  fprintf (stderr, "Line %d: %s at '%s'\n", yylineno, msg, yytext);
	return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Uso: %s arquivo_de_entrada\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    if (!yyin) {
        fprintf(stderr, "Não foi possível abrir o arquivo %s\n", argv[1]);
        return 1;
    }

    yyparse();

    fclose(yyin);

    return 0;
}