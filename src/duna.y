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
program : declarations ;

declarations : declaration
  | declaration declarations
  ;

declaration : varDecl
  | typedef
  | proc
  | func
  | enum
  | union
  | struct
  | tuple
  ;

varDecl : type IDENTIFIER          { printf(">>> Var declaration v1\n"); }
  | type assignment                { printf(">>> Var declaration v2\n"); }
  | typequalifiers type IDENTIFIER { printf(">>> Var declaration v3\n"); }
  | typequalifiers type assignment { printf(">>> Var declaration v4\n"); }

typedef : TYPEDEF type IDENTIFIER ';' ;

proc : PROC IDENTIFIER '(' ')' block
  | PROC IDENTIFIER '(' params ')' block
  | PROC IDENTIFIER '<' types '>' '(' ')' block
  | PROC IDENTIFIER '<' types '>' '(' params ')' block
  ;

func : FUNC IDENTIFIER '(' ')' ':' type block
  | FUNC IDENTIFIER '(' params ')' ':' type block
  | FUNC IDENTIFIER '<' ids '>' '(' ')' ':' type block
  | FUNC IDENTIFIER '<' ids '>' '(' params ')' ':' type block
  ;

enum : ENUM IDENTIFIER '{' enumDef '}'
  | ENUM IDENTIFIER '<' type '>' '{' enumDef '}'
  ;

enumDef : IDENTIFIER
  | IDENTIFIER ','
  | IDENTIFIER '=' expr
  | IDENTIFIER '=' expr ','
  | IDENTIFIER '=' expr ',' enumDef
  ;

union : UNION IDENTIFIER '{' fields '}'                       {printf(">>> Union\n");}
  ;

struct : STRUCT IDENTIFIER '{' fields '}'                     {printf(">>> Struct v1\n");}
  | STRUCT IDENTIFIER '<' types '>' '{' fields '}'            {printf(">>> Struct v2\n");}
  ;

tuple : TUPLE IDENTIFIER '{' types '}'                     {printf(">>> Struct v1\n");}
  | TUPLE IDENTIFIER '<' types '>' '{' types '}'            {printf(">>> Struct v2\n");}
  ;

ids : IDENTIFIER
  | IDENTIFIER ',' ids
  ;


statements : statement
  | statement statements
  ;

statement : varDecl ';'
  | assignment ';'
  | while
  | for
  | foreach
  | BREAK ';'
  | CONTINUE ';'
  | return ';'
  | if
  ;

assignment : IDENTIFIER '=' literal        {printf(">>> Assignment v1\n");}
  | IDENTIFIER '=' IDENTIFIER                      {printf(">>> Assignment v2\n");}
  /* | IDENTIFIER '=' expr                    {printf(">>> Assignment v3\n");} */
  ;

while : WHILE '(' expr ')' block            {printf(">>> While\n");}
  ;

for : FOR '(' statement expr statement ')' block
  | FOR '(' ';' ';' ')' block
  | FOR '(' ';' ';' statement ')' block
  | FOR '(' ';' expr ';' ')' block
  | FOR '(' ';' expr ';' statement ')' block
  | FOR '(' statement ';' ';' ')' block
  | FOR '(' statement ';' expr ';' ')' block
  ;

foreach : FOREACH '(' type IDENTIFIER ':' IDENTIFIER ')' block        {printf(">>> Foreach v1\n");}
  | FOREACH '('typequalifier type IDENTIFIER ':' IDENTIFIER ')' block {printf(">>> Foreach v2\n");}
  ;

return : RETURN
  | RETURN expr
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

fields : field ';'
  | field ';' fields
  ;

params : field
  | field ',' params
  ;

field : type IDENTIFIER;

typequalifiers : typequalifier
  | typequalifier typequalifiers
  ;

typequalifier : CONST
  | STATIC
  ;

types : type
  | types ',' type
  ;

type : USIZE 
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | BOOL
  | STRING
  | CHAR
  | type '[' ']'
  | type '*'
  | IDENTIFIER
  ;

expr : BOOLEAN_LITERAL;

literal : INT_LITERAL
  | FLOAT_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | BOOLEAN_LITERAL
  ;

block : '{' '}'
  | '{' statements '}'
  ;

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
