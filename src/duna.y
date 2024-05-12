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
%token EQUALITY INEQUALITY LESS_THAN_EQUALS MORE_THAN_EQUALS EQUALS_ARROW INCREMENT DECREMENT DOUBLE_COLON
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

varDecl : type IDENTIFIER ';'
  | type IDENTIFIER '=' expr ';'
  | typequalifiers type IDENTIFIER ';'
  | typequalifiers type IDENTIFIER '=' expr ';'

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

enum : ENUM IDENTIFIER '{' enumValues '}'
  | ENUM IDENTIFIER '<' type '>' '{' enumValues '}'
  ;

enumValues : IDENTIFIER
  | IDENTIFIER ','
  | IDENTIFIER '=' INT_LITERAL
  | IDENTIFIER '=' INT_LITERAL ','
  | enumValues IDENTIFIER '=' INT_LITERAL ','
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
  | ids ',' IDENTIFIER
  ;

statements : statement
  | statements statement
  ;

statement : varDecl
  | assignment
  | while
  | for
  | foreach
  | BREAK ';'
  | CONTINUE ';'
  | PRINT expr ';'
  | DELETE expr ';'
  | match
  | return
  | if
  ;

assignment : IDENTIFIER '=' expr ';' { printf(">>> Assignment\n"); } ;

while : WHILE '(' expr ')' block            {printf(">>> While\n");}
  ;

for : FOR '(' forHeader ')' block ;
forHeader : ';' ';'
  | statement expr statement
  | ';' expr ';' statement
  | ';' ';' statement
  | statement ';' ';'
  | statement ';' expr ';'
  | ';' expr ';'
  ;

foreach : FOREACH '(' type IDENTIFIER ':' IDENTIFIER ')' block        {printf(">>> Foreach v1\n");}
  | FOREACH '(' typequalifier type IDENTIFIER ':' IDENTIFIER ')' block {printf(">>> Foreach v2\n");}
  ;

match : MATCH '(' expr ')' '{' matchCases '}'
  | MATCH '(' expr ')' '{' matchCases ',' '}'
  ;
matchCases : matchCase
  | matchCases ',' matchCase
  ;
matchCase : matchLeft EQUALS_ARROW matchRight ;
matchLeft : multipleMatchLeft | '_' ;
multipleMatchLeft : literal
  | multipleMatchLeft '|' literal
  ;
matchRight : block | statement ;

return : RETURN ';'
  | RETURN expr ';'
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
  | fields field ';'
  ;

params : field
  | params ',' field
  ;

field : type IDENTIFIER;

typequalifiers : typequalifier
  | typequalifiers typequalifier
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
  | IDENTIFIER
  | type '[' ']'
  /* | type '*' */
  ;

expr : logicalOr ;

logicalOr : logicalOr OR logicalAnd
  | logicalAnd
  ;

logicalAnd : logicalAnd AND comparison
  | comparison
  ;

comparison : comparison '<' equality
  | comparison '>' equality
  | comparison LESS_THAN_EQUALS equality
  | comparison MORE_THAN_EQUALS equality
  | equality
  ;

equality : equality EQUALITY term
  | equality INEQUALITY term
  | term
  ;

term : term '+' factor
  | term '-' factor
  | factor
  ;

factor : factor '*' cast
  | factor '/' cast
  | factor '%' cast 
  | cast
  ;

cast : '(' type ')' addressOf
  | addressOf
  ;

addressOf : '&' dereference
  | dereference
  ;

dereference : '*' negation
  | negation
  ;

negation : NOT unary
  | unary
  ;

unary : INCREMENT primary
  | DECREMENT primary
  | primary INCREMENT
  | primary DECREMENT
  | '+' primary
  | '-' primary
  | '#' primary
  | primary
  ;

primary : literal
  | IDENTIFIER
  | subprogramCall
  | NEW type
  /* | '(' expr ')' */
  /* | arrayIndex */
  | arrayDef
  | enumDef
  | compoundTypeDef
  /* | tupleDef */
  /* | fieldAccess */
  ;

literal : CHAR_LITERAL
  | STRING_LITERAL
  | FLOAT_LITERAL
  | INT_LITERAL
  | BOOLEAN_LITERAL
  ;

arrayIndex : arrayDef '[' expr ']'
  | IDENTIFIER '[' expr ']'
  ;

subprogramCall : IDENTIFIER '(' ')'
  | IDENTIFIER '<' ids '>' '(' ')'
  | IDENTIFIER '(' arguments ')'
  | IDENTIFIER '<' ids '>' '(' arguments ')'
  ;
arguments : expr | arguments ',' expr ;

arrayDef : '[' ']' | '[' commaSeparatedExpr ']' ;
commaSeparatedExpr : expr
  | expr ','
  | expr ',' commaSeparatedExpr
  ;

enumDef : IDENTIFIER DOUBLE_COLON IDENTIFIER ;

compoundTypeDef : type '{' '}'
  | type '{' compoundTypeFields '}'
  ;
compoundTypeFields : type ':' expr
  | type ':' expr ','
  | type ':' expr ',' compoundTypeFields
  ;

tupleDef : '<' commaSeparatedExpr '>' ;

fieldAccess : expr '.' IDENTIFIER 
  | expr
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
