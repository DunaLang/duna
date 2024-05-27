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
    char* bValue;
};


// Tokens terminais
%token <iValue> INT_LITERAL
%token <fValue> FLOAT_LITERAL
%token <sValue> STRING_LITERAL
%token <cValue> CHAR_LITERAL
%token <bValue> BOOLEAN_LITERAL
%token IF ELSE WHILE FOR FOREACH FUNC PROC RETURN BREAK CONTINUE 
%token MATCH ENUM UNION STRUCT TUPLE CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR 
%token NOT AND OR NEW DELETE TYPEDEF PRINT
%token EQUALITY DIFFERENT ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS LESS_THAN MORE_THAN
%token <sValue> IDENTIFIER

%start program

%%
program : decls {}
  ;

decls : decl | decl decls ;
decl : funcDef
  | procDef
  | enumDecl
  | unionDecl
  | structDecl
  | var
  | pointer
  | statement
  ;

statement : assignment
  | var
  | pointer
  | return
  | if
  | for
  | while
  | foreach
  | subProgramCall ';'
  | typedef
  | delete
  | match
  | break
  | continue
  ;

delete : DELETE expression ;

typedef: TYPEDEF type type ';' ;

var : typeQualifiers type id ';'
  | type id ';'
  | typeQualifiers type assignment
  ;

pointer : typeQualifier type '*' id ';'
  | type '*' id ';'
  ;

assignment : id ASSIGN expression ';' ;

if : IF '(' expression ')' block
  | IF '(' expression ')' block elseif
  | IF '(' expression ')' block ELSE block
  | IF '(' expression ')' block elseif ELSE block
  ;

elseif : ELSE IF '(' expression ')' block
  | ELSE IF '(' expression')' block elseif
  ;

while : WHILE '(' expression ')' block ;

for : FOR '(' statement ';' expression ';' statement ')' block ;

foreach : FOREACH '(' type id ':' id ')' block ;

return : RETURN ';'
  | RETURN expression ';'
  ;

break : BREAK ';' ;
continue : CONTINUE ';' ;

match : MATCH '(' expression ')' '{' matchCases '}' ;

matchCases : matchCase
  | matchCase ','
  | matchCase ',' matchCases
  ;

matchCase : matchLeft ASSIGN MORE_THAN matchRight ;

matchLeft : literals
  | '_'
  ;

matchRight : block
  | statement
  ;

subProgramCall : id '(' ')'
  | id LESS_THAN ids MORE_THAN '(' ')'
  | id '(' arguments ')'
  | id LESS_THAN ids MORE_THAN '(' arguments ')'
  ;

arguments : expression
  | expression ',' arguments
  ;

expression : equality
  ;

equality : equality EQUALITY comparison
  | equality DIFFERENT comparison
  | comparison
  ;

comparison : comparison LESS_THAN term
  | comparison MORE_THAN term
  | comparison LESS_THAN_EQUALS term
  | comparison MORE_THAN_EQUALS term
  | term
  ;

term : term '+' factor
  | term '-' factor
  | factor
  ;

factor : factor '*' unary
  | factor '/' unary
  | factor '%' unary
  | unary
  ;

unary : '+' '+' unary
  | '-' '-' unary
  | unary '+' '+'
  | unary '-' '-'
  | '+' unary
  | '-' unary
  | NOT unary
  | '&' unary
  | '*' unary
  | '#' unary
  | primary
  ;

primary : id
  | literal
  | subProgramCall
  | cast
  | NEW type
  | '(' expression ')'
  | expression '[' expression ']'
  | expression '.' id
  | arrayDef
  | tupleDecl
  | unionDecl
  | expression
  ; 

literal : STRING_LITERAL
  | CHAR_LITERAL
  | INT_LITERAL
  | FLOAT_LITERAL
  | BOOLEAN_LITERAL
  ;

literals : literal
  | literal ',' literals
  ;

cast : '(' type ')' expression ;

arrayDef : '[' ']' 
  | '[' expression ']' 
  ;

funcDef : FUNC id '(' ')' ':' type block
  | FUNC id LESS_THAN types MORE_THAN '(' ')' ':' type block
  | FUNC id '(' params ')' ':' type block
  | FUNC id LESS_THAN types MORE_THAN '(' params ')' ':' type block
  ;

procDef : PROC id '(' ')' block
  | PROC id LESS_THAN types MORE_THAN '(' ')' block
  | PROC id '(' params ')' block
  | PROC id LESS_THAN types MORE_THAN '(' params ')' block
  ;

ids : id
  | id ',' ids
  ;

enumDecl : ENUM id '{' enumDef '}'
  | ENUM id LESS_THAN type MORE_THAN '{' enumDef '}'
  ;

enumDef : id
  | id ','
  | id ASSIGN expression
  | id ASSIGN expression ','
  | id ASSIGN expression ',' enumDef
  ;
unionDecl : UNION id '{' fields '}' ;
structDecl : STRUCT id '{' fields '}' ;
tupleDecl : TUPLE id '{' fields '}'  ;

field : type id ';' ;

fields : field
  | field fields
  ;

typeQualifier : CONST | STATIC ;

typeQualifiers : typeQualifier
  | typeQualifier typeQualifiers
  ;

param : type id
  | CONST type id
  ;

params : param 
  | param ',' params 
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
  | type '['']' {}
  | type '*'    {}
  | id  {}
  ;


types : type {}
  | type ',' types {}
  ;

statements : statement ';' {}
  | statement ';' statements  {}
  ;

block : '{' '}' 
  | '{' statements '}' 
  ;

id : IDENTIFIER ;
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