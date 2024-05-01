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
%token IF ELSE WHILE FOR FOREACH FUNC PROC RETURN BREAK CONTINUE MATCH ENUM UNION STRUCT TUPLE CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR 
%token TRUE FALSE NOT AND OR
%token EQUALITY ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS LESS_THAN MORE_THAN
%token <sValue> IDENTIFIER

%start program

%%
program : statements {}
  ;

statements : statement ';' {}
  | statement ';' statements  {}
  ;

statement : type_specifier IDENTIFIER ASSIGN expression { printf("Assign{name: %s}\n", $2); }
  ;

type_specifier : USIZE 
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
  ;

expression : INT_LITERAL {printf("Expr_INT_LITERAL\n");}
  | STRING_LITERAL {}
  | FLOAT_LITERAL {}
  | CHAR_LITERAL {}
  | IDENTIFIER {}
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