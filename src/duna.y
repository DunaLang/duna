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
%token MATCH ENUM UNION STRUCT CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR TYPEDEF
%token NOT AND OR NEW DELETE PRINT CAST T_NULL
%token ADD_ASSIGN SUB_ASSIGN MULT_ASSIGN DIV_ASSIGN
%token EQUALITY INEQUALITY ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS LESS_THAN MORE_THAN PLUS MINUS ASTERISK SLASH DOUBLE_COLON EQUALS_ARROW AMPERSAND HASHTAG PERCENTAGE
%token <sValue> IDENTIFIER

%nonassoc ULITERAL UPRIMARY
%nonassoc UHASHTAG UMINUS UPLUS
%nonassoc UNOT
%nonassoc UAMPERSAND
%nonassoc UTYPE
%right ARRAY_TYPE
%nonassoc UNEW
%left PERCENTAGE SLASH ASTERISK
%left MINUS PLUS
%left EQUALITY INEQUALITY
%left LESS_THAN MORE_THAN LESS_THAN_EQUALS MORE_THAN_EQUALS
%left AND
%left OR
%nonassoc UPARENTESISEXPR

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
  ;

varDecl : type IDENTIFIER ';'
  | type IDENTIFIER ASSIGN expr ';'
  | typequalifiers type IDENTIFIER ';'
  | typequalifiers type IDENTIFIER ASSIGN expr ';'

typedef : TYPEDEF type IDENTIFIER ';' 
  ;

proc : PROC IDENTIFIER '(' ')' block
  | PROC IDENTIFIER '(' params ')' block
  ;

func : FUNC IDENTIFIER '(' ')' ':' type block
  | FUNC IDENTIFIER '(' params ')' ':' type block
  ;

enum : ENUM IDENTIFIER '{' enumValues '}'
  | ENUM IDENTIFIER '{' enumValues ',' '}'
  ;

enumValues : IDENTIFIER
  | enumValues ',' IDENTIFIER
  | IDENTIFIER ASSIGN INT_LITERAL
  | enumValues ',' IDENTIFIER ASSIGN INT_LITERAL
  ;

union : UNION IDENTIFIER '{' fields '}'                       {printf(">>> Union\n");}
  ;

struct : STRUCT IDENTIFIER '{' fields '}'                     {printf(">>> Struct v1\n");}
  ;

statements : statement
  | statements statement
  ;

statement : varDecl
  | assignment
  | compound_assignment ';'
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
  | subprogramCall ';'
  ;

assignment : IDENTIFIER ASSIGN expr ';' { printf(">>> Assignment\n"); }
  | arrayIndex ASSIGN expr ';'
  | derreferencing ASSIGN expr ';'
  | fieldAccess ASSIGN expr ';'
  ;

compound_assignment : add_assignment
  | sub_assignment 
  | mul_assignment 
  | div_assignment
  ;

add_assignment : IDENTIFIER ADD_ASSIGN expr
  | arrayIndex ADD_ASSIGN expr
  | derreferencing ADD_ASSIGN expr
  | fieldAccess ADD_ASSIGN expr
  ;
sub_assignment : IDENTIFIER SUB_ASSIGN expr
  | arrayIndex SUB_ASSIGN expr
  | derreferencing SUB_ASSIGN expr
  | fieldAccess SUB_ASSIGN expr
  ;
mul_assignment : IDENTIFIER MULT_ASSIGN expr
  | arrayIndex MULT_ASSIGN expr
  | derreferencing MULT_ASSIGN expr
  | fieldAccess MULT_ASSIGN expr
  ;
div_assignment : IDENTIFIER DIV_ASSIGN expr
  | arrayIndex DIV_ASSIGN expr
  | derreferencing DIV_ASSIGN expr
  | fieldAccess DIV_ASSIGN expr
  ;

while : WHILE '(' expr ')' block            {printf(">>> While\n");}
  ;

for : FOR '(' forHeader ')' block ;
forHeader : ';' ';'
  | statement expr ';' compound_assignment
  | ';' expr ';' compound_assignment
  | ';' ';' compound_assignment
  | statement ';'
  | statement expr ';'
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
matchCase : matchLeft EQUALS_ARROW block ;
matchLeft : multipleMatchLeft | '_' ;
multipleMatchLeft : literal
  | multipleMatchLeft '|' literal
  ;

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
  | '[' expr ']' type %prec ARRAY_TYPE
  | '[' ']' type %prec ARRAY_TYPE
  | pointer
  ;

pointer : type ASTERISK;

primary : IDENTIFIER
  | subprogramCall
  | NEW type %prec UNEW
  | arrayIndex
  | arrayDef
  | enumDef
  | compoundTypeDef
  | fieldAccess
  | derreferencing
  ;

derreferencing : ASTERISK IDENTIFIER;

literal : CHAR_LITERAL
  | STRING_LITERAL
  | FLOAT_LITERAL
  | INT_LITERAL
  | BOOLEAN_LITERAL
  | T_NULL
  ;

expr: primary %prec UPRIMARY 
  | literal %prec ULITERAL
  | expr OR expr
  | expr AND expr
  | expr LESS_THAN expr
  | expr MORE_THAN expr
  | expr LESS_THAN_EQUALS expr
  | expr MORE_THAN_EQUALS expr
  | expr EQUALITY expr
  | expr INEQUALITY expr
  | expr PLUS expr
  | expr MINUS expr
  | expr ASTERISK expr
  | expr SLASH expr
  | expr PERCENTAGE expr
  | CAST LESS_THAN type MORE_THAN  '(' expr ')' %prec UTYPE
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | PLUS expr %prec UPLUS
  | MINUS expr %prec UMINUS
  | HASHTAG expr %prec UHASHTAG
  | '(' expr ')' %prec UPARENTESISEXPR

arrayIndex : arrayDef '[' expr ']'
  | IDENTIFIER '[' expr ']'
  | arrayIndex '[' expr ']'
  ;

subprogramCall : IDENTIFIER '(' ')'
  | IDENTIFIER '(' arguments ')'
  ;
arguments : expr | arguments ',' expr ;

arrayDef : '{' '}' | '{' commaSeparatedExpr '}' ;

commaSeparatedExpr : expr
  | commaSeparatedExpr ',' expr
  ;

enumDef : IDENTIFIER DOUBLE_COLON IDENTIFIER ;

compoundTypeDef : IDENTIFIER '{' '}'
  | IDENTIFIER '{' compoundTypeFields '}'
  ;

compoundTypeFields : type ':' expr
  | type ':' expr ','
  | type ':' expr ',' compoundTypeFields
  ;

fieldAccess : IDENTIFIER '.' IDENTIFIER
  | fieldAccess '.' IDENTIFIER
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
