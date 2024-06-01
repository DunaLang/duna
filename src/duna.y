%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lib/record.h"

int yylex(void);
int yyerror(char *s);
extern int yylineno;
extern char * yytext;
extern FILE * yyin;
extern FILE * yyout;

char * cat(char *, char *, char *, char *, char *);
%}

%union {
    char* sValue;
    char cValue;
	  struct record * rec;
};


// Tokens terminais
%token <sValue> INT_LITERAL
%token <sValue> FLOAT_LITERAL
%token <sValue> STRING_LITERAL
%token <cValue> CHAR_LITERAL
%token <sValue> BOOLEAN_LITERAL
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

%type <rec> declarations declaration varDecl block proc type expr pointer statements statement literal primary

%start program

%%
program : declarations
  {
    fprintf(yyout, "#include <stddef.h>\n");
    fprintf(yyout, "#include <stdint.h>\n");
    fprintf(yyout, "#include <stdio.h>\n");
    fprintf(yyout, "%s\n", $1->code);
    freeRecord($1);
  };

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
  {
    char* s1 = cat($1->code, $2, "", "", "");
    free($2);
    $$ = createRecord(s1, "");
    free(s1);
  }
  | type IDENTIFIER ASSIGN expr ';'
  {
    char* s1 = cat($1->code, $2, " = ", $4->code, "");
    free($2);
    free($4);
    $$ = createRecord(s1, "");
    free(s1);
  }
  | typequalifiers type IDENTIFIER ';' { $$ = createRecord("", ""); }
  | typequalifiers type IDENTIFIER ASSIGN expr ';'{ $$ = createRecord("", ""); }

typedef : TYPEDEF type IDENTIFIER ';' 
  ;

proc : PROC IDENTIFIER '(' ')' block
  {
    char* s1 = cat("void", " ", $2, "()", $5->code);
    free($2);
    freeRecord($5);
    $$ = createRecord(s1, "");
    free(s1);
  }
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

union : UNION IDENTIFIER '{' fields '}';

struct : STRUCT IDENTIFIER '{' fields '}';

statements : statement
  | statements statement
  {
    char* s1 = cat($1->code, "\n", $2->code, "", "");
    freeRecord($1);
    freeRecord($2);
    $$ = createRecord(s1, ""); free(s1);
  }
  ;

statement : varDecl
  {
    char* s1 = cat($1->code, ";", "", "", "");
    freeRecord($1);
    $$ = createRecord(s1, "");  free(s1);
  }
  | assignment
  | compound_assignment ';'
  | while
  | for
  | foreach
  | BREAK ';'
  | CONTINUE ';'
  | PRINT expr ';'
  {
    char* s1 = cat("printf(\"%s\", ", $2->code, ");", "", "");
    $$ = createRecord(s1, "");
    freeRecord($2);
    free(s1);
  }
  | DELETE expr ';'
  | match
  | return
  | if
  | subprogramCall ';'
  ;

assignment : IDENTIFIER ASSIGN expr ';'
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

while : WHILE '(' expr ')' block;

for : FOR '(' forHeader ')' block;
forHeader : ';' ';'
  | statement expr ';' compound_assignment
  | ';' expr ';' compound_assignment
  | ';' ';' compound_assignment
  | statement ';'
  | statement expr ';'
  | ';' expr ';'
  ;

foreach : FOREACH '(' type IDENTIFIER ':' IDENTIFIER ')' block
  | FOREACH '(' typequalifier type IDENTIFIER ':' IDENTIFIER ')' block
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

if : IF '(' expr ')' block
  | IF '(' expr ')' block elseifs
  | IF '(' expr ')' block ELSE block
  | IF '(' expr ')' block elseifs ELSE block
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

type : USIZE { $$ = createRecord("size_t ", ""); }
  | U8  { $$ = createRecord("uint8_t ", ""); }
  | U16 { $$ = createRecord("uint16_t ", ""); }
  | U32 { $$ = createRecord("uint32_t ", ""); }
  | U64 { $$ = createRecord("uint64_t ", ""); }
  | I8  { $$ = createRecord("int8_t ", ""); }
  | I16 { $$ = createRecord("int16_t ", ""); }
  | I32 { $$ = createRecord("int32_t ", ""); }
  | I64 { $$ = createRecord("int64_t ", ""); }
  | F32 { $$ = createRecord("float ", ""); }
  | F64 { $$ = createRecord("double ", ""); }
  | BOOL { $$ = createRecord("_Bool ", ""); }
  | STRING { $$ = createRecord("string ", ""); }
  | CHAR { $$ = createRecord("char ", ""); }
  | IDENTIFIER { $$ = createRecord($1, ""); }
  | '[' expr ']' type %prec ARRAY_TYPE
  {
    char* s1 = cat("[", $2->code ,"]", "", "");
    freeRecord($2);
    $$ = createRecord($4->code, s1);
    freeRecord($4);
    free(s1);
  }
  | '[' ']' type %prec ARRAY_TYPE
  {
    $$ = createRecord($3->code, "[]");
    freeRecord($3);
  }
  | pointer { $$ = createRecord($1->code, ""); freeRecord($1); }
  ;

pointer : type ASTERISK
  {
    char* s1 = cat($1->code, "*", "","","");
    freeRecord($1);
    $$ = createRecord(s1, "");
    free(s1);
  };

primary : IDENTIFIER { $$ = createRecord($1, ""); }
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
  | STRING_LITERAL { $$ = createRecord($1, ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, ""); free($1);}
  | INT_LITERAL { $$ = createRecord($1, ""); free($1);}
  | BOOLEAN_LITERAL
  | T_NULL
  ;

expr: primary %prec UPRIMARY 
  | literal %prec ULITERAL { $$ = createRecord($1->code, ""); freeRecord($1); }
  | expr OR expr
  | expr AND expr
  | expr LESS_THAN expr
  | expr MORE_THAN expr
  | expr LESS_THAN_EQUALS expr
  | expr MORE_THAN_EQUALS expr
  | expr EQUALITY expr
  | expr INEQUALITY expr
  /*
    Operações numéricas são verificadas utilizando os seguintes passos, sequencialmente:
    1. Tipos tem que ser numéricos (f32, f64, u8, u16, u32, u64, i8, i16, i32, i64)
    2.
      a. Tipo da esquerda tem que ser igual ao da direita.
      OU
      b. Um tipo deve ser possível de realizar a coerção para o outro sem perda de informação
        Casos de coerção:
        - i8 -> i16 -> i32 -> i64
        - u8 -> u16 -> u32 -> u64
        - f32 -> f64
        - u8 -> i16
        - u16 -> i32
        - u32 -> i64
    3. Tipo de retorno é igual ao maior tipo
  */
  | expr PLUS expr
  {
    char* s1 = cat($1->code, " + ", $3->code, "", "");
    $$ = createRecord(s1, "");
    free($1);
    free($3);
  }
  | expr MINUS expr
  {
    char* s1 = cat($1->code, " - ", $3->code, "", "");
    $$ = createRecord(s1, "");
    free($1);
    free($3);
  }
  | expr ASTERISK expr
  {
    char* s1 = cat($1->code, " * ", $3->code, "", "");
    $$ = createRecord(s1, "");
    free($1);
    free($3);
  }
  | expr SLASH expr
  {
    char* s1 = cat($1->code, " / ", $3->code, "", "");
    $$ = createRecord(s1, "");
    free($1);
    free($3);
  }
  | expr PERCENTAGE expr
  {
    char* s1 = cat($1->code, " % ", $3->code, "", "");
    $$ = createRecord(s1, "");
    free($1);
    free($3);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ')' %prec UTYPE
  {
    // Checar se o tipo é string para casos especiais
    char* s1 = cat("(", $3->code, ")", $6->code, "");
    $$ = createRecord(s1, "");
    free($3);
    free($6);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | PLUS expr %prec UPLUS
  {
    char* s1 = cat(" + ", $2->code, "", "", "");
    $$ = createRecord(s1, "");
    free($2);
  }
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

block : '{' '}' { $$ = createRecord("{}", ""); }
  | '{' statements '}'
  {
    char* s1 = cat("{\n",$2->code,"\n}", "", "");
    freeRecord($2);
    $$ = createRecord(s1, "");
    free(s1);
  };

%%

int yyerror(char* msg) {
  fprintf (stderr, "Line %d: %s at '%s'\n", yylineno, msg, yytext);
	return 0;
}

int main(int argc, char **argv) {
    int code;
    if (argc < 2) {
        fprintf(stderr, "Uso: %s arquivo_de_entrada\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    yyout = fopen("duna.c", "w");
    
    if (!yyin) {
        fprintf(stderr, "Não foi possível abrir o arquivo %s\n", argv[1]);
        return 1;
    }

    code = yyparse();

    fclose(yyin);
    fclose(yyout);

    return 0;
}

char* cat(char * s1, char * s2, char * s3, char * s4, char * s5) {
  int tam;
  char * output;

  tam = strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4) + strlen(s5)+ 1;
  output = (char *) malloc(sizeof(char) * tam);
  
  if (!output){
    printf("Allocation problem. Closing application...\n");
    exit(0);
  }
  
  sprintf(output, "%s%s%s%s%s", s1, s2, s3, s4, s5);
  
  return output;
}