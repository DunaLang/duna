%{
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../lib/record.h"
#include "../../lib/utils.h"
#include "../../lib/symbol_table.h"

int yylex(void);
int yyerror(char *s);
extern int yylineno;
extern char * yytext;
extern FILE * yyin;
extern FILE * yyout;

SymbolTable symbolTable;
%}

%union {
    char* sValue;
    char cValue;
	  struct record *rec;
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
%token EQUALITY INEQUALITY ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS LESS_THAN MORE_THAN PLUS MINUS ASTERISK SLASH DOUBLE_COLON EQUALS_ARROW AMPERSAND HASHTAG PERCENTAGE CONCAT
%token <sValue> IDENTIFIER

%nonassoc ULITERAL UPRIMARY
%nonassoc UHASHTAG UMINUS UPLUS
%nonassoc UNOT
%nonassoc UAMPERSAND
%nonassoc UTYPE
%right ARRAY_TYPE
%nonassoc UNEW
%nonassoc UPARENTESISEXPR
%left OR
%left AND
%left LESS_THAN MORE_THAN LESS_THAN_EQUALS MORE_THAN_EQUALS
%left EQUALITY INEQUALITY
%left CONCAT
%left MINUS PLUS
%left PERCENTAGE SLASH ASTERISK

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
    // free($2);
    $$ = createRecord(s1, $1->opt1, "");
    free($1);
    free(s1);
  }
  | type IDENTIFIER ASSIGN expr ';'
  {
    // assert $1 == $4.tipo
    // Mudar nome de variável para colocar subprograma atual

    if (isString($4)) {
      /*
        size_t a_len = TAMANHO_CALCULADO DE $4;
        char* a = malloc(sizeof(char*) * a_len);
        *a = "ola mundo";
      */
    } else {
      insert(&symbolTable, $2, $1->opt1);
      char* s1 = cat($1->code, $2, " = ", $4->code, "");
      //free($2);
      free($4);
      $$ = createRecord(s1, "", "");
      free(s1);
    }
  }
  | typequalifiers type IDENTIFIER ';' { $$ = createRecord("", "", ""); }
  | typequalifiers type IDENTIFIER ASSIGN expr ';'{ $$ = createRecord("", "", ""); }

typedef : TYPEDEF type IDENTIFIER ';' 
  ;

proc : PROC IDENTIFIER '(' ')' block
  {
    char* s1 = cat("void", " ", $2, "()", $5->code);
    free($2);
    freeRecord($5);
    $$ = createRecord(s1, "", "");
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
    $$ = createRecord(s1, "", ""); free(s1);
  }
  ;

statement : varDecl
  {
    char* s1 = cat($1->code, ";", "", "", "");
    freeRecord($1);
    $$ = createRecord(s1, "", "");  free(s1);
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
    /// TODO: Tipo esperado de $2 é string
    char* s1;
    if($2->prefix != NULL) {
      s1 = cat($2->prefix, "\n", "printf(\"%s\", ", $2->code, ");");
    }
    else {
      s1 = cat("printf(\"%s\", ", $2->code, ");", "", "");
    }
    $$ = createRecord(s1, "", "");
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

type : USIZE { $$ = createRecord("size_t ", "usize", ""); }
  | U8  { $$ = createRecord("uint8_t ", "u8", ""); }
  | U16 { $$ = createRecord("uint16_t ", "u16", ""); }
  | U32 { $$ = createRecord("uint32_t ", "u32", ""); }
  | U64 { $$ = createRecord("uint64_t ", "u64", ""); }
  | I8  { $$ = createRecord("int8_t ", "i8", ""); }
  | I16 { $$ = createRecord("int16_t ", "i16", ""); }
  | I32 { $$ = createRecord("int32_t ", "i32", ""); }
  | I64 { $$ = createRecord("int64_t ", "i64", ""); }
  | F32 { $$ = createRecord("float ", "f32", ""); }
  | F64 { $$ = createRecord("double ", "f64", ""); }
  | BOOL { $$ = createRecord("_Bool ", "bool", ""); }
  | STRING { $$ = createRecord("string ", "string", ""); }
  | CHAR { $$ = createRecord("char ", "char", ""); }
  | IDENTIFIER { $$ = createRecord($1, "", ""); }
  | '[' expr ']' type %prec ARRAY_TYPE
  {
    char* s1 = cat("[", $2->code ,"]", "", "");
    freeRecord($2);
    $$ = createRecord($4->code, s1, "");
    freeRecord($4);
    free(s1);
  }
  | '[' ']' type %prec ARRAY_TYPE
  {
    $$ = createRecord($3->code, "[]", "");
    freeRecord($3);
  }
  | pointer { $$ = createRecord($1->code, "", ""); freeRecord($1); }
  ;

pointer : type ASTERISK
  {
    char* s1 = cat($1->code, "*", "","","");
    freeRecord($1);
    $$ = createRecord(s1, "", "");
    free(s1);
  };

primary : IDENTIFIER {
    char* type = lookup(&symbolTable, $1);
    $$ = createRecord($1, type, "");
    // free($1);
  }
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
  | STRING_LITERAL { $$ = createRecord($1, "", ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, "", ""); free($1);}
  | INT_LITERAL { $$ = createRecord($1, "", ""); free($1);}
  | BOOLEAN_LITERAL
  | T_NULL
  ;

expr: primary %prec UPRIMARY { $$ = $1; }
  | literal %prec ULITERAL { $$ = createRecord($1->code, "", ""); freeRecord($1); }
  | expr OR expr
  | expr AND expr
  | expr LESS_THAN expr
  | expr MORE_THAN expr
  | expr LESS_THAN_EQUALS expr
  | expr MORE_THAN_EQUALS expr
  | expr EQUALITY expr
  | expr INEQUALITY expr
  | expr CONCAT expr
  {
    $$ = createRecord($1->code, "", ""); freeRecord($1);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | HASHTAG expr %prec UHASHTAG
  | '(' expr ')' %prec UPARENTESISEXPR {
    char* s1 = cat("(", $2->code, ")", "", "");
    $$ = createRecord(s1, "", "");
    free($2);
  }
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

    3. Por fim, tipo de retorno é igual ao maior tipo
  */
  | expr PLUS expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      // Erro (1)
      printf("Erro 1\n");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
      // Erro (2)
      printf("Erro 2\n");
      exit(-1);
    }

    char* s1 = cat($1->code, " + ", $3->code, "", "");
    $$ = createRecord(s1, resultType, "");
    free($1);
    free($3);
  }
  | expr MINUS expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      // Erro (1)
      printf("Erro 1\n");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
      // Erro (2)
      printf("Erro 2\n");
      exit(-1);
    }
    
    char* s1 = cat($1->code, " - ", $3->code, "", "");
    $$ = createRecord(s1, resultType, "");
    // free($1);
    // free($3);
  }
  | expr ASTERISK expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      // Erro (1)
      printf("Erro 1\n");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
      // Erro (2)
      printf("Erro 2\n");
      exit(-1);
    }
    
    char* s1 = cat($1->code, " * ", $3->code, "", "");
    $$ = createRecord(s1, resultType, "");
    // free($1);
    // free($3);
  }
  | expr SLASH expr
  {
    char* s1 = cat($1->code, " / ", $3->code, "", "");
    $$ = createRecord(s1, "", "");
    free($1);
    free($3);
  }
  | expr PERCENTAGE expr
  {
    char* s1 = cat($1->code, " % ", $3->code, "", "");
    $$ = createRecord(s1, "", "");
    free($1);
    free($3);
  }
  | CAST LESS_THAN type MORE_THAN '(' expr ')' %prec UTYPE
  {
    // Checar se o tipo é string para casos especiais
    if (strcmp($3->opt1, "string") == 0)
    {
      /// TODO: Checar se o tipo origem é numérico.
      /// TODO: Boolean, struct, union, enum, são casos especiais e devem ser tratados.

      char *s1 = cat("str", "", "", "", "");
      char *s2 = cat("int length = snprintf(NULL, 0, \"%f\", ", $6->code, ");\nchar str[length + 1];\nsnprintf(str, length + 1, \"%f\", ", $6->code, ");\n");

      $$ = createRecord(s1, "string", s2);
      free(s1);
    }
    else if (strcmp($6->opt1, "string") == 0)
    {

    }
    else
    {
      char* s1 = cat("(", $3->code, ")", $6->code, "");
      $$ = createRecord(s1, $3->opt1, "");
    }
    
    free($3);
    free($6);
  }
  | PLUS expr %prec UPLUS
  {
    char* s1 = cat(" + ", $2->code, "", "", "");
    $$ = createRecord(s1, "", "");
    free($2);
  }
  | MINUS expr %prec UMINUS
  ;

arrayIndex : arrayDef '[' expr ']'
  | IDENTIFIER '[' expr ']'
  | arrayIndex '[' expr ']'
  ;

subprogramCall : IDENTIFIER '(' ')'
  | IDENTIFIER '(' arguments ')'
  ;
arguments : expr | arguments ',' expr ;

arrayDef : '{' '}'
  | '{' commaSeparatedExpr '}'
  | '{' commaSeparatedExpr ',' '}'
  ;

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

block : '{' '}' { $$ = createRecord("{}", "", ""); }
  | '{' statements '}'
  {
    char* s1 = cat("{\n",$2->code,"\n}", "", "");
    freeRecord($2);
    $$ = createRecord(s1, "", "");
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
    yyout = fopen("./out/duna.c", "w");
    
    if (!yyin) {
        fprintf(stderr, "Não foi possível abrir o arquivo %s\n", argv[1]);
        return 1;
    }

    symbolTable = createSymbolTable();

    code = yyparse();

    fclose(yyin);
    fclose(yyout);

    return 0;
}
