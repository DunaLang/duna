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
%token <sValue> T_NULL
%token IF ELSE WHILE FOR FOREACH FUNC PROC RETURN BREAK CONTINUE
%token MATCH ENUM UNION STRUCT CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR TYPEDEF
%token NOT AND OR NEW DELETE PRINT READ CAST
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

%type <rec> declarations declaration varDecl block proc type expr pointer statements statement literal primary while

%start program

%%
program : declarations
  {
    fprintf(yyout, "#include <stdbool.h>\n");
    fprintf(yyout, "#include <stddef.h>\n");
    fprintf(yyout, "#include <stdint.h>\n");
    fprintf(yyout, "#include <stdio.h>\n");
    fprintf(yyout, "#include <string.h>\n");

    fprintf(yyout, "void $_readInput(char *dest) { char buf[1024]; scanf(\"%%[^\\n]\", buf); strcpy(dest, buf); }\n");

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
    if (lookup(&symbolTable, $2) != NULL)
    {
      printf("Identifier \"%s\" is already defined.\n", $2);
      exit(0);
    }

    char *code = formatStr("%s %s", $1->code, $2);
    $$ = createRecord(code, $1->opt1, "");

    freeRecord($1);
    free($2);
    free(code);
  }
  | type IDENTIFIER ASSIGN expr ';'
  {
    if (strcmp($1->opt1, $4->opt1) != 0)
    {
      printf("Identifier type is not expected. Actual: \"%s\". Expected: \"%s\"\n", $4->opt1, $1->opt1);
      exit(-1);
    }

    if (lookup(&symbolTable, $2) != NULL)
    {
      printf("Identifier \"%s\" is already defined.\n", $2);
      exit(-1);
    }
    insert(&symbolTable, $2, $1->opt1);

    if (isString($4))
    {
      char *code = formatStr("%schar %s[strlen(%s)];\nstrcpy(%s, %s)", $4->prefix, $2, $4->code, $2, $4->code);
      $$ = createRecord(code, "", "");
      free(code);
    }
    else
    {
      char *code = formatStr("%s%s %s = %s", $4->prefix, $1->code, $2, $4->code);
      $$ = createRecord(code, "", "");
      free(code);
    }

    freeRecord($1);
    free($2);
    freeRecord($4);
  }
  | typequalifiers type IDENTIFIER ';' { $$ = createRecord("", "", ""); }
  | typequalifiers type IDENTIFIER ASSIGN expr ';'{ $$ = createRecord("", "", ""); }

typedef : TYPEDEF type IDENTIFIER ';' 
  ;

proc : PROC IDENTIFIER '(' ')' block
  {
    char *code = formatStr("void %s() %s", $2, $5->code);
    $$ = createRecord(code, "", "");

    free($2);
    freeRecord($5);
    free(code);
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
  {
    $$ = createRecord($1->code, "", "");
    freeRecord($1);
  }
  | statements statement
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    $$ = createRecord(code, "", "");

    freeRecord($1);
    freeRecord($2);
    free(code);
  }
  ;

statement : varDecl
  {
    char *code = formatStr("%s;", $1->code);
    $$ = createRecord(code, "", "");

    freeRecord($1);
    free(code);
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
    if (!isString($2)) {
      char *errorMsg = formatStr("Expected print expression type to be string. Actual type: %s", $2->opt1);
      yyerror(errorMsg);
      exit(0);
    }

    char *code = formatStr("%s\nprintf(\"%%s\", %s);", $2->prefix, $2->code);
    $$ = createRecord(code, "", "");

    freeRecord($2);
    free(code);
  }
  | DELETE expr ';'
  | match
  | return
  | if
  | subprogramCall ';'
  ;

assignment : IDENTIFIER ASSIGN expr ';' {/* TODO Problema 2 precisa disso*/}
  | arrayIndex ASSIGN expr ';' {/* Problema 3 precisa disso*/}
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

while : WHILE '(' expr ')' block
{
  char *code = formatStr(
    "%swhile_l%d:\n%s\nif (%s) goto while_l%d;\n",
    $3->prefix, yylineno, $5->code, $3->code, yylineno
  );

  $$ = createRecord(code, "", "");

  freeRecord($3);
  freeRecord($5);
  free(code);
};

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

type : USIZE { $$ = createRecord("size_t", "usize", ""); }
  | U8  { $$ = createRecord("uint8_t", "u8", ""); }
  | U16 { $$ = createRecord("uint16_t", "u16", ""); }
  | U32 { $$ = createRecord("uint32_t", "u32", ""); }
  | U64 { $$ = createRecord("uint64_t", "u64", ""); }
  | I8  { $$ = createRecord("int8_t", "i8", ""); }
  | I16 { $$ = createRecord("int16_t", "i16", ""); }
  | I32 { $$ = createRecord("int32_t", "i32", ""); }
  | I64 { $$ = createRecord("int64_t", "i64", ""); }
  | F32 { $$ = createRecord("float", "f32", ""); }
  | F64 { $$ = createRecord("double", "f64", ""); }
  | BOOL { $$ = createRecord("_Bool", "bool", ""); }
  | STRING { $$ = createRecord("string", "string", ""); }
  | CHAR { $$ = createRecord("char", "char", ""); }
  | IDENTIFIER { $$ = createRecord($1, "", ""); free($1); }
  | '[' expr ']' type %prec ARRAY_TYPE
  {
    char *code = formatStr("[%s]", $2->code);
    $$ = createRecord($4->code, code, "");

    freeRecord($2);
    freeRecord($4);
    free(code);
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
    char *code = formatStr("%s*", $1->code);
    $$ = createRecord(code, "", "");

    freeRecord($1);
    free(code);
  };

primary : IDENTIFIER {
    char* type = lookup(&symbolTable, $1);
    if (type == NULL || strlen(type) == 1) {
      char *errorMsg = formatStr("variable %s is not defined.", $1);
      yyerror(errorMsg);
      exit(0);
    }
    $$ = createRecord($1, type, "");

    free($1);
  }
  | subprogramCall
  | NEW type %prec UNEW
  | arrayIndex
  | arrayDef
  | enumDef
  | compoundTypeDef
  | fieldAccess
  | derreferencing
  | READ '(' ')'
  {
    char *code = generateVariable();
    char *prefix = formatStr("char %s[1024];\n$_readInput(%s);\n", code, code);
    $$ = createRecord(code, "string", prefix);

    free(code);
    free(prefix);
  }
  ;

derreferencing : ASTERISK IDENTIFIER;

literal : CHAR_LITERAL
  | STRING_LITERAL { $$ = createRecord($1, "string", ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, "f32", ""); free($1); }
  | INT_LITERAL { $$ = createRecord($1, "i32", ""); free($1); }
  | BOOLEAN_LITERAL { $$ = createRecord($1, "bool", ""); free($1); }
  | T_NULL { $$ = createRecord($1, "null", ""); free($1); }
  ;

expr: primary %prec UPRIMARY
  | literal %prec ULITERAL
  | expr OR expr
  | expr AND expr
  | expr LESS_THAN expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = "<, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *code = formatStr("%s < %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr MORE_THAN expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = ">, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *code = formatStr("%s > %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr LESS_THAN_EQUALS expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = "<=, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *code = formatStr("%s <= %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr MORE_THAN_EQUALS expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = ">=, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *code = formatStr("%s >= %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr EQUALITY expr
  | expr INEQUALITY expr
  | expr CONCAT expr
  {
    if (!(isString($1) && isString($3)))
    {
      printf("++, operands must be string\n");
      exit(-1);
    }

    char *code = generateVariable();
    char *prefix = formatStr(
      "%s%schar %s[strlen(%s) + strlen(%s)];\nstrcpy(%s, %s);\nstrcat(%s, %s);\n",
      $1->prefix, $3->prefix, code, $1->code, $3->code, code, $1->code, code, $3->code
    );
    $$ = createRecord(code, "string", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | CAST LESS_THAN type MORE_THAN '(' expr ')' %prec UTYPE
  {
    // Checar se o tipo é string para casos especiais
    if (isString($3))
    {
      /// TODO: Boolean, struct, union, enum, são casos especiais e devem ser tratados.
      $$ = cast($3->opt1, $6);
    }
    else if (isString($6))
    {

    }
    else
    {
      char *code = formatStr("(%s) %s", $3->code, $6->code);
      $$ = createRecord(code, $3->opt1, "");

      free(code);
    }

    freeRecord($3);
    freeRecord($6);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | HASHTAG expr %prec UHASHTAG
  | '(' expr ')' %prec UPARENTESISEXPR {
    char *code = formatStr("(%s)", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);

    freeRecord($2);
    free(code);
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
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(-1);
    }

    char *code = formatStr("%s + %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, resultType, prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr MINUS expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(-1);
    }

    char *code = formatStr("%s - %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, resultType, prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr ASTERISK expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(-1);
    }
    
    char *code = formatStr("%s * %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, resultType, prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr SLASH expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(-1);
    }

    char *code = formatStr("%s / %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, resultType, prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr PERCENTAGE expr
  {
    if (!isNumeric($1) || !isNumeric($3)) {
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(-1);
    }

    char *code = formatStr("%s %% %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, resultType, prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | PLUS expr %prec UPLUS
  {
    if (!isNumeric($2)) {
      yyerror("Operation invalid: operand is not numeric.");
      exit(-1);
    }

    char *code = formatStr(" + %s", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);

    freeRecord($2);
    free(code);
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
    char *code = formatStr("{\n%s\n}", $2->code);
    $$ = createRecord(code, "", "");

    freeRecord($2);
    free(code);
  };

%%

int yyerror(char* msg) {
  fprintf (stderr, "Line %d: %s at '%s'\n", yylineno, msg, yytext);
	return 0;
}

int main(int argc, char **argv) {
    int code;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s entry_file\n", argv[0]);
        return 1;
    }

    yyin = fopen(argv[1], "r");
    yyout = fopen("./out/duna.c", "w");
    
    if (!yyin) {
        fprintf(stderr, "Error while opening the file %s\n", argv[1]);
        return 1;
    }

    symbolTable = createSymbolTable();

    code = yyparse();

    fclose(yyin);
    fclose(yyout);

    return 0;
}
