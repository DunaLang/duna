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
%token NOT AND OR NEW DELETE PRINT CAST
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
    char* s1 = cat($1->code, " ", $2, "", "");
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
      char* s1 = cat($1->code, " ", $2, " = ", $4->code);
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
  {
    $$ = createRecord($1->code, "", "");
    free(s1);
  }
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
  | while { $$ = $1; }
  | for
  | foreach
  | BREAK ';'
  | CONTINUE ';'
  | PRINT expr ';'
  {
    if (!isString($2)) {
      char* errorMsg = cat("Expected print expression type to be string. Actual type: ", $2->opt1, "","","");
      yyerror(errorMsg);
      exit(0);
    }

    char* s1 = cat($2->prefix, "\n", "printf(\"%s\", ", $2->code, ");");
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
  char *s1 = formatStr(
    "%swhile_l%d:\n%s\nif (%s) goto while_l%d;\n",
    $3->prefix, yylineno, $5->code, $3->code, yylineno
  );

  $$ = createRecord(s1, "", "");
  free(s1);
  freeRecord($3);
  freeRecord($5);
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
    if (type == NULL || strlen(type) == 1) {
      printf("[Line %d] Variable %s is not defined\n", yylineno, $1);
      exit(1);
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
  ;

derreferencing : ASTERISK IDENTIFIER;

literal : CHAR_LITERAL
  | STRING_LITERAL { $$ = createRecord($1, "string", ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, "f32", ""); free($1);}
  | INT_LITERAL { $$ = createRecord($1, "i32", ""); free($1);}
  | BOOLEAN_LITERAL { $$ = createRecord($1, "bool", ""); free($1);}
  | T_NULL { $$ = createRecord($1, "null", ""); free($1);}
  ;

expr: primary %prec UPRIMARY { $$ = $1; }
  | literal %prec ULITERAL { $$ = $1; }
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

    char *s1 = formatStr("%s < %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);

    $$ = createRecord(s1, "bool", prefix);

    free(s1);
    free(prefix);
    freeRecord($1);
    freeRecord($3);
  }
  | expr MORE_THAN expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = ">, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *s1 = formatStr("%s > %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);

    $$ = createRecord(s1, "bool", prefix);

    free(s1);
    free(prefix);
    freeRecord($1);
    freeRecord($3);
  }
  | expr LESS_THAN_EQUALS expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = "<=, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *s1 = formatStr("%s <= %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);

    $$ = createRecord(s1, "bool", prefix);

    free(s1);
    free(prefix);
    freeRecord($1);
    freeRecord($3);
  }
  | expr MORE_THAN_EQUALS expr
  {
    if (!(isNumeric($1) && isNumeric($3)))
    {
      char* errorMsg = ">=, operands must be numeric\n";
      yyerror(errorMsg);
      exit(-1);
    }

    char *s1 = formatStr("%s >= %s", $1->prefix, $3->prefix);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);

    $$ = createRecord(s1, "bool", prefix);

    free(s1);
    free(prefix);
    freeRecord($1);
    freeRecord($3);
  }
  | expr EQUALITY expr
  | expr INEQUALITY expr
  | expr CONCAT expr
  {
    /// TODO: Mudar nomes das variáveis
    if (!(isString($1) && isString($3)))
    {
      printf("++, operands must be string\n");
      exit(-1);
    }
    // Adicionar no prefix
    char *currentConcatStr = formatStr("concat_str_%d", yylineno);
    // char *currentConcatStr = cat("concat_str_", lineStr3, "", "", "");
    // char *prefix1 = cat($1->prefix, $3->prefix, "char ", currentConcatStr, "[strlen(");
    // char *prefix2 = cat(prefix1, $1->code, ") + strlen(", $3->code, ")];\nstrcpy(");
    // char *prefix3 = cat(prefix2, currentConcatStr,",", $1->code, ");\nstrcat(");
    // char *prefix4 = cat(prefix3, currentConcatStr, ",", $3->code, ");");

    char *prefix = formatStr(
      "%s%schar %s[strlen(%s) + strlen(%s)];\nstrcpy(%s, %s);\nstrcat(%s, %s);\n",
      $1->prefix, $3->prefix, currentConcatStr, $1->code, $3->code, currentConcatStr, $1->code, currentConcatStr, $3->code
    );

    $$ = createRecord(currentConcatStr, "string", prefix);

    // free(prefix1);
    // free(prefix2);
    // free(prefix3);
    // free(prefix4);
    free(prefix);
    free(currentConcatStr);
    freeRecord($1);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | HASHTAG expr %prec UHASHTAG
  | '(' expr ')' %prec UPARENTESISEXPR {
    char* s1 = cat("(", $2->code, ")", "", "");
    $$ = createRecord(s1, $2->opt1, $2->prefix);
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
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
       yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
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
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
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
      yyerror("Operation invalid: one of the operands is not numeric.");
      exit(-1);
    }

    char *resultType = resultNumericType($1->opt1, $3->opt1);

    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
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
    free(s1);
    free($1);
    free($3);
  }
  | CAST LESS_THAN type MORE_THAN '(' expr ')' %prec UTYPE
  {
    // Checar se o tipo é string para casos especiais
    if (isString($3))
    {
      /// TODO: Boolean, struct, union, enum, são casos especiais e devem ser tratados.
      char* typeFormat;
      if(isInteger($6)) {
        typeFormat = "\"%ld\"";
      }
      else if(isNumeric($6)) {
        typeFormat = "\"%f\"";
      }
      
      /*
      -      char *s2 = cat("int length = snprintf(NULL, 0,", typeFormat, ", ", $6->code, ");\n");
      -      char *s3 = cat(s2, "char str[length + 1];\nsnprintf(str, length + 1,", typeFormat, ", ", $6->code);
      -      char *s4 = cat(s3, ");\n", "", "", "");
      -      $$ = createRecord(s1, "string", s4);
      */
      
      char *lineStr2 = itoa(yylineno);
      char *currentStr = cat("str_", lineStr2, "", "", "");
      char *length = cat("length_", lineStr2, "", "", "");
      char *code = cat(currentStr, "", "", "", "");
      char *s1 = cat("int ", length, " = snprintf(NULL, 0,", typeFormat, ", ");
      char *s2 = cat(s1, $6->code, ");\n", "char ", currentStr);
      char *s3 = cat(s2, "[", length," + 1];\nsnprintf(", currentStr); 
      char *s4 = cat(s3, ", ", length, "+ 1, ", typeFormat); 
      char *s5 = cat(s4, ", ", $6->code, ");\n", "");
      $$ = createRecord(code, "string", s5);
      free(s1);
      free(s2);
      free(s3);
      free(s4);
      free(s5);
      free(code);
      free(lineStr2);
      free(length);
      free(currentStr);
    }
    else if (isString($6))
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
