%{
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../lib/symbol_table.h"
#include "../../lib/scope_stack.h"
#include "../../lib/symbol_utils.h"
#include "../../lib/record.h"
#include "../../lib/utils.h"
#include "../../lib/checks.h"

int yylex(void);
// yylineno and yytext in checks.c
extern FILE * yyin;
extern FILE * yyout;

extern SymbolTable symbolTable;
extern ScopeStack scopeStack;

char* actualSubprogramName;
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

%type <rec> declarations declaration varDecl proc
%type <rec> block type expr pointer statements statement literal primary assignment compound_assignment
%type <rec> ifStatement if else elseifs elseif
%type <rec> while for
%type <rec> arrayDef commaSeparatedExpr arrayIndex

%start program

%%
program : { insertScope(&scopeStack, "GLOBAL", ""); } declarations
  {
    fprintf(yyout, "#include <stdbool.h>\n");
    fprintf(yyout, "#include <stddef.h>\n");
    fprintf(yyout, "#include <stdint.h>\n");
    fprintf(yyout, "#include <stdio.h>\n");
    fprintf(yyout, "#include <stdlib.h>\n");
    fprintf(yyout, "#include <string.h>\n");

    fprintf(yyout, "void $_readInput(char *dest) { char buf[1024]; scanf(\"%%[^\\n]\", buf); getc(stdin); strcpy(dest, buf); }\n");

    fprintf(yyout, "%s\n", $2->code);
    freeRecord($2);

    pop(&scopeStack);
  };

declarations : declaration
  | declaration declarations
  ;

declaration : varDecl
  | typedef
  | { insertScope(&scopeStack, generateVariable(), "proc"); }
  proc
  { $$ = $2; pop(&scopeStack); }
  | func
  | enum
  | union
  | struct
  ;

varDecl : type IDENTIFIER ';'
  {
    check_symbol_not_exists_already($2);

    char *code = formatStr("%s %s", $1->code, $2);

    if (isString($1))
    {
      code = formatStr("char *%s = NULL", $2);
    }
    else if (isArray($1) && isSizeDefinedArray($1))
    {
      code = formatStr("%s %s%s", $1->prefix, $2, $1->code);  
    }
    else if (isArray($1))
    {
      yyerror("Invalid array definition: no size provided");
      exit(1);
    }
    symbolInsert($2, $1->opt1);
    $$ = createRecord(code, $1->opt1, "");

    freeRecord($1);
    free($2);
    free(code);
  }
  | type IDENTIFIER ASSIGN expr ';'
  {
    check_coerce_to_expected_numeric($1->opt1, $4->opt1);
    check_symbol_not_exists_already($2);

    symbolInsert($2, $1->opt1);

    if (isString($4))
    {
      char *code = formatStr("%schar *%s = strdup(%s)", $4->prefix, $2, $4->code);
      addDeallocationToScope(&scopeStack, $2);
      $$ = createRecord(code, "", "");
      free(code);
    }
    else
    {
      char *code = formatStr("%s%s %s = %s", $4->prefix, $1->code, $2, $4->code);
      if(isArray($1)) {
        code = formatStr("%s%s %s%s = %s", $4->prefix, $1->prefix, $2, $1->code, $4->code);
      }
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
    check_symbol_not_exists_already($2);

    symbolInsert($2, "proc");

    char *code = formatStr("void %s() %s", $2, $5->code);
    $$ = createRecord(code, "", "");

    free($2);
    freeRecord($5);
    free(code);
    free(actualSubprogramName);
    actualSubprogramName = NULL;
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
  | compound_assignment ';' {
    char *code = formatStr("%s;", $1->code);
    $$ = createRecord(code, "", $1->prefix);
    free(code);
  }
  | { insertScope(&scopeStack, generateVariable(), "while"); }
    while
    { $$ = $2; pop(&scopeStack); }
  | { insertScope(&scopeStack, generateVariable(), "for"); }
    for
    { $$ = $2; pop(&scopeStack); }
  | foreach
  | BREAK ';'
  {
    Scope *scope = nearestIteration(&scopeStack);
    if (scope == NULL)
    {
      char *errorMsg = formatStr("Break statement must be placed in a valid iteration statement (WHILE or FOR)\n");
      yyerror(errorMsg);
      free(errorMsg);
      exit(0);
    }

    char *deallocationCode = deallocationCodeCurrentScope(&scopeStack);
    char *code = formatStr("%sgoto end_%s;", (deallocationCode == NULL) ? "" : deallocationCode, scope->name);

    $$ = createRecord(code, "", "");
    free(code);
    free(deallocationCode);
  }
  | CONTINUE ';'
  {
    Scope *scope = nearestIteration(&scopeStack);
    if (scope == NULL)
    {
      char *errorMsg = formatStr("Continue statement must be placed in a valid iteration statement (WHILE or FOR)\n");
      yyerror(errorMsg);
      free(errorMsg);
      exit(0);
    }

    char *deallocationCode = deallocationCodeCurrentScope(&scopeStack);
    char *code = formatStr("%sgoto pre_end_%s;", (deallocationCode == NULL) ? "" : deallocationCode, scope->name);

    $$ = createRecord(code, "", "");
    free(code);
    free(deallocationCode);
  }
  | PRINT expr ';'
  {
    if (!isString($2)) {
      char *errorMsg = formatStr("Expected print expression type to be string. Actual type: %s", $2->opt1);
      yyerror(errorMsg);
      free(errorMsg);
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
  | { insertScope(&scopeStack, generateVariable(), "" ); }
    ifStatement
    {
      Scope *scope = top(&scopeStack, 0);
      char *code = formatStr("%s\nend_%s:;", $2->code, scope->name);
      $$ = createRecord(code, "", "");
      pop(&scopeStack);

      free(code);
      freeRecord($2);
    }
  | subprogramCall ';'
  ;

assignment : IDENTIFIER ASSIGN expr
  {
    char *type = symbolLookup($1);

    check_symbol_exists($1);
    char *exprType = $3->opt1;
    if(isNumeric($3)) {
      exprType = resultNumericType(type, $3->opt1);
    }
    check_expected_actual_type(type, exprType);

    if (isString($3))
    {
      // strcpy
    }
    else
    {
      char *code = formatStr("%s%s = %s;", $3->prefix, $1, $3->code);
      $$ = createRecord(code, "", "");
      free(code);
    }

    free($1);
    freeRecord($3);
  }
  | arrayIndex ASSIGN expr {
    char *exprType = $3->opt1;
    if(isNumeric($3)) {
      exprType = resultNumericType($1->opt1, $3->opt1);
    }
    check_expected_actual_type($1->opt1, exprType);
    
    char *code = formatStr("%s%s%s = %s", $1->prefix, $3->prefix, $1->code, $3->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($3);
  }
  | derreferencing ASSIGN expr
  | fieldAccess ASSIGN expr
  ;

compound_assignment : assignment {$$ = $1;}
  | add_assignment
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
  if (!isBoolean($3))
  {
    char *errorMsg = formatStr("Expression in WHILE parenthesis %s must be type=\"boolean\". Actual type=\"%s\".", $3->opt1, $3->opt1);
    yyerror(errorMsg);
    free(errorMsg);
    exit(0);
  }
  Scope *scope = top(&scopeStack, 0);

  char *code = formatStr(
    "%sbegin_%s:{\nif (!(%s)) goto end_%s;\n%s\ngoto begin_%s; \n} end_%s:;\n",
    $3->prefix, scope->name, $3->code, scope->name, $5->code, scope->name, scope->name
  );

  $$ = createRecord(code, "", "");

  freeRecord($3);
  freeRecord($5);
  free(code);
};

for : FOR '(' ';' ';' ')' block
  {
    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "begin_%s:%s\npre_end_%s:;\ngoto begin_%s;\nend_%s:;",
      scopeName, $6->code, scopeName, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($6);
    free(code);
  }
  | FOR '(' statement expr ';' compound_assignment ')' block
  {
    if (!isBoolean($4))
    {
      char *error = formatStr("Expected boolean expression. Actual type=%s.\n", $4->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "%s{\n%s\nbegin_%s:\nif (!(%s)) goto end_%s;\n%s\npre_end_%s:;\n%s\ngoto begin_%s;\n}\nend_%s:;",
      $4->prefix, $3->code, scopeName, $4->code, scopeName, $8->code, scopeName, $6->code, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($3);
    freeRecord($4);
    freeRecord($6);
    freeRecord($8);
    free(code);
  }
  | FOR '(' ';' expr ';' compound_assignment ')' block
  {
    if (!isBoolean($4))
    {
      char *error = formatStr("Expected boolean expression. Actual type=%s.\n", $4->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "%s{\nbegin_%s:\nif (!(%s)) goto end_%s;\n%s\npre_end_%s:;\n%s\ngoto begin_%s;\n}\nend_%s:;",
      $4->prefix, scopeName, $4->code, scopeName, $8->code, scopeName, $6->code, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($4);
    freeRecord($6);
    freeRecord($8);
    free(code);
  }
  | FOR '(' ';' ';' compound_assignment ')' block
  {
    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "{\nbegin_%s:\n%s\npre_end_%s:;\n%s\ngoto begin_%s;\n}\nend_%s:;",
      scopeName, $7->code, scopeName, $5->code, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($5);
    freeRecord($7);
    free(code);
  }
  | FOR '(' statement ';' ')' block
  {
    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "{\n%s\nbegin_%s:\n%s\npre_end_%s:;\ngoto begin_%s;\n}\nend_%s:;",
      $3->code, scopeName, $6->code, scopeName, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($3);
    freeRecord($6);
    free(code);
  }
  | FOR '(' statement expr ';' ')' block
  {
    if (!isBoolean($4))
    {
      char *error = formatStr("Expected boolean expression. Actual type=%s.\n", $4->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "%s{\n%s\nbegin_%s:\nif (!(%s)) goto end_%s;\n%s\npre_end_%s:;\ngoto begin_%s;\n}\nend_%s:;",
      $4->prefix, $3->code, scopeName, $4->code, scopeName, $7->code, scopeName, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($3);
    freeRecord($4);
    freeRecord($7);
    free(code);
  }
  | FOR '(' ';' expr ';' ')' block
  {
    if (!isBoolean($4))
    {
      char *error = formatStr("Expected boolean expression. Actual type=%s.\n", $4->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "%s{\nbegin_%s:\nif (!(%s)) goto end_%s;\n%s\npre_end_%s:;\ngoto begin_%s;\n}\nend_%s:;",
      $4->prefix, scopeName, $4->code, scopeName, $7->code, scopeName, scopeName, scopeName
    );
    $$ = createRecord(code, "", "");

    freeRecord($4);
    freeRecord($7);
    free(code);
  };

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

ifStatement : if
  | if else
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($2);
  }
  | if elseifs
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($2);
  }
  | if elseifs else
  {
    char *code = formatStr("%s\n%s%s", $1->code, $2->code, $3->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($2);
    freeRecord($3);
  }
  ;

if : { insertScope(&scopeStack, generateVariable(), "" ); }
  IF '(' expr ')' block
  {
    if (!isBoolean($4))
    {
      char *error = formatStr("Expected boolean expression in if conditional. Actual=%s\n", $4->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    Scope *outer = top(&scopeStack, 1);
    char* s1 = formatStr("%sif (!(%s)) goto end_%s;\n%s\ngoto end_%s;\nend_%s:;", $4->prefix, $4->code, scope->name, $6->code, outer->name, scope->name);

    $$ = createRecord(s1, "", "");

    pop(&scopeStack);

    freeRecord($4);
    freeRecord($6);
    free(s1);
  }

else : ELSE { insertScope(&scopeStack, generateVariable(), "" ); } block
  { $$ = $3; pop(&scopeStack); };

elseifs : elseif
  | elseifs elseif
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($2);
  };

elseif : ELSE { insertScope(&scopeStack, generateVariable(), "" ); } IF '(' expr ')' block
  {
    if (!isBoolean($5))
    {
      char *error = formatStr("Expected boolean expression in if conditional. Actual=%s\n", $5->opt1);
      yyerror(error);
      free(error);
      exit(0);
    }

    Scope *scope = top(&scopeStack, 0);
    Scope *outer = top(&scopeStack, 1);
    char *code = formatStr("%sif (!(%s)) goto end_%s;\n%s\ngoto end_%s;\nend_%s:;", $5->prefix, $5->code, scope->name, $7->code, outer->name, scope->name);

    $$ = createRecord(code, "", "");

    pop(&scopeStack);

    free(code);
    freeRecord($5);
    freeRecord($7);
  };

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
  | IDENTIFIER { 
    $$ = createRecord($1, "", ""); free($1); yyerror("IDENTIFIER as type not supported"); }
  | '[' expr ']' type %prec ARRAY_TYPE
  {
    if(!isInteger($2)) {
      char *errorMsg = formatStr("Type error: expected %s to be numeric but instead got: %s", $2->code, $2->opt1);
      yyerror(errorMsg);
      exit(1);
    }
    char *code = formatStr("[%s]", $2->code);
    char *prefix = $4->code;
    // If type has prefix, then it is a multidimensional array
    if(strcmp($4->prefix, "") != 0) {
      code = formatStr("[%s]%s", $2->code, $4->code);
      prefix = $4->prefix;
    }
    $$ = createRecord(code, $4->opt1, prefix);

    freeRecord($2);
    freeRecord($4);
    free(code);
  }
  | '[' ']' type %prec ARRAY_TYPE
  {
    $$ = createRecord("[]", $3->opt1, $3->code);
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
    char* type = symbolLookup($1);

    check_symbol_exists($1);

    $$ = createRecord($1, type, "");

    free($1);
  }
  | subprogramCall
  | NEW type %prec UNEW
  | arrayIndex {$$ = $1;}
  | arrayDef {
    $$ = createRecord($1->code, $1->opt1, $1->prefix);
    freeRecord($1);
  }
  | enumDef
  | compoundTypeDef
  | fieldAccess
  | derreferencing
  | READ '(' ')'
  {
    char *code = generateVariable();
    char *prefix = formatStr("char %s[1024]; $_readInput(%s);\n", code, code);
    $$ = createRecord(code, "string", prefix);

    free(code);
    free(prefix);
  }
  ;

derreferencing : ASTERISK IDENTIFIER;

literal : CHAR_LITERAL
  | STRING_LITERAL { $$ = createRecord($1, "string", ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, "f32", ""); free($1); }
  | INT_LITERAL { 
    char *type = typeByNumberBitRange($1);
    $$ = createRecord($1, type, ""); 
    free($1);
  }
  | BOOLEAN_LITERAL { $$ = createRecord($1, "bool", ""); free($1); }
  | T_NULL { $$ = createRecord($1, "null", ""); free($1); }
  ;

expr: primary {$$ = $1;} %prec UPRIMARY
  | literal {$$ = $1;} %prec ULITERAL 
  | expr OR expr
  {
    if (!(isBoolean($1) && isBoolean($3)))
    {
      char* errorMsg = "or, operands must be boolean\n";
      yyerror(errorMsg);
      exit(0);
    }

    char *code = formatStr("%s || %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr AND expr
  {
    check_operands_boolean($1,$3, "and");

    char *code = formatStr("%s && %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr LESS_THAN expr
  {
    check_operands_numeric($1, $3, "<");

    char *code = formatStr("%s < %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr MORE_THAN expr
  {
    check_operands_numeric($1,$3,">");

    char *code = formatStr("%s > %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr LESS_THAN_EQUALS expr
  {
    check_operands_numeric($1,$3,"<=");

    char *code = formatStr("%s <= %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr MORE_THAN_EQUALS expr
  {
    check_operands_numeric($1,$3,">=");

    char *code = formatStr("%s >= %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr EQUALITY expr
  {
    char *code;
    char *prefix;

    if (isBoolean($1) && isBoolean($3))
    {
      code = formatStr("%s == %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else if (isNumeric($1) && isNumeric($3))
    {
      code = formatStr("%s == %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else if (isString($1) && isString($3))
    {
      code = formatStr("strcmp(%s, %s) == 0", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    // missing struct, array, ...
    else
    {
      char* errorMsg = "==, operands must be both boolean, numeric or string\n";
      yyerror(errorMsg);
      exit(0);
    }

    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr INEQUALITY expr
  {
    char *code;
    char *prefix;

    if (isBoolean($1) && isBoolean($3))
    {
      code = formatStr("%s != %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else if (isNumeric($1) && isNumeric($3))
    {
      code = formatStr("%s != %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else if (isString($1) && isString($3))
    {
      code = formatStr("strcmp(%s, %s) != 0", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    // missing struct, array, ...
    else
    {
      char* errorMsg = "!=, operands must be both boolean, numeric or string\n";
      yyerror(errorMsg);
      exit(0);
    }

    $$ = createRecord(code, "bool", prefix);

    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  | expr CONCAT expr
  {
    if (!(isString($1) && isString($3)))
    {
      printf("++, operands must be string\n");
      exit(0);
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
    $$ = castR($3, $6);

    freeRecord($3);
    freeRecord($6);
  }
  | CAST LESS_THAN type MORE_THAN  '(' expr ',' expr ')' %prec UTYPE
  {
    // Se o segundo argumento é null, o código c gerado é o mesmo do cast com 1 argumento
    if(strcmp($8->opt1, "null") == 0){
      $$ = castR($3, $6);
    }

    freeRecord($3);
    freeRecord($6);
    freeRecord($8);
  }
  | AMPERSAND expr %prec UAMPERSAND
  | NOT expr %prec UNOT
  | HASHTAG expr %prec UHASHTAG {
    check_symbol_exists($2->code);

    char *code = generateVariable();
    char *prefix = formatStr("size_t %s = sizeof(%s) / sizeof(%s[0]);\n", code, $2->code, $2->code);

    $$ = createRecord(code, "usize", prefix);
    free(code);
    free(prefix);
    freeRecord($2);
  }
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
    check_operands_numeric($1,$3,"+");

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(0);
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
    check_operands_numeric($1,$3,">=");

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(0);
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
    check_operands_numeric($1,$3,"*");

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(0);
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
    check_operands_numeric($1,$3,"/");

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(0);
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
    check_operands_numeric($1,$3,"%");

    char *resultType = resultNumericType($1->opt1, $3->opt1);
    if (resultType == NULL) {
      yyerror("Cohersion error: operands types does not match automatic cohersion. Consider a cast instead.");
      exit(0);
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
      exit(0);
    }

    char *code = formatStr(" + %s", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);

    freeRecord($2);
    free(code);
  }
  | MINUS expr %prec UMINUS
  ;

arrayIndex : arrayDef '[' expr ']' {/* Não quero fazer isso, Nathãn! grr*/}
  | IDENTIFIER '[' expr ']' {
    char* type = symbolLookup($1);
    check_symbol_exists($1);
    if(!isInteger($3)) {
      char *errorMsg = formatStr("Type error: expected %s to be integer but instead got: %s", $3->code, $3->opt1);
      yyerror(errorMsg);
      exit(1);
    }
    
    char *code = formatStr("%s[%s]", $1, $3->code);
    $$ = createRecord(code, type, $3->prefix);
    freeRecord($3);
    free(code);
  }
  | arrayIndex '[' expr ']' {
    if(!isInteger($3)) {
      char *errorMsg = formatStr("Type error: expected %s to be integer but instead got: %s", $3->code, $3->opt1);
      yyerror(errorMsg);
      exit(1);
    }

    char *code = formatStr("%s[%s]", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, $1->opt1, prefix);
    freeRecord($1);
    freeRecord($3);
    free(code);
    free(prefix);
  }
  ;

subprogramCall : IDENTIFIER '(' ')'
  | IDENTIFIER '(' arguments ')'
  ;

arguments : expr 
  | arguments ',' expr 
  ;

arrayDef : '{' commaSeparatedExpr '}' {
    char *code = formatStr("{%s}", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);
    freeRecord($2);
    free(code);
  }
  | '{' commaSeparatedExpr ',' '}' {
    char *code = formatStr("{%s}", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);
    freeRecord($2);
    free(code);
  }
  ;

commaSeparatedExpr : expr {$$ = createRecord($1->code, $1->opt1, $1->prefix);}
  | commaSeparatedExpr ',' expr {
    check_expected_actual_type($1->opt1, $3->opt1);

    char *code = formatStr("%s, %s", $1->code, $3->code);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, $1->opt1, prefix);
    free(code);
    free(prefix);
    freeRecord($1);
    freeRecord($3);
  }
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
    char *deallocationCode = deallocationCodeCurrentScope(&scopeStack);
    char *code = formatStr("{\n%s\n%s}", $2->code, (deallocationCode == NULL) ? "" : deallocationCode);

    $$ = createRecord(code, "", "");

    freeRecord($2);
    free(code);
    free(deallocationCode);
  };

%%


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
    scopeStack = createScopeStack();

    code = yyparse();

    fclose(yyin);
    fclose(yyout);

    return 0;
}
