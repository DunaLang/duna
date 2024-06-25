%{
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../lib/table/subprogram_table.h"
#include "../../lib/table/symbol_table.h"
#include "../../lib/table/struct_table.h"
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

struct SubprogramTable subprogramTable;

extern ScopeStack scopeStack;

struct StructTable structTable;

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
%token IF ELSE WHILE FOR FUNC PROC RETURN BREAK CONTINUE
%token STRUCT CONST STATIC
%token USIZE U8 U16 U32 U64 I8 I16 I32 I64 F32 F64 BOOL STRING CHAR
%token NOT AND OR NEW DELETE PRINT READ CAST
%token EQUALITY INEQUALITY ASSIGN LESS_THAN_EQUALS MORE_THAN_EQUALS LESS_THAN MORE_THAN PLUS MINUS ASTERISK SLASH AMPERSAND HASHTAG PERCENTAGE CONCAT
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

%type <rec> declarations declaration varDecl
%type <rec> block type expr pointer statements statement literal primary assignment derreferencing
%type <rec> ifStatement if else elseifs elseif
%type <rec> while for
%type <rec> arrayDef commaSeparatedExpr arrayIndex
%type <rec> func proc params param subprogramCall arguments return
%type <rec> struct fields field compoundTypeFields compoundTypeDef fieldAccess

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

    check_main_procedure();

    pop(&scopeStack);
  };

declarations : declaration
  | declarations declaration
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    $$ = createRecord(code, "", "");

    free(code);
    free($1);
    free($2);
  }
  ;

declaration : varDecl
  | proc { $$ = $1; pop(&scopeStack); }
  | func { $$ = $1; pop(&scopeStack); }
  | struct {$$ = $1;}
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
      code = returnCFormattedArray($2, $1->code);
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
    if (isNumeric(createRecord("", $1->opt1, "")) && !isIntLiteral($4))
    {
      check_coerce_to_expected_numeric($1->opt1, $4->opt1);
    }

    check_symbol_not_exists_already($2);

    symbolInsert($2, $1->opt1);

    if (!isNumeric($4))
    {
      check_expected_actual_type($1->opt1, $4->opt1);
    }

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
        char *generatedCode = returnCFormattedArray($2, $1->code);
        code = formatStr("%s%s = %s", $4->prefix, generatedCode, $4->code);
        free(generatedCode);
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

proc : PROC IDENTIFIER '('
  {
    check_subprogram_not_exists_already($2);
    insertScope(&scopeStack, $2, "proc");
    insertSubprogramTable(&subprogramTable, $2, newSubprogram(NULL, NULL));
  }
  ')'
  block
  {
    char *code = formatStr("void %s() %s", $2, $6->code);
    $$ = createRecord(code, "", "");

    free($2);
    freeRecord($6);
    free(code);
  }
  | PROC IDENTIFIER '('
  {
    check_subprogram_not_exists_already($2);
    insertScope(&scopeStack, $2, "proc");
  }
  params ')'
  {
    insertSubprogramTable(&subprogramTable, $2, newSubprogram($5->opt1, NULL));
  }
  block
  {
    char *code = formatStr("void %s(%s) %s", $2, $5->code, $8->code);
    $$ = createRecord(code, "", "");

    free(code);
    free($2);
    freeRecord($5);
    freeRecord($8);
  }

func : FUNC IDENTIFIER '('
  {
    check_subprogram_not_exists_already($2);
    insertScope(&scopeStack, $2, "func");
  }
  ')' ':' type
  {
    insertSubprogramTable(&subprogramTable, $2, newSubprogram(NULL, $7->opt1));
  }
  block
  {
    check_valid_return($9);

    char *code = formatStr("%s %s() %s", $7->code, $2, $9->code);
    $$ = createRecord(code, "", "");

    free($2);
    freeRecord($7);
    freeRecord($9);
    free(code);
  }
  | FUNC IDENTIFIER '('
  {
    check_subprogram_not_exists_already($2);
    insertScope(&scopeStack, $2, "func");
  }
  params ')' ':' type
  {
    insertSubprogramTable(&subprogramTable, $2, newSubprogram($5->opt1, $8->opt1));
  }
  block
  {
    check_valid_return($10);

    char *code = formatStr("%s %s(%s) %s", $8->code, $2, $5->code, $10->code);

    $$ = createRecord(code, "", "");

    free(code);
    free($2);
    freeRecord($5);
    freeRecord($8);
    freeRecord($10);
  }
  ;

params : param
  | params ',' param
  {
    char *code = formatStr("%s, %s", $1->code, $3->code);
    char *opt = formatStr("%s,%s", $1->opt1, $3->opt1);

    $$ = createRecord(code, opt, "");

    free(opt);
    free(code);
    freeRecord($1);
    free($3);
  };

struct : STRUCT IDENTIFIER
  {
    check_struct_not_exists_already($2);
    insertScope(&scopeStack, $2, "structDef");
  }
  '{' fields '}'
  {
    int fieldsLength = 0;
    struct StructField *fields = extractFields($5->prefix, $5->opt1, &fieldsLength);
    insertStructTable(&structTable, $2, fields, fieldsLength);

    char *code = formatStr("struct %s {\n%s\n};", $2, $5->code);
    $$ = createRecord(code, "", "");

    pop(&scopeStack);
    freeRecord($5);
  }
;

fields : field ';' {
    char *code = formatStr("%s;", $1->code);
    $$ = createRecord(code, $1->opt1, $1->prefix); 
    free($1);
  }
  | fields field ';' {
    char *code = formatStr("%s\n%s;", $1->code, $2->code);
    char *opt = formatStr("%s;%s", $1->opt1, $2->opt1);
    char *prefix = formatStr("%s;%s", $1->prefix, $2->prefix);
    $$ = createRecord(code, opt, prefix); 
    freeRecord($1);
    freeRecord($2);
    free(code);
    free(opt);
    free(prefix);
  }
  ;

statements : statement { $$ = $1; }
  | statements statement
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    char *opt = (strstr($1->opt1, "return") != NULL || strstr($2->opt1, "return") != NULL) ? "return" : "";
    $$ = createRecord(code, opt, "");

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
  | assignment ';' {
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
  {
    if (!isPointer($2))
    {
      char *errorMsg = formatStr("Expected pointer expression. Got \"%s\".", $2->opt1);
      yyerror(errorMsg);
      free(errorMsg);
      exit(-1);
    }

    char *code = formatStr("%sfree(%s);", $2->prefix, $2->code);
    $$ = createRecord(code, "", "");

    free(code);
  }
  | return
  | { insertScope(&scopeStack, generateVariable(), "" ); }
    ifStatement
    {
      Scope *scope = top(&scopeStack, 0);
      char *code = formatStr("%s\nend_%s:;", $2->code, scope->name);
      $$ = createRecord(code, $2->opt1, "");
      pop(&scopeStack);

      free(code);
      freeRecord($2);
    }
  | subprogramCall ';'
  {
    char *code = formatStr("%s;", $1->code);
    $$ = createRecord(code, "", "");

    free(code);
    freeRecord($1);
  }
  ;

assignment : IDENTIFIER ASSIGN expr
  {
    check_symbol_exists($1);
    char *type = symbolLookup($1);

    char *exprType = $3->opt1;
    if (isNumeric($3))
    {
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
  {
    char *exprType = $3->opt1;
    if (isNumeric($3))
    {
      exprType = resultNumericType($1->opt1, $3->opt1);
    }
    check_expected_actual_type($1->opt1, exprType);

    char *code = formatStr("%s%s = %s", $3->prefix, $1->code, $3->code);
    $$ = createRecord(code, "", "");

    free(code);
    freeRecord($1);
    freeRecord($3);
  }
  | fieldAccess ASSIGN expr {
    char *exprType = $3->opt1;
    if (isNumeric($3))
    {
      exprType = resultNumericType($1->opt1, $3->opt1);
    }
    check_expected_actual_type($1->opt1, exprType);

    char *code = formatStr("%s%s%s = %s", $1->prefix, $3->prefix, $1->code, $3->code);

    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($1);
    freeRecord($3);
  }
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

  $$ = createRecord(code, $5->opt1, "");

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
    $$ = createRecord(code, $6->opt1, "");

    freeRecord($6);
    free(code);
  }
  | FOR '(' statement expr ';' assignment ')' block
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
    $$ = createRecord(code, $6->opt1, "");

    freeRecord($3);
    freeRecord($4);
    freeRecord($6);
    freeRecord($8);
    free(code);
  }
  | FOR '(' ';' expr ';' assignment ')' block
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
    $$ = createRecord(code, $8->opt1, "");

    freeRecord($4);
    freeRecord($6);
    freeRecord($8);
    free(code);
  }
  | FOR '(' ';' ';' assignment ')' block
  {
    Scope *scope = top(&scopeStack, 0);
    char *scopeName = scope->name;
    char *code = formatStr(
      "{\nbegin_%s:\n%s\npre_end_%s:;\n%s\ngoto begin_%s;\n}\nend_%s:;",
      scopeName, $7->code, scopeName, $5->code, scopeName, scopeName
    );
    $$ = createRecord(code, $7->opt1, "");

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
    $$ = createRecord(code, $6->opt1, "");

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
    $$ = createRecord(code, $7->opt1, "");

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
    $$ = createRecord(code, $7->opt1, "");

    freeRecord($4);
    freeRecord($7);
    free(code);
  };

return : RETURN ';'
  {
    check_current_scope_is_procedure();
    $$ = createRecord("return;", "return", "");
  }
  | RETURN expr ';'
  {
    check_current_scope_is_function();

    Scope *scope = nearestFunction(&scopeStack);
    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, scope->name);
    char *resultType = type->returnType;

    char *exprType = $2->opt1;
    if (!(isNumeric($2) && isNumeric(createRecord("", resultType, "")) && resultNumericType(resultType, $2->opt1))) {
      check_expected_actual_type(resultType, exprType);
    }

    char *code = formatStr("%sreturn %s;", $2->prefix, $2->code);
    $$ = createRecord(code, "return", "");

    free(code);
    freeRecord($2);
  }
  ;

ifStatement : if
  | if else
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    char *opt = (strstr($1->opt1, "return") != NULL && strstr($2->opt1, "return") != NULL) ? "return" : "";
    $$ = createRecord(code, opt, "");

    free(code);
    freeRecord($1);
    freeRecord($2);
  }
  | if elseifs
  {
    char *code = formatStr("%s\n%s", $1->code, $2->code);
    char *opt = (strstr($1->opt1, "return") != NULL && strstr($2->opt1, "return") != NULL) ? "return" : "";
    $$ = createRecord(code, opt, "");

    free(code);
    freeRecord($1);
    freeRecord($2);
  }
  | if elseifs else
  {
    char *code = formatStr("%s\n%s%s", $1->code, $2->code, $3->code);
    char *opt = (strstr($1->opt1, "return") != NULL && strstr($2->opt1, "return") != NULL && strstr($3->opt1, "return") != NULL) ? "return" : "";
    $$ = createRecord(code, opt, "");

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

    $$ = createRecord(s1, $6->opt1, "");

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
    char *opt = (strstr($1->opt1, "return") != NULL && strstr($2->opt1, "return") != NULL) ? "return" : "";
    $$ = createRecord(code, opt, "");

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

    $$ = createRecord(code, $7->opt1, "");

    pop(&scopeStack);

    free(code);
    freeRecord($5);
    freeRecord($7);
  };

param : type IDENTIFIER
  {
    check_symbol_not_exists_already($2);
    if (strlen($1->prefix) > 1)
    {
      // printf("Prefix=%s", $1->prefix);
      printf("Operações com strings são inválidas em argumentos de subprogramas.");
      exit(-1);
    }

    symbolInsert($2, $1->opt1);

    char *code = formatStr("%s %s", $1->code, $2);
    $$ = createRecord(code, $1->opt1, $2);

    free(code);
    freeRecord($1);
    free($2);
  };
field: type IDENTIFIER {
    if (strlen($1->prefix) > 1)
    {
      printf("Operações com strings são inválidas em structs.");
      exit(-1);
    }

    // Checa se o tipo é o struct atual
    Scope *scope = top(&scopeStack, 0);
    if (scope && strcmp(scope->type, "structDef") == 0 && strcmp(scope->name, $1->opt1) == 0)
    {
      yyerror("Recursive struct definition is not permitted. Try using pointers.");
      exit(-1);
    }

    char *code = formatStr("%s %s", $1->code, $2);
    $$ = createRecord(code, $1->opt1, $2);

    free(code);
    freeRecord($1);
    free($2);
};

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
  | STRING { $$ = createRecord("char*", "string", ""); }
  | CHAR { $$ = createRecord("char", "char", ""); }
  | IDENTIFIER {
      check_struct_exists($1); 
      char *code = formatStr("struct %s", $1);
      $$ = createRecord(code, $1, ""); 

      free($1); 
      free(code); 
    }
  | '[' expr ']' type %prec ARRAY_TYPE
  {
    if ($2->prefix && strlen($2->prefix) > 1)
    {
      char *errorMsg = formatStr("String operation in types is not permitted.");
      yyerror(errorMsg);
      exit(1);
    }

    if (!isInteger($2))
    {
      char *errorMsg = formatStr("Type error: expected %s to be numeric but instead got: %s", $2->code, $2->opt1);
      yyerror(errorMsg);
      exit(1);
    }
    char *code = formatStr("[%s]@%s", $2->code, $4->code);
    // If it has more than one @, then it is a multidimensional array
    if(countCharacter(code, '@') > 1) {
      code = formatStr("[%s]%s", $2->code, $4->code);
    }
    $$ = createRecord(code, $4->opt1, "");

    freeRecord($2);
    freeRecord($4);
    free(code);
  }
  | '[' ']' type %prec ARRAY_TYPE
  {
    char *code = formatStr("[]@%s", $3->code);
    $$ = createRecord(code, $3->opt1, "");
    freeRecord($3);
    free(code);
  }
  | pointer
  ;

pointer : type ASTERISK
  {
    char *code = formatStr("%s*", $1->code);
    char *opt = formatStr("%s*", $1->opt1);
    $$ = createRecord(code, opt, "");

    freeRecord($1);
    free(opt);
    free(code);
  };

primary : IDENTIFIER {
    char* type = symbolLookup($1);

    check_symbol_exists($1);

    $$ = createRecord($1, type, "");

    free($1);
  }
  | subprogramCall
  {
    if ($1->opt1 == NULL) {
      yyerror("Cannot call procedure as expression.\n");
      exit(0);
    }

    $$ = $1;
  }
  | NEW type %prec UNEW
  {
    char *code = formatStr("malloc(sizeof(%s))", $2->code);
    char *opt = formatStr("%s*", $2->opt1);

    $$ = createRecord(code, opt, "");

    free(opt);
    free(code);
  }
  | arrayIndex {$$ = $1;}
  | arrayDef {
    $$ = createRecord($1->code, $1->opt1, $1->prefix);
    freeRecord($1);
  }
  | compoundTypeDef {$$ = $1; }
  | fieldAccess { $$ = $1; }
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

derreferencing : ASTERISK IDENTIFIER
  {
    check_symbol_exists($2);
    char* type = symbolLookup($2);
    check_type_is_pointer(type);

    char *code = formatStr("*%s", $2);
    char *opt = formatStr("%.*s", strlen(type)-1, type);
    $$ = createRecord(code, opt, "");

    free(opt);
    free(code);
    free($2);
  }
  | ASTERISK '(' expr ')'
  {
    check_type_is_pointer($3->opt1);

    char *code = formatStr("*(%s)", $3->code);
    char *opt = formatStr("%.*s", strlen($3->opt1)-1, $3->opt1);
    $$ = createRecord(code, opt, "");

    free(opt);
    free(code);
    free($3);
  };

literal : CHAR_LITERAL { char *code = formatStr("'%c'", $1); $$ = createRecord(code, "char", ""); free(code); }
  | STRING_LITERAL { $$ = createRecord($1, "string", ""); free($1); }
  | FLOAT_LITERAL { $$ = createRecord($1, "f32", ""); free($1); }
  | INT_LITERAL { 
    $$ = createRecord($1, "int_literal", ""); 
    free($1);
  }
  | BOOLEAN_LITERAL { $$ = createRecord($1, "bool", ""); free($1); }
  | T_NULL { $$ = createRecord("NULL", "null", ""); free($1); }
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
    else if (isPointer($1) && isPointer($3))
    {
      if (strcmp($1->opt1, "null") != 0 && strcmp($3->opt1, "null") != 0 && strcmp($1->opt1, $3->opt1) != 0)
      {
        char *errorMsg = formatStr("Pointers comparison must be of the same type. Got \"%s\" and \"%s\".", $1->opt1, $3->opt1);
        yyerror(errorMsg);
        free(errorMsg);
        exit(-1);
      }
      code = formatStr("%s == %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else
    {
      char* errorMsg = "==, operands must be both boolean, numeric, string or pointers of the same type.";
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
    else if (isPointer($1) && isPointer($3))
    {
      if (strcmp($1->opt1, "null") != 0 && strcmp($3->opt1, "null") != 0 && strcmp($1->opt1, $3->opt1) != 0)
      {
        char *errorMsg = formatStr("Pointers comparison must be of the same type. Got \"%s\" and \"%s\".", $1->opt1, $3->opt1);
        yyerror(errorMsg);
        free(errorMsg);
        exit(-1);
      }
      code = formatStr("%s != %s", $1->code, $3->code);
      prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    }
    else
    {
      char* errorMsg = "!=, operands must be both boolean, numeric, string or pointers of the same type.";
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
      char *errorMsg = formatStr(
        "++, operands must be string. Actual: %s and %s\n",
        $1->opt1, $3->opt1);
      yyerror(errorMsg);
      free(errorMsg);
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
  {
    char *code = formatStr("&%s", $2->code);
    char *opt = formatStr("%s*", $2->opt1);

    $$ = createRecord(code, opt, $2->prefix);

    free(opt);
    free(code);
    freeRecord($2);
  }
  | NOT expr %prec UNOT
  {
    if (!isBoolean($2))
    {
      char *errorMsg = formatStr("Expected boolean. Got %s.", $2->opt1);
      yyerror(errorMsg);
      free(errorMsg);
      exit(-1);
    }

    char *code = formatStr("!(%s)", $2->code);
    $$ = createRecord(code, "bool", $2->prefix);

    free(code);
    freeRecord($2);
  }
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
  {
    if (!isNumeric($2)) {
      yyerror("Operation invalid: operand is not numeric.");
      exit(0);
    }

    char *code = formatStr(" - %s", $2->code);
    $$ = createRecord(code, $2->opt1, $2->prefix);

    freeRecord($2);
    free(code);
  };

arrayIndex : IDENTIFIER '[' expr ']' {
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
  {
    check_subprogram_exists($1);
    check_subprogram_params_type_match($1, NULL);

    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, $1);

    char *code = formatStr("%s()", $1);
    $$ = createRecord(code, type->returnType, "");

    free(code);
    free($1);
  }
  | IDENTIFIER '(' arguments ')'
  {
    check_subprogram_exists($1);
    check_subprogram_params_type_match($1, $3->opt1);

    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, $1);

    char *code = formatStr("%s(%s)", $1, $3->code);
    $$ = createRecord(code, type->returnType, $3->prefix);

    free(code);
    free($1);
    freeRecord($3);
  }
  ;

arguments : expr 
  | arguments ',' expr
  {
    char *code = formatStr("%s, %s", $1->code, $3->code);
    char *opt1 = formatStr("%s,%s", $1->opt1, $3->opt1);
    char *prefix = formatStr("%s%s", $1->prefix, $3->prefix);
    $$ = createRecord(code, opt1, prefix);

    free(prefix);
    free(opt1);
    free(code);
    freeRecord($1);
    freeRecord($3);
  }
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

compoundTypeDef : IDENTIFIER '{' '}'
  {
    char *code = formatStr("(struct %s) {}", $1);
    $$ = createRecord(code, $1, "");
    free(code);
  }
  | IDENTIFIER '{' { insertScope(&scopeStack, $1, "struct"); } compoundTypeFields { pop(&scopeStack); } '}' {
    char *code = formatStr("(struct %s) { %s }", $1, $4->code);
    $$ = createRecord(code, $1, "");
    free(code);
    freeRecord($4);
  }
  ;

compoundTypeFields : IDENTIFIER ':' expr
  {
    Scope *scope = top(&scopeStack, 0);
    if(strcmp(scope->type, "struct") != 0) {
      yyerror("Bad scope");
      exit(1);
    }

    struct StructField *field = lookupStructField(&structTable, scope->name, $1);
    check_expected_actual_type(field->type, $3->opt1);
    char *code = formatStr(".%s = %s", $1, $3->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($3);
  }
  | IDENTIFIER ':' expr ',' {
    Scope *scope = top(&scopeStack, 0);
    if(strcmp(scope->type, "struct") != 0) {
      yyerror("Bad scope");
      exit(1);
    }
    struct StructField *field = lookupStructField(&structTable, scope->name, $1);
    check_expected_actual_type(field->type, $3->opt1);
    char *code = formatStr(".%s = %s", $1, $3->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($3);
  }
  | IDENTIFIER ':' expr ',' compoundTypeFields {
    Scope *scope = top(&scopeStack, 0);
    if(strcmp(scope->type, "struct") != 0) {
      yyerror("Bad scope");
      exit(1);
    }
    struct StructField *field = lookupStructField(&structTable, scope->name, $1);
    check_expected_actual_type(field->type, $3->opt1);
    char *code = formatStr(".%s = %s,\n%s", $1, $3->code, $5->code);
    $$ = createRecord(code, "", "");
    free(code);
    freeRecord($3);
    freeRecord($5);
  }
  ;

fieldAccess :  '(' expr ')' '.' IDENTIFIER
  {
    char* structType = $2->opt1;

    check_can_access_field(structType);

    struct StructField *field = lookupStructField(&structTable, structType, $5);
    if (!field)
    {
      char *errorMsg = formatStr("Field \"%s\" of struct \"%s\" is not defined.\n", $5, structType);
      yyerror(errorMsg);
      free(errorMsg);
      exit(1);
    }

    char *code = formatStr("(%s).%s", $2->code, $5);
    $$ = createRecord(code, field->type, $2->prefix);

    free(code);
    freeRecord($2);
    free($5);
  }
  | IDENTIFIER '.' IDENTIFIER {
    check_symbol_exists($1);
    char* structType = symbolLookup($1);

    check_can_access_field(structType);

    struct StructField *field = lookupStructField(&structTable, structType, $3);
    if (!field)
    {
      char *errorMsg = formatStr("Field \"%s\" of struct \"%s\" is not defined.\n", $3, structType);
      yyerror(errorMsg);
      free(errorMsg);
      exit(1);
    }

    char *code = formatStr("%s.%s", $1, $3);
    $$ = createRecord(code, field->type, "");

    free(code);
    free($1);
    free($3);
  }
  | fieldAccess '.' IDENTIFIER
  {
    char* structType = $1->opt1;
    check_can_access_field(structType);

    struct StructField *field = lookupStructField(&structTable, structType, $3);
    if (!field)
    {
      char *errorMsg = formatStr("Field \"%s\" of struct \"%s\" is not defined.\n", $3, structType);
      yyerror(errorMsg);
      free(errorMsg);
      exit(1);
    }

    char *code = formatStr("%s.%s", $1->code, $3);
    $$ = createRecord(code, field->type, $1->prefix);
    free(code);
    freeRecord($1);
    free($3);
  }
  ;

block : '{' '}' { $$ = createRecord("{}", "", ""); }
  | '{' statements '}'
  {
    char *deallocationCode = deallocationCodeCurrentScope(&scopeStack);
    char *code = formatStr("{\n%s\n%s}", $2->code, (deallocationCode == NULL) ? "" : deallocationCode);

    $$ = createRecord(code, $2->opt1, "");

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
    subprogramTable = createSubprogramTable();
    scopeStack = createScopeStack();
    structTable = createStructTable();

    code = yyparse();

    fclose(yyin);
    fclose(yyout);

    return 0;
}
