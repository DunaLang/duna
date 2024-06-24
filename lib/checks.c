#include "utils.h"
#include "record.h"
#include "checks.h"
#include "scope_stack.h"
#include "symbol_utils.h"
#include "table/subprogram_table.h"
#include "table/struct_table.h"
#include <inttypes.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

extern struct ScopeStack scopeStack;
extern struct SubprogramTable subprogramTable;
extern struct StructTable structTable;

int yyerror(char *msg)
{
    fprintf(stderr, "Line %d: %s at '%s'\n", yylineno, msg, yytext);
    return 0;
}

void check_subprogram_exists(char *subprogram)
{
    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, subprogram);
    if (type == NULL)
    {
        char *errorMsg = formatStr("Subprogram \"%s\" is not defined.\n", subprogram);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_subprogram_not_exists_already(char *subprogram)
{
    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, subprogram);
    if (type != NULL)
    {
        char *errorMsg = formatStr("Subprogram \"%s\" is already defined.\n", subprogram);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_subprogram_params_type_match(char *subprogram, char *arguments)
{
    // É implícito que o subprograma exista, mas apenas para garantir
    check_subprogram_exists(subprogram);

    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, subprogram);

    size_t expected_argc = type->parametersLength;
    size_t actual_argc = (arguments == NULL) ? 0 : countCharacter(arguments, ',') + 1;

    if (expected_argc != actual_argc)
    {
        char *errorMsg = formatStr("Subprogram call parameters mismatch. Expected: %d. Actual: %d.\n", expected_argc, actual_argc);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }

    if (actual_argc == 0)
    {
        return;
    }

    char *begin_argument = arguments;
    char *begin_param = type->parameters;
    while (true)
    {
        char *end_arg = strchr(begin_argument, ',');
        char *end_param = strchr(begin_param, ',');

        if (!end_arg)
        {
            char *exprType = begin_argument;
            if (isNumeric(createRecord("", begin_argument, "")))
            {
                exprType = resultNumericType(begin_param, begin_argument);
            }
            check_expected_actual_type(begin_param, exprType);
            return;
        }
        else
        {
            int arg_diff = end_arg - begin_argument;
            char *current_arg = formatStr("%.*s", arg_diff, begin_argument);

            int param_diff = end_param - begin_param;
            char *current_param = formatStr("%.*s", param_diff, begin_param);

            char *exprType = current_arg;
            if (isNumeric(createRecord("", current_arg, "")))
            {
                exprType = resultNumericType(current_param, current_arg);
            }
            check_expected_actual_type(current_param, exprType);

            begin_argument = end_arg + 1;
            begin_param = end_param + 1;
        }
    }
}

void check_valid_return(struct record *r)
{
    if (r->opt1 == NULL || strstr(r->opt1, "return") == NULL)
    {
        yyerror("Function does not have valid return statements.\n");
        exit(0);
    }
}

void check_current_scope_is_procedure()
{
    struct Scope *scope = nearestProcedure(&scopeStack);
    if (!scope || strcmp(scope->type, "proc") != 0)
    {
        yyerror("Current subprogram is not a procedure.\n");
        exit(0);
    }
}

void check_current_scope_is_function()
{
    struct Scope *scope = nearestFunction(&scopeStack);
    if (!scope || strcmp(scope->type, "func") != 0)
    {
        yyerror("Current subprogram is not a function.\n");
        exit(0);
    }
}

void check_struct_exists(char *structName)
{
    // Checa se atualmente está criando o struct
    Scope *scope = top(&scopeStack, 0);
    if (scope && strcmp(scope->type, "structDef") == 0 && strcmp(scope->name, structName) == 0)
    {
        return;
    }

    struct StructTableNode *node = lookupStructTable(&structTable, structName);
    if (node == NULL)
    {
        char *errorMsg = formatStr("Struct \"%s\" is not defined.\n", structName);
        yyerror(errorMsg);
        free(errorMsg);
        exit(1);
    }
}

void check_can_access_field(char *type)
{
    struct StructTableNode *node = lookupStructTable(&structTable, type);
    if (node == NULL)
    {
        char *errorMsg = formatStr("Cannot access field of type \"%s\". Try derreferencing it or using another type.", type);
        yyerror(errorMsg);
        free(errorMsg);
        exit(-1);
    }
}

void check_struct_not_exists_already(char *structName)
{
    struct StructTableNode *node = lookupStructTable(&structTable, structName);
    if (node != NULL)
    {
        char *errorMsg = formatStr("Struct \"%s\" is already defined.\n", structName);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_symbol_exists(char *id)
{
    char *type = symbolLookup(id);
    if (type == NULL || strlen(type) == 1)
    {
        char *errorMsg = formatStr("Identifier \"%s\" is not defined.\n", id);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_symbol_not_exists_already(char *id)
{
    if (symbolLookup(id) != NULL)
    {
        char *errorMsg = formatStr("Identifier \"%s\" is already defined.\n", id);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_expected_actual_type(char *expected, char *actual)
{
    if (expected == NULL)
    {
    }
    if (expected == NULL || actual == NULL)
    {
    }
    if (strcmp(expected, actual) != 0)
    {
        char *errorMsg = formatStr("RHS type is not expected. Actual: \"%s\". Expected: \"%s\"\n", actual, expected);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}

void check_operands_numeric(record *operand1, record *operand2, char *operat)
{
    char *errorMsg;
    if (operat != NULL)
    {
        errorMsg = formatStr("Operation invalid: \"%s\", operands must be numeric\n", operat);
    }
    else
    {
        errorMsg = "Operation invalid: one of the operands is not numeric.";
    }

    if (!isNumeric(operand1) || !isNumeric(operand2))
    {
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }

    free(errorMsg);
}

void check_operands_boolean(record *operand1, record *operand2, char *operat)
{
    char *errorMsg;
    if (operat != NULL)
    {
        errorMsg = formatStr("Operation invalid: \"%s\", operands must be boolean\n", operat);
    }
    else
    {
        errorMsg = "Operation invalid: one of the operands is not boolean.";
    }

    if (!isBoolean(operand1) || !isBoolean(operand2))
    {
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }

    free(errorMsg);
}

void throws_cannot_coerce(char *toType, char *fromType)
{
    char *errorMsg = formatStr("Invalid: cannot coerce from %s to %s", fromType, toType);
    yyerror(errorMsg);
    free(errorMsg);
    exit(0);
}

void check_coerce_to_expected_numeric(char *expected, char *actual)
{
    char *coersion = resultNumericType(expected, actual);
    if (coersion == NULL)
    {
        throws_cannot_coerce(expected, actual);
    }
    if (strcmp(coersion, expected) != 0)
    {
        throws_cannot_coerce(expected, actual);
    }
}

void check_type_is_pointer(char *type)
{
    if (type[strlen(type) - 1] != '*')
    {
        yyerror("Expected pointer type");
        exit(0);
    }
}

void check_main_procedure()
{
    struct SubprogramType *type = lookupSubprogramTable(&subprogramTable, "main");
    if (!type || type->returnType || type->parametersLength > 0)
    {
        yyerror("procedure \"main\" with no args not found.\n");
        exit(0);
    }
}
