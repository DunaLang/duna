#include "utils.h"
#include "record.h"
#include "checks.h"
#include "symbol_utils.h"
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

int yyerror(char *msg)
{
    fprintf(stderr, "Line %d: %s at '%s'\n", yylineno, msg, yytext);
    return 0;
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
