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
    if (strcmp(expected, actual) != 0)
    {
        char *errorMsg = formatStr("RHS type is not expected. Actual: \"%s\". Expected: \"%s\"\n", actual, expected);
        yyerror(errorMsg);
        free(errorMsg);
        exit(0);
    }
}