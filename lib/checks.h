#pragma once

#include <stddef.h>
#include "record.h"

extern int yylineno;
extern char *yytext;

int yyerror(char *msg);

void check_symbol_exists(char *id);
void check_symbol_not_exists_already(char *id);
// void check_expected_actual_type(record *expected, record *actual);
void check_expected_actual_type(char *expected, char *actual);
void check_operands_numeric(record *operand1, record *operand2, char *operat);
void check_operands_boolean(record *operand1, record *operand2, char *operat);
void check_coerce_to_expected_numeric(char *expected, char *actual);