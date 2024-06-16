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