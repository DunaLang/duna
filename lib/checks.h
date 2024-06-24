#pragma once

#include <stddef.h>
#include "record.h"

extern int yylineno;
extern char *yytext;

int yyerror(char *msg);

void check_subprogram_exists(char *subprogram);
void check_subprogram_not_exists_already(char *subprogram);
void check_subprogram_params_type_match(char *subprogram, char *arguments);
void check_valid_return(struct record *r);

void check_current_scope_is_procedure();
void check_current_scope_is_function();

void check_struct_exists(char *structName);
void check_struct_not_exists_already(char *structName);
void check_can_access_field(char *type);

void check_symbol_exists(char *id);
void check_symbol_not_exists_already(char *id);
// void check_expected_actual_type(record *expected, record *actual);
void check_expected_actual_type(char *expected, char *actual);
void check_operands_numeric(record *operand1, record *operand2, char *operat);
void check_operands_boolean(record *operand1, record *operand2, char *operat);
void check_coerce_to_expected_numeric(char *expected, char *actual);
void check_type_is_pointer(char *type);

void check_main_procedure();
