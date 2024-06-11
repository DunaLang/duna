#pragma once

#include "record.h"

static int counter = 0;

char *cat(char *s1, char *s2, char *s3, char *s4, char *s5);
_Bool equalTypes(record *r1, record *r2);
_Bool isNumeric(const record *rec);
_Bool isInteger(const record *rec);
_Bool isString(const record *rec);

char *resultNumericType(char *type1, char *type2);

char *itoa(int i);
char *formatStr(const char *fmt, ...);
char *generateVariable();
record *cast(char *dest_type, const record *src_expr);