#pragma once

#include <stddef.h>
#include "record.h"

static size_t counter = 0;

_Bool equalTypes(record *r1, record *r2);
_Bool isBoolean(const record *rec);
_Bool isNumeric(const record *rec);
_Bool isInteger(const record *rec);
_Bool isString(const record *rec);
_Bool isArray(const record *rec);
_Bool isSizeDefinedArray(const record *rec);

char *resultNumericType(char *type1, char *type2);

char *formatPrintDecimalNumber(char *type);
char *formatScanDecimalNumber(char *type);

char *itoa(int i);
char *formatStr(const char *fmt, ...);
char *generateVariable();
char *typeByNumberBitRange(const char *number);
record *castR(record *castTo, record *castFrom);
