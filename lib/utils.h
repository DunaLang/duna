#pragma once

#include "record.h"

char *cat(char *s1, char *s2, char *s3, char *s4, char *s5);
_Bool equalTypes(record *r1, record *r2);
_Bool isNumeric(record *rec);
_Bool isString(record *rec);

char *resultNumericType(char *type1, char *type2);
