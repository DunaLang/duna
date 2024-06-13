#include "utils.h"
#include "record.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

typedef struct
{
    size_t length;
    char *str;
} string;

string createString(char *str)
{
    string s;
    s.length = strlen(str);
    s.str = strdup(str);
    return s;
}

_Bool equalTypes(record *r1, record *r2)
{
    return strcmp(r1->opt1, r2->opt1) == 0;
}

_Bool isNumeric(const record *rec)
{
    printf("Record code: %s\n", rec->code);
    printf("Record type: %s\n", rec->opt1);
    _Bool isFloat = strcmp(rec->opt1, "f32") == 0 || strcmp(rec->opt1, "f64") == 0;

    printf("Record result (isNumeric): %s\n-----\n", (isFloat || isInteger(rec) == 1 ? "true" : "false"));
    return isFloat || isInteger(rec);
}

_Bool isInteger(const record *rec)
{
    _Bool isUnsigned = strcmp(rec->opt1, "u8") == 0 || strcmp(rec->opt1, "u16") == 0 || strcmp(rec->opt1, "u32") == 0 || strcmp(rec->opt1, "u64") == 0;
    _Bool isSigned = strcmp(rec->opt1, "i8") == 0 || strcmp(rec->opt1, "i16") == 0 || strcmp(rec->opt1, "i32") == 0 || strcmp(rec->opt1, "i64") == 0;
    return isUnsigned || isSigned;
}

_Bool isString(const record *rec)
{
    return strcmp(rec->opt1, "string") == 0;
}

int sizeNumericType(char *type)
{
    int numericSize = 0;
    if (type[2] == '\0')
    {
        numericSize = type[1] - 48;
    }
    else
    {
        numericSize = (type[1] - 48) * 10 + type[2] - 48;
    }

    return numericSize;
}

char *itoa(int i)
{
    int length = snprintf(NULL, 0, "%d", i);
    char *str = malloc(length + 1);
    snprintf(str, length + 1, "%d", i);
    return str;
}

char *generateVariable()
{
    counter += 1;
    return formatStr("temp_%ld", counter - 1);
}

char *formatStr(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    size_t sz = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    char *s1 = malloc(sz + 1);
    va_start(args, fmt);
    vsnprintf(s1, sz + 1, fmt, args);
    va_end(args);

    return s1;
}

// somente utilizar quando ambos os types forem numericos (i, u, f)
char *resultNumericType(char *type1, char *type2)
{
    int size1 = sizeNumericType(type1);
    int size2 = sizeNumericType(type2);

    if (strcmp(type1, type2) == 0)
    {
        return type1;
    }

    // f32 -> f64
    if (strcmp(type1, "f32") == 0 || strcmp(type2, "f32") == 0)
    {
        if (strcmp(type1, "f64") == 0 || strcmp(type2, "f64") == 0)
        {
            return "f64";
        }
    }
    // u8 -> i16
    else if (strcmp(type1, "u8") == 0 || strcmp(type2, "u8") == 0)
    {
        if (strcmp(type1, "i16") == 0 || strcmp(type2, "i16") == 0)
        {
            return "i16";
        }
    }
    // u16 -> i32
    else if (strcmp(type1, "u16") == 0 || strcmp(type2, "u16") == 0)
    {
        if (strcmp(type1, "i32") == 0 || strcmp(type2, "i32") == 0)
        {
            return "i32";
        }
    }
    // u32 -> i64
    else if (strcmp(type1, "u32") == 0 || strcmp(type2, "u32") == 0)
    {
        if (strcmp(type1, "i64") == 0 || strcmp(type2, "i64") == 0)
        {
            return "i64";
        }
    }
    // i8 -> i16 -> i32 -> i64
    // u8 -> u16 -> u32 -> u64
    else if (type1[0] == 'u' && type2[0] == 'i')
    {
        if (size1 < size2)
        {
            return type2;
        }
    }
    else if (type1[0] == 'i' && type2[0] == 'u')
    {
        if (size1 > size2)
        {
            return type1;
        }
    }
    else if (type1[0] == type2[0] && type1[0] != 'f')
    {
        if (size1 > size2)
        {
            return type1;
        }
        else
        {
            return type2;
        }
    }

    return NULL;
}
