#include "utils.h"
#include "record.h"
#include "checks.h"
#include <inttypes.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>

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

_Bool isBoolean(const record *rec)
{
    return strcmp(rec->opt1, "bool") == 0;
}

_Bool isNumeric(const record *rec)
{
    // printf("Record code: %s\n", rec->code);
    // printf("Record type: %s\n", rec->opt1);
    _Bool isFloat = strcmp(rec->opt1, "f32") == 0 || strcmp(rec->opt1, "f64") == 0;

    // printf("Record result (isNumeric): %s\n-----\n", (isFloat || isInteger(rec) == 1 ? "true" : "false"));
    return isFloat || isInteger(rec);
}

_Bool isIntLiteral(const record *rec)
{
    return strcmp(rec->opt1, "int_literal") == 0;
}

_Bool isInteger(const record *rec)
{
    _Bool isUnsigned = strcmp(rec->opt1, "u8") == 0 || strcmp(rec->opt1, "u16") == 0 || strcmp(rec->opt1, "u32") == 0 || strcmp(rec->opt1, "u64") == 0;
    _Bool isSigned = strcmp(rec->opt1, "i8") == 0 || strcmp(rec->opt1, "i16") == 0 || strcmp(rec->opt1, "i32") == 0 || strcmp(rec->opt1, "i64") == 0;
    return isUnsigned || isSigned || isIntLiteral(rec);
}

_Bool isString(const record *rec)
{
    return strcmp(rec->opt1, "string") == 0;
}

_Bool isArray(const record *rec)
{
    return rec->code[0] == '[';
}

_Bool isPointer(const record *rec)
{
    return strcmp(rec->opt1, "null") == 0 || rec->opt1[strlen(rec->opt1) - 1] == '*';
}

_Bool isSizeDefinedArray(const record *rec)
{
    bool isValid = false;
    char *delimiter = strchr(rec->code, '@');
    if(delimiter != NULL) {
        char token[delimiter - rec->code +1];
        strncpy(token, rec->code, delimiter - rec->code);
        token[delimiter - rec->code + 1] = '\0';
       isValid = strlen(token) > 2;
    }
    return isValid;
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

char *formatPrintDecimalNumber(char *type)
{
    int size = sizeNumericType(type);

    switch (type[0])
    {
    case 'f':
        return "%f";
    case 'i':
        switch (size)
        {
        case 8:
            return "%" PRId8;
        case 16:
            return "%" PRId16;
        case 32:
            return "%" PRId32;
        }
        return "%" PRId64;
    case 'u':
        switch (size)
        {
        case 8:
            return "%" PRIu8;
        case 16:
            return "%" PRIu16;
        case 32:
            return "%" PRIu32;
        }
        return "%" PRIu64;
    default:
        printf("Unreachable.");
        exit(-1);
    }
}

char *formatScanDecimalNumber(char *type)
{
    int size = sizeNumericType(type);

    switch (type[0])
    {
    case 'f':
        return "%f";
    case 'i':
        switch (size)
        {
        case 8:
            return "%" SCNd8;
        case 16:
            return "%" SCNd16;
        case 32:
            return "%" SCNd32;
        }
        return "%" SCNd64;
    case 'u':
        switch (size)
        {
        case 8:
            return "%" SCNu8;
        case 16:
            return "%" SCNu16;
        case 32:
            return "%" SCNu32;
        }
        return "%" SCNu64;
    default:
        printf("Unreachable.");
        exit(-1);
    }
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
    return formatStr("$%ld", counter - 1);
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
    if (strcmp(type1, "int_literal") == 0)
    {
        return type2;
    }

    if (strcmp(type2, "int_literal") == 0)
    {
        return type1;
    }

    int size1 = sizeNumericType(type1);
    int size2 = sizeNumericType(type2);

    if (strcmp(type1, type2) == 0)
    {
        return type1;
    }

    // f32 -> f64
    if ((strcmp(type1, "f32") == 0 || strcmp(type2, "f32") == 0) && (strcmp(type1, "f64") == 0 || strcmp(type2, "f64") == 0))
    {
        return "f64";
    }
    // u8 -> i16
    if ((strcmp(type1, "u8") == 0 || strcmp(type2, "u8") == 0) && (strcmp(type1, "i16") == 0 || strcmp(type2, "i16") == 0))
    {
        return "i16";
    }
    // u16 -> i32
    if ((strcmp(type1, "u16") == 0 || strcmp(type2, "u16") == 0) && (strcmp(type1, "i32") == 0 || strcmp(type2, "i32") == 0))
    {
        return "i32";
    }
    // u32 -> i64
    if ((strcmp(type1, "u32") == 0 || strcmp(type2, "u32") == 0) && (strcmp(type1, "i64") == 0 || strcmp(type2, "i64") == 0))
    {
        return "i64";
    }
    // i8 -> i16 -> i32 -> i64
    // u8 -> u16 -> u32 -> u64
    if (type1[0] == 'u' && type2[0] == 'i')
    {
        if (size1 < size2)
        {
            return type2;
        }
    }

    if (type1[0] == 'i' && type2[0] == 'u')
    {
        if (size1 > size2)
        {
            return type1;
        }
    }

    if (type1[0] == type2[0] && type1[0] != 'f')
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

record *castR(record *castTo, record *castFrom)
{
    // cast to string
    if (isString(castTo))
    {
        // cast to string && from bool
        if (strcmp(castFrom->opt1, "bool") == 0)
        {
            char *code = generateVariable();
            char *prefix = formatStr("char *%s = \"true\"; if (%s) goto label_%s; %s = \"false\"; label_%s:;", code, castFrom->code, code, code, code);

            return createRecord(code, castTo->opt1, prefix);
            free(code);
            free(prefix);
        }
        // cast to string && expr not bool
        else
        {
            char *typeFormat;
            // cast to string from numeric
            if (isNumeric(castFrom))
            {
                typeFormat = formatPrintDecimalNumber(castFrom->opt1);
            }
            else if (strcmp(castFrom->opt1, "usize") == 0)
            {
                typeFormat = "%lu";
            }
            else
            {
                char *errorMsg = formatStr("Can only cast to string from number and bool, got %s.", castFrom->opt1);
                yyerror(errorMsg);
                free(errorMsg);
                exit(-1);
            }

            char *length = generateVariable();
            char *casted_str = generateVariable();

            char *prefix = formatStr(
                "%sint %s = snprintf(NULL, 0, \"%s\", %s);\nchar %s[%s + 1];\nsnprintf(%s, %s + 1, \"%s\", %s);\n",
                castFrom->prefix, length, typeFormat, castFrom->code, casted_str, length, casted_str, length, typeFormat, castFrom->code);

            return createRecord(casted_str, castTo->opt1, prefix);
            free(length);
            free(casted_str);
            free(prefix);
        }
    }
    // cast to numeric
    else if (isNumeric(castTo))
    {
        if (isString(castFrom))
        {
            if (castTo->code[0] == 'f')
            {
                char *code = formatStr("atof(%s)", castFrom->code);
                return createRecord(code, castTo->opt1, castFrom->prefix);
                free(code);
            }
            else if (castTo->code[0] == 'u')
            {
                char *code = formatStr("atoll(%s)", castFrom->code);
                return createRecord(code, castTo->opt1, castFrom->prefix);
                free(code);
            }
            else
            {
                char *code = formatStr("atoll(%s)", castFrom->code);
                return createRecord(code, castTo->opt1, castFrom->prefix);
                free(code);
            }
        }
        else if (isNumeric(castFrom))
        {
            char *code = formatStr("(%s) %s", castTo->code, castFrom->code);
            return createRecord(code, castTo->opt1, castFrom->prefix);
            free(code);
        }
        else
        {
            char *errorMsg = formatStr("Can only cast to number from number and string, got %s.", castFrom->opt1);
            yyerror(errorMsg);
            free(errorMsg);
            exit(-1);
        }
    }
    else
    {
        char *errorMsg = formatStr("Can only cast to string and number, got %s.", castTo->opt1);
        yyerror(errorMsg);
        free(errorMsg);
        exit(-1);
    }
}

int countCharacter(char *str, char c)
{
    int total = 0;
    for (size_t i = 0; str[i]; i++)
    {
        total += str[i] == c;
    }
    return total;
}

char *returnCFormattedArray(char *variable, char *other) {
    const char *test = other;
    char *delimiter = strchr(test, '@');
    if (delimiter != NULL) {
        // Extracting arrSize to another variable
        size_t arrSizeLength = delimiter - other;
        char arrSize[arrSizeLength + 1];
        strncpy(arrSize, other, arrSizeLength);
        arrSize[arrSizeLength] = '\0';

        size_t typeLength = strlen(delimiter + 1);
        char type[typeLength + 1];
        strncpy(type, delimiter + 1, typeLength);
        type[typeLength] = '\0';
        return formatStr("%s %s%s", type, variable, arrSize);
    }
    return NULL;
}