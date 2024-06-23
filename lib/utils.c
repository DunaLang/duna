#include "utils.h"
#include "record.h"
#include "checks.h"
#include <inttypes.h>
#include <string.h>
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

_Bool isArray(const record *rec)
{
    return rec->code[0] == '[';
}

_Bool isSizeDefinedArray(const record *rec)
{
    return strlen(rec->code) > 2;
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

char *typeByNumberBitRange(const char *number)
{
    int64_t n = atoi(number);
    if (n >= INT8_MIN && n <= INT8_MAX)
    {
        return "i8";
    }
    else if (n >= INT16_MIN && n <= INT16_MAX)
    {
        return "i16";
    }
    else if (n >= INT32_MIN && n <= INT32_MAX)
    {
        return "i32";
    }
    else if (n >= INT64_MIN && n <= INT64_MAX)
    {
        return "i64";
    }
    else
    {
        return "Unreachable";
    }
}
/*
    [Regras de Cast]
    type_variants: boolean, enum, numeric, string, struct
    src -> dest_type

    [Boolean como origem]
    ??? boolean -> numeric (false = 0, true = 1)
    ??? boolean -> _ = INVALID

    [Enum como origem]
    ??? enum -> numeric = ENUM_VALUE
    ??? enum -> _ = INVALID

    [Número como origem]
    ??? numeric -> boolean (0 = false, _ = true)
    ??? numeric -> enum
    ??? numeric -> _ = INVALID

    [String como origem]
    string -> numeric = OK
    string -> boolean = true | false
    ??? string -> _ = INVALID

    [String como destino]
    boolean -> string = "true" | "false"
    enum -> string = "ENUM_NAME::VARIANT"
    numeric -> string = NUMBER_STRING
    ??? struct -> string = "STRUCT_NAME{field: value, ...}"
    ??? union -> string = INVALID
    */
record *castR(record *castTo, record *castFrom)
{
    // cast to string
    if (isString(castTo))
    {
        // cast to string && from bool
        if (strcmp(castFrom->opt1, "bool") == 0)
        {
            char *code = generateVariable();
            char *prefix = formatStr("char *%s = \"true\"; if (%s) goto label_%s; %s = \"false\"; label_%s:", code, castFrom->code, code, code, code);

            return createRecord(code, castTo->code, prefix);
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

            char *length = generateVariable();
            char *casted_str = generateVariable();

            char *prefix = formatStr(
                "%s\nint %s = snprintf(NULL, 0, \"%s\", %s);\nchar %s[%s + 1];\nsnprintf(%s, %s + 1, \"%s\", %s);\n",
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
        else
        {
            char *code = formatStr("(%s) %s", castTo->code, castFrom->code);
            return createRecord(code, castTo->opt1, castFrom->prefix);
            free(code);
        }
    }
    // cast to not string and not numeric
    else
    {
        /// TODO: Verificar o que dá pra fazer apenas com isso para simplicar a quantidade de IF-ELSE IF
        char *code = formatStr("(%s) %s", castTo->code, castFrom->code);
        return createRecord(code, castTo->opt1, castFrom->prefix);

        free(code);
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
