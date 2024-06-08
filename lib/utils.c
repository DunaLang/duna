#include "utils.h"
#include "record.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct
{
    size_t length;
    char *str;
} string;

char *cat(char *s1, char *s2, char *s3, char *s4, char *s5)
{
    int tam;
    char *output;

    tam = strlen(s1) + strlen(s2) + strlen(s3) + strlen(s4) + strlen(s5) + 1;
    output = (char *)malloc(sizeof(char) * tam);

    if (!output)
    {
        printf("Allocation problem. Closing application...\n");
        exit(0);
    }

    sprintf(output, "%s%s%s%s%s", s1, s2, s3, s4, s5);

    return output;
}

string createString(char *str)
{
    string s;
    s.length = strlen(str);
    s.str = strdup(str);
    return s;
}

record *cast(record *dst_type, record *src_expr)
{
    if (strcmp("string", dst_type->code))
    {
        // string
        char *s1 = cat("", "", "", "", "");
        record *rec = createRecord(s1, "", "");
        free(s1);
        return rec;
    }

    char *s1 = cat("(", dst_type->code, ")", src_expr->code, "");
    return createRecord(s1, "", "");
}

_Bool equalTypes(record *r1, record *r2)
{
    return strcmp(r1->opt1, r2->opt1) == 0;
}

_Bool isNumeric(record *rec)
{
    printf("Record code: %s\n", rec->code);
    printf("Record type: %s\n", rec->opt1);
    _Bool isFloat = strcmp(rec->opt1, "f32") == 0 || strcmp(rec->opt1, "f64") == 0;
    _Bool isUnsigned = strcmp(rec->opt1, "u8") == 0 || strcmp(rec->opt1, "u16") == 0 || strcmp(rec->opt1, "u32") == 0 || strcmp(rec->opt1, "u64") == 0;
    _Bool isSigned = strcmp(rec->opt1, "i8") == 0 || strcmp(rec->opt1, "i16") == 0 || strcmp(rec->opt1, "i32") == 0 || strcmp(rec->opt1, "i64") == 0;
    printf("Record result: %d\n-----\n", (isFloat || isUnsigned || isSigned));
    return isFloat || isUnsigned || isSigned;
}

_Bool isString(record *rec)
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
