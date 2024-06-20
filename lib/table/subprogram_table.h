#pragma once

#include <stdlib.h>
#include "hashmap_utils.h"

struct SubprogramTable
{
    struct SubprogramTableNode *nodes[HASH_SIZE];
};

struct SubprogramType
{
    char *parameters;
    size_t parametersLength;

    char *returnType;
};

struct SubprogramTableNode
{
    char *key;
    struct SubprogramType value;

    struct SubprogramTableNode *next;
};

struct SubprogramTable createSubprogramTable(void);
struct SubprogramType *lookupSubprogramTable(struct SubprogramTable *this, char *key);
void insertSubprogramTable(struct SubprogramTable *this, char *key, struct SubprogramType type);

struct SubprogramType newSubprogram(char *parameters, char *returnType);
