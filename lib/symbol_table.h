#pragma once

#include "hashmap_utils.h"

typedef struct
{
    struct SymbolTableNode *nodes[HASH_SIZE];
} SymbolTable;

typedef struct SymbolTableNode
{
    char *key;
    char *value;
    struct SymbolTableNode *next;
} SymbolTableNode;

SymbolTable createSymbolTable(void);
char *lookup(SymbolTable *table, char *key);
void insert(SymbolTable *table, char *key, char *type);
