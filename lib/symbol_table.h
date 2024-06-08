#pragma once

#define HASH_SIZE 30

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

SymbolTable createSymbolTable();
char *lookup(SymbolTable *env, char *key);
void insert(SymbolTable *env, char *key, char *type);