#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "symbol_table.h"
#include "utils.h"

size_t hash(char *key)
{
    size_t hashedKey = 0;

    int i = 1;
    while (key[0] != '\0')
    {
        hashedKey = (hashedKey + key[0] * i) % HASH_SIZE;
        i++;
        key++;
    }
    return hashedKey;
}

SymbolTable createSymbolTable()
{
    SymbolTable table;
    for (size_t i = 0; i < HASH_SIZE; i++)
    {
        table.nodes[i] = NULL;
    }

    return table;
}

char *lookup(SymbolTable *table, char *key)
{
    size_t index = hash(key);

    SymbolTableNode *node = table->nodes[index];
    while (node != NULL)
    {
        if (strcmp(key, node->key) == 0)
        {
            return node->value;
        }
        node = node->next;
    }

    return NULL;
}

void insert(SymbolTable *table, char *key, char *type)
{
    size_t index = hash(key);
    SymbolTableNode *node = table->nodes[index];
    SymbolTableNode *previous = NULL;

    while (node != NULL && strcmp(key, node->key) != 0)
    {
        previous = node;
        node = node->next;
    }

    SymbolTableNode *newNode = malloc(sizeof(SymbolTableNode));
    newNode->key = key;
    newNode->value = type;
    newNode->next = NULL;

    if (previous == NULL)
    {
        table->nodes[index] = newNode;
    }
    else
    {
        previous->next = newNode;
    }
}
