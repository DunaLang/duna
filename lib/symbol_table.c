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
    printf("Hash: %ld\n", index);
    SymbolTableNode *node = table->nodes[index];
    while (node != NULL)
    {
        printf("Node: %s, %s\n", node->key, node->value);
        if (strcmp(key, node->key) == 0)
        {
            printf("Identifier '%s' found. Type: %s\n", node->key, node->value);
            return node->value;
        }
        node = node->next;
    }

    return NULL;
}

void insert(SymbolTable *table, char *key, char *type)
{
    printf("Creating new node: %s\n", key);
    size_t index = hash(key);
    // printf("Hash: %ld\n", index);
    SymbolTableNode *node = table->nodes[index];
    SymbolTableNode *previous = NULL;
    // printf("Node is null: %d\n", node == NULL);
    while (node != NULL && strcmp(key, node->key) != 0)
    {
        previous = node;
        node = node->next;
    }

    SymbolTableNode *newNode = malloc(sizeof(SymbolTableNode));
    newNode->key = strdup(key);
    newNode->value = strdup(type);
    newNode->next = NULL;
    // printf("Previous is null: %d\n", previous == NULL);
    if (previous == NULL)
    {
        table->nodes[index] = newNode;
    }
    else
    {
        previous->next = newNode;
    }
    printf("New node: %s, %s\n", newNode->key, newNode->value);
}
