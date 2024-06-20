#include "subprogram_table.h"
#include <stdio.h>
#include <string.h>

struct SubprogramTable createSubprogramTable(void)
{
    struct SubprogramTable table;
    for (size_t i = 0; i < HASH_SIZE; i++)
    {
        table.nodes[i] = NULL;
    }

    return table;
}

struct SubprogramType *lookupSubprogramTable(struct SubprogramTable *this, char *key)
{
    size_t index = hash(key);
    struct SubprogramTableNode *node = this->nodes[index];
    while (node != NULL)
    {
        if (strcmp(key, node->key) == 0)
        {
            return &node->value;
        }
        node = node->next;
    }

    return NULL;
}

struct SubprogramTableNode *newSubprogramTableNode(char *key, struct SubprogramType type)
{
    struct SubprogramTableNode *node = malloc(sizeof(struct SubprogramTableNode));
    node->key = strdup(key);
    node->value = type;
    node->next = NULL;
    return node;
}

void insertSubprogramTable(struct SubprogramTable *this, char *key, struct SubprogramType type)
{
    size_t index = hash(key);
    struct SubprogramTableNode *node = this->nodes[index];
    struct SubprogramTableNode *previous = NULL;
    while (node != NULL && strcmp(key, node->key) != 0)
    {
        previous = node;
        node = node->next;
    }

    if (previous == NULL)
    {
        this->nodes[index] = newSubprogramTableNode(key, type);
    }
    else
    {
        previous->next = newSubprogramTableNode(key, type);
    }
}

struct SubprogramType newSubprogram(char *parameters, char *returnType)
{
    size_t parametersLength = 0;
    if (parameters != NULL)
    {
        parametersLength += 1;
        for (size_t i = 0; parameters[i]; i++)
        {
            parametersLength += parameters[i] == ',';
        }
    }

    return (struct SubprogramType){
        .parametersLength = parametersLength,
        .parameters = (parameters == NULL) ? NULL : strdup(parameters),
        .returnType = (returnType == NULL) ? NULL : strdup(returnType),
    };
}
