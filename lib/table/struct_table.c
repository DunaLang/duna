#include "struct_table.h"
#include "../utils.h"
#include <stdio.h>
#include <string.h>

struct StructTable createStructTable(void)
{
    struct StructTable table;
    for (size_t i = 0; i < HASH_SIZE; i++)
    {
        table.nodes[i] = NULL;
    }

    return table;
}

struct StructField *lookupStructField(struct StructTable *this, char *name, char *fieldName)
{
    size_t index = hash(name);
    struct StructTableNode *node = this->nodes[index];
    while (node != NULL)
    {
        if (strcmp(name, node->key) == 0)
        {
            size_t fieldHash = hash(fieldName);
            struct StructField *field = node->fields[fieldHash];
            while (field != NULL)
            {
                if (strcmp(fieldName, field->key) == 0)
                {
                    return field;
                }
                field = field->next;
            }
            return NULL;
        }
        node = node->next;
    }

    return NULL;
}

struct StructTableNode *lookupStructTable(struct StructTable *this, char *name)
{
    size_t index = hash(name);
    struct StructTableNode *node = this->nodes[index];
    while (node != NULL)
    {
        if (strcmp(name, node->key) == 0)
        {
            return node;
        }
        node = node->next;
    }

    return NULL;
}

void insertStructField(struct StructTableNode *this, struct StructField *field)
{
    size_t index = hash(field->key);
    struct StructField *node = this->fields[index];
    struct StructField *previous = NULL;
    while (node != NULL && strcmp(field->key, node->key) != 0)
    {
        previous = node;
        node = node->next;
    }

    if (previous == NULL)
    {
        this->fields[index] = field;
    }
    else
    {
        previous->next = field;
    }
}

struct StructTableNode *newStructTableNode(char *key, struct StructField *fields, int length)
{
    struct StructTableNode *node = malloc(sizeof(struct StructTableNode));
    if (!node)
    {
        printf("Allocation error.");
        exit(-1);
    }

    node->key = strdup(key);
    for (size_t i = 0; i < HASH_SIZE; i++)
    {
        node->fields[i] = NULL;
    }
    for (size_t i = 0; i < length; i++)
    {
        insertStructField(node, &fields[i]);
        printf("Adding into struct %s, field: %s %s\n", key, fields[i].key, fields[i].type);
    }
    node->next = NULL;
    return node;
}

void insertStructTable(struct StructTable *this, char *key, struct StructField *fields, int length)
{
    size_t index = hash(key);
    struct StructTableNode *node = this->nodes[index];
    struct StructTableNode *previous = NULL;
    while (node != NULL && strcmp(key, node->key) != 0)
    {
        previous = node;
        node = node->next;
    }

    if (previous == NULL)
    {
        this->nodes[index] = newStructTableNode(key, fields, length);
    }
    else
    {
        previous->next = newStructTableNode(key, fields, length);
    }
    printf("Struct: %s created successfully! Fields: %d\n", key, length);
}

struct StructField newField(char *key, char *type)
{
    return (struct StructField){
        .key = key,
        .type = type,
    };
}

char *substring(char *start, char *end)
{
    if (start == NULL || end == NULL || start > end)
    {
        return NULL;
    }

    int length = end - start + 1;

    char *dest = (char *)malloc((length + 1) * sizeof(char));
    if (dest == NULL)
    {
        return NULL;
    }

    strncpy(dest, start, length);
    dest[length] = '\0';

    return dest;
}

struct StructField *extractFields(char *names, char *types, int *length)
{
    int fieldsLength = countCharacter(names, ';') + 1;
    struct StructField *structFields = (struct StructField *)malloc(fieldsLength * sizeof(struct StructField));
    if (structFields == NULL)
    {
        return NULL; // Memory allocation failed
    }

    char *startName = names;
    char *endName = strchr(names, ';');

    char *startType = types;
    char *endType = strchr(types, ';');

    for (int i = 0; i < fieldsLength; i++)
    {
        // Handle the last field
        if (endName == NULL)
        {
            endName = names + strlen(names);
        }
        // Handle the last field
        if (endType == NULL)
        {
            endType = types + strlen(types);
        }

        char *name = substring(startName, endName - 1);
        char *type = substring(startType, endType - 1);

        structFields[i] = newField(name, type);

        startName = endName + 1;
        endName = strchr(startName, ';');

        startType = endType + 1;
        endType = strchr(startType, ';');
    }
    *length = fieldsLength;
    return structFields;
}
