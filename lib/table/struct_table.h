#pragma once

#include <stdlib.h>
#include "hashmap_utils.h"

struct StructTable
{
  struct StructTableNode *nodes[HASH_SIZE];
};

struct StructField
{
  char *key;
  char *type;
  struct StructField *next;
};

struct StructTableNode
{
  char *key;
  struct StructField *fields[HASH_SIZE];
  struct StructTableNode *next;
};

struct StructTable createStructTable(void);
struct StructField *lookupStructField(struct StructTable *this, char *name, char *fieldName);
struct StructTableNode *lookupStructTable(struct StructTable *this, char *name);
void insertStructTable(struct StructTable *this, char *key, struct StructField *fields, int length);

struct StructField *extractFields(char *codes, char *types, int* length);
