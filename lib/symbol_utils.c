#include <stdlib.h>
#include "symbol_utils.h"
#include "scope_stack.h"
#include "symbol_table.h"
#include "utils.h"

SymbolTable symbolTable;
ScopeStack scopeStack;

void symbolInsert(char *name, char *type)
{
    Scope *currentScope = top(&scopeStack, 0);
    char *nameToLookup = formatStr("%s$%s", currentScope->name, name);
    insert(&symbolTable, nameToLookup, type);
}

char *symbolLookup(char *name)
{
    for (size_t i = 0; i < scopeStack.size; i++)
    {
        Scope *scope = top(&scopeStack, i);

        char *key = formatStr("%s$%s", scope->name, name);
        char *type = lookup(&symbolTable, key);
        if (type != NULL)
        {
            free(key);
            return type;
        }

        free(key);
    }

    return NULL;
}