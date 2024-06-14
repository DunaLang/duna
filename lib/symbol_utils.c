#include "symbol_table.h"
#include "scope_stack.h"
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
    // for loop with scope top and table lookup
    char *type = lookup(&symbolTable, name);
}