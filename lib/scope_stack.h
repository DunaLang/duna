#pragma once

// #define size_t int

typedef struct ScopeStack
{
    struct Scope *stack;
    int size;
    int capacity;
} ScopeStack;

typedef struct Scope
{
    char *name;
    char *type;
} Scope;

ScopeStack createScopeStack();

void insertScope(ScopeStack *scopeStack, char *name, char *type);
Scope *top(ScopeStack *scopeStack, int k);
void pop(ScopeStack *scopeStack);

Scope *nearestIteration(ScopeStack *scopeStack);
Scope *nearestProcedure(ScopeStack *scopeStack);
Scope *nearestFunction(ScopeStack *scopeStack);
