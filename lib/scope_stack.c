#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "scope_stack.h"
#include "utils.h"

#define INITIAL_SCOPE_SIZE 50

ScopeStack createScopeStack()
{
    return (struct ScopeStack){
        .stack = malloc(sizeof(Scope) * INITIAL_SCOPE_SIZE),
        .size = 0,
        .capacity = INITIAL_SCOPE_SIZE,
    };
}

void resize(ScopeStack *scopeStack)
{
    size_t newSize = scopeStack->capacity * 2;
    scopeStack->stack = realloc(scopeStack, newSize);
    scopeStack->capacity = newSize;
}

void insertScope(ScopeStack *scopeStack, char *name, char *type)
{
    if (scopeStack->size >= scopeStack->capacity)
    {
        resize(scopeStack);
    }

    scopeStack->stack[scopeStack->size] = (struct Scope){
        .name = name,
        .type = type,
    };
    scopeStack->size += 1;
}

Scope *top(ScopeStack *scopeStack, int k)
{
    if (k >= scopeStack->size)
    {
        return NULL;
    }

    return scopeStack->stack + (scopeStack->size - 1 - k);
}

void pop(ScopeStack *scopeStack)
{
    if (scopeStack->size == 0)
    {
        return;
    }
    scopeStack->size -= 1;
}

Scope *nearestIteration(ScopeStack *scopeStack)
{
    char *whileType = "while";
    char *forType = "for";
    for (size_t i = 0; i < scopeStack->size; i++)
    {
        Scope *scope = top(scopeStack, i);
        if (strcmp(whileType, scope->type) == 0 || strcmp(forType, scope->type) == 0)
        {
            return scope;
        }
    }

    return NULL;
}

Scope *nearestProcedure(ScopeStack *scopeStack)
{
    char *procType = "proc";
    for (size_t i = 0; i < scopeStack->size; i++)
    {
        Scope *scope = top(scopeStack, i);
        if (strcmp(procType, scope->type) == 0)
        {
            return scope;
        }
    }

    return NULL;
}

Scope *nearestFunction(ScopeStack *scopeStack)
{
    char *funcType = "func";
    for (size_t i = 0; i < scopeStack->size; i++)
    {
        Scope *scope = top(scopeStack, i);
        if (strcmp(funcType, scope->type) == 0)
        {
            return scope;
        }
    }

    return NULL;
}
