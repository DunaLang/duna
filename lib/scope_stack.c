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
    scopeStack->stack = realloc(scopeStack->stack, newSize);
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
        .deallocationLinkedList = NULL,
    };
    scopeStack->size += 1;
}

Scope *top(ScopeStack *scopeStack, size_t k)
{
    if (k >= scopeStack->size)
    {
        return NULL;
    }

    return scopeStack->stack + (scopeStack->size - 1 - k);
}

char *deallocationCode(Scope *scope)
{
    if (!scope || scope->deallocationLinkedList == NULL)
    {
        return NULL;
    }

    size_t strLength = 0; // '\0'
    struct DeallocationLinkedList *head = scope->deallocationLinkedList;
    while (head != NULL)
    {
        strLength += strlen(head->variable) + 8; // 8 = "free();\n"
        head = head->next;
    }

    char *deallocationCode = malloc(sizeof(char) * strLength);
    if (!deallocationCode)
    {
        printf("Allocation error.");
        exit(-1);
    }

    deallocationCode[0] = '\0';
    head = scope->deallocationLinkedList;
    while (head != NULL)
    {
        char *str = formatStr("free(%s);\n", head->variable);
        strcat(deallocationCode, str);
        free(str);
        head = head->next;
    }
    return deallocationCode;
}

char *deallocationCodeCurrentScope(ScopeStack *scopeStack)
{
    return deallocationCode(top(scopeStack, 0));
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

struct DeallocationLinkedList *newDeallocationNode(char *variable)
{
    struct DeallocationLinkedList *node = malloc(sizeof(struct DeallocationLinkedList));
    if (!node)
    {
        printf("Allocation error.");
        exit(-1);
    }

    node->variable = strdup(variable);
    node->next = NULL;
    return node;
}

void addDeallocationToScope(ScopeStack *scopeStack, char *variable)
{
    Scope *scope = top(scopeStack, 0);
    if (scope == NULL)
    {
        return;
    }

    struct DeallocationLinkedList *previousHead = scope->deallocationLinkedList;
    scope->deallocationLinkedList = newDeallocationNode(variable);
    scope->deallocationLinkedList->next = previousHead;
}
