#pragma once

typedef struct ScopeStack
{
    struct Scope *stack;
    int size;
    int capacity;
} ScopeStack;

struct DeallocationLinkedList
{
    char *variable;
    struct DeallocationLinkedList *next;
};

typedef struct Scope
{
    char *name;
    char *type;

    struct DeallocationLinkedList *deallocationLinkedList;
} Scope;

ScopeStack createScopeStack();

void insertScope(ScopeStack *scopeStack, char *name, char *type);
Scope *top(ScopeStack *scopeStack, int k);
void pop(ScopeStack *scopeStack);

Scope *nearestIteration(ScopeStack *scopeStack);
Scope *nearestProcedure(ScopeStack *scopeStack);
Scope *nearestFunction(ScopeStack *scopeStack);

char *deallocationCodeCurrentScope(ScopeStack *scopeStack);
void addDeallocationToScope(ScopeStack *scopeStack, char *variable);
