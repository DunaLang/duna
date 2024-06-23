#include "record.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void freeRecord(record *r)
{
  if (r)
  {
    if (r->code != NULL)
      free(r->code);
    if (r->opt1 != NULL)
      free(r->opt1);
    if (r->prefix != NULL)
      free(r->prefix);
    free(r);
  }
}

// c1 = code, c2 = attribute, c3 = prefix
record *createRecord(char *c1, char *c2, char *c3)
{
  record *r = (record *)malloc(sizeof(record));

  if (!r)
  {
    printf("Allocation problem. Closing application...\n");
    exit(0);
  }

  if (c1 == NULL) {
    r->code = NULL;
  } else {
    r->code = strdup(c1);
  }

  if (c2 == NULL) {
    r->opt1 = NULL;
  } else {
    r->opt1 = strdup(c2);
  }

  if (c3 == NULL) {
    r->prefix = NULL;
  } else {
    r->prefix = strdup(c3);
  }

  return r;
}
