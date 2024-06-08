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

// c1 = code, c2 = attribute
record *createRecord(char *c1, char *c2, char *c3)
{
  record *r = (record *)malloc(sizeof(record));

  if (!r)
  {
    printf("Allocation problem. Closing application...\n");
    exit(0);
  }

  r->code = strdup(c1);
  r->opt1 = strdup(c2);
  r->prefix = strdup(c3);

  return r;
}
