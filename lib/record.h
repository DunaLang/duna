#ifndef RECORD
#define RECORD

struct record {
	   char * code; /* field for storing the output code */
	   char * opt1; /* field for another purpose */
};

typedef struct record record;
 
void freeRecord(record *);
record * createRecord(char *, char *);

#endif