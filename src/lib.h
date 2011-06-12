#ifndef _LIB_H
#define _LIB_H

#include <stdio.h>

typedef struct lib *lib_t;

lib_t lib_find(const char *name);
lib_t lib_new(const char *name);
void lib_free(lib_t lib);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);
void lib_destroy(lib_t lib);

#endif // _LIB_H
