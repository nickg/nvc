#ifndef _LIB_H
#define _LIB_H

#include <stdio.h>

typedef struct lib *lib_t;

lib_t find_lib(const char *name);
lib_t make_lib(const char *name);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);

#endif // _LIB_H
