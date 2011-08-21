#ifndef _LIB_H
#define _LIB_H

#include <stdio.h>
#include <stdbool.h>

struct trie;
struct tree;

typedef struct lib *lib_t;

lib_t lib_find(const char *name, bool verbose);
lib_t lib_new(const char *name);
lib_t lib_tmp(void);
void lib_free(lib_t lib);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);
void lib_destroy(lib_t lib);
struct trie *lib_name(lib_t lib);
void lib_save(lib_t lib);

lib_t lib_work(void);
void lib_set_work(lib_t lib);

void lib_put(lib_t lib, struct tree *unit);
struct tree *lib_get(lib_t lib, struct trie *ident);


#endif // _LIB_H
