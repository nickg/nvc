#ifndef _IDENT_H
#define _IDENT_H

#include "lib.h"

typedef struct ident *ident_t;

ident_t make_ident(const char *str);
const char *istr(ident_t ident);

void ident_freeze(void);
void ident_store(lib_t lib);
void ident_load(lib_t lib);
size_t ident_key(ident_t ident);

#endif // _IDENT_H
