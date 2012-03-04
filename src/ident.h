#ifndef _IDENT_H
#define _IDENT_H

#include "lib.h"

typedef struct trie *ident_t;
typedef struct ident_wr_ctx *ident_wr_ctx_t;
typedef struct ident_rd_ctx *ident_rd_ctx_t;

// Intern a string as an identifier.
ident_t ident_new(const char *str);

// Generate a unique identifier with the given prefix.
ident_t ident_uniq(const char *prefix);

// Create a new identifier which is a prepended to b separated
// by a dot.
ident_t ident_prefix(ident_t a, ident_t b, char sep);

// Strips a suffix from an identifier or returns NULL if this
// is not possible.
ident_t ident_strip(ident_t a, ident_t b);

// Return the Nth character of an identifier counting from the end.
char ident_char(ident_t i, unsigned n);

// Return the the prefix of i that does not include c
ident_t ident_until(ident_t i, char c);

// Compare identifier against a NULL-terminated string
bool icmp(ident_t i, const char *s);

// Convert an identifier reference to a NULL-terminated string.
// This function is quite slow so its use should be avoided except
// for printing. The pointer returned is only valid for the next
// ISTR_MAX_BUFS calls to istr.
#define ISTR_MAX_BUFS 8
const char *istr(ident_t ident);

ident_wr_ctx_t ident_write_begin(FILE *f);
void ident_write(ident_t ident, ident_wr_ctx_t ctx);
void ident_write_end(ident_wr_ctx_t ctx);

ident_rd_ctx_t ident_read_begin(FILE *f);
ident_t ident_read(ident_rd_ctx_t ctx);
void ident_read_end(ident_rd_ctx_t ctx);

#endif // _IDENT_H
