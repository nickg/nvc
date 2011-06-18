#ifndef _IDENT_H
#define _IDENT_H

#include "lib.h"

typedef struct trie *ident_t;

/**
 * Intern a string as an identifier.
 *
 * \param str NULL-terminated string to intern. Conformity to VHDL
 *            identifier rules is not checked.
 * \return Opaque reference for this identifier. Every string equal
 *         under strcasecmp will return the same value when interned.
 *         Note this is not case sensitive.
 */
ident_t make_ident(const char *str);

/**
 * Convert an identifier reference to a NULL-terminated string.
 *
 * This function is quite slow so its use should be avoid except
 * for printing. The pointer returned is only valid until the
 * next call to istr.
 *
 * \param ident Identifier reference.
 * \return Printable representation of identifier. You shouldn't
 *         assume anything about the case of the string returned.
 */
const char *istr(ident_t ident);

/**
 * Serialise and identifier to a file.
 *
 * \param ident Identifier to store.
 * \param f File to write into.
 */
void ident_write(ident_t ident, FILE *f);

/**
 * Read a serialised identifier from a file.
 *
 * \param f File to read from.
 * \return De-serialised identifier.
 */
ident_t ident_read(FILE *f);

#endif // _IDENT_H
