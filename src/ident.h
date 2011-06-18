#ifndef _IDENT_H
#define _IDENT_H

#include "lib.h"

typedef struct ident *ident_t;

/**
 * Intern a string as an identifier.
 *
 * \param str NULL-terminated string to intern. Conformity to VHDL
 *            identifier rules is not checked.
 * \return Opaque reference for this identifier. Every string equal
 *         under strncmp will return the same value when interned.
 *         Note this is not case sensitive.
 */
ident_t make_ident(const char *str);

/**
 * Convert an identifier reference to a NULL-terminated string.
 *
 * \param ident Identifier reference.
 * \return Printable representation of identifier. You shouldn't
 *         assume anything about the case of the string returned.
 */
const char *istr(ident_t ident);

/**
 * Freeze the identifier table so that it can be serialised.
 *
 * After calling this no more identifiers can be interned.
 * \see ident_key
 */
void ident_freeze(void);

/**
 * Write the frozen identifier table into a library.
 *
 * \param lib Library to store identifiers in.
 * \see ident_freeze
 */
void ident_store(lib_t lib);

/**
 * Read an identifier table from a library and merge it with the
 * existing identifier table.
 *
 * \param lib Library to load identifiers from.
 * \see ident_store
 */
void ident_load(lib_t lib);

/**
 * ???
 */
size_t ident_key(ident_t ident);

#endif // _IDENT_H
