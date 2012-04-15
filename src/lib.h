//
//  Copyright (C) 2011-2012  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef _LIB_H
#define _LIB_H

#include <stdio.h>
#include <stdbool.h>
#include <time.h>

struct trie;
struct tree;
struct tree_rd_ctx;

typedef struct lib *lib_t;

lib_t lib_find(const char *name, bool verbose, bool search);
lib_t lib_new(const char *name);
lib_t lib_tmp(void);
void lib_free(lib_t lib);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);
void lib_realpath(lib_t lib, const char *name, char *buf, size_t buflen);
void lib_destroy(lib_t lib);
struct trie *lib_name(lib_t lib);
void lib_save(lib_t lib);
void lib_load_all(lib_t lib);

lib_t lib_work(void);
void lib_set_work(lib_t lib);

void lib_put(lib_t lib, struct tree *unit);
struct tree *lib_get(lib_t lib, struct trie *ident);
struct tree *lib_get_ctx(lib_t lib, struct trie *ident,
                         struct tree_rd_ctx **ctx);
time_t lib_mtime(lib_t lib, struct trie *ident);

typedef void (*lib_iter_fn_t)(struct tree *t, void *context);
void lib_foreach(lib_t lib, lib_iter_fn_t fn, void *context);


#endif // _LIB_H
