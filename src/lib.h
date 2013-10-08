//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include <stdint.h>

#include "fbuf.h"
#include "prim.h"

typedef struct lib *lib_t;

typedef uint64_t lib_mtime_t;

lib_t lib_find(const char *name, bool verbose, bool search);
lib_t lib_new(const char *name);
lib_t lib_tmp(void);
void lib_free(lib_t lib);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);
fbuf_t *lib_fbuf_open(lib_t lib, const char *name, fbuf_mode_t mode);
void lib_realpath(lib_t lib, const char *name, char *buf, size_t buflen);
void lib_destroy(lib_t lib);
ident_t lib_name(lib_t lib);
void lib_save(lib_t lib);
void lib_mkdir(lib_t lib, const char *name);
void lib_enum_paths(const char ***result);
void lib_add_search_path(const char *path);

lib_t lib_work(void);
void lib_set_work(lib_t lib);

void lib_put(lib_t lib, struct tree *unit);
tree_t lib_get(lib_t lib, struct trie *ident);
tree_t lib_get_ctx(lib_t lib, struct trie *ident,
                         struct tree_rd_ctx **ctx);
lib_mtime_t lib_mtime(lib_t lib, struct trie *ident);

typedef void (*lib_index_fn_t)(struct trie *ident, int kind, void *context);
void lib_walk_index(lib_t lib, lib_index_fn_t fn, void *context);


#endif // _LIB_H
