//
//  Copyright (C) 2011-2022  Nick Gasson
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

typedef uint64_t lib_mtime_t;

lib_t lib_find(ident_t name);
lib_t lib_require(ident_t name) RETURNS_NONNULL;
lib_t lib_loaded(ident_t name);
lib_t lib_new(const char *name, const char *path);
lib_t lib_tmp(const char *name);
lib_t lib_at(const char *path);
void lib_free(lib_t lib);
FILE *lib_fopen(lib_t lib, const char *name, const char *mode);
fbuf_t *lib_fbuf_open(lib_t lib, const char *name, fbuf_mode_t mode);
const char *lib_path(lib_t lib);
void lib_realpath(lib_t lib, const char *name, char *buf, size_t buflen);
void lib_destroy(lib_t lib);
ident_t lib_name(lib_t lib);
void lib_save(lib_t lib);
void lib_mkdir(lib_t lib, const char *name);
void lib_add_search_path(const char *path);
bool lib_stat(lib_t lib, const char *name, lib_mtime_t *mt);
void lib_add_map(const char *name, const char *path);
void lib_delete(lib_t lib, const char *name);
void lib_print_search_paths(text_buf_t *tb);
bool lib_contains(lib_t lib, tree_t unit);

typedef bool (*lib_walk_fn_t)(lib_t, void *);
void lib_for_all(lib_walk_fn_t fn, void *ctx);

lib_t lib_work(void);
void lib_set_work(lib_t lib);

void lib_put(lib_t lib, tree_t unit);
tree_t lib_get(lib_t lib, ident_t ident);
tree_t lib_get_check_stale(lib_t lib, ident_t ident);
tree_t lib_get_qualified(ident_t qual);
lib_mtime_t lib_mtime(lib_t lib, ident_t ident);
unsigned lib_index_size(lib_t lib);
int lib_index_kind(lib_t lib, ident_t ident);

typedef void (*lib_index_fn_t)(lib_t lib, ident_t ident, int kind, void *ctx);
void lib_walk_index(lib_t lib, lib_index_fn_t fn, void *context);

void lib_put_vcode(lib_t lib, tree_t unit, vcode_unit_t vu);
vcode_unit_t lib_get_vcode(lib_t lib, tree_t unit);

void lib_put_eopt(lib_t lib, tree_t unit, e_node_t e);
e_node_t lib_get_eopt(lib_t lib, tree_t unit);

#endif // _LIB_H
