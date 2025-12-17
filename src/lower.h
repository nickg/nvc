//
//  Copyright (C) 2023-2024  Nick Gasson
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

#ifndef _LOWER_H
#define _LOWER_H

#include "prim.h"
#include "mir/mir-unit.h"

typedef int32_t vcode_reg_t;
typedef int32_t vcode_type_t;
typedef int32_t vcode_stamp_t;

typedef void (*lower_fn_t)(lower_unit_t *, object_t *);
typedef vcode_unit_t (*emit_fn_t)(ident_t, object_t *, vcode_unit_t);

unit_registry_t *unit_registry_new(mir_context_t *mc);
void unit_registry_free(unit_registry_t *ur);
vcode_unit_t unit_registry_get(unit_registry_t *ur, ident_t ident);
void unit_registry_put(unit_registry_t *ur, lower_unit_t *lu);
void unit_registry_defer(unit_registry_t *ur, ident_t ident,
                         lower_unit_t *parent, emit_fn_t emit_fn,
                         lower_fn_t fn, cover_data_t *cover,
                         object_t *object);
void unit_registry_defer2(unit_registry_t *ur, ident_t name,
                          lower_unit_t *parent, mir_unit_kind_t kind,
                          mir_lower_fn_t fn, object_t *object);
void unit_registry_purge(unit_registry_t *ur, ident_t prefix);
bool unit_registry_query(unit_registry_t *ur, ident_t ident);
void unit_registry_finalise(unit_registry_t *ur, lower_unit_t *lu);
void unit_registry_flush(unit_registry_t *ur, ident_t name);
vcode_unit_t unit_registry_get_parent(unit_registry_t *ur, ident_t name);
void unit_registry_import(unit_registry_t *ur, vcode_unit_t vu);

lower_unit_t *lower_unit_new(unit_registry_t *ur, lower_unit_t *parent,
                             vcode_unit_t vunit, cover_data_t *cover,
                             tree_t container);
void lower_unit_free(lower_unit_t *lu);
void lower_finished(lower_unit_t *lu);

vcode_unit_t get_vcode(lower_unit_t *lu);
cover_scope_t *lower_get_cover_scope(lower_unit_t *lu);

vcode_reg_t lower_lvalue(lower_unit_t *lu, tree_t expr);
vcode_reg_t lower_rvalue(lower_unit_t *lu, tree_t expr);

vcode_type_t lower_type(type_t type);
vcode_stamp_t lower_bounds(type_t type);

lower_unit_t *lower_instance(unit_registry_t *ur, lower_unit_t *parent,
                             cover_data_t *cover, tree_t block);
void lower_process(lower_unit_t *parent, tree_t proc, driver_set_t *ds);
vcode_unit_t lower_global_thunk(unit_registry_t *registry, tree_t t);
vcode_unit_t lower_thunk_in_context(unit_registry_t *registry, tree_t t,
                                    lower_unit_t *parent);
vcode_unit_t lower_case_generate_thunk(lower_unit_t *parent, tree_t t);

int lower_search_vcode_obj(void *key, lower_unit_t *scope, int *hops);
void lower_put_vcode_obj(void *key, int obj, lower_unit_t *scope);

#endif  // _LOWER_H
