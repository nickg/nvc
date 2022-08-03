//
//  Copyright (C) 2020-2022  Nick Gasson
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

#ifndef _NAMES_H
#define _NAMES_H

#include "util.h"
#include "tree.h"

typedef enum {
   F_NONE,
   F_GENERIC_MAP,
   F_PORT_MAP,
   F_SUBPROGRAM,
   F_RECORD
} formal_kind_t;

typedef enum {
   S_DESIGN_UNIT,
   S_LOOP,
   S_SUBPROGRAM,
   S_PROCESS,
   S_PROTECTED,
} scope_kind_t;

typedef enum {
   SPEC_EXACT,
   SPEC_ALL,
   SPEC_OTHERS,
} spec_kind_t;

typedef enum {
   N_OBJECT = (1 << 0),
   N_TYPE   = (1 << 1),
   N_ERROR  = (1 << 2),
   N_FUNC   = (1 << 3),
   N_PROC   = (1 << 4),
   N_LABEL  = (1 << 5),
} name_mask_t;

#define N_OVERLOADS(mask) (((mask) >> 16) & 0xffff)
#define N_SUBPROGRAM (N_FUNC | N_PROC)

nametab_t *nametab_new(void);
void nametab_finish(nametab_t *tab);

void push_scope(nametab_t *tab);
void pop_scope(nametab_t *tab);
void push_scope_for_fields(nametab_t *tab, type_t type);
void push_scope_for_formals(nametab_t *tab, formal_kind_t kind, tree_t unit);
void scope_set_prefix(nametab_t *tab, ident_t prefix);
ident_t scope_prefix(nametab_t *tab);
formal_kind_t scope_formal_kind(nametab_t *tab);
void scope_set_container(nametab_t *tab, tree_t container);
void scope_set_subprogram(nametab_t *tab, tree_t subprog);
tree_t find_enclosing(nametab_t *tab, scope_kind_t kind);
void suppress_errors(nametab_t *tab);

void map_generic_type(nametab_t *tab, type_t generic, type_t actual);
void map_generic_package(nametab_t *tab, tree_t inst);
void map_generic_box(nametab_t *tab, tree_t inst, tree_t g, unsigned pos);
void map_generic_subprogram(nametab_t *tab, tree_t generic, tree_t actual);
hash_t *get_generic_map(nametab_t *tab);
type_t get_mapped_type(nametab_t *tab, type_t type);

void mangle_func(nametab_t *tab, tree_t decl);
void mangle_type(nametab_t *tab, type_t type);
void mangle_decl(nametab_t *tab, tree_t decl);

void insert_name(nametab_t *tab, tree_t decl, ident_t alias);
void insert_names_from_use(nametab_t *tab, tree_t use);
void insert_names_from_context(nametab_t *tab, tree_t unit);
void insert_decls(nametab_t *tab, tree_t container);
void insert_ports(nametab_t *tab, tree_t container);
void insert_generics(nametab_t *tab, tree_t container);
void insert_protected_decls(nametab_t *tab, type_t type);
void insert_names_for_config(nametab_t *tab, tree_t unit);
void insert_spec(nametab_t *tab, tree_t spec, spec_kind_t kind,
                 ident_t ident, int depth);

ident_t get_implicit_label(tree_t t, nametab_t *tab);
void continue_proc_labelling_from(tree_t t, nametab_t *tab);

tree_t resolve_name(nametab_t *tab, const loc_t *loc, ident_t name);
type_t resolve_type(nametab_t *tab, type_t incomplete);
tree_t resolve_subprogram_name(nametab_t *tab, tree_t ref, type_t constraint);
void resolve_resolution(nametab_t *tab, tree_t rname, type_t type);
name_mask_t query_name(nametab_t *tab, ident_t name, tree_t *p_decl);
tree_t query_spec(nametab_t *tab, tree_t object);
tree_t find_std(nametab_t *tab);
tree_t find_forward_decl(nametab_t *tab, tree_t decl);

type_t solve_types(nametab_t *tab, tree_t expr, type_t constraint);
type_t solve_condition(nametab_t *tab, tree_t *expr, type_t constraint);

#endif // _NAMES_H
