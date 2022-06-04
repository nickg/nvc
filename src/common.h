//
//  Copyright (C) 2013-2022  Nick Gasson
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

#ifndef _COMMON_H
#define _COMMON_H

#include "tree.h"

//
// Various utility functions
//

int64_t assume_int(tree_t t);
void range_bounds(tree_t r, int64_t *low, int64_t *high);
bool folded_int(tree_t t, int64_t *i);
bool folded_real(tree_t t, double *d);
bool folded_bool(tree_t t, bool *b);
bool folded_length(tree_t r, int64_t *l);
bool folded_enum(tree_t t, unsigned *pos);
bool folded_bounds(tree_t r, int64_t *low, int64_t *high);
bool folded_bounds_real(tree_t r, double *low, double *high);
tree_t get_int_lit(tree_t t, type_t type, int64_t i);
tree_t get_enum_lit(tree_t t, type_t type, int pos);
tree_t get_real_lit(tree_t t, type_t type, double r);
const char *package_signal_path_name(ident_t i);
bool parse_value(type_t type, const char *str, int64_t *value);
tree_t make_ref(tree_t to);
int record_field_to_net(type_t type, unsigned pos);
tree_t find_record_field(tree_t rref);
class_t class_of(tree_t t);
bool class_has_type(class_t c);
const char *class_str(class_t c);
const char *assoc_kind_str(assoc_kind_t akind);
tree_t add_param(tree_t call, tree_t value, param_kind_t kind, tree_t name);
type_t array_aggregate_type(type_t array, int from_dim);
unsigned bits_for_range(int64_t low, int64_t high);
unsigned dimension_of(type_t type);
type_t index_type_of(type_t type, unsigned dim);
range_kind_t direction_of(type_t type, unsigned dim);
tree_t range_of(type_t type, unsigned dim);
tree_t str_to_literal(const char *start, const char *end, type_t type);
int64_t rebase_index(type_t array_type, int dim, int64_t value);
bool unit_needs_cgen(tree_t t);
bool package_needs_body(tree_t pack);
bool is_subprogram(tree_t t);
bool is_container(tree_t t);
bool is_package(tree_t t);
bool is_guarded_signal(tree_t decl);
bool is_type_decl(tree_t t);
bool is_loop_stmt(tree_t t);
bool is_design_unit(tree_t t);
bool is_uninstantiated_package(tree_t pack);
tree_t search_decls(tree_t container, ident_t name, int nth);
bool is_builtin(subprogram_kind_t kind);
bool is_open_coded_builtin(subprogram_kind_t kind);
tree_t find_mangled_decl(tree_t container, ident_t name);
tree_t name_to_ref(tree_t name);
const char *port_mode_str(port_mode_t mode);
const char *ename_kind_str(ename_kind_t kind);
void mangle_one_type(text_buf_t *buf, type_t type);
tree_t primary_unit_of(tree_t unit);
int64_t encode_case_choice(tree_t value, int length, int bits);
void to_string(text_buf_t *tb, type_t type, int64_t value);
tree_t longest_static_prefix(tree_t expr);
tree_t body_of(tree_t pack);
tree_t find_generic_map(tree_t unit, int pos, tree_t g);

int fmt_time_r(char *buf, size_t len, uint64_t t);
const char *fmt_time(uint64_t t);

#define MAX_CONSTRAINTS 8
int pack_constraints(type_t type, tree_t out[MAX_CONSTRAINTS]);

//
// Utility typedefs
//

typedef unsigned (*tree_formals_t)(tree_t t);
typedef tree_t (*tree_formal_t)(tree_t t, unsigned n);
typedef unsigned (*tree_actuals_t)(tree_t t);
typedef tree_t (*tree_actual_t)(tree_t t, unsigned n);

//
// VHDL standard revisions
//

typedef enum {
   STD_87,
   STD_93,
   STD_00,
   STD_02,
   STD_08
} vhdl_standard_t;

vhdl_standard_t standard(void);
void set_standard(vhdl_standard_t s);
const char *standard_text(vhdl_standard_t s);

//
// Standard types
//

typedef enum {
   STD_UNIVERSAL_INTEGER,
   STD_UNIVERSAL_REAL,
   STD_INTEGER,
   STD_REAL,
   STD_BOOLEAN,
   STD_STRING,
   STD_TIME,
   STD_BIT,
   STD_FILE_OPEN_KIND,
   STD_FILE_OPEN_STATUS,
   STD_NATURAL,
   STD_BIT_VECTOR,
   STD_SEVERITY_LEVEL,
} std_type_t;

type_t std_type(tree_t std, std_type_t which);
tree_t std_func(ident_t mangled);

typedef enum {
   IEEE_STD_ULOGIC,
   IEEE_STD_LOGIC,
} ieee_type_t;

type_t ieee_type(ieee_type_t which);

//
// Disable some pedantic rule checks
//

#define RELAX_PREFER_EXPLICT  (1 << 0)
#define RELAX_LOCALLY_STATIC  (1 << 1)
#define RELAX_UNIVERSAL_BOUND (1 << 2)
#define RELAX_PURE_FILES      (1 << 3)
#define RELAX_IMPURE          (1 << 4)
#define RELAX_SHARED          (1 << 5)

int relax_rules(void);
void set_relax_rules(int mask);

//
// Shared interned strings
//

typedef enum {
   W_STD_STANDARD,
   W_ALL,
   W_STD_LOGIC,
   W_STD_ULOGIC,
   W_STD_BOOL,
   W_STD_CHAR,
   W_STD_BIT,
   W_STD_NATURAL,
   W_STD_POSITIVE,
   W_IEEE_UNSIGNED,
   W_IEEE_SIGNED,
   W_FOREIGN,
   W_WORK,
   W_STD,
   W_THUNK,
   W_BODY,
   W_CARET,
   W_IEEE,
   W_IEEE_1164,

   NUM_WELL_KNOWN
} well_known_t;

ident_t well_known(well_known_t id);
well_known_t is_well_known(ident_t ident);
void intern_strings();

#endif  // _COMMON_H
