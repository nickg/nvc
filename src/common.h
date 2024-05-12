//
//  Copyright (C) 2013-2024  Nick Gasson
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
bool folded_bounds(tree_t r, int64_t *low, int64_t *high);
bool folded_bounds_real(tree_t r, double *low, double *high);
tree_t get_int_lit(tree_t t, type_t type, int64_t i);
tree_t get_enum_lit(tree_t t, type_t type, int pos);
tree_t get_real_lit(tree_t t, type_t type, double r);
tree_t get_discrete_lit(tree_t t, type_t type, int64_t i);
tree_t make_ref(tree_t to);
tree_t find_record_field(tree_t rref);
tree_t find_element_mode_indication(tree_t view, tree_t field, bool *converse);
port_mode_t converse_mode(tree_t port, bool converse);
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
int64_t rebase_index(type_t array_type, int dim, int64_t value);
bool unit_needs_cgen(tree_t t);
bool package_needs_body(tree_t pack);
bool is_subprogram(tree_t t);
bool is_container(tree_t t);
bool is_concurrent_block(tree_t t);
bool is_package(tree_t t);
bool is_guarded_signal(tree_t decl);
bool is_type_decl(tree_t t);
tree_t aliased_type_decl(tree_t decl);
bool is_loop_stmt(tree_t t);
bool is_design_unit(tree_t t);
bool is_literal(tree_t t);
bool is_body(tree_t t);
bool is_uninstantiated_package(tree_t pack);
bool is_uninstantiated_subprogram(tree_t decl);
bool is_anonymous_subtype(type_t type);
tree_t search_decls(tree_t container, ident_t name, int nth);
bool is_open_coded_builtin(subprogram_kind_t kind);
bool attribute_has_param(attr_kind_t attr);
tree_t name_to_ref(tree_t name);
const char *port_mode_str(port_mode_t mode);
void mangle_one_type(text_buf_t *buf, type_t type);
tree_t primary_unit_of(tree_t unit);
unsigned get_case_choice_char(tree_t value, int depth);
int64_t encode_case_choice(tree_t value, int length, int bits);
void to_string(text_buf_t *tb, type_t type, int64_t value);
tree_t longest_static_prefix(tree_t expr);
tree_t body_of(tree_t pack);
tree_t find_generic_map(tree_t unit, int pos, tree_t g);
bool relaxed_rules(void);
bool is_type_attribute(attr_kind_t kind);
type_t get_type_or_null(tree_t t);
type_t subtype_for_string(tree_t str, type_t base);
tree_t change_ref(tree_t name, tree_t new);
bool all_character_literals(type_t type);
bool is_operator_symbol(ident_t ident);
bool same_tree(tree_t a, tree_t b);
void instance_name_to_path(text_buf_t *tb, const char *str);
bool calculate_aggregate_bounds(tree_t expr, range_kind_t *kind,
                                int64_t *left, int64_t *right);
type_t calculate_aggregate_subtype(tree_t expr);

void analyse_file(const char *file, jit_t *jit, unit_registry_t *ur);

void print_syntax(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void capture_syntax(text_buf_t *tb);

typedef void (*build_wait_fn_t)(tree_t, void *);
void build_wait(tree_t expr, build_wait_fn_t fn, void *ctx);

typedef struct {
   unsigned count;
   uint8_t  values[];
} enum_array_t;

typedef union {
   int64_t       integer;
   double        real;
   enum_array_t *enums;
} parsed_value_t;

bool parse_value(type_t type, const char *str, parsed_value_t *value);

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
   STD_08,
   STD_19
} vhdl_standard_t;

vhdl_standard_t standard(void);
void set_standard(vhdl_standard_t s);
void set_default_standard(vhdl_standard_t s);
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
   STD_FILE_ORIGIN_KIND,
   STD_FILE_OPEN_STATE,
} std_type_t;

type_t std_type(tree_t std, std_type_t which);
tree_t std_func(ident_t mangled);

typedef enum {
   IEEE_STD_ULOGIC,
   IEEE_STD_LOGIC,
} ieee_type_t;

type_t ieee_type(ieee_type_t which);

typedef enum {
   VERILOG_LOGIC,
   VERILOG_PACKED_LOGIC,
   VERILOG_INT64,
   VERILOG_NET_VALUE,
   VERILOG_NET_ARRAY,
   VERILOG_RESOLVED_NET,
   VERILOG_RESOLVED_NET_ARRAY,
} verilog_type_t;

type_t verilog_type(verilog_type_t which);
tree_t verilog_func(ident_t mangled);

typedef enum {
   REFLECT_VALUE_MIRROR,
   REFLECT_SUBTYPE_MIRROR,
} reflect_type_t;

type_t reflection_type(reflect_type_t which);

//
// Shared interned strings
//

typedef enum {
   W_STD_STANDARD,
   W_ALL,
   W_STD_BOOL,
   W_STD_CHAR,
   W_STD_BIT,
   W_STD_NATURAL,
   W_STD_POSITIVE,
   W_STD_INTEGER,
   W_STD_STRING,
   W_STD_REAL,
   W_STD_TIME,
   W_STD_BIT_VECTOR,
   W_IEEE_LOGIC,
   W_IEEE_ULOGIC,
   W_IEEE_UNSIGNED,
   W_IEEE_SIGNED,
   W_IEEE_LOGIC_VECTOR,
   W_IEEE_ULOGIC_VECTOR,
   W_IEEE_1164_AND,
   W_IEEE_1164_NAND,
   W_IEEE_1164_OR,
   W_IEEE_1164_NOR,
   W_IEEE_1164_XOR,
   W_IEEE_1164_XNOR,
   W_FOREIGN,
   W_WORK,
   W_STD,
   W_THUNK,
   W_BODY,
   W_CARET,
   W_IEEE,
   W_IEEE_1164,
   W_ERROR,
   W_CCONV,
   W_ELAB,
   W_NUMERIC_STD,
   W_NUMERIC_BIT,
   W_NUMERIC_STD_UNSIGNED,
   W_NUMERIC_BIT_UNSIGNED,
   W_NVC,
   W_DEFAULT_CLOCK,
   W_DOLLAR_DISPLAY,
   W_DOLLAR_FINISH,
   W_DOLLAR_WRITE,
   W_STD_REFLECTION,
   W_NVC_PSL_SUPPORT,
   W_VITAL,
   W_NEVER_WAITS,
   W_NVC_VERILOG,
   W_SHAPE,
   W_INSTANCE_NAME,
   W_PATH_NAME,
   W_DOLLAR_TIME,

   NUM_WELL_KNOWN
} well_known_t;

ident_t well_known(well_known_t id);
well_known_t is_well_known(ident_t ident);
void intern_strings(void);

#endif  // _COMMON_H
