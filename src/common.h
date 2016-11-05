//
//  Copyright (C) 2013-2016  Nick Gasson
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
void range_bounds(range_t r, int64_t *low, int64_t *high);
tree_t call_builtin(const char *builtin, type_t type, ...);
bool folded_int(tree_t t, int64_t *i);
bool folded_real(tree_t t, double *d);
bool folded_bool(tree_t t, bool *b);
bool folded_length(range_t r, int64_t *l);
bool folded_enum(tree_t t, unsigned *pos);
bool folded_bounds(range_t r, int64_t *low, int64_t *high);
bool folded_bounds_real(range_t r, double *low, double *high);
tree_t get_int_lit(tree_t t, int64_t i);
tree_t get_enum_lit(tree_t t, int pos);
tree_t get_real_lit(tree_t t, double r);
const char *package_signal_path_name(ident_t i);
bool parse_value(type_t type, const char *str, int64_t *value);
tree_t make_ref(tree_t to);
int record_field_to_net(type_t type, ident_t name);
tree_t find_record_field(tree_t rref);
class_t class_of(tree_t t);
bool class_has_type(class_t c);
const char *class_str(class_t c);
tree_t add_param(tree_t call, tree_t value, param_kind_t kind, tree_t name);
type_t array_aggregate_type(type_t array, int from_dim);
tree_t make_default_value(type_t type, const loc_t *loc);
unsigned bits_for_range(int64_t low, int64_t high);
unsigned array_dimension(type_t a);
type_t index_type_of(type_t type, int dim);
tree_t str_to_literal(const char *start, const char *end, type_t type);
int64_t rebase_index(type_t array_type, int dim, int64_t value);

const char *fmt_time_r(char *buf, size_t len, uint64_t t);
const char *fmt_time(uint64_t t);

//
// Utility typedefs
//

typedef unsigned (*tree_formals_t)(tree_t t);
typedef tree_t (*tree_formal_t)(tree_t t, unsigned n);
typedef unsigned (*tree_actuals_t)(tree_t t);
typedef tree_t (*tree_actual_t)(tree_t t, unsigned n);

typedef enum {
   WAITS_NO    = 0x0,
   WAITS_MAYBE = 0x1,
   WAITS_YES   = 0x3,
} wait_level_t;

typedef enum {
   IMPURE_FILE   = 0x1,
   IMPURE_SHARED = 0x2,
} impure_io_t;

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
// Disable some pedantic rule checks
//

#define RELAX_PREFER_EXPLICT  (1 << 0)
#define RELAX_LOCALLY_STATIC  (1 << 1)
#define RELAX_UNIVERSAL_BOUND (1 << 2)
#define RELAX_PURE_FILES      (1 << 3)

//
// Pre-defined attributes
//

typedef enum {
   ATTR_LAST_EVENT,
   ATTR_EVENT,
   ATTR_ACTIVE,
   ATTR_LAST_VALUE,
   ATTR_PATH_NAME,
   ATTR_INSTANCE_NAME,
   ATTR_DELAYED,
   ATTR_STABLE,
   ATTR_QUIET,
   ATTR_TRANSACTION,
   ATTR_LENGTH,
   ATTR_LEFT,
   ATTR_LOW,
   ATTR_HIGH,
   ATTR_RIGHT,
   ATTR_ASCENDING,
   ATTR_IMAGE,
   ATTR_LAST_ACTIVE,
   ATTR_DRIVING,
   ATTR_DRIVING_VALUE,
   ATTR_VALUE,
   ATTR_SUCC,
   ATTR_PRED,
   ATTR_LEFTOF,
   ATTR_RIGHTOF,
   ATTR_POS,
   ATTR_VAL
} predef_attr_t;

//
// Shared interned strings
//

#ifndef COMMON_IMPL
#define GLOBAL extern const
#else
#define GLOBAL
#endif

GLOBAL ident_t builtin_i;
GLOBAL ident_t std_standard_i;
GLOBAL ident_t formal_i;
GLOBAL ident_t elab_copy_i;
GLOBAL ident_t all_i;
GLOBAL ident_t protected_i;
GLOBAL ident_t inst_name_i;
GLOBAL ident_t fst_dir_i;
GLOBAL ident_t scope_pop_i;
GLOBAL ident_t partial_map_i;
GLOBAL ident_t fst_data_i;
GLOBAL ident_t std_logic_i;
GLOBAL ident_t std_ulogic_i;
GLOBAL ident_t std_bool_i;
GLOBAL ident_t std_char_i;
GLOBAL ident_t std_bit_i;
GLOBAL ident_t natural_i;
GLOBAL ident_t positive_i;
GLOBAL ident_t unsigned_i;
GLOBAL ident_t signed_i;
GLOBAL ident_t foreign_i;
GLOBAL ident_t nested_i;
GLOBAL ident_t drives_all_i;
GLOBAL ident_t driver_init_i;
GLOBAL ident_t GLOBAL_i;
GLOBAL ident_t mangled_i;
GLOBAL ident_t null_range_i;
GLOBAL ident_t deferred_i;
GLOBAL ident_t prot_field_i;
GLOBAL ident_t stmt_tag_i;
GLOBAL ident_t cond_tag_i;
GLOBAL ident_t sub_cond_i;
GLOBAL ident_t static_i;
GLOBAL ident_t range_var_i;
GLOBAL ident_t work_i;
GLOBAL ident_t llvm_i;
GLOBAL ident_t wait_level_i;
GLOBAL ident_t impure_io_i;
GLOBAL ident_t simple_name_i;
GLOBAL ident_t std_i;
GLOBAL ident_t nnets_i;

void intern_strings();

#endif  // _COMMON_H
