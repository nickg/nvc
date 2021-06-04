//
//  Copyright (C) 2011-2021  Nick Gasson
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

#ifndef _TREE_H
#define _TREE_H

#include "lib.h"
#include "ident.h"
#include "prim.h"
#include "type.h"

#include <stdint.h>

typedef enum port_mode {
   PORT_INVALID,
   PORT_IN,
   PORT_OUT,
   PORT_INOUT,
   PORT_BUFFER,
   PORT_LINKAGE
} port_mode_t;

typedef enum class {
   C_DEFAULT,
   C_SIGNAL,
   C_VARIABLE,
   C_CONSTANT,
   C_FILE,
   C_ENTITY,
   C_COMPONENT,
   C_CONFIGURATION,
   C_ARCHITECTURE,
   C_FUNCTION,
   C_PACKAGE,
   C_TYPE,
   C_SUBTYPE,
   C_LABEL,
   C_PROCEDURE,
   C_LITERAL,
   C_UNITS,
   C_LIBRARY,
   C_ELAB
} class_t;

typedef enum param_kind {
   P_POS,
   P_NAMED
} param_kind_t;

typedef enum assoc_kind {
   A_POS,
   A_NAMED,
   A_RANGE,
   A_OTHERS
} assoc_kind_t;

typedef enum literal_kind {
   L_INT,
   L_REAL,
   L_NULL,
   L_STRING,
   L_PHYSICAL
} literal_kind_t;

typedef enum constraint_kind {
   C_RANGE,
   C_INDEX,
   C_COMPUTED
} constraint_kind_t;

typedef enum {
   RANGE_TO,
   RANGE_DOWNTO,
   RANGE_EXPR,
   RANGE_ERROR
} range_kind_t;

typedef enum {
   S_USER,
   S_FOREIGN,
   S_ARRAY_EQ,
   S_ARRAY_NEQ,
   S_ARRAY_LT,
   S_ARRAY_LE,
   S_ARRAY_GT,
   S_ARRAY_GE,
   S_CONCAT,
   S_RECORD_EQ,
   S_RECORD_NEQ,
   S_MUL,
   S_MUL_PR,
   S_MUL_RP,
   S_DIV,
   S_DIV_PR,
   S_ADD,
   S_SUB,
   S_IDENTITY,
   S_NEGATE,
   S_SCALAR_LT,
   S_SCALAR_LE,
   S_SCALAR_GT,
   S_SCALAR_GE,
   S_SCALAR_EQ,
   S_SCALAR_NEQ,
   S_ABS,
   S_MOD,
   S_REM,
   S_EXP,
   S_MUL_RI,
   S_MUL_IR,
   S_DIV_RI,
   S_SCALAR_AND,
   S_SCALAR_OR,
   S_SCALAR_XOR,
   S_SCALAR_NAND,
   S_SCALAR_NOR,
   S_SCALAR_XNOR,
   S_SCALAR_NOT,
   S_ARRAY_AND,
   S_ARRAY_OR,
   S_ARRAY_XOR,
   S_ARRAY_NAND,
   S_ARRAY_NOR,
   S_ARRAY_XNOR,
   S_ARRAY_NOT,
   S_SLL,
   S_SRL,
   S_SLA,
   S_SRA,
   S_ROL,
   S_ROR,
   S_FILE_OPEN1,
   S_FILE_OPEN2,
   S_FILE_CLOSE,
   S_FILE_READ,
   S_FILE_WRITE,
   S_ENDFILE,
   S_DEALLOCATE,
   S_MINIMUM,
   S_MAXIMUM,
   S_TO_STRING,
   S_RISING_EDGE,
   S_FALLING_EDGE,
   S_REDUCE_AND,
   S_REDUCE_OR,
   S_REDUCE_NAND,
   S_REDUCE_NOR,
   S_REDUCE_XOR,
   S_REDUCE_XNOR,
} subprogram_kind_t;

typedef enum {
   ATTR_USER,
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
   ATTR_VAL,
   ATTR_RANGE,
   ATTR_REVERSE_RANGE,
   ATTR_BASE,
} attr_kind_t;

typedef enum tree_kind {
   T_ENTITY,
   T_ARCH,
   T_PORT_DECL,
   T_FCALL,
   T_LITERAL,
   T_SIGNAL_DECL,
   T_VAR_DECL,
   T_PROCESS,
   T_REF,
   T_WAIT,
   T_TYPE_DECL,
   T_VAR_ASSIGN,
   T_PACKAGE,
   T_SIGNAL_ASSIGN,
   T_QUALIFIED,
   T_ENUM_LIT,
   T_CONST_DECL,
   T_FUNC_DECL,
   T_ELAB,
   T_AGGREGATE,
   T_ASSERT,
   T_ATTR_REF,
   T_ARRAY_REF,
   T_ARRAY_SLICE,
   T_INSTANCE,
   T_IF,
   T_NULL,
   T_PACK_BODY,
   T_FUNC_BODY,
   T_RETURN,
   T_CASSIGN,
   T_WHILE,
   T_WAVEFORM,
   T_ALIAS,
   T_FOR,
   T_ATTR_DECL,
   T_ATTR_SPEC,
   T_PROC_DECL,
   T_PROC_BODY,
   T_EXIT,
   T_PCALL,
   T_CASE,
   T_BLOCK,
   T_COND,
   T_TYPE_CONV,
   T_SELECT,
   T_COMPONENT,
   T_IF_GENERATE,
   T_FOR_GENERATE,
   T_FILE_DECL,
   T_OPEN,
   T_FIELD_DECL,
   T_RECORD_REF,
   T_ALL,
   T_NEW,
   T_CASSERT,
   T_CPCALL,
   T_UNIT_DECL,
   T_NEXT,
   T_GENVAR,
   T_PARAM,
   T_ASSOC,
   T_USE,
   T_HIER,
   T_SPEC,
   T_BINDING,
   T_LIBRARY,
   T_DESIGN_UNIT,
   T_CONFIGURATION,
   T_PROT_BODY,
   T_CONTEXT,
   T_CTXREF,
   T_CONSTRAINT,
   T_BLOCK_CONFIG,
   T_PROT_FCALL,
   T_PROT_PCALL,
   T_RANGE,

   T_LAST_TREE_KIND
} tree_kind_t;

typedef enum {
   TREE_F_LOCALLY_STATIC  = (1 << 0),
   TREE_F_GLOBALLY_STATIC = (1 << 1),
   TREE_F_UNCONSTRAINED   = (1 << 2),
   TREE_F_IMPURE          = (1 << 3),
   TREE_F_CONVERSION      = (1 << 4),
   TREE_F_POSTPONED       = (1 << 5),
   TREE_F_SHARED          = (1 << 6),
   TREE_F_REPORT          = (1 << 7),
   TREE_F_GUARDED         = (1 << 8),
   TREE_F_ELIDE_BOUNDS    = (1 << 9),
   TREE_F_LAST_VALUE      = (1 << 10),
   TREE_F_PACKAGE_SIGNAL  = (1 << 11),
   TREE_F_SYNTHETIC_NAME  = (1 << 12),
   TREE_F_PREDEFINED      = (1 << 13),
   TREE_F_UNIVERSAL       = (1 << 14),
   TREE_F_LOOP_VAR        = (1 << 15),
   TREE_F_FOREIGN         = (1 << 16),
   TREE_F_PROTECTED       = (1 << 17),
   TREE_F_STATIC_WAIT     = (1 << 18),
   TREE_F_NULL_RANGE      = (1 << 19),
} tree_flags_t;

tree_t tree_new(tree_kind_t kind);
tree_kind_t tree_kind(tree_t t);
void tree_change_kind(tree_t t, tree_kind_t kind);
const char *tree_kind_str(tree_kind_t t);

const loc_t *tree_loc(tree_t t);
void tree_set_loc(tree_t t, const loc_t *loc);

// See `has_map' in tree.c for definition of which fields each tree
// kind contains

type_t tree_type(tree_t t);
void tree_set_type(tree_t t, type_t ty);
bool tree_has_type(tree_t t);

ident_t tree_ident(tree_t t);
void tree_set_ident(tree_t t, ident_t i);
bool tree_has_ident(tree_t t);

ident_t tree_ident2(tree_t t);
void tree_set_ident2(tree_t t, ident_t i);
bool tree_has_ident2(tree_t t);

unsigned tree_ports(tree_t t);
tree_t tree_port(tree_t t, unsigned n);
void tree_add_port(tree_t t, tree_t d);

tree_t tree_file_mode(tree_t t);
void tree_set_file_mode(tree_t t, tree_t m);

unsigned tree_generics(tree_t t);
tree_t tree_generic(tree_t t, unsigned n);
void tree_add_generic(tree_t t, tree_t d);

unsigned tree_genmaps(tree_t t);
tree_t tree_genmap(tree_t t, unsigned n);
void tree_add_genmap(tree_t t, tree_t e);

unsigned tree_params(tree_t t);
tree_t tree_param(tree_t t, unsigned n);
void tree_add_param(tree_t t, tree_t e);

int64_t tree_ival(tree_t t);
void tree_set_ival(tree_t t, int64_t i);

double tree_dval(tree_t t);
void tree_set_dval(tree_t t, double d);

bool tree_has_value(tree_t t);
tree_t tree_value(tree_t t);
void tree_set_value(tree_t t, tree_t v);

unsigned tree_waveforms(tree_t t);
tree_t tree_waveform(tree_t t, unsigned n);
void tree_add_waveform(tree_t t, tree_t w);

unsigned tree_decls(tree_t t);
tree_t tree_decl(tree_t t, unsigned n);
void tree_add_decl(tree_t t, tree_t d);

unsigned tree_stmts(tree_t t);
tree_t tree_stmt(tree_t t, unsigned n);
void tree_add_stmt(tree_t t, tree_t d);

unsigned tree_else_stmts(tree_t t);
tree_t tree_else_stmt(tree_t t, unsigned n);
void tree_add_else_stmt(tree_t t, tree_t d);

unsigned tree_conds(tree_t t);
tree_t tree_cond(tree_t t, unsigned n);
void tree_add_cond(tree_t t, tree_t d);

bool tree_has_delay(tree_t t);
tree_t tree_delay(tree_t t);
void tree_set_delay(tree_t t, tree_t d);

unsigned tree_triggers(tree_t t);
tree_t tree_trigger(tree_t t, unsigned n);
void tree_add_trigger(tree_t t, tree_t s);

tree_t tree_target(tree_t t);
void tree_set_target(tree_t t, tree_t lhs);

tree_t tree_ref(tree_t t);
bool tree_has_ref(tree_t t);
void tree_set_ref(tree_t t, tree_t decl);

unsigned tree_contexts(tree_t t);
tree_t tree_context(tree_t t, unsigned n);
void tree_add_context(tree_t t, tree_t ctx);

unsigned tree_assocs(tree_t t);
tree_t tree_assoc(tree_t t, unsigned n);
void tree_add_assoc(tree_t t, tree_t a);

tree_t tree_severity(tree_t t);
void tree_set_severity(tree_t t, tree_t s);

tree_t tree_message(tree_t t);
bool tree_has_message(tree_t t);
void tree_set_message(tree_t t, tree_t m);

unsigned tree_pos(tree_t t);
void tree_set_pos(tree_t t, unsigned pos);

tree_t tree_left(tree_t t);
void tree_set_left(tree_t t, tree_t left);

tree_t tree_right(tree_t t);
void tree_set_right(tree_t t, tree_t right);

unsigned tree_subkind(tree_t t);
void tree_set_subkind(tree_t t, unsigned sub);

void tree_add_range(tree_t t, tree_t r);
tree_t tree_range(tree_t t, unsigned n);
unsigned tree_ranges(tree_t t);
void tree_change_range(tree_t t, unsigned n, tree_t r);  // XXX: remove

class_t tree_class(tree_t t);
void tree_set_class(tree_t t, class_t c);

tree_t tree_reject(tree_t t);
void tree_set_reject(tree_t t, tree_t r);
bool tree_has_reject(tree_t t);

tree_t tree_name(tree_t t);
void tree_set_name(tree_t t, tree_t n);
bool tree_has_name(tree_t t);

tree_t tree_spec(tree_t t);
bool tree_has_spec(tree_t t);
void tree_set_spec(tree_t t, tree_t s);

unsigned tree_chars(tree_t t);
tree_t tree_char(tree_t t, unsigned n);
void tree_add_char(tree_t t, tree_t ref);

unsigned tree_nets(tree_t t);
netid_t tree_net(tree_t t, unsigned n);
void tree_add_net(tree_t t, netid_t n);
void tree_change_net(tree_t t, unsigned n, netid_t i);

tree_flags_t tree_flags(tree_t t);
void tree_set_flag(tree_t t, tree_flags_t mask);

void tree_add_attr_str(tree_t t, ident_t name, ident_t str);
ident_t tree_attr_str(tree_t t, ident_t name);
void tree_add_attr_int(tree_t t, ident_t name, int n);
int tree_attr_int(tree_t t, ident_t name, int def);
void tree_add_attr_ptr(tree_t t, ident_t name, void *ptr);
void *tree_attr_ptr(tree_t t, ident_t name);
tree_t tree_attr_tree(tree_t t, ident_t name);
void tree_add_attr_tree(tree_t t, ident_t name, tree_t val);
void tree_remove_attr(tree_t t, ident_t name);

typedef void (*tree_visit_fn_t)(tree_t t, void *context);
unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context);
unsigned tree_visit_only(tree_t t, tree_visit_fn_t fn,
                         void *context, tree_kind_t kind);

typedef tree_t (*tree_rewrite_fn_t)(tree_t t, void *context);
tree_t tree_rewrite(tree_t t, tree_rewrite_fn_t fn, void *context);

typedef bool (*tree_copy_fn_t)(tree_t t, void *context);
tree_t tree_copy(tree_t t, tree_copy_fn_t fn, void *context);

void tree_gc(void);

tree_wr_ctx_t tree_write_begin(fbuf_t *f);
void tree_write(tree_t t, tree_wr_ctx_t ctx);
void tree_write_end(tree_wr_ctx_t ctx);

tree_rd_ctx_t tree_read_begin(fbuf_t *f, const char *name);
tree_t tree_read(tree_rd_ctx_t ctx);
void tree_read_end(tree_rd_ctx_t ctx);

#endif  // _TREE_H
