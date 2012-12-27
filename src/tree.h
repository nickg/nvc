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

#ifndef _TREE_H
#define _TREE_H

#include "lib.h"
#include "ident.h"
#include "type.h"
#include "loc.h"

#include <stdint.h>

typedef enum port_mode {
   PORT_INVALID,
   PORT_IN,
   PORT_OUT,
   PORT_INOUT,
   PORT_BUFFER
} port_mode_t;

typedef enum class {
   C_DEFAULT,
   C_SIGNAL,
   C_VARIABLE,
   C_CONSTANT,
   C_FILE,
   C_ENTITY,
   C_COMPONENT,
   C_CONFIGURATION
} class_t;

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
   T_CONCAT,
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

   T_LAST_TREE_KIND
} tree_kind_t;

typedef struct tree *tree_t;

typedef struct literal {
   union {
      int64_t i;
      double  r;
   };
   enum { L_INT, L_REAL, L_NULL } kind;
} literal_t;

typedef struct assoc {
   union {
      unsigned pos;
      tree_t   name;
      range_t  range;
   };
   tree_t value;
   enum { A_POS, A_NAMED, A_RANGE, A_OTHERS } kind;
} assoc_t;

typedef struct param {
   tree_t value;
   union {
      ident_t  name;    // P_NAME
      unsigned pos;     // P_POS
   };
   enum { P_POS, P_NAMED } kind;
} param_t;

typedef struct context {
   ident_t name;
   loc_t   loc;
} context_t;

typedef struct tree_wr_ctx *tree_wr_ctx_t;
typedef struct tree_rd_ctx *tree_rd_ctx_t;

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

unsigned tree_ports(tree_t t);
tree_t tree_port(tree_t t, unsigned n);
void tree_add_port(tree_t t, tree_t d);

port_mode_t tree_port_mode(tree_t t);
void tree_set_port_mode(tree_t t, port_mode_t mode);

tree_t tree_file_mode(tree_t t);
void tree_set_file_mode(tree_t t, tree_t m);

unsigned tree_generics(tree_t t);
tree_t tree_generic(tree_t t, unsigned n);
void tree_add_generic(tree_t t, tree_t d);

unsigned tree_genmaps(tree_t t);
param_t tree_genmap(tree_t t, unsigned n);
void tree_add_genmap(tree_t t, param_t e);

unsigned tree_params(tree_t t);
param_t tree_param(tree_t t, unsigned n);
void tree_add_param(tree_t t, param_t e);

literal_t tree_literal(tree_t t);
void tree_set_literal(tree_t t, literal_t lit);

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
void tree_set_ref(tree_t t, tree_t decl);

unsigned tree_contexts(tree_t t);
context_t tree_context(tree_t t, unsigned n);
void tree_add_context(tree_t t, context_t ctx);

unsigned tree_assocs(tree_t t);
assoc_t tree_assoc(tree_t t, unsigned n);
void tree_add_assoc(tree_t t, assoc_t a);
void tree_change_assoc(tree_t t, unsigned i, assoc_t a);

tree_t tree_severity(tree_t t);
void tree_set_severity(tree_t t, tree_t s);

tree_t tree_message(tree_t t);
void tree_set_message(tree_t t, tree_t m);

unsigned tree_pos(tree_t t);
void tree_set_pos(tree_t t, unsigned pos);

range_t tree_range(tree_t t);
void tree_set_range(tree_t t, range_t r);

class_t tree_class(tree_t t);
void tree_set_class(tree_t t, class_t c);

tree_t tree_reject(tree_t t);
void tree_set_reject(tree_t t, tree_t r);

uint32_t tree_index(tree_t t);

void tree_add_attr_str(tree_t t, ident_t name, ident_t str);
ident_t tree_attr_str(tree_t t, ident_t name);
void tree_add_attr_int(tree_t t, ident_t name, int n);
int tree_attr_int(tree_t t, ident_t name, int def);
void tree_add_attr_ptr(tree_t t, ident_t name, void *ptr);
void *tree_attr_ptr(tree_t t, ident_t name);
tree_t tree_attr_tree(tree_t t, ident_t name);
void tree_add_attr_tree(tree_t t, ident_t name, tree_t val);

// Utility functions
int64_t assume_int(tree_t t);
void range_bounds(range_t r, int64_t *low, int64_t *high);
tree_t call_builtin(const char *builtin, type_t type, ...);

typedef void (*tree_visit_fn_t)(tree_t t, void *context);
unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context);
unsigned tree_visit_only(tree_t t, tree_visit_fn_t fn,
                         void *context, tree_kind_t kind);

typedef tree_t (*tree_rewrite_fn_t)(tree_t t, void *context);
tree_t tree_rewrite(tree_t t, tree_rewrite_fn_t fn, void *context);

tree_t tree_copy(tree_t t);

void tree_gc(void);

tree_wr_ctx_t tree_write_begin(FILE *f);
void tree_write(tree_t t, tree_wr_ctx_t ctx);
void tree_write_end(tree_wr_ctx_t ctx);
FILE *tree_write_file(tree_wr_ctx_t ctx);

tree_rd_ctx_t tree_read_begin(FILE *f, const char *name);
tree_t tree_read(tree_rd_ctx_t ctx);
void tree_read_end(tree_rd_ctx_t ctx);
FILE *tree_read_file(tree_rd_ctx_t ctx);
tree_t tree_read_recall(tree_rd_ctx_t ctx, uint32_t index);

// Utility typedefs
typedef unsigned (*tree_formals_t)(tree_t t);
typedef tree_t (*tree_formal_t)(tree_t t, unsigned n);
typedef unsigned (*tree_actuals_t)(tree_t t);
typedef param_t (*tree_actual_t)(tree_t t, unsigned n);

#endif  // _TREE_H
