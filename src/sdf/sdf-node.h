//
//  Copyright (C) 2022  Nick Gasson
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

#ifndef _SDF_NODE_H
#define _SDF_NODE_H

#include "prim.h"
#include "common.h"

typedef enum {
   S_HEADER_SDF_VERSION    = (1 << 0),
   S_HEADER_DESIGN         = (1 << 1),
   S_HEADER_DATE           = (1 << 2),
   S_HEADER_VENDOR         = (1 << 3),
   S_HEADER_PROGRAM        = (1 << 4),
   S_HEADER_VERSION        = (1 << 5),
   S_HEADER_DIVIDER        = (1 << 6),
   S_HEADER_VOLTAGE        = (1 << 7),
   S_HEADER_PROCESS        = (1 << 8),
   S_HEADER_TEMPERATURE    = (1 << 9),
   S_HEADER_TIMESCALE      = (1 << 10),
} sdf_header_item_kind_t;

typedef enum {
   S_SIGNAL_SCALAR,
   S_SIGNAL_BUS
} sdf_signal_kind_t;

typedef enum {
   S_DELAY_KIND_IOPATH,
   S_DELAY_KIND_PORT,
   S_DELAY_KIND_INTERCONNECT,
   S_DELAY_KIND_NETDELAY,
   S_DELAY_KIND_DEVICE,
   S_DELAY_KIND_PATHPULSE,
   S_DELAY_KIND_PATHPULSEP
} sdf_delay_kind_t;

typedef enum {
   S_CONSTR_KIND_PATH,
   S_CONSTR_KIND_PERIOD,
   S_CONSTR_KIND_SUM,
   S_CONSTR_KIND_DIFF,
   S_CONSTR_KIND_SKEW
} sdf_constraint_kind_t;

typedef enum {
   S_TENV_KIND_ARRIVAL,
   S_TENV_KIND_DEPARTURE,
   S_TENV_KIND_SLACK,
   S_TENV_KIND_WAVEFORM
} sdf_tenv_kind_t;

typedef enum {
   S_F_POSEDGE             = (1 << 0),
   S_F_NEGEDGE             = (1 << 1),
   S_F_VALUE_ABSOLUTE      = (1 << 2),
   S_F_VALUE_INCREMENT     = (1 << 3),
   S_F_MIN_VALUES          = (1 << 4),
   S_F_TYP_VALUES          = (1 << 5),
   S_F_MAX_VALUES          = (1 << 6)
} sdf_flags_t;

#define S_F_MIN_MAX_SPEC_ALL (S_F_MIN_VALUES | S_F_TYP_VALUES | S_F_MAX_VALUES)
#define S_F_DELTYPE_ALL (S_F_DELTYPE_ABSOLUTE  | S_F_DELTYPE_INCREMENT)

typedef enum {
   S_NUMBER_INTEGER,
   S_NUMBER_DOUBLE
} sdf_number_kind_t;

typedef enum {
   S_TCHECK_SETUP,
   S_TCHECK_HOLD,
   S_TCHECK_SETUPHOLD,
   S_TCHECK_RECOVERY,
   S_TCHECK_REMOVAL,
   S_TCHECK_RECREM,
   S_TCHECK_SKEW,
   S_TCHECK_BIDIRSKEW,
   S_TCHECK_WIDTH,
   S_TCHECK_PERIOD,
   S_TCHECK_NOCHANGE
} sdf_tcheck_kind_t;

typedef enum {
   S_UNARY_EXPR_PLUS,
   S_UNARY_EXPR_MINUS,
   S_UNARY_EXPR_LOGNOT,
   S_UNARY_EXPR_BITNOT,
   S_UNARY_EXPR_AND,
   S_UNARY_EXPR_NAND,
   S_UNARY_EXPR_OR,
   S_UNARY_EXPR_NOR,
   S_UNARY_EXPR_XOR,
   S_UNARY_EXPR_XNOR,

   S_UNARY_EXPR_NONE,
} sdf_unary_expr_kind_t;

typedef enum {
   S_BINARY_EXPR_PLUS,
   S_BINARY_EXPR_MINUS,
   S_BINARY_EXPR_MULT,
   S_BINARY_EXPR_DIV,
   S_BINARY_EXPR_MOD,
   S_BINARY_EXPR_LOGEQ,
   S_BINARY_EXPR_LOGNEQ,
   S_BINARY_EXPR_CASEEQ,
   S_BINARY_EXPR_CASENEQ,
   S_BINARY_EXPR_LOGAND,
   S_BINARY_EXPR_LOGOR,
   S_BINARY_EXPR_LT,
   S_BINARY_EXPR_LTEQ,
   S_BINARY_EXPR_GT,
   S_BINARY_EXPR_GTEQ,
   S_BINARY_EXPR_BITAND,
   S_BINARY_EXPR_BITOR,
   S_BINARY_EXPR_BITXOR,
   S_BINARY_EXPR_BITXNOR,
   S_BINARY_EXPR_SHRIGHT,
   S_BINARY_EXPR_SHLEFT,

   S_BINARY_EXPR_NONE
} sdf_binary_expr_kind_t;

typedef enum {
   S_COMPLEX_TERNARY,
   S_COMPLEX_CONCAT
} sdf_complex_kind_t;

typedef enum {
   S_COND_COND,
   S_COND_CONDELSE,
   S_COND_SCOND,
   S_COND_CCOND
} sdf_cond_kind_t;

typedef enum {
   S_DELAY_FILE,
   S_HEADER_ITEM,
   S_CELL,
   S_DELAY,
   S_TIMING_CHECK,
   S_LABEL,
   S_COND,
   S_SIGNAL,
   S_DELVAL,
   S_VALUE,
   S_TRIPPLE,
   S_NUMBER,
   S_CONSTRAINT,
   S_EXCEPTION,
   S_CONSTR_PATH,
   S_TIMING_ENV,
   S_UNARY,
   S_BINARY,
   S_COMPLEX,
   S_EDGE,
   S_RETAIN,

   S_LAST_NODE_KIND
} sdf_kind_t;

struct _sdf_file {
   // Top SDF node
   sdf_node_t  root;

   // SDF standard
   sdf_std_t   std;

   // Multiplier for each time unit to convert into fs.
   double      unit_mult;

   // Hierarchy separator
   char        hchar;
   char        hchar_other;

   // Hierarchy -> Node list map
   shash_t    *hier_map;

   // Cell name -> Node list map (for wildcards)
   shash_t    *name_map;

   // Mask of delays that are parsed:
   //    S_F_MIN_DELAYS, S_F_TYP_DELAYS, S_F_MAX_DELAYS
   sdf_flags_t min_max_spec;
};

sdf_node_t sdf_new(sdf_kind_t kind);
sdf_kind_t sdf_kind(sdf_node_t v);
const char *sdf_kind_str(sdf_kind_t kind);

const loc_t *sdf_loc(sdf_node_t s);
void sdf_set_loc(sdf_node_t s, const loc_t *loc);

void sdf_add_decl(sdf_node_t s, sdf_node_t decl);
unsigned sdf_decls(sdf_node_t s);
sdf_node_t sdf_decl(sdf_node_t s, unsigned int n);

void sdf_add_cell(sdf_node_t s, sdf_node_t cell);
unsigned sdf_cells(sdf_node_t s);
sdf_node_t sdf_cell(sdf_node_t s, unsigned int n);

void sdf_add_signal(sdf_node_t s, sdf_node_t port);
unsigned sdf_signals(sdf_node_t s);
sdf_node_t sdf_signal(sdf_node_t s, unsigned int n);

void sdf_add_value(sdf_node_t s, sdf_node_t literal);
unsigned sdf_values(sdf_node_t s);
sdf_node_t sdf_value(sdf_node_t s, unsigned int n);

void sdf_set_subkind(sdf_node_t s, unsigned int sub);
unsigned sdf_subkind(sdf_node_t s);

void sdf_set_flag(sdf_node_t s, sdf_flags_t flag);
sdf_flags_t sdf_flags(sdf_node_t s);

ident_t sdf_ident(sdf_node_t s);
void sdf_set_ident(sdf_node_t s, ident_t i);
bool sdf_has_ident(sdf_node_t s);

ident_t sdf_ident2(sdf_node_t s);
void sdf_set_ident2(sdf_node_t s, ident_t i);
bool sdf_has_ident2(sdf_node_t s);

void sdf_set_number(sdf_node_t s, sdf_node_t v);
sdf_node_t sdf_number(sdf_node_t s);
bool sdf_has_number(sdf_node_t s);

void sdf_set_ival(sdf_node_t s, int64_t i);
int64_t sdf_ival(sdf_node_t s);

void sdf_set_dval(sdf_node_t s, double d);
double sdf_dval(sdf_node_t s);

void sdf_set_min(sdf_node_t s, sdf_node_t min);
void sdf_set_typ(sdf_node_t s, sdf_node_t typ);
void sdf_set_max(sdf_node_t s, sdf_node_t max);

sdf_node_t sdf_min(sdf_node_t s);
sdf_node_t sdf_typ(sdf_node_t s);
sdf_node_t sdf_max(sdf_node_t s);

bool sdf_has_min(sdf_node_t s);
bool sdf_has_typ(sdf_node_t s);
bool sdf_has_max(sdf_node_t s);

sdf_node_t sdf_unit(sdf_node_t s);
void sdf_set_unit(sdf_node_t s, sdf_node_t u);

void sdf_add_delay(sdf_node_t s, sdf_node_t d);
unsigned sdf_delays(sdf_node_t s);
sdf_node_t sdf_delay(sdf_node_t s, unsigned int n);

void sdf_add_tcheck(sdf_node_t s, sdf_node_t tcheck);
unsigned sdf_tchecks(sdf_node_t s);
sdf_node_t sdf_tcheck(sdf_node_t s, unsigned int n);

void sdf_add_label(sdf_node_t s, sdf_node_t label);
unsigned sdf_labels(sdf_node_t s);
sdf_node_t sdf_label(sdf_node_t s, unsigned int n);

void sdf_add_dim(sdf_node_t s, sdf_node_t dim);
sdf_node_t sdf_dim(sdf_node_t s, unsigned n);
unsigned sdf_dims(sdf_node_t s);

void sdf_add_cond(sdf_node_t s, sdf_node_t c);
unsigned sdf_conds(sdf_node_t s);
sdf_node_t sdf_cond(sdf_node_t s, unsigned int n);

void sdf_set_expr(sdf_node_t s, sdf_node_t e);
bool sdf_has_expr(sdf_node_t s);
sdf_node_t sdf_expr(sdf_node_t s);

void sdf_add_tenv(sdf_node_t s, sdf_node_t c);
unsigned sdf_tenvs(sdf_node_t s);
sdf_node_t sdf_tenv(sdf_node_t s, unsigned int n);

void sdf_add_exception(sdf_node_t s, sdf_node_t e);
unsigned sdf_exceptions(sdf_node_t s);
sdf_node_t sdf_exception(sdf_node_t s, unsigned int n);

#endif  // _SDF_NODE_H
