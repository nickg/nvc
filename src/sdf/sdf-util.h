//
//  Copyright (C) 2024 Nick Gasson
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

#ifndef _SDF_UTIL_H
#define _SDF_UTIL_H

#include "prim.h"

//
// SDF standard revisions
//
typedef enum {
   SDF_STD_1_0,
   SDF_STD_2_0,
   SDF_STD_2_1,
   SDF_STD_3_0,
   SDF_STD_4_0
} sdf_std_t;

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
   S_COND_COND,
   S_COND_CONDELSE,
   S_COND_SCOND,
   S_COND_CCOND
} sdf_cond_kind_t;

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

struct _sdf_file {
   // SDF standard
   sdf_std_t   std;

   // Multiplier for each time unit to convert into fs.
   double      unit_mult;

   // Hierarchy separator
   char        hchar;
   char        hchar_other;

   // hier_map: Hierarchy -> Cell map
   // name_map: Cell name -> Cell map (for wildcards)
   // Each cell is placed in only one of these two hash tables.
   // Duplicities are prevented by looking up cells in the hash tables before
   // adding the cell. Thus cells with equal:
   //    - hierarchy (INSTANCE [hierarchy])
   //    - cell name and wildcard (CELLNAME, INSTANCE *)
   // are merged into single S_CELL object.
   // TODO: Resolve location tracking. SDF standard says that information from
   //       SDF file should be annotated in order it is present in the SDF file.
   //       If there is a cell annotated due to cell_name and instance wildcard,
   //       and also annotated by cell with hierarchy entry, then it is currently
   //       not possible to figure out which one to apply earlier. This is because
   //       loc_t is invalid since it counts only until 1M lines! Maybe somehow
   //       tracking only line number ?
   hash_t     *hier_map;
   hash_t     *name_map;

   // Mask of delays that are parsed:
   //    S_F_MIN_DELAYS, S_F_TYP_DELAYS, S_F_MAX_DELAYS
   sdf_flags_t min_max_spec;
};

sdf_file_t *sdf_file_new(int exp_hier_cells, int exp_wild_cells);
void sdf_file_free(sdf_file_t *sdf);

#endif  // _SDF_UTIL_H
