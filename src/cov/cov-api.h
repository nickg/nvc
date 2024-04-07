//
//  Copyright (C) 2013-2023  Nick Gasson
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

#ifndef _COV_API_H
#define _COV_API_H

#include "prim.h"
#include "diag.h"
#include "fbuf.h"

typedef enum {
   // Statement/Line coverage
   //
   // Each execution of a statement increments counter. If counter is bigger
   // than 0, statement is covered.
   COV_ITEM_STMT,

   // Branch / Decision coverage
   //
   // Each statement in which code flow diverges (if, case, while, when/else,
   // with/select) contains two flags. If each diverging path of the code
   // flow is taken/executed, a flag is set. If both flags are set, decision /
   // branch is 100% covered. If only one is taken it is 50% covered. If none,
   // decision / branch is not covered.
   COV_ITEM_BRANCH,

   // Toggle coverage
   //
   // Each logic signal contains two flags. Upon 0->1 transition, one flag is
   // set. Upon 1 -> 0 transition, the other flag is set. If both flags are set,
   // signal is 100% covered. Ports and internal signals are flagged.
   COV_ITEM_TOGGLE,

   // Expression coverage
   //
   // Each logic operation: = /=, >, <, <=, >= , AND, OR, NOR, NAND, XOR, XNOR
   // contains flags:
   //    =, /=, <, >, <=, >= - Flags for result values: true, false
   //    AND   - Flags for operand values: 11, 01, 10.
   //    NAND  - Flags for operand values: 11, 10, 01
   //    OR    - Flags for operand values: 00, 01, 10.
   //    NOR   - Flags for operand values: 00, 01, 10
   //    XOR   - Flags for operand values: 01, 10, 11, 00
   //    XNOR  - Flags for operand values: 01, 10, 11, 00
   COV_ITEM_EXPRESSION,

   // FSM state coverage
   //
   // Signal of user defined enum type is tracked to see if all enum values were
   // reached.
   COV_ITEM_STATE,

   // Functional coverage
   //
   // Count of how many times the assertion passed.
   COV_ITEM_FUNCTIONAL,
} cover_item_kind_t;

typedef enum {
   COV_SRC_IF_CONDITION,
   COV_SRC_CASE_CHOICE,
   COV_SRC_LOOP_CONTROL,
   COV_SRC_ASSERT,
   COV_SRC_REPORT,
   COV_SRC_IF_STMT,
   COV_SRC_WAIT,
   COV_SRC_VAR_ASSIGN,
   COV_SRC_SIGNAL_ASSIGN,
   COV_SRC_LOOP_STMT,
   COV_SRC_CONDITION,
   COV_SRC_STATEMENT,
   COV_SRC_UNKNOWN,
} cover_src_t;

typedef struct _cover_item {
   // Type of coverage
   cover_item_kind_t kind;

   // Index of the tag of given kind.
   //    - 0 ... 2^31 - 1 - Valid tag entry, Index to run-time arrays with
   //            coverage data
   //    - -1 - Invalid tag
   int32_t           tag;

   // Coverage data:
   //    COV_ITEM_STMT        - Number of times statement was executed
   //    COV_ITEM_BRANCH      - Bit COV_FLAG_TRUE:        Branch evaluated to True
   //                           Bit COV_FLAG_FALSE:       Branch evaluated to False
   //                           Bit COV_FLAG_CHOICE:      Case/Select choice was selected
   //    COV_ITEM_TOOGLE      - Bit COV_FLAG_TOGGLE_TO_1: 0 -> 1 transition
   //                           Bit COV_FLAG_TOGGLE_TO_0: 1 -> 0 transition
   //    COV_ITEM_EXPRESSION  - Bit COV_FLAG_TRUE:        Expression evaluated to True
   //                           Bit COV_FLAG_FALSE:       Expression evaluated to False
   //                           Bit COV_FLAG_00:          LHS = 0/False and RHS = 0/False
   //                           Bit COV_FLAG_01:          LHS = 0/False and RHS = 1/True
   //                           Bit COV_FLAG_10:          LHS = 1/True  and RHS = 0/False
   //                           Bit COV_FLAG_11:          LHS = 1/True  and RHS = 1/True
   //    COV_ITEM_STATE       - Each bit corresponds to FSM state reached value.
   //                              N = (N_LITERALS-1) / 32 + 1
   //    COV_ITEM_FUNCTIONAL  - Count of passes
   int32_t           data;

   // Flags for coverage item
   int32_t           flags;

   // Exclude mask - Bit corresponding to a bin excludes it
   int32_t           excl_msk;

   // Unreachable mask - Bit corresponding to a bin indicates bin is un-reachable
   int32_t           unrc_msk;

   // Location of the item in the source file
   loc_t             loc;

   // Locations of LHS/RHS operands
   loc_t             loc_rhs;
   loc_t             loc_lhs;

   // Hierarchy path of the covered object
   ident_t           hier;

   // Name of the function for expression coverage
   ident_t           func_name;

   // Type of source statement or expression
   cover_src_t       source;

   // Numeric data related to the cover item:
   //    COV_ITEM_STATE  - Number of literals/states (N_LITERALS) in the enum/FSM.
   //    COV_ITEM_TOGGLE - Start position for signal name
   int               num;
} cover_item_t;

typedef enum {
   COV_FLAG_TRUE           = (1 << 0),
   COV_FLAG_FALSE          = (1 << 1),
   COV_FLAG_CHOICE         = (1 << 2),
   COV_FLAG_00             = (1 << 3),
   COV_FLAG_01             = (1 << 4),
   COV_FLAG_10             = (1 << 5),
   COV_FLAG_11             = (1 << 6),
   COV_FLAG_STATE          = (1 << 7),
   COV_FLAG_TOGGLE_TO_0    = (1 << 15),
   COV_FLAG_TOGGLE_TO_1    = (1 << 16),
   COV_FLAG_TOGGLE_SIGNAL  = (1 << 17),
   COV_FLAG_TOGGLE_PORT    = (1 << 18),
   COV_FLAG_CONST_DRIVEN   = (1 << 19),
   COV_FLAG_EXPR_STD_LOGIC = (1 << 24)
} cover_flags_t;

#define COVER_FLAGS_AND_EXPR (COV_FLAG_11 | COV_FLAG_10 | COV_FLAG_01)
#define COVER_FLAGS_OR_EXPR (COV_FLAG_00 | COV_FLAG_10 | COV_FLAG_01)
#define COVER_FLAGS_XOR_EXPR (COV_FLAG_11 | COV_FLAG_00 | COV_FLAG_10 | COV_FLAG_01)

#define COVER_FLAGS_LHS_RHS_BINS (COV_FLAG_11 | COV_FLAG_00 | COV_FLAG_10 | COV_FLAG_01)

#define COVER_FLAGS_ALL_BINS (COV_FLAG_TRUE | COV_FLAG_FALSE | COV_FLAG_CHOICE | \
                              COV_FLAG_00 | COV_FLAG_01 | COV_FLAG_10 | COV_FLAG_11 | \
                              COV_FLAG_TOGGLE_TO_0 | COV_FLAG_TOGGLE_TO_1)

typedef enum {
   COV_DUMP_ELAB,
   COV_DUMP_RUNTIME,
   COV_DUMP_PROCESSING
} cover_dump_t;

typedef enum {
   COVER_MASK_STMT                        = (1 << 0),
   COVER_MASK_BRANCH                      = (1 << 1),
   COVER_MASK_TOGGLE                      = (1 << 2),
   COVER_MASK_EXPRESSION                  = (1 << 3),
   COVER_MASK_STATE                       = (1 << 4),
   COVER_MASK_FUNCTIONAL                  = (1 << 5),
   COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED = (1 << 8),
   COVER_MASK_TOGGLE_COUNT_FROM_TO_Z      = (1 << 9),
   COVER_MASK_TOGGLE_INCLUDE_MEMS         = (1 << 10),
   COVER_MASK_EXCLUDE_UNREACHABLE         = (1 << 11),
   COVER_MASK_FSM_NO_DEFAULT_ENUMS        = (1 << 12),
   COVER_MASK_DONT_PRINT_COVERED          = (1 << 16),
   COVER_MASK_DONT_PRINT_UNCOVERED        = (1 << 17),
   COVER_MASK_DONT_PRINT_EXCLUDED         = (1 << 18)
} cover_mask_t;

#define COVER_MASK_ALL (COVER_MASK_STMT | COVER_MASK_BRANCH             \
                        | COVER_MASK_TOGGLE | COVER_MASK_EXPRESSION     \
                        | COVER_MASK_STATE | COVER_MASK_FUNCTIONAL)

cover_data_t *cover_data_init(cover_mask_t mask, int array_limit);
bool cover_enabled(cover_data_t *data, cover_mask_t mask);

unsigned cover_count_items(cover_data_t *data);

void cover_dump_items(cover_data_t *data, fbuf_t *f, cover_dump_t dt,
                      const int32_t *counts);
cover_data_t *cover_read_items(fbuf_t *f, uint32_t pre_mask);

fbuf_t *cover_open_lib_file(tree_t top, fbuf_mode_t mode, bool check_null);

void cover_merge_items(fbuf_t *f, cover_data_t *data);

//
// Spec and exclude file handling
//

void cover_ignore_from_pragmas(cover_data_t *data, tree_t unit);
void cover_load_spec_file(cover_data_t *data, const char *path);
void cover_load_exclude_file(const char *path, cover_data_t *data);

//
// Report generation and export
//

void cover_report(const char *path, cover_data_t *data, int item_limit);
void cover_export_cobertura(cover_data_t *data, FILE *f,
                            const char *relative);

//
// Interface to code generator
//

void cover_push_scope(cover_data_t *data, tree_t t);
void cover_pop_scope(cover_data_t *data);

bool cover_is_stmt(tree_t t);
bool cover_skip_type_state(cover_data_t *data, type_t type);

unsigned cover_get_std_log_expr_flags(tree_t decl);

cover_item_t *cover_add_items(cover_data_t *data, object_t *obj,
                              cover_item_kind_t kind);

// XXX: remove
cover_item_t *cover_add_item(cover_data_t *data, object_t *obj, ident_t suffix,
                             cover_item_kind_t kind, uint32_t flags);

#endif   // _COV_API_H
