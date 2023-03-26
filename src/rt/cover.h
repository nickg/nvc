//
//  Copyright (C) 2013-2021  Nick Gasson
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

#ifndef _COVER_H
#define _COVER_H

#include "util.h"
#include "tree.h"
#include "fbuf.h"
#include "diag.h"
#include "rt.h"

typedef struct _cover_tagging cover_tagging_t;

typedef enum {
   // Statement/Line coverage
   //
   // Each execution of a statement increments counter. If counter is bigger
   // than 0, statement is covered.
   TAG_STMT,

   // Branch / Decision coverage
   //
   // Each statement in which code flow diverges (if, case, while, when/else,
   // with/select) contains two flags. If each diverging path of the code
   // flow is taken/executed, a flag is set. If both flags are set, decision /
   // branch is 100% covered. If only one is taken it is 50% covered. If none,
   // decision / branch is not covered.
   TAG_BRANCH,

   // Toggle coverage
   //
   // Each logic signal contains two flags. Upon 0->1 transition, one flag is
   // set. Upon 1 -> 0 transition, the other flag is set. If both flags are set,
   // signal is 100% covered. Ports and internal signals are flagged.
   TAG_TOGGLE,

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
   TAG_EXPRESSION,

   // Tag used to represent hierarchy break in the linear sequence of tags.
   // Does not hold any coverage information
   TAG_HIER,

   // Last tag out of all tags, used to indicate no more tags are present in
   // coverage database.
   TAG_LAST
} tag_kind_t;

typedef struct _cover_tag {
   // Type of coverage
   tag_kind_t     kind;

   // Index of the tag of given kind.
   //    - 0 ... 2^31 - 1 - Valid tag entry, Index to run-time arrays with
   //            coverage data
   //    - -1 - Invalid tag
   int32_t        tag;

   // Coverage data:
   //    TAG_STMT       - Number of times statement was executed
   //    TAG_BRANCH     - Bit COV_FLAG_TRUE:        Branch evaluated to True
   //                   - Bit COV_FLAG_FALSE:       Branch evaluated to False
   //                   - Bit COV_FLAG_CHOICE:      Case/Select choice was selected
   //    TAG_TOOGLE     - Bit COV_FLAG_TOGGLE_TO_1: 0 -> 1 transition
   //                   - Bit COV_FLAG_TOGGLE_TO_0: 1 -> 0 transition
   //    TAG_EXPRESSION - Bit COV_FLAG_TRUE:        Expression evaluated to True
   //                     Bit COV_FLAG_FALSE:       Expression evaluated to False
   //                     Bit COV_FLAG_00:          LHS = 0/False and RHS = 0/False
   //                     Bit COV_FLAG_01:          LHS = 0/False and RHS = 1/True
   //                     Bit COV_FLAG_10:          LHS = 1/True  and RHS = 0/False
   //                     Bit COV_FLAG_11:          LHS = 1/True  and RHS = 1/True
   int32_t        data;

   // Flags for coverage tag
   int32_t        flags;

   // Exclude mask - Bit corresponding to a bin excludes it
   int32_t        excl_msk;

   // Unreachable mask - Bit corresponding to a bin indicates bin is un-reachable
   int32_t        unrc_msk;

   // Location in the source file
   loc_t          loc;

   // Hierarchy path of the covered object
   ident_t        hier;

   // Start position for signal name
   int            sig_pos;
} cover_tag_t;

typedef enum {
   COV_FLAG_TRUE           = (1 << 0),
   COV_FLAG_FALSE          = (1 << 1),
   COV_FLAG_CHOICE         = (1 << 2),
   COV_FLAG_00             = (1 << 3),
   COV_FLAG_01             = (1 << 4),
   COV_FLAG_10             = (1 << 5),
   COV_FLAG_11             = (1 << 6),
   COV_FLAG_HIER_UP        = (1 << 8),
   COV_FLAG_HIER_DOWN      = (1 << 9),
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
   COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED = (1 << 8),
   COVER_MASK_TOGGLE_COUNT_FROM_TO_Z      = (1 << 9),
   COVER_MASK_TOGGLE_INCLUDE_MEMS         = (1 << 10),
   COVER_MASK_EXCLUDE_UNREACHABLE         = (1 << 11),
   COVER_MASK_DONT_PRINT_COVERED          = (1 << 16),
   COVER_MASK_DONT_PRINT_UNCOVERED        = (1 << 17),
   COVER_MASK_DONT_PRINT_EXCLUDED         = (1 << 18)
} cover_mask_t;

#define COVER_MASK_ALL (COVER_MASK_STMT | COVER_MASK_BRANCH | COVER_MASK_TOGGLE | COVER_MASK_EXPRESSION)

cover_tagging_t *cover_tags_init(cover_mask_t mask, int array_limit);
bool cover_enabled(cover_tagging_t *tagging, cover_mask_t mask);

void cover_reset_scope(cover_tagging_t *tagging, ident_t hier);
void cover_push_scope(cover_tagging_t *tagging, tree_t t);
void cover_pop_scope(cover_tagging_t *tagging);
void cover_set_block_name(cover_tagging_t *tagging, ident_t name);

void cover_ignore_from_pragmas(cover_tagging_t *tagging, tree_t unit);
void cover_load_spec_file(cover_tagging_t *tagging, const char *path);

void cover_inc_array_depth(cover_tagging_t *tagging);
void cover_dec_array_depth(cover_tagging_t *tagging);

bool cover_is_stmt(tree_t t);
bool cover_skip_array_toggle(cover_tagging_t *tagging, int a_size);

unsigned cover_get_std_log_expr_flags(tree_t decl);

fbuf_t *cover_open_lib_file(tree_t top, fbuf_mode_t mode, bool check_null);

cover_tag_t *cover_add_tag(tree_t t, ident_t suffix, cover_tagging_t *ctx,
                           tag_kind_t kind, uint32_t flags);

void cover_load_exclude_file(const char *path, cover_tagging_t *tagging);
void cover_report(const char *path, cover_tagging_t *tagging, int item_limit);

void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_branches, int32_t *n_toggles,
                      int32_t *n_expressions);

void cover_dump_tags(cover_tagging_t *ctx, fbuf_t *f, cover_dump_t dt,
                     const int32_t *stmts, const int32_t *branches,
                     const int32_t *toggles, const int32_t *expressions);

cover_tagging_t *cover_read_tags(fbuf_t *f, uint32_t pre_mask);

void cover_merge_tags(fbuf_t *f, cover_tagging_t *tagging);

#endif  // _COVER_H
