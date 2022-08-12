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

   // TODO: Expression coverage

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
   //    TAG_STMT    - Counter of hits
   //    TAG_BRANCH  - Bit 0 - Evaluated to True
   //                - Bit 1 - Evaluated to False
   //    TAG_TOOGLE  - Bit 0 - 0 -> 1 transition
   //                - Bit 1 - 1 -> 0 transition
   int32_t        data;

   // Flags for coverage tag
   int32_t        flags;

   // Location in the source file
   loc_t          loc;
   
   // Hierarchy path of the covered object
   ident_t        hier;
} cover_tag_t;

typedef enum {
   COV_FLAG_HAS_TRUE    = (1 << 0),
   COV_FLAG_HAS_FALSE   = (1 << 1),
   COV_FLAG_HIER_UP     = (1 << 8),
   COV_FLAG_HIER_DOWN   = (1 << 9),
   COV_FLAG_TOGGLE_TO_1 = (1 << 16),
   COV_FLAG_TOGGLE_TO_0 = (1 << 15),
} cover_flags_t;

typedef enum {
   COV_DUMP_ELAB,
   COV_DUMP_RUNTIME,
   COV_DUMP_PROCESSING
} cover_dump_t;


cover_tagging_t *cover_tags_init();

bool cover_is_stmt(tree_t t);

fbuf_t *cover_open_lib_file(tree_t top, fbuf_mode_t mode, bool check_null);

void cover_toggle_event_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                           void *user);

cover_tag_t *cover_add_tag(tree_t t, ident_t hier, cover_tagging_t *ctx,
                           tag_kind_t kind, uint32_t flags);

void cover_report(const char *path, cover_tagging_t *tagging);

void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_branches, int32_t *n_toggles);

void cover_dump_tags(cover_tagging_t *ctx, fbuf_t *f, cover_dump_t dt,
                     const int32_t *stmts, const int32_t *branches,
                     const int32_t *toggles);


cover_tagging_t *cover_read_tags(fbuf_t *f);

void cover_merge_tags(fbuf_t *f, cover_tagging_t *tagging);

#endif  // _COVER_H
