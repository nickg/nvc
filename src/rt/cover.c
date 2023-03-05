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

#include "util.h"
#include "array.h"
#include "common.h"
#include "cover.h"
#include "lib.h"
#include "option.h"
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/structs.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>

//#define COVER_DEBUG_EMIT
//#define COVER_DEBUG_DUMP
//#define COVER_DEBUG_SCOPE
//#define COVER_DEBUG_MERGE
//#define COVER_DEBUG_CALLBACK
//#define COVER_DEBUG_EXCLUDE

#define MARGIN_LEFT "20%%"
#define SIDEBAR_WIDTH "15%%"

typedef struct {
   int start;
   int end;
} line_range_t;

typedef A(cover_tag_t) tag_array_t;
typedef A(line_range_t) range_array_t;
typedef A(text_buf_t*) tb_array_t;

typedef struct _cover_report_ctx    cover_report_ctx_t;
typedef struct _cover_file          cover_file_t;
typedef struct _cover_scope         cover_scope_t;
typedef struct _cover_exclude_ctx   cover_exclude_ctx_t;
typedef struct _cover_rpt_buf       cover_rpt_buf_t;
typedef struct _cover_spec          cover_spec_t;

typedef struct _cover_scope {
   ident_t        name;
   int            branch_label;
   int            stmt_label;
   int            expression_label;
   cover_scope_t *parent;
   range_array_t  ignore_lines;
   ident_t        block_name;
   int            sig_pos;
   bool           emit;
} cover_scope_t;

struct _cover_rpt_buf {
   text_buf_t      *tb;
   cover_rpt_buf_t *prev;
};

struct _cover_spec {
   tb_array_t hier_include;
   tb_array_t hier_exclude;
   tb_array_t block_include;
   tb_array_t block_exclude;
};

struct _cover_tagging {
   int               next_stmt_tag;
   int               next_branch_tag;
   int               next_toggle_tag;
   int               next_expression_tag;
   int               next_hier_tag;
   ident_t           hier;
   tag_array_t       tags;
   cover_mask_t      mask;
   int               array_limit;
   int               array_depth;
   int               report_item_limit;
   cover_rpt_buf_t  *rpt_buf;
   cover_spec_t     *spec;
   cover_scope_t    *top_scope;
};

typedef struct {
   unsigned    total_stmts;
   unsigned    hit_stmts;
   unsigned    total_branches;
   unsigned    hit_branches;
   unsigned    total_toggles;
   unsigned    hit_toggles;
   unsigned    total_expressions;
   unsigned    hit_expressions;
} cover_stats_t;

typedef struct {
   char       *text;
   size_t      len;
} cover_line_t;

typedef struct {
   cover_line_t *line;
   cover_tag_t *tag;
   int flags;
} cover_pair_t;

typedef struct {
   cover_pair_t *hits;
   cover_pair_t *miss;
   cover_pair_t *excl;
   int          n_hits;
   int          n_miss;
   int          n_excl;
   int          alloc_hits;
   int          alloc_miss;
   int          alloc_excl;
} cover_chain_t;

struct _cover_file {
   const char   *name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
   cover_file_t *next;
};

struct _cover_report_ctx {
   cover_tagging_t      *tagging;
   cover_stats_t        flat_stats;
   cover_stats_t        nested_stats;
   cover_report_ctx_t   *parent;
   cover_tag_t          *start_tag;
   cover_chain_t        ch_stmt;
   cover_chain_t        ch_branch;
   cover_chain_t        ch_toggle;
   cover_chain_t        ch_expression;
   int                  lvl;
};

static cover_file_t  *files;

struct _cover_exclude_ctx {
   FILE    *ef;
   char    *line;
   loc_t    loc;
};

static const char* tag_kind_str[] = {
   "statement",
   "branch",
   "toggle",
   "expression",
   "hierarchy",
   "last"
};

static const struct {
   const char *name;
   uint32_t   flag;
} bin_map[] = {
   { "BIN_TRUE",       COV_FLAG_TRUE},
   { "BIN_FALSE",      COV_FLAG_FALSE},
   { "BIN_CHOICE",     COV_FLAG_CHOICE},
   { "BIN_0_0",        COV_FLAG_00},
   { "BIN_0_1",        COV_FLAG_01},
   { "BIN_1_0",        COV_FLAG_10},
   { "BIN_1_1",        COV_FLAG_11},
   { "BIN_0_TO_1",     COV_FLAG_TOGGLE_TO_1},
   { "BIN_1_TO_0",     COV_FLAG_TOGGLE_TO_0},
};

enum std_ulogic {
   _U  = 0x0,
   _X  = 0x1,
   _0  = 0x2,
   _1  = 0x3,
   _Z  = 0x4,
   _W  = 0x5,
   _L  = 0x6,
   _H  = 0x7,
   _DC = 0x8
};

typedef enum {
   PAIR_UNCOVERED    = 0,
   PAIR_EXCLUDED     = 1,
   PAIR_COVERED      = 2,
   PAIR_LAST         = 3
} cov_pair_kind_t;

bool cover_is_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_IF:
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
   case T_RETURN:
   case T_FOR:
   case T_PCALL:
   case T_FCALL:
   case T_CASE:
      return true;

   // Static waits are introduced during simp pass. These are hidden
   // for user, no need to cover them.
   case T_WAIT:
      if (tree_flags(t) & TREE_F_STATIC_WAIT)
         return false;
      return true;

   default:
      return false;
   }
}

static bool cover_is_branch(tree_t branch)
{
   return tree_kind(branch) == T_ASSOC || tree_kind(branch) == T_COND;
}

static bool cover_is_toggle_first(tree_t decl)
{
   const tree_kind_t kind = tree_kind(decl);
   return kind == T_SIGNAL_DECL || kind == T_PORT_DECL;
}

unsigned cover_get_std_log_expr_flags(tree_t decl)
{
   assert(tree_kind(decl) == T_FUNC_DECL);

   struct {
      well_known_t op;
      unsigned flags;
   } std_log_ops[] = {
      { W_IEEE_1164_AND    , COVER_FLAGS_AND_EXPR},
      { W_IEEE_1164_NAND   , COVER_FLAGS_AND_EXPR},
      { W_IEEE_1164_OR     , COVER_FLAGS_OR_EXPR},
      { W_IEEE_1164_NOR    , COVER_FLAGS_OR_EXPR},
      { W_IEEE_1164_XOR    , COVER_FLAGS_XOR_EXPR},
      { W_IEEE_1164_XNOR   , COVER_FLAGS_XOR_EXPR}
   };

   unsigned flags = 0;
   for (int i = 0; i < ARRAY_LEN(std_log_ops); i++)
      if (ident_starts_with(tree_ident2(decl), well_known(std_log_ops[i].op)))
         flags |= std_log_ops[i].flags;

   return flags;
}

bool cover_skip_array_toggle(cover_tagging_t *tagging, int a_size)
{
   assert (tagging);

   // Array is equal to or than configured limit
   if (tagging->array_limit != 0 && a_size >= tagging->array_limit)
      return true;

   // Array is multi-dimensional or nested
   if ((!cover_enabled(tagging, COVER_MASK_TOGGLE_INCLUDE_MEMS)) &&
       tagging->array_depth > 0)
      return true;

   return false;
}

fbuf_t *cover_open_lib_file(tree_t top, fbuf_mode_t mode, bool check_null)
{
   char *dbname LOCAL = xasprintf("_%s.covdb", istr(tree_ident(top)));
   fbuf_t *f = lib_fbuf_open(lib_work(), dbname, mode, FBUF_CS_NONE);

   if (check_null && (f == NULL))
      fatal_errno("failed to open coverage db file: %s", dbname);

   return f;
}

cover_tag_t *cover_add_tag(tree_t t, ident_t suffix, cover_tagging_t *ctx,
                           tag_kind_t kind, uint32_t flags)
{
   int *cnt;
   assert (ctx != NULL);

   if (!ctx->top_scope->emit && kind != TAG_HIER)
      return NULL;

   // TODO: need a better way to determine if a scope comes from an instance
   cover_scope_t *ignore_scope = ctx->top_scope;
   for (; ignore_scope->ignore_lines.count == 0 && ignore_scope->parent;
        ignore_scope = ignore_scope->parent)
      ;

   const loc_t *loc = tree_loc(t);
   for (int i = 0; i < ignore_scope->ignore_lines.count; i++) {
      line_range_t *lr = &(ignore_scope->ignore_lines.items[i]);
      if (loc->first_line > lr->start && loc->first_line <= lr->end)
         return NULL;
   }

   switch (kind) {
      case TAG_STMT:       cnt = &(ctx->next_stmt_tag);        break;
      case TAG_BRANCH:     cnt = &(ctx->next_branch_tag);      break;
      case TAG_TOGGLE:     cnt = &(ctx->next_toggle_tag);      break;
      case TAG_EXPRESSION: cnt = &(ctx->next_expression_tag);  break;
      case TAG_HIER:       cnt = &(ctx->next_hier_tag);        break;
      default:
         fatal("unknown coverage type: %d", kind);
   }
   assert (cnt != NULL);

   // Everything creates scope, so name of current tag is already given
   // by scope in hierarchy.
   ident_t hier = ctx->hier;
   if (suffix)
      hier = ident_prefix(hier, suffix, '\0');

   // Expression tags do not nest scope, expression name must be created
   if (kind == TAG_EXPRESSION) {
      char buf[16];
      checked_sprintf(buf, sizeof(buf), "_E%d", ctx->top_scope->expression_label);
      hier = ident_prefix(hier, ident_new(buf), '.');
      ctx->top_scope->expression_label++;
   }

#ifdef COVER_DEBUG_EMIT
   printf("Tag: %s\n", istr(hier));
   printf("    First line: %d\n", tree_loc(t)->first_line);
   printf("    First column: %d\n", tree_loc(t)->first_column);
   printf("    Line delta: %d\n", tree_loc(t)->line_delta);
   printf("    Column delta: %d\n", tree_loc(t)->column_delta);
   printf("\n\n");
#endif

   cover_tag_t new = {
      .kind       = kind,
      .tag        = *cnt,
      .data       = 0,
      .flags      = flags,
      .excl_msk   = 0,
      .loc        = *tree_loc(t),
      .hier       = hier,
      .sig_pos    = ctx->top_scope->sig_pos,
   };

   APUSH(ctx->tags, new);
   (*cnt)++;

   return AREF(ctx->tags, ctx->tags.count - 1);
}

void cover_dump_tags(cover_tagging_t *ctx, fbuf_t *f, cover_dump_t dt,
                     const int32_t *stmts, const int32_t *branches,
                     const int32_t *toggles, const int32_t *expressions)
{

#ifdef COVER_DEBUG_DUMP
   printf("Dumping coverage entries:\n");
   printf("Number of statement tags: %d\n", ctx->next_stmt_tag);
   printf("Number of branch tags: %d\n", ctx->next_branch_tag);
   printf("Number of toggle tags: %d\n", ctx->next_toggle_tag);
   printf("Number of expression tags: %d\n", ctx->next_expression_tag);
   printf("Number of hierarchy tags: %d\n", ctx->next_hier_tag);
   printf("Total tag count: %d\n", ctx->tags.count);
#endif

   write_u32(ctx->mask, f);
   write_u32(ctx->array_limit, f);
   write_u32(ctx->next_stmt_tag, f);
   write_u32(ctx->next_branch_tag, f);
   write_u32(ctx->next_toggle_tag, f);
   write_u32(ctx->next_expression_tag, f);
   write_u32(ctx->next_hier_tag, f);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   for (int i = 0; i < ctx->tags.count; i++) {
      cover_tag_t *tag = &(ctx->tags.items[i]);

      write_u8(tag->kind, f);
      write_u32(tag->tag, f);

      if (dt == COV_DUMP_RUNTIME) {
         const int32_t *cnts = NULL;
         if (tag->kind == TAG_STMT)
            cnts = stmts;
         else if (tag->kind == TAG_BRANCH)
            cnts = branches;
         else if (tag->kind == TAG_TOGGLE)
            cnts = toggles;
         else if (tag->kind == TAG_EXPRESSION)
            cnts = expressions;

         int32_t data = (cnts) ? cnts[tag->tag] : 0;
         write_u32(data, f);

#ifdef COVER_DEBUG_DUMP
         printf("Index: %4d Tag: %s Kind: %d  Data: %d\n", tag->tag,
                istr(tag->hier), tag->kind, data);
#endif

      }
      else {
         write_u32(tag->data, f);
#ifdef COVER_DEBUG_DUMP
         printf("Index: %4d Tag: %s Kind: %d  Data: %d\n", tag->tag,
                istr(tag->hier), tag->kind, tag->data);
#endif
      }
      write_u32(tag->flags, f);
      write_u32(tag->excl_msk, f);
      loc_write(&(tag->loc), loc_wr);
      ident_write(tag->hier, ident_ctx);
      write_u32(tag->sig_pos, f);
   }

   write_u8(TAG_LAST, f);

   loc_write_end(loc_wr);
   ident_write_end(ident_ctx);
}

cover_tagging_t *cover_tags_init(cover_mask_t mask, int array_limit)
{
   cover_tagging_t *ctx = xcalloc(sizeof(cover_tagging_t));
   ctx->mask = mask;
   ctx->array_limit = array_limit;

   return ctx;
}

bool cover_enabled(cover_tagging_t *tagging, cover_mask_t mask)
{
   return tagging != NULL && (tagging->mask & mask);
}

void cover_reset_scope(cover_tagging_t *tagging, ident_t hier)
{
   if (tagging == NULL)
      return;

   assert(tagging->top_scope == NULL);

   cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
   s->name = hier;

   tagging->top_scope = s;
   tagging->hier = hier;
}

static bool cover_should_emit_scope(cover_tagging_t *tagging, tree_t t)
{
   cover_scope_t *ts = tagging->top_scope;
   cover_spec_t *spc = tagging->spec;

   // Block (entity, package instance or block) name
   if (ts->block_name) {
      for (int i = 0; i < spc->block_exclude.count; i++)

         if (ident_glob(ts->block_name, tb_get(AGET(spc->block_exclude, i)), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: False, block (Block: %s, Pattern: %s)\n",
                   istr(ts->block_name), tb_get(AGET(spc->block_exclude, i)));
#endif
            return false;
         }

      for (int i = 0; i < tagging->spec->block_include.count; i++)
         if (ident_glob(ts->block_name, tb_get(AGET(spc->block_include, i)), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: True, block (Block: %s, Pattern: %s)\n",
                   istr(ts->block_name), tb_get(AGET(spc->block_include, i)));
#endif
            return true;
         }
   }

   // Hierarchy
   for (int i = 0; i < spc->hier_exclude.count; i++)
      if (ident_glob(tagging->hier, tb_get(AGET(spc->hier_exclude, i)), -1)) {
#ifdef COVER_DEBUG_EMIT
         printf("Cover emit: False, hierarchy (Hierarchy: %s, Pattern: %s)\n",
                   istr(tagging->hier), tb_get(AGET(spc->hier_exclude, i)));
#endif
         return false;
      }

   for (int i = 0; i < spc->hier_include.count; i++)
      if (ident_glob(tagging->hier, tb_get(AGET(spc->hier_include, i)), -1)) {
#ifdef COVER_DEBUG_EMIT
         printf("Cover emit: True, hierarchy (Hierarchy: %s, Pattern: %s)\n",
                   istr(tagging->hier), tb_get(AGET(spc->hier_include, i)));
#endif
         return true;
      }

   return false;
}

void cover_push_scope(cover_tagging_t *tagging, tree_t t)
{
   if (tagging == NULL || t == NULL)
      return;

   cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
   ident_t name = NULL;
   char prefix[16] = {0};
   int *cnt;
   char c = 0;

   if (cover_is_branch(t)) {
      if (tree_kind(t) == T_ASSOC && tree_subkind(t) == A_OTHERS)
         checked_sprintf(prefix, sizeof(prefix), "_B_OTHERS");
      else {
         assert(tagging->top_scope);
         cnt = &tagging->top_scope->branch_label;
         c = 'B';
      }
   }
   // For toggle coverage, remember the position where its name in
   // the hierarchy starts.
   else if (cover_is_toggle_first(t)) {
      assert(tree_has_ident(t));
      name = tree_ident(t);
      s->sig_pos = ident_len(tagging->hier) + 1;
   }
   // Consider everything else as statement
   // Expressions do not get scope pushed, so if scope for e.g.
   // T_FCALL is pushed it will be concurent function call -> Label as statement
   else {
      if (tree_has_ident(t))
         name = tree_ident(t);
      else {
         assert(tagging->top_scope);
         cnt = &tagging->top_scope->stmt_label;
         c = 'S';
      }
   }

   if (c) {
      checked_sprintf(prefix, sizeof(prefix), "_%c%u", c, *cnt);
      (*cnt)++;
   }
   if (name == NULL)
      name = ident_new(prefix);

   s->name = name;
   s->block_name = tagging->top_scope->block_name;
   s->parent = tagging->top_scope;
   if (s->sig_pos == 0)
      s->sig_pos = tagging->top_scope->sig_pos;

   tagging->top_scope = s;
   tagging->hier = ident_prefix(tagging->hier, name, '.');

   s->emit = (tagging->spec && cover_should_emit_scope(tagging, t));

#ifdef COVER_DEBUG_SCOPE
   printf("Pushing cover scope: %s\n", istr(tagging->hier));
   printf("Tree_kind: %s\n\n", tree_kind_str(tree_kind(t)));
   printf("Coverage emit: %d\n\n", s->emit);
#endif
}

void cover_pop_scope(cover_tagging_t *tagging)
{
   if (tagging == NULL)
      return;

   assert(tagging->top_scope != NULL);

   ACLEAR(tagging->top_scope->ignore_lines);

   cover_scope_t *tmp = tagging->top_scope->parent;
   free(tagging->top_scope);
   tagging->top_scope = tmp;

#ifdef COVER_DEBUG_SCOPE
   printf("Popping cover scope: %s\n", istr(tagging->hier));
#endif

   tagging->hier = ident_runtil(tagging->hier, '.');
   assert(tagging->hier != NULL);
}

void cover_set_block_name(cover_tagging_t *tagging, ident_t name)
{
   assert(tagging != NULL);
   tagging->top_scope->block_name = name;
}

void cover_ignore_from_pragmas(cover_tagging_t *tagging, tree_t unit)
{
   assert(tagging->top_scope != NULL);

   if (!is_design_unit(unit))
      return;   // Generate block, etc.

   range_array_t *excl = &(tagging->top_scope->ignore_lines);
   bool state = true;
   const int npragmas = tree_pragmas(unit);
   for (int i = 0; i < npragmas; i++) {
      tree_t p = tree_pragma(unit, i);
      const pragma_kind_t kind = tree_subkind(p);
      if (kind != PRAGMA_COVERAGE_OFF && kind != PRAGMA_COVERAGE_ON)
         continue;

      if (kind == PRAGMA_COVERAGE_OFF && state) {
         line_range_t lr = { tree_loc(p)->first_line, INT_MAX };
         APUSH(*excl, lr);
         state = false;
      }
      else if (kind == PRAGMA_COVERAGE_ON && !state) {
         assert(excl->count > 0);
         assert(excl->items[excl->count - 1].end == INT_MAX);
         excl->items[excl->count - 1].end = tree_loc(p)->first_line;
         state = true;
      }
   }
}

void cover_inc_array_depth(cover_tagging_t *tagging)
{
   assert(tagging != NULL);
   tagging->array_depth++;
#ifdef COVER_DEBUG_SCOPE
   printf("Adding dimension: %d\n", tagging->array_depth);
#endif
}

void cover_dec_array_depth(cover_tagging_t *tagging)
{
   assert(tagging != NULL);
   assert(tagging->array_depth > 0);
   tagging->array_depth--;
#ifdef COVER_DEBUG_SCOPE
   printf("Subtracting dimension: %d\n", tagging->array_depth);
#endif
}

static void cover_read_header(fbuf_t *f, cover_tagging_t *tagging)
{
   assert(tagging != NULL);

   tagging->mask = read_u32(f);
   tagging->array_limit = read_u32(f);
   tagging->next_stmt_tag = read_u32(f);
   tagging->next_branch_tag = read_u32(f);
   tagging->next_toggle_tag = read_u32(f);
   tagging->next_expression_tag = read_u32(f);
   tagging->next_hier_tag = read_u32(f);
}

void cover_read_one_tag(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                        ident_rd_ctx_t ident_ctx, cover_tag_t *tag)
{
   tag->kind = read_u8(f);
   if (tag->kind == TAG_LAST)
      return;

   tag->tag = read_u32(f);
   tag->data = read_u32(f);
   tag->flags = read_u32(f);
   tag->excl_msk = read_u32(f);

   loc_read(&(tag->loc), loc_rd);
   tag->hier = ident_read(ident_ctx);
   tag->sig_pos = read_u32(f);
}

cover_tagging_t *cover_read_tags(fbuf_t *f, uint32_t pre_mask)
{
   cover_tagging_t *tagging = xcalloc(sizeof(cover_tagging_t));
   cover_read_header(f, tagging);
   tagging->mask |= pre_mask;

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   for (;;) {
      cover_tag_t new;
      cover_read_one_tag(f, loc_rd, ident_ctx, &new);

      if (new.kind == TAG_LAST)
         break;

      APUSH(tagging->tags, new);
   }

   loc_read_end(loc_rd);
   return tagging;
}

void cover_merge_tags(fbuf_t *f, cover_tagging_t *tagging)
{
   assert (tagging != NULL);

   cover_read_header(f, tagging);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   for (;;) {
      cover_tag_t new;
      cover_read_one_tag(f, loc_rd, ident_ctx, &new);

      if (new.kind == TAG_LAST)
         break;

      // TODO: Could merging be done more efficiently?
      bool found = false;
      for (int i = 0; i < tagging->tags.count; i++) {
         cover_tag_t *old = AREF(tagging->tags, i);

         // Compare based on hierarchical path, each
         // statement / branch / signal has unique hierarchical name
         if (new.hier == old->hier) {
            assert(new.kind == old->kind);
#ifdef COVER_DEBUG_MERGE
            printf("Merging coverage tag: %s\n", istr(old->hier));
#endif
            switch (new.kind) {
            case TAG_STMT:
               old->data += new.data;
               break;
            case TAG_TOGGLE:
            case TAG_BRANCH:
            case TAG_EXPRESSION:
               old->data |= new.data;
               break;
            default:
               break;
            }

            found = true;
            break;
         }
      }

      // TODO: Append the new tag just before popping hierarchy tag
      //       with longest common prefix of new tag. That will allow to
      //       merge coverage of IPs from different configurations of
      //       generics which form hierarchy differently!
      if (!found)
         warnf("Dropping coverage tag: %s\n", istr(new.hier));
   }
}

void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_branches, int32_t *n_toggles,
                      int32_t *n_expressions)
{
   if (tagging == NULL) {
      *n_stmts = 0;
      *n_branches = 0;
      *n_toggles = 0;
      *n_expressions = 0;
   }
   else {
      *n_stmts = tagging->next_stmt_tag;
      *n_branches = tagging->next_branch_tag;
      *n_toggles = tagging->next_toggle_tag;
      *n_expressions = tagging->next_expression_tag;
   }
}

void cover_load_spec_file(cover_tagging_t *tagging, const char *path)
{
   cover_exclude_ctx_t ctx = {};

   ctx.ef = fopen(path, "r");
   if (ctx.ef == NULL)
      fatal_errno("failed to open coverage specification file: %s\n", path);

   tagging->spec = xcalloc(sizeof(cover_spec_t));

   const char *delim = " ";
   ssize_t read;
   loc_file_ref_t file_ref = loc_file_ref(path, NULL);
   int line_num = 1;
   size_t line_len;

   while ((read = getline(&(ctx.line), &line_len, ctx.ef)) != -1) {

      // Strip comments and newline
      char *p = ctx.line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      ctx.loc = get_loc(line_num, 0, line_num, p - ctx.line - 1, file_ref);

      // Parse line as space separated tokens
      for (char *tok = strtok(ctx.line, delim); tok; tok = strtok(NULL, delim)) {
         bool include;
         if (tok[0] == '+')
            include = true;
         else if (tok[0] == '-')
            include = false;
         else
            fatal_at(&ctx.loc, "Coverage specification command must "
                               "start with '+' or '-");
         tok++;

         tb_array_t *arr;
         if (!strcmp(tok, "hierarchy")) {
            if (include)
               arr = &(tagging->spec->hier_include);
            else
               arr = &(tagging->spec->hier_exclude);
         }
         else if (!strcmp(tok, "block")) {
            if (include)
               arr = &(tagging->spec->block_include);
            else
               arr = &(tagging->spec->block_exclude);
         }
         else
            fatal_at(&ctx.loc, "invalid command: $bold$%s$$", tok);

         char *hier = strtok(NULL, delim);
         if (!hier)
            fatal_at(&ctx.loc, "%s name missing", tok);

         text_buf_t *tb = tb_new();
         tb_printf(tb, "%s", hier);
         tb_upcase(tb);
         APUSH(*arr, tb);

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ctx.ef);
}


///////////////////////////////////////////////////////////////////////////////
// Runtime handling
///////////////////////////////////////////////////////////////////////////////

static inline void cover_toggle_check_0_1(uint8_t old, uint8_t new,
                                          int32_t *toggle_mask)
{
   if (old == _0 && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _1 && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_u(uint8_t old, uint8_t new,
                                        int32_t *toggle_mask)
{
   if (old == _U && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _U && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_z(uint8_t old, uint8_t new,
                                        int32_t *toggle_mask)
{
   if (old == _0 && new == _Z)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _Z && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;

   if (old == _1 && new == _Z)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
   if (old == _Z && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_0_1_u(uint8_t old, uint8_t new,
                                            int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_u(old, new, toggle_mask);
}

static inline void cover_toggle_check_0_1_z(uint8_t old, uint8_t new,
                                            int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_z(old, new, toggle_mask);
}

static inline void cover_toggle_check_0_1_u_z(uint8_t old, uint8_t new,
                                              int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_u(old, new, toggle_mask);
   cover_toggle_check_z(old, new, toggle_mask);
}

#ifdef COVER_DEBUG_CALLBACK
#define COVER_TGL_CB_MSG(signal)                                              \
   do {                                                                       \
      printf("Time: %lu Callback on signal: %s\n",                            \
              now, istr(tree_ident(signal->where)));                          \
   } while (0);

#define COVER_TGL_SIGNAL_DETAILS(signal, size)                                \
   do {                                                                       \
      printf("New signal value:\n");                                          \
      for (int i = 0; i < size; i++)                                          \
         printf("0x%x ", ((uint8_t*)signal_value(signal))[i]);                \
      printf("\n");                                                           \
      printf("Old signal value:\n");                                          \
      for (int i = 0; i < size; i++)                                          \
         printf("0x%x ", ((const uint8_t *)signal_last_value(signal))[i]);    \
      printf("\n\n");                                                         \
   } while (0);

#else
#define COVER_TGL_CB_MSG(signal)
#define COVER_TGL_SIGNAL_DETAILS(signal, size)
#endif


#define DEFINE_COVER_TOGGLE_CB(name, check_fnc)                               \
   static void name(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)  \
   {                                                                          \
      uint32_t s_size = s->shared.size;                                       \
      int32_t *toggle_mask = ((int32_t *)user);                               \
      COVER_TGL_CB_MSG(s)                                                     \
      for (int i = 0; i < s_size; i++) {                                      \
         uint8_t new = ((uint8_t*)signal_value(s))[i];                        \
         uint8_t old = ((uint8_t*)signal_last_value(s))[i];                   \
         check_fnc(old, new, toggle_mask);                                    \
         toggle_mask++;                                                       \
      }                                                                       \
      COVER_TGL_SIGNAL_DETAILS(s, s_size)                                     \
   }                                                                          \


DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1,     cover_toggle_check_0_1)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_u,   cover_toggle_check_0_1_u)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_z,   cover_toggle_check_0_1_z)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_u_z, cover_toggle_check_0_1_u_z)


static bool is_constant_input(rt_signal_t *s)
{
   tree_t decl = s->where;
   tree_kind_t kind = tree_kind(decl);

   if (kind == T_FIELD_DECL) {
      rt_scope_t *sc = s->parent;
      while (sc->parent->kind == SCOPE_SIGNAL) {
         sc = sc->parent;
      }
      decl = sc->where;
   }

   if (tree_kind(decl) != T_PORT_DECL)
      return false;
   else if (tree_subkind(decl) != PORT_IN)
      return false;
   else {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++, n = n->chain) {
         if (n->n_sources > 0)
            return false;
      }
      return true;
   }
}

void x_cover_setup_toggle_cb(sig_shared_t *ss, int32_t *toggle_mask)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();
   cover_mask_t op_mask = get_coverage(m)->mask;
   sig_event_fn_t fn = &cover_toggle_cb_0_1;

   if (is_constant_input(s)) {
      for (int i = 0; i < s->shared.size; i++) {
         // Remember un-reachability in run-time data. Exclude mask not
         // available at run-time.
         (*toggle_mask++) |= COV_FLAG_UNREACHABLE;
      }
      return;
   }

   if ((op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED) &&
       (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z))
      fn = &cover_toggle_cb_0_1_u_z;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED)
      fn = &cover_toggle_cb_0_1_u;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z)
      fn = &cover_toggle_cb_0_1_z;

   model_set_event_cb(m, s, fn, toggle_mask, false);
}

///////////////////////////////////////////////////////////////////////////////
// Exclude file
///////////////////////////////////////////////////////////////////////////////

static char* cover_bmask_to_bin_list(uint32_t bmask)
{
   LOCAL_TEXT_BUF tb = tb_new();
   bool empty = true;
   for (int i = 0; i < ARRAY_LEN(bin_map); i++)
      if (bmask & bin_map[i].flag) {
         if (!empty)
            tb_cat(tb, ", ");
         tb_cat(tb, bin_map[i].name);
         empty = false;
      }
   return tb_claim(tb);
}

static uint32_t cover_bin_str_to_bmask(cover_exclude_ctx_t *ctx, const char *bin,
                                       cover_tag_t *tag)
{
   uint32_t allowed = tag->flags & COVER_FLAGS_ALL_BINS;

   // Check if bin is valid for given coverage tag kind
   for (int i = 0; i < ARRAY_LEN(bin_map); i++) {
      if (strcmp(bin, bin_map[i].name))
         continue;
      if ((allowed & bin_map[i].flag) != 0)
         return bin_map[i].flag;
   }

   char LOCAL *bin_list = cover_bmask_to_bin_list(allowed);
   fatal_at(&ctx->loc, "invalid bin: $bold$'%s'$$ for %s: '%s'."
            " Valid bins are: %s", bin, tag_kind_str[tag->kind],
            istr(tag->hier), bin_list);
   return 0;
}

static void cover_exclude_hier(cover_tagging_t *tagging, cover_exclude_ctx_t *ctx,
                               const char *excl_hier, const char *bin)
{
   bool match = false;
   int len = strlen(excl_hier);

   for (int i = 0; i < tagging->tags.count; i++) {
      cover_tag_t *tag = AREF(tagging->tags, i);

      if (tag->kind == TAG_HIER || tag->kind == TAG_LAST)
         continue;

      if (ident_glob(tag->hier, excl_hier, len)) {
         match = true;

         // By default, exclude all bins of a tag
         uint32_t bmask = (tag->kind == TAG_STMT) ?
                              0xFFFFFFFF : (tag->flags & (COVER_FLAGS_ALL_BINS));

#ifdef COVER_DEBUG_EXCLUDE
         printf("Applying matching exclude:\n");
         printf("    Tag:        %s\n", istr(tag->hier));
         printf("    Tag flags:  %x\n", tag->flags);
         printf("    Tag data:   %x\n", tag->data);
         printf("    Bin mask:   %x\n", bmask);
#endif

         if (bin) {
            if (tag->kind == TAG_STMT)
               fatal_at(&ctx->loc, "statements do not contain bins, "
                        "but bin '%s' was given for statement: '%s'",
                        bin, istr(tag->hier));
            bmask = cover_bin_str_to_bmask(ctx, bin, tag);
         }

         // Info about exclude
         if (tag->kind == TAG_STMT)
            note_at(&ctx->loc, "excluding statement: '%s'", istr(tag->hier));
         else {
            char LOCAL *blist = cover_bmask_to_bin_list(bmask);
            note_at(&ctx->loc, "excluding %s: '%s' bins: %s",
                    tag_kind_str[tag->kind], istr(tag->hier), blist);
         }

         // Check attempt to exclude something already covered
         uint32_t excl_cov = bmask & tag->data;
         if (excl_cov) {
            if (tag->kind == TAG_STMT)
               warn_at(&ctx->loc, "statement: '%s' already covered!",
                       istr(tag->hier));
            else {
               char LOCAL *blist = cover_bmask_to_bin_list(excl_cov);
               warn_at(&ctx->loc, "%s: '%s' bins: %s already covered!",
                       tag_kind_str[tag->kind], istr(tag->hier), blist);
            }
         }

         // Mark as excluded
         tag->excl_msk |= bmask;
      }
   }

   if (!match)
      warn_at(&ctx->loc, "exluded hierarchy does not match any "
              "coverage item: '%s'", excl_hier);
}

void cover_load_exclude_file(const char *path, cover_tagging_t *tagging)
{
   cover_exclude_ctx_t ctx = {};

   ctx.ef = fopen(path, "r");
   if (ctx.ef == NULL)
      fatal_errno("failed to open exclude file: %s\n", path);

   char *delim = " ";
   ssize_t read;
   loc_file_ref_t file_ref = loc_file_ref(path, NULL);
   int line_num = 1;
   size_t line_len;

   while ((read = getline(&(ctx.line), &line_len, ctx.ef)) != -1) {

      // Strip comments and newline
      char *p = ctx.line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      ctx.loc = get_loc(line_num, 0, line_num, p - ctx.line - 1, file_ref);

      // Parse line as space separated tokens
      for (char *tok = strtok(ctx.line, delim); tok; tok = strtok(NULL, delim)) {
         if (!strcmp(tok, "exclude")) {
            char *excl_hier = strtok(NULL, delim);
            char *bin = strtok(NULL, delim);

            if (!excl_hier)
               fatal_at(&ctx.loc, "exclude hierarchy missing!");

            int i = 0;
            while (excl_hier[i]) {
               excl_hier[i] = toupper(excl_hier[i]);
               i++;
            }

            cover_exclude_hier(tagging, &ctx, excl_hier, bin);
         }
         else
            fatal_at(&ctx.loc, "invalid command: $bold$%s$$", tok);

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ctx.ef);
}


///////////////////////////////////////////////////////////////////////////////
// Report generation
///////////////////////////////////////////////////////////////////////////////

static void cover_append_line(cover_file_t *f, const char *buf)
{
   if (f->n_lines == f->alloc_lines) {
      f->alloc_lines *= 2;
      f->lines = xrealloc_array(f->lines, f->alloc_lines, sizeof(cover_line_t));
   }

   cover_line_t *l = &(f->lines[(f->n_lines)++]);
   l->text = xstrdup(buf);
   l->len  = strlen(buf);
}

static cover_file_t *cover_file(const loc_t *loc)
{
   if (loc_invalid_p(loc))
      return NULL;

   cover_file_t *f;
   for (f = files; f != NULL; f = f->next) {
      // Comparing pointers directly here is OK since only one copy
      // of the file name string will be created by tree_read
      if (f->name == loc_file_str(loc))
         return f->valid ? f : NULL;
   }

   f = xmalloc(sizeof(cover_file_t));
   f->name        = loc_file_str(loc);
   f->n_lines     = 0;
   f->alloc_lines = 1024;
   f->lines       = xmalloc_array(f->alloc_lines, sizeof(cover_line_t));
   f->next        = files;

   FILE *fp = fopen(loc_file_str(loc), "r");

   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *path LOCAL =
         xasprintf("%s/../%s", lib_path(lib_work()), loc_file_str(loc));
      fp = fopen(path, "r");
   }

   if (fp == NULL) {
      warnf("failed to open %s for coverage report", loc_file_str(loc));
      f->valid = false;
   }
   else {
      f->valid = true;

      while (!feof(fp)) {
         char buf[1024];
         if (fgets(buf, sizeof(buf), fp) != NULL)
            cover_append_line(f, buf);
         else if (ferror(fp))
            fatal("error reading %s", loc_file_str(loc));
      }

      fclose(fp);
   }

   return (files = f);
}


static void cover_print_html_header(FILE *f, cover_report_ctx_t *ctx, bool top,
                                    const char *title, ...)
{
   fprintf(f, "<!DOCTYPE html>\n"
              "<html>\n"
              "  <head>\n"
              "  <title>\n");

   va_list ap;
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);

   fprintf(f, "  </title>\n"
              "  <style>\n"
              "\n"
              "   header {\n"
              "      padding: 30px;\n"
              "      text-align: center;\n"
              "      font-size: 35px;\n"
              "   }\n"
              "\n"
              "   h2 {\n"
              "      word-wrap: break-word;\n"
              "      width:70%%\n"
              "   }\n"
              "   h3 {\n"
              "      word-wrap: break-word;\n"
              "      margin-bottom: 0px;\n"
              "   }\n"
              "\n"
              "   hr {\n"
              "      border:none;\n"
              "      height: 2px;\n"
              "      background: black;\n"
              "   }\n"
              "   nav {\n"
              "      float: left;\n"
              "      background-color: #ccc;\n"
              "      width: " SIDEBAR_WIDTH ";\n"
              "      height: 100%%;\n"
              "      padding: 10px;\n"
              "      margin-top: 100px;\n"
              "      word-wrap: break-word;\n"
              "     }\n"
              "\n"
              "     table {\n"
              "        table-layout: fixed;"
              "     }\n"
              "\n"
              "     table, th, td {\n"
              "        border: 2px solid black;\n"
              "        border-collapse: collapse;\n"
              "        word-wrap: break-word;\n"
              "     }\n"
              "\n"
              "     .tabcontent {\n"
              "         display: none;\n"
              "         padding: 0px 0px;\n"
              "         border: 2px solid #ccc;\n"
              "         border-top: none;\n"
              "         word-wrap: break-word;\n"
              "      }\n"
              "\n"
              "      .tab {\n"
              "         overflow: hidden;\n"
              "         border: none;\n"
              "         background-color: none;\n"
              "         margin-left: " MARGIN_LEFT ";\n"
              "         margin-top: 10px;\n"
              "      }\n"
              "\n"
              "      .tab button.active {\n"
              "         background-color: #ccc;\n"
              "      }\n"
              "\n"
              "      .tab button:hover {\n"
              "         background-color: #ddd;\n"
              "      }\n"
              "\n"
              "      .tab button {\n"
              "         background-color: inherit;\n"
              "         float: left;\n"
              "         margin-left: 20px\n"
              "         border: 2px solid black;\n"
              "         cursor: pointer;\n"
              "         padding: 14px 16px;\n"
              "         font-size: 17px;\n"
              "      }\n"
              "\n"
              "  </style>\n"
              "  </head>\n"
              "  <section>\n");

   if (!top) {
      fprintf(f, "<nav>");
      fprintf(f, "   <b>Hierarchy:</b><br>\n");
      int offset = 0;

      ident_t full_hier = ctx->start_tag->hier;
      ident_t curr_id;
      ident_t curr_hier = NULL;
      const char *link = "../index";
      do {
         curr_id = ident_walk_selected(&full_hier);
         curr_hier = ident_prefix(curr_hier, curr_id, '.');
         if (offset > 0)
            link = istr(curr_hier);
         if (curr_id)
            fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                        offset * 10, link, istr(curr_id));
         offset++;
      } while (curr_id != NULL);
      fprintf(f, "</nav>");
   }

   fprintf(f, "  <header>\n");
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);
   fprintf(f, "  </header>\n");

   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "     Instance:&nbsp;%s\n", istr(ctx->start_tag->hier));
   else
      fprintf(f, "     Instance:");
   fprintf(f, "  </h2>\n");

   // start_tag has still loc corresponding to a file where hierarchy
   // is instantiated.
   cover_file_t *src = cover_file(&((ctx->start_tag + 1)->loc));
   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "     File:&nbsp; <a href=\"../../%s\">../../%s</a>\n",
               src->name, src->name);
   else
      fprintf(f, "     File:");
   fprintf(f, "  </h2>\n");

}

static void cover_print_percents_cell(FILE *f, unsigned hit, unsigned total)
{
   if (total > 0) {
      float perc = ((float) hit / (float) total) * 100;
      char color[8];
      if (hit == total)
         checked_sprintf(color, sizeof(color), "#00cc00");
      else if (perc > 90)
         checked_sprintf(color, sizeof(color), "#e6e600");
      else if (perc > 80)
         checked_sprintf(color, sizeof(color), "#ff9900");
      else
         checked_sprintf(color, sizeof(color), "#ff0000");

      fprintf(f, "      <td bgcolor=%s>%.1f %% (%d/%d)</td>\n",
              color, perc, hit, total);
      return;
   }

   fprintf(f, "      <td bgcolor=#aaaaaa>N.A.</td>\n");
}

static void cover_print_hierarchy_header(FILE *f)
{
   fprintf(f, "<table style=\"width:70%%;margin-left:" MARGIN_LEFT ";margin-right:auto;\"> \n"
              "  <tr>\n"
              "     <th bgcolor=#999999 style=\"width:30%%\">Instance</th>\n"
              "     <th bgcolor=#999999 style=\"width:10%%\">Statement</th>\n"
              "     <th bgcolor=#999999 style=\"width:10%%\">Branch</th>\n"
              "     <th bgcolor=#999999 style=\"width:10%%\">Toggle</th>\n"
              "     <th bgcolor=#999999 style=\"width:10%%\">Expression</th>\n"
              "     <th bgcolor=#999999 style=\"width:10%%\">Average</th>\n"
              "  </tr>\n");
}

static void cover_print_hierarchy_footer(FILE *f)
{
   fprintf(f, "</table>\n");
}

static void cover_print_timestamp(FILE *f)
{
   time_t t;
   time(&t);

   fprintf(f, "  </section>\n");

   fprintf(f, "<footer>");
   fprintf(f, "   <p> NVC version: %s </p>\n", PACKAGE_VERSION);
   fprintf(f, "   <p> Generated on: %s </p>\n", ctime(&t));
   fprintf(f,  "</footer>");
}

static void cover_print_hierarchy_summary(FILE *f, cover_report_ctx_t *ctx, ident_t hier,
                                          bool top, bool full_hier, bool flat)
{
   ident_t print_hier = (full_hier) ? hier : ident_rfrom(hier, '.');
   cover_stats_t *stats;

   if (flat)
      stats = &(ctx->flat_stats);
   else
      stats = &(ctx->nested_stats);

   fprintf(f, "   <tr>\n"
              "      <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(hier), istr(print_hier));

   cover_print_percents_cell(f, stats->hit_stmts, stats->total_stmts);
   cover_print_percents_cell(f, stats->hit_branches, stats->total_branches);
   cover_print_percents_cell(f, stats->hit_toggles, stats->total_toggles);
   cover_print_percents_cell(f, stats->hit_expressions, stats->total_expressions);

   int avg_total = stats->total_stmts + stats->total_branches +
                   stats->total_toggles + stats->total_expressions;
   int avg_hit = stats->hit_stmts + stats->hit_branches +
                 stats->hit_toggles + stats->hit_expressions;

   cover_print_percents_cell(f, avg_hit, avg_total);

   fprintf(f, "   </tr>\n");

   float perc_stmt = 0.0f;
   float perc_branch = 0.0f;
   float perc_toggle = 0.0f;
   float perc_expr = 0.0f;

   if (stats->total_stmts > 0)
      perc_stmt = 100.0 * ((float)stats->hit_stmts) / stats->total_stmts;
   if (stats->total_branches > 0)
      perc_branch = 100.0 * ((float)stats->hit_branches) / stats->total_branches;
   if (stats->total_toggles > 0)
      perc_toggle = 100.0 * ((float)stats->hit_toggles) / stats->total_toggles;
   if (stats->total_expressions > 0)
      perc_expr = 100.0 * ((float)stats->hit_expressions) / stats->total_expressions;

   if (top) {
      notef("code coverage results for: %s", istr(hier));

      if (perc_stmt > 0)
         notef("     statement:     %.1f %% (%d/%d)", perc_stmt,
               stats->hit_stmts, stats->total_stmts);
      else
         notef("     statement:     N.A.");

      if (perc_branch > 0)
         notef("     branch:        %.1f %% (%d/%d)", perc_branch,
               stats->hit_branches, stats->total_branches);
      else
         notef("     branch:        N.A.");

      if (perc_toggle > 0)
         notef("     toggle:        %.1f %% (%d/%d)", perc_toggle,
               stats->hit_toggles, stats->total_toggles);
      else
         notef("     toggle:        N.A.");

      if (perc_expr > 0)
         notef("     expression:    %.1f %% (%d/%d)", perc_expr,
               stats->hit_expressions, stats->total_expressions);
      else
         notef("     expression:    N.A.");
   }
   else if (opt_get_int(OPT_VERBOSE) && !flat) {

      cover_rpt_buf_t *new = xcalloc(sizeof(cover_rpt_buf_t));
      new->tb = tb_new();
      new->prev = ctx->tagging->rpt_buf;
      ctx->tagging->rpt_buf = new;

      tb_printf(new->tb,
         "%*s %-*s %10.1f %% (%d/%d)  %10.1f %% (%d/%d) %10.1f %% (%d/%d) %10.1f %% (%d/%d)",
         ctx->lvl, "", 50-ctx->lvl, istr(ident_rfrom(ctx->start_tag->hier, '.')),
         perc_stmt, stats->hit_stmts, stats->total_stmts,
         perc_branch, stats->hit_branches, stats->total_branches,
         perc_toggle, stats->hit_toggles, stats->total_toggles,
         perc_expr, stats->hit_expressions, stats->total_expressions);
   }
}

static void cover_print_code_line(FILE *f, loc_t loc, cover_line_t *line)
{
   fprintf(f, "&nbsp;<code>");
   int last = strlen(line->text);
   int curr = 0;
   while (curr <= last) {
      if (curr == loc.first_column)
         fprintf(f, "<code style=\"background-color:#bbbbbb;\">");
      fprintf(f, "%c", line->text[curr]);
      if (curr == (loc.first_column + loc.column_delta) &&
            loc.line_delta == 0)
         fprintf(f, "</code>");
      curr++;
   }
   if (loc.line_delta > 0)
      fprintf(f, "</code>");
   fprintf(f, "</code>");
}

static void cover_print_get_exclude_button(FILE *f, cover_tag_t *tag,
                                           uint32_t flag, bool add_td)
{

   const char *bin_name = "";
   if (flag) {
      for (int i = 0; i < ARRAY_LEN(bin_map); i++) {
         if (bin_map[i].flag == flag)
            bin_name = bin_map[i].name;
      }
   }

   if (add_td)
      fprintf(f, "<td>");

   fprintf(f, "<button onclick=\"GetExclude('exclude %s %s')\" %s>"
              "Copy %sto Clipoard</button>", istr(tag->hier), bin_name,
           (tag->kind == TAG_STMT) ? "style=\"float: right;\"" : "",
           (tag->kind == TAG_STMT) ? "Exclude Command " : "");

   if (add_td)
      fprintf(f, "</td>");
}

static void cover_print_bin(FILE *f, cover_pair_t *pair, uint32_t flag,
                            cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   if (pair->flags & flag) {
      fprintf(f, "<tr><td><b>Bin</b></td>");

      for (int i = 0; i < cols; i++) {
         const char *val = va_arg(argp, const char *);
         fprintf(f, "<td>%s</td>", val);
      }

      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(f, pair->tag, flag, true);

      if (pkind == PAIR_EXCLUDED) {
         const char *er = (pair->flags & COV_FLAG_UNREACHABLE) ?
            "Unreachable" : "Exclude file";
         fprintf(f, "<td>%s</td>", er);
      }

      fprintf(f, "</tr>");
   }
}

static void cover_print_bin_header(FILE *f, cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   fprintf(f, "<tr style=\"background-color:#999999;\">");
   fprintf(f, "<th style=\"width:40px;\"></th>");

   for (int i = 0; i < cols; i++) {
      const char *val = va_arg(argp, const char *);
      fprintf(f, "<th style=\"width:100px;\">%s</th>", val);
   }

   if (pkind == PAIR_UNCOVERED)
      fprintf(f, "<th style=\"width:150px;\">Exclude Command</th>");

   if (pkind == PAIR_EXCLUDED)
      fprintf(f, "<th style=\"width:150px;\">Excluded due to</th>");

   fprintf(f, "</tr>");
}

static void cover_print_bins(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind)
{
   loc_t loc = pair->tag->loc;

   fprintf(f, "<br><table style=\"border:2px;text-align:center;margin-top:8px;\">");

   switch (pair->tag->kind) {
   case TAG_BRANCH:
      cover_print_bin_header(f, pkind, 1, (pair->flags & COV_FLAG_CHOICE) ?
                              "Choice of" : "Evaluated to");
      cover_print_bin(f, pair, COV_FLAG_TRUE, pkind, 1, "True");
      cover_print_bin(f, pair, COV_FLAG_FALSE, pkind, 1, "False");

      if (pair->flags & COV_FLAG_CHOICE) {
         int curr = loc.first_column;
         int last = (loc.line_delta) ? strlen(pair->line->text) :
                                       loc.column_delta + curr;

         LOCAL_TEXT_BUF tb = tb_new();
         tb_printf(tb, "<code>");
         while (curr <= last) {
            tb_printf(tb, "%c", pair->line->text[curr]);
            curr++;
         }
         tb_printf(tb, "</code>");

         cover_print_bin(f, pair, COV_FLAG_CHOICE, pkind, 1, tb_get(tb));
      }
      break;

   case TAG_TOGGLE:
      cover_print_bin_header(f, pkind, 2, "From", "To");
      cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_1, pkind, 2, "0", "1");
      cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_0, pkind, 2, "1", "0");
      break;

   case TAG_EXPRESSION:
      {
      char *t_str = (pair->tag->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'1'" : "True";
      char *f_str = (pair->tag->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'0'" : "False";

      if ((pair->flags & COV_FLAG_TRUE) || (pair->flags & COV_FLAG_FALSE)) {
         cover_print_bin_header(f, pkind, 1, "Evaluated to");
         cover_print_bin(f, pair, COV_FLAG_TRUE, pkind, 1, t_str);
         cover_print_bin(f, pair, COV_FLAG_FALSE, pkind, 1, f_str);
      }

      if (pair->flags & COV_FLAG_00 || pair->flags & COV_FLAG_01 ||
          pair->flags & COV_FLAG_10 || pair->flags & COV_FLAG_11) {
         cover_print_bin_header(f, pkind, 2, "LHS", "RHS");

         cover_print_bin(f, pair, COV_FLAG_00, pkind, 2, f_str, f_str);
         cover_print_bin(f, pair, COV_FLAG_01, pkind, 2, f_str, t_str);
         cover_print_bin(f, pair, COV_FLAG_10, pkind, 2, t_str, f_str);
         cover_print_bin(f, pair, COV_FLAG_11, pkind, 2, t_str, t_str);
      }
      }
      break;
   default:
      fatal("unsupported type of code coverage: %d !", pair->tag->kind);
   }
   fprintf(f, "</table>");
}

static void cover_print_pair(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind)
{
   loc_t loc = pair->tag->loc;
   fprintf(f, "<p>");

   switch (pair->tag->kind) {
   case TAG_STMT:
      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(f, pair->tag, 0, false);
      if (pkind == PAIR_EXCLUDED) {
         const char *er = (pair->flags & COV_FLAG_UNREACHABLE) ?
            "Unreachable" : "Exclude file";
         fprintf(f, "<div style=\"float: right\"><b>Excluded due to:</b> %s</div>", er);
      }
      fprintf(f, "<h3>Line %d:</h3>", loc.first_line);
      cover_print_code_line(f, loc, pair->line);
      fprintf(f, "<hr>");
      break;

   case TAG_BRANCH:
      fprintf(f, "<h3>Line %d:</h3>", loc.first_line);
      cover_print_code_line(f, loc, pair->line);
      cover_print_bins(f, pair, pkind);
      fprintf(f, "<hr>");
      break;

   case TAG_TOGGLE:
      if (pair->tag->flags & COV_FLAG_TOGGLE_SIGNAL)
         fprintf(f, "<h3>Signal:</h3>");
      else if (pair->tag->flags & COV_FLAG_TOGGLE_PORT)
         fprintf(f, "<h3>Port:</h3>");

      const char *sig_name = istr(pair->tag->hier);
      sig_name += pair->tag->sig_pos;
      fprintf(f, "&nbsp;<code>%s</code>", sig_name);

      cover_print_bins(f, pair, pkind);
      fprintf(f, "<hr>");
      break;

   case TAG_EXPRESSION:
      fprintf(f, "<h3>Line %d:</h3>", loc.first_line);
      cover_print_code_line(f, loc, pair->line);
      cover_print_bins(f, pair, pkind);
      fprintf(f, "<hr>");
      break;

   default:
      fatal("unsupported type of code coverage: %d !", pair->tag->kind);
   }
   fprintf(f, "</p>");
}

static void cover_print_chain(FILE *f, cover_tagging_t *tagging,
                              cover_chain_t *chn, tag_kind_t kind)
{
   // HTML TAB
   fprintf(f, "<div id=\"");
   if (kind == TAG_STMT)
      fprintf(f, "Statement");
   else if (kind == TAG_BRANCH)
      fprintf(f, "Branch");
   else if (kind == TAG_TOGGLE)
      fprintf(f, "Toggle");
   else if (kind == TAG_EXPRESSION)
      fprintf(f, "Expression");

   fprintf(f, "\" class=\"tabcontent\" style=\"width:68.5%%;margin-left:" MARGIN_LEFT "; "
                          "margin-right:auto; margin-top:10px; border: 2px solid black;\">\n");

   for (cov_pair_kind_t pkind = PAIR_UNCOVERED; pkind < PAIR_LAST; pkind++) {
      int n;
      cover_pair_t *pair;

      if (pkind == PAIR_UNCOVERED) {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_UNCOVERED))
            continue;
         pair = chn->miss;
         n = chn->n_miss;
      }
      else if (pkind == PAIR_EXCLUDED) {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_EXCLUDED))
            continue;
         pair = chn->excl;
         n = chn->n_excl;
      }
      else {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_COVERED))
            continue;
         pair = chn->hits;
         n = chn->n_hits;
      }

      fprintf(f, "<section style=\"background-color:");
      if (pkind == PAIR_UNCOVERED)
         fprintf(f, "#ffcccc;\">");
      else if (pkind == PAIR_EXCLUDED)
         fprintf(f, "#d6eaf8;\">");
      else
         fprintf(f, "#ccffcc;\">");

      fprintf(f, "   <h2>");
      if (pkind == PAIR_UNCOVERED)
         fprintf(f, "Uncovered ");
      else if (pkind == PAIR_EXCLUDED)
         fprintf(f, "Excluded ");
      else
         fprintf(f, "Covered ");

      if (kind == TAG_STMT)
         fprintf(f, "statements:");
      else if (kind == TAG_BRANCH)
         fprintf(f, "branches:");
      else if (kind == TAG_TOGGLE)
         fprintf(f, "toggles:");
      else if (kind == TAG_EXPRESSION)
         fprintf(f, "expressions:");
      fprintf(f, "   </h2>");

      fprintf(f, "<section style=\"padding:0px 10px;\"");
      for (int j = 0; j < n; j++) {
         cover_print_pair(f, pair, pkind);
         pair++;
      }
      fprintf(f, "</section></section>");
   }

   fprintf(f, "</div>\n");
}

static void cover_print_hierarchy_guts(FILE *f, cover_report_ctx_t *ctx)
{
   fprintf(f, "<div class=\"tab\">"
              "   <button class=\"tablinks\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Branch')\">Branch</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Expression')\">Expression</button>\n"
              "</div>\n");

   cover_print_chain(f, ctx->tagging, &(ctx->ch_stmt), TAG_STMT);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_branch), TAG_BRANCH);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_toggle), TAG_TOGGLE);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_expression), TAG_EXPRESSION);

   fprintf(f, "<script>\n"
              "   document.getElementById(\"defaultOpen\").click();"
              "   function selectCoverage(evt, coverageType) {\n"
              "      var i, tabcontent, tablinks;\n"
              "      tabcontent = document.getElementsByClassName(\"tabcontent\");\n"
              "      for (i = 0; i < tabcontent.length; i++) {\n"
              "         tabcontent[i].style.display = \"none\";\n"
              "      }\n"
              "      tablinks = document.getElementsByClassName(\"tablinks\");\n"
              "      for (i = 0; i < tablinks.length; i++) {\n"
              "         tablinks[i].className = tablinks[i].className.replace(\" active\", \"\");\n"
              "      }\n"
              "      document.getElementById(coverageType).style.display = \"block\";\n"
              "      evt.currentTarget.className += \" active\";\n"
              "   }\n"
              "   function GetExclude(excludeCmd) {\n"
              "      navigator.clipboard.writeText(excludeCmd);\n"
              "   }\n"
              "</script>\n");
}

static int cover_append_to_chain(cover_chain_t *chain, cover_tag_t *tag,
                                 cover_line_t *line, unsigned hits,
                                 unsigned misses, unsigned excludes,
                                 int limit)
{
   int rv = 0;
   if (hits) {
      if (chain->n_hits <= limit) {
         if (chain->n_hits == chain->alloc_hits) {
            chain->alloc_hits *= 2;
            chain->hits = xrealloc_array(chain->hits, chain->alloc_hits,
                                       sizeof(cover_pair_t));
         }
         chain->hits[chain->n_hits].tag = tag;
         chain->hits[chain->n_hits].line = line;
         chain->hits[chain->n_hits].flags = hits;
         chain->n_hits++;
      }
      else
         rv++;
   }

   if (misses) {
      if (chain->n_miss <= limit) {
         if (chain->n_miss == chain->alloc_miss) {
            chain->alloc_miss *= 2;
            chain->miss = xrealloc_array(chain->miss, chain->alloc_miss,
                                       sizeof(cover_pair_t));
         }
         chain->miss[chain->n_miss].tag = tag;
         chain->miss[chain->n_miss].line = line;
         chain->miss[chain->n_miss].flags = misses;
         chain->n_miss++;
      }
      else
         rv++;
   }

   if (excludes) {
      if (chain->n_excl <= limit) {
         if (chain->n_excl == chain->alloc_excl) {
            chain->alloc_excl *= 2;
            chain->excl = xrealloc_array(chain->excl, chain->alloc_excl,
                                       sizeof(cover_pair_t));
         }
         chain->excl[chain->n_excl].tag = tag;
         chain->excl[chain->n_excl].line = line;
         chain->excl[chain->n_excl].flags = excludes;
         chain->n_excl++;
      }
      else
         rv++;
   }

   return rv;
}

static bool cover_tag_unreachable(cover_report_ctx_t *ctx, cover_tag_t *tag)
{
   if ((ctx->tagging->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;
   return !!(tag->data & COV_FLAG_UNREACHABLE);
}

static void cover_tag_to_chain(cover_report_ctx_t *ctx, cover_tag_t *tag,
                               cover_flags_t flag, unsigned *hits,
                               unsigned *misses, unsigned *excludes)
{
   unsigned *flat_total;
   unsigned *nested_total;
   unsigned *flat_hits;
   unsigned *nested_hits;

   switch (tag->kind) {
   case TAG_BRANCH:
      flat_total = &(ctx->flat_stats.total_branches);
      nested_total = &(ctx->nested_stats.total_branches);
      flat_hits = &(ctx->flat_stats.hit_branches);
      nested_hits = &(ctx->nested_stats.hit_branches);
      break;
   case TAG_TOGGLE:
      flat_total = &(ctx->flat_stats.total_toggles);
      nested_total = &(ctx->nested_stats.total_toggles);
      flat_hits = &(ctx->flat_stats.hit_toggles);
      nested_hits = &(ctx->nested_stats.hit_toggles);
      break;
   case TAG_EXPRESSION:
      flat_total = &(ctx->flat_stats.total_expressions);
      nested_total = &(ctx->nested_stats.total_expressions);
      flat_hits = &(ctx->flat_stats.hit_expressions);
      nested_hits = &(ctx->nested_stats.hit_expressions);
      break;
   default:
      fatal("unsupported type of code coverage: %d !", tag->kind);
   }

   (*flat_total)++;
   (*nested_total)++;

   if (tag->data & flag) {
      (*flat_hits)++;
      (*nested_hits)++;
      (*hits) |= flag;
   }
   else if ((tag->excl_msk & flag) || cover_tag_unreachable(ctx, tag)) {
      (*flat_hits)++;
      (*nested_hits)++;
      (*excludes) |= flag;
      if (cover_tag_unreachable(ctx, tag))
         (*excludes) |= COV_FLAG_UNREACHABLE;
   }
   else
      (*misses) |= flag;

}

static cover_tag_t* cover_report_hierarchy(cover_report_ctx_t *ctx,
                                           const char *dir)
{
   char *hier LOCAL = xasprintf("%s/%s.html", dir, istr(ctx->start_tag->hier));
   cover_tag_t *tag = ctx->start_tag;

   // TODO: Handle escaped identifiers in hierarchy path!
   FILE *f = fopen(hier, "w");
   if (f == NULL)
      fatal("failed to open report file: %s\n", hier);

   ctx->ch_stmt.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_stmt.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_stmt.excl = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_stmt.alloc_hits = 1024;
   ctx->ch_stmt.alloc_miss = 1024;
   ctx->ch_stmt.alloc_excl = 1024;

   ctx->ch_branch.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_branch.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_branch.excl = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_branch.alloc_hits = 1024;
   ctx->ch_branch.alloc_miss = 1024;
   ctx->ch_branch.alloc_excl = 1024;

   ctx->ch_toggle.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_toggle.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_toggle.excl = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_toggle.alloc_hits = 1024;
   ctx->ch_toggle.alloc_miss = 1024;
   ctx->ch_toggle.alloc_excl = 1024;

   ctx->ch_expression.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_expression.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_expression.excl = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_expression.alloc_hits = 1024;
   ctx->ch_expression.alloc_miss = 1024;
   ctx->ch_expression.alloc_excl = 1024;

   cover_print_html_header(f, ctx, false, "NVC code coverage report");

   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\"> Sub-instances: </h2>\n");
   cover_print_hierarchy_header(f);

   int skipped = 0;

   for(;;) {
      tag++;

      if (tag->kind == TAG_HIER) {
         if (tag->flags & COV_FLAG_HIER_DOWN) {

            // Collect coverage of sub-block
            cover_report_ctx_t sub_ctx = {0};
            sub_ctx.start_tag = tag;
            sub_ctx.parent = ctx;
            sub_ctx.tagging = ctx->tagging;
            sub_ctx.lvl = ctx->lvl + 2;
            tag = cover_report_hierarchy(&sub_ctx, dir);
            cover_print_hierarchy_summary(f, &sub_ctx,
                                          tag->hier, false, false, false);

            // Add coverage from sub-hierarchies
            ctx->nested_stats.hit_stmts += sub_ctx.nested_stats.hit_stmts;
            ctx->nested_stats.total_stmts += sub_ctx.nested_stats.total_stmts;
            ctx->nested_stats.hit_branches += sub_ctx.nested_stats.hit_branches;
            ctx->nested_stats.total_branches += sub_ctx.nested_stats.total_branches;
            ctx->nested_stats.hit_toggles += sub_ctx.nested_stats.hit_toggles;
            ctx->nested_stats.total_toggles += sub_ctx.nested_stats.total_toggles;
            ctx->nested_stats.hit_expressions += sub_ctx.nested_stats.hit_expressions;
            ctx->nested_stats.total_expressions += sub_ctx.nested_stats.total_expressions;

         }
         else if (tag->flags & COV_FLAG_HIER_UP)
            break;

      }
      else {

         cover_file_t *f_src = cover_file(&(tag->loc));
         if (f_src == NULL)
            continue;
         cover_line_t *line = &(f_src->lines[tag->loc.first_line-1]);

         unsigned hits = 0;
         unsigned misses = 0;
         unsigned excludes = 0;
         int limit = ctx->tagging->report_item_limit;

         switch (tag->kind) {
         case TAG_STMT:
            (ctx->flat_stats.total_stmts)++;
            (ctx->nested_stats.total_stmts)++;

            hits = (tag->data != 0);
            misses = (tag->data == 0) && (tag->excl_msk == 0);
            excludes = (tag->data == 0) && (tag->excl_msk != 0);

            if (hits | excludes) {
               (ctx->flat_stats.hit_stmts)++;
               (ctx->nested_stats.hit_stmts)++;
            }
            skipped += cover_append_to_chain(&(ctx->ch_stmt), tag, line,
                                             hits, misses, excludes, limit);
            break;

         case TAG_BRANCH:
            // If/else, when else
            if (tag->flags & COV_FLAG_TRUE && tag->flags & COV_FLAG_FALSE) {
               cover_tag_to_chain(ctx, tag, COV_FLAG_TRUE,
                                  &hits, &misses, &excludes);
               cover_tag_to_chain(ctx, tag, COV_FLAG_FALSE,
                                  &hits, &misses, &excludes);
               skipped += cover_append_to_chain(&(ctx->ch_branch), tag, line,
                                                hits, misses, excludes, limit);
            }

            // Case, with select
            if (tag->flags & COV_FLAG_CHOICE) {
               cover_tag_to_chain(ctx, tag, COV_FLAG_CHOICE,
                                  &hits, &misses, &excludes);
               skipped += cover_append_to_chain(&(ctx->ch_branch), tag, line,
                                                hits, misses, excludes, limit);
            }
            break;

         case TAG_TOGGLE:
            cover_tag_to_chain(ctx, tag, COV_FLAG_TOGGLE_TO_1,
                               &hits, &misses, &excludes);
            cover_tag_to_chain(ctx, tag, COV_FLAG_TOGGLE_TO_0,
                               &hits, &misses, &excludes);
            skipped += cover_append_to_chain(&(ctx->ch_toggle), tag, line,
                                             hits, misses, excludes, limit);
            break;

         case TAG_EXPRESSION:
            if (tag->flags & COV_FLAG_00)
               cover_tag_to_chain(ctx, tag, COV_FLAG_00,
                                  &hits, &misses, &excludes);
            if (tag->flags & COV_FLAG_01)
               cover_tag_to_chain(ctx, tag, COV_FLAG_01,
                                  &hits, &misses, &excludes);
            if (tag->flags & COV_FLAG_10)
               cover_tag_to_chain(ctx, tag, COV_FLAG_10,
                                  &hits, &misses, &excludes);
            if (tag->flags & COV_FLAG_11)
               cover_tag_to_chain(ctx, tag, COV_FLAG_11,
                                  &hits, &misses, &excludes);
            if (tag->flags & COV_FLAG_TRUE)
               cover_tag_to_chain(ctx, tag, COV_FLAG_TRUE,
                                  &hits, &misses, &excludes);
            if (tag->flags & COV_FLAG_FALSE)
               cover_tag_to_chain(ctx, tag, COV_FLAG_FALSE,
                                  &hits, &misses, &excludes);

            skipped += cover_append_to_chain(&(ctx->ch_expression), tag, line,
                                             hits, misses, excludes, limit);
            break;

         default:
            fatal("unsupported type of code coverage: %d !", tag->kind);
         }
      }
   }

   cover_print_hierarchy_footer(f);

   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\"> Current Instance: </h2>\n");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, ctx, tag->hier, false, true, true);
   cover_print_hierarchy_footer(f);

   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\"> Details: </h2>\n");
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h2>", ctx->tagging->report_item_limit, skipped);
   cover_print_hierarchy_guts(f, ctx);
   cover_print_timestamp(f);

   fclose(f);
   return tag;
}


void cover_report(const char *path, cover_tagging_t *tagging, int item_limit)
{
   char *subdir LOCAL = xasprintf("%s/hier", path);
   make_dir(path);
   make_dir(subdir);

   static const struct {
      const char *name;
      cover_mask_t mask;
   } lst[] = {
      { "covered",      COVER_MASK_DONT_PRINT_COVERED       },
      { "uncovered",    COVER_MASK_DONT_PRINT_UNCOVERED     },
      { "excluded",     COVER_MASK_DONT_PRINT_EXCLUDED      },
   };

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "Code coverage report contains: ");

   bool first = true;
   for (int i = 0; i < ARRAY_LEN(lst); i++)
      if (!cover_enabled(tagging, lst[i].mask)) {
         if (first)
            first = false;
         else
            tb_printf(tb, ", ");
         tb_printf(tb, "%s", lst[i].name);
      }
   tb_printf(tb, " coverage details.");

   notef("Code coverage report folder: %s.", path);
   notef("%s", tb_get(tb));

   assert(tagging->tags.items[0].kind == TAG_HIER);

   cover_report_ctx_t top_ctx = {0};
   top_ctx.start_tag = AREF(tagging->tags, 0);
   top_ctx.tagging = tagging;
   tagging->report_item_limit = item_limit;
   cover_report_hierarchy(&top_ctx, subdir);

   char *top LOCAL = xasprintf("%s/index.html", path);
   FILE *f = fopen(top, "w");

   cover_print_html_header(f, &top_ctx, true, "NVC code coverage report");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, &top_ctx, top_ctx.start_tag->hier,
                                 true, true, false);
   cover_print_hierarchy_footer(f);
   cover_print_timestamp(f);

   if (opt_get_int(OPT_VERBOSE)) {
      notef("Coverage for sub-hierarchies:");
      printf("%-55s %-20s %-20s %-20s %-20s\n",
             "Hierarchy", "Statement", "Branch", "Toggle", "Expression");
      cover_rpt_buf_t *buf = tagging->rpt_buf;
      while (buf) {
         printf("%s\n", tb_get(buf->tb));
         tb_free(buf->tb);
         tagging->rpt_buf = buf->prev;
         free(buf);
         buf = tagging->rpt_buf;
      };
   }

   fclose(f);
}
