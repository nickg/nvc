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

#include "util.h"
#include "array.h"
#include "common.h"
#include "cover.h"
#include "hash.h"
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
#include <libgen.h>
#include <time.h>

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
typedef A(char*) char_array_t;

typedef struct _cover_report_ctx    cover_report_ctx_t;
typedef struct _cover_file          cover_file_t;
typedef struct _cover_scope         cover_scope_t;
typedef struct _cover_exclude_ctx   cover_exclude_ctx_t;
typedef struct _cover_rpt_buf       cover_rpt_buf_t;
typedef struct _cover_spec          cover_spec_t;

typedef enum {
   CSCOPE_UNKNOWN,
   CSCOPE_INSTANCE,
} scope_type_t;

typedef struct _cover_scope {
   scope_type_t   type;
   ident_t        name;
   ident_t        hier;
   loc_t          loc;
   int            branch_label;
   int            stmt_label;
   int            expression_label;
   cover_scope_t *parent;
   ptr_list_t     children;
   tag_array_t    tags;
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
   char_array_t hier_include;
   char_array_t hier_exclude;
   char_array_t block_include;
   char_array_t block_exclude;
   char_array_t fsm_type_include;
   char_array_t fsm_type_exclude;
};

struct _cover_tagging {
   int               next_tag;
   cover_mask_t      mask;
   int               array_limit;
   int               array_depth;
   int               report_item_limit;
   cover_rpt_buf_t  *rpt_buf;
   cover_spec_t     *spec;
   cover_scope_t    *top_scope;
   cover_scope_t    *root_scope;
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
   unsigned    total_states;
   unsigned    hit_states;
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
   cover_chain_t        ch_stmt;
   cover_chain_t        ch_branch;
   cover_chain_t        ch_toggle;
   cover_chain_t        ch_expression;
   cover_chain_t        ch_state;
   int                  lvl;
};

#define INIT_CHAIN(ctx, name)                                           \
   ctx->name.hits = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.miss = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.excl = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.alloc_hits = 1024;                                         \
   ctx->name.alloc_miss = 1024;                                         \
   ctx->name.alloc_excl = 1024;                                         \

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
   "FSM state"
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

typedef enum {
   CTRL_PUSH_SCOPE,
   CTRL_POP_SCOPE,
   CTRL_END_OF_FILE,
} cov_control_t;

static void cover_report_children(cover_report_ctx_t *ctx,
                                  cover_scope_t *s, const char *dir,
                                  FILE *summf, int *skipped);

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
   return tree_kind(branch) == T_ASSOC || tree_kind(branch) == T_COND_STMT;
}

static bool cover_is_toggle_first(tree_t decl)
{
   const tree_kind_t kind = tree_kind(decl);
   return kind == T_SIGNAL_DECL || kind == T_PORT_DECL;
}

unsigned cover_get_std_log_expr_flags(tree_t decl)
{
   if (tree_kind(decl) != T_FUNC_DECL)
      return 0;

   tree_t container = tree_container(decl);
   if (is_well_known(tree_ident(container)) != W_IEEE_1164)
      return 0;

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

bool cover_skip_type_state(cover_tagging_t *tagging, type_t type)
{
   if (!type_is_enum(type))
      return true;

   // Ignore enums from built-in libraries
   ident_t full_name = type_ident(type);
   if (ident_starts_with(full_name, well_known(W_STD)) ||
       ident_starts_with(full_name, well_known(W_IEEE)) ||
       ident_starts_with(full_name, well_known(W_NVC)) ||
       ident_starts_with(full_name, well_known(W_VITAL)))
      return true;

   ident_t name = ident_rfrom(full_name, '.');
   cover_spec_t *spc = tagging->spec;

   // Type should be included
   if (spc)
      for (int i = 0; i < spc->fsm_type_include.count; i++)
         if (ident_glob(name, AGET(spc->fsm_type_include, i), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: True, fsm-type (Type: %s, Pattern: %s)\n",
                  istr(name), AGET(spc->fsm_type_include, i));
#endif
            return false;
         }

   // By default enums should not included
   if (tagging->mask & COVER_MASK_FSM_NO_DEFAULT_ENUMS)
      return true;

   // Type should not be included
   if (spc) {
      for (int i = 0; i < spc->fsm_type_exclude.count; i++)
         if (ident_glob(name, AGET(spc->fsm_type_exclude, i), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: False, fsm-type (Type: %s, Pattern: %s)\n",
                   istr(name), AGET(spc->fsm_type_exclude, i));
#endif
            return true;
         }
     }

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

cover_tag_t *cover_add_tag(tree_t t, const loc_t *loc, ident_t suffix,
                           cover_tagging_t *ctx, tag_kind_t kind,
                           uint32_t flags)
{
   assert(ctx != NULL);

   if (!ctx->top_scope->emit)
      return NULL;

   cover_scope_t *ignore_scope = ctx->top_scope;
   for (; ignore_scope->type != CSCOPE_INSTANCE && ignore_scope->parent;
        ignore_scope = ignore_scope->parent)
      ;

   for (int i = 0; i < ignore_scope->ignore_lines.count; i++) {
      line_range_t *lr = &(ignore_scope->ignore_lines.items[i]);
      if (loc->first_line > lr->start && loc->first_line <= lr->end)
         return NULL;
   }

   // Everything creates scope, so name of current tag is already given
   // by scope in hierarchy.
   ident_t hier = ctx->top_scope->hier;
   if (suffix)
      hier = ident_prefix(hier, suffix, '\0');

   // Expression tags do not nest scope, expression name must be created
   if (kind == TAG_EXPRESSION) {
      char buf[16];
      checked_sprintf(buf, sizeof(buf), "_E%d", ctx->top_scope->expression_label);
      hier = ident_prefix(hier, ident_new(buf), '.');
      ctx->top_scope->expression_label++;
   }

   // Query LHS/RHS operand locations of binary expressions
   loc_t loc_lhs = LOC_INVALID, loc_rhs = LOC_INVALID;
   if (flags & COVER_FLAGS_LHS_RHS_BINS) {
      assert(tree_params(t) > 1);
      loc_lhs = *tree_loc(tree_param(t, 0));
      loc_rhs = *tree_loc(tree_param(t, 1));
   }

   ident_t func_name = NULL;
   if (kind == TAG_EXPRESSION)
      func_name = tree_ident(t);
   else if (kind == TAG_STATE)
      func_name = ident_rfrom(type_ident(tree_type(t)), '.');

#ifdef COVER_DEBUG_EMIT
   printf("Tag: %s\n", istr(hier));
   printf("    First line: %d\n", tree_loc(t)->first_line);
   printf("    First column: %d\n", tree_loc(t)->first_column);
   printf("    Line delta: %d\n", tree_loc(t)->line_delta);
   printf("    Column delta: %d\n", tree_loc(t)->column_delta);
   printf("\n\n");
#endif

   int num = 0;
   if (kind == TAG_STATE) {
      type_t enum_type = tree_type(t);
      assert(type_is_enum(enum_type));
      num = type_enum_literals(enum_type);
   }
   else
      num = ctx->top_scope->sig_pos;

   cover_tag_t new = {
      .kind       = kind,
      .tag        = ctx->next_tag++,
      .data       = 0,
      .flags      = flags,
      .excl_msk   = 0,
      .unrc_msk   = 0,
      .loc        = *loc,
      .loc_lhs    = loc_lhs,
      .loc_rhs    = loc_rhs,
      .hier       = hier,
      .tree_kind  = tree_kind(t),
      .func_name  = func_name,
      .num        = num,
   };

   APUSH(ctx->top_scope->tags, new);

   return AREF(ctx->top_scope->tags, ctx->top_scope->tags.count - 1);
}

LCOV_EXCL_START
static void cover_debug_dump(cover_scope_t *s, int indent)
{
   color_printf("%*s$!blue$%s", indent, "", istr(s->name));
   switch (s->type) {
   case CSCOPE_INSTANCE: color_printf(" : instance"); break;
   default: break;
   }
   color_printf("$$\n");

   for (int i = 0; i < s->tags.count; i++) {
      cover_tag_t *tag = &(s->tags.items[i]);

      char *path LOCAL = xstrdup(loc_file_str(&tag->loc));
      printf("%*s%d: %s %s %s:%d => %x\n", indent + 2, "", tag->tag,
             tag_kind_str[tag->kind], istr(tag->hier),
             basename(path), tag->loc.first_line, tag->data);
   }

   for (list_iter(cover_scope_t *, c, s->children))
      cover_debug_dump(c, indent + 2);
}
LCOV_EXCL_STOP

static void cover_merge_one_tag(cover_tag_t *tag, int32_t data)
{
   switch (tag->kind) {
   case TAG_STMT:
      tag->data += data;
      break;
   case TAG_TOGGLE:
   case TAG_BRANCH:
   case TAG_EXPRESSION:
   case TAG_STATE:
      tag->data |= data;
      break;
   default:
      break;
   }
}

static void cover_update_counts(cover_scope_t *s, const int32_t *counts)
{
   for (int i = 0; i < s->tags.count; i++) {
      cover_tag_t *tag = &(s->tags.items[i]);

      const int32_t data = counts ? counts[tag->tag] : 0;
      cover_merge_one_tag(tag, data);
   }

   for (list_iter(cover_scope_t *, it, s->children))
      cover_update_counts(it, counts);
}

static void cover_write_scope(cover_scope_t *s, fbuf_t *f,
                              ident_wr_ctx_t ident_ctx, loc_wr_ctx_t *loc_ctx)
{
   write_u8(CTRL_PUSH_SCOPE, f);

   fbuf_put_uint(f, s->type);
   ident_write(s->name, ident_ctx);
   ident_write(s->hier, ident_ctx);
   loc_write(&s->loc, loc_ctx);

   if (s->type == CSCOPE_INSTANCE)
      ident_write(s->block_name, ident_ctx);

   fbuf_put_uint(f, s->tags.count);
   for (int i = 0; i < s->tags.count; i++) {
      cover_tag_t *tag = &(s->tags.items[i]);

      write_u8(tag->kind, f);
      write_u32(tag->tag, f);
      write_u32(tag->data, f);
      write_u32(tag->flags, f);
      write_u32(tag->tree_kind, f);
      // Do not dump "excl_msk" since it is only filled at
      // report generation time
      write_u32(tag->unrc_msk, f);

      loc_write(&(tag->loc), loc_ctx);
      if (tag->flags & COVER_FLAGS_LHS_RHS_BINS) {
         loc_write(&(tag->loc_lhs), loc_ctx);
         loc_write(&(tag->loc_rhs), loc_ctx);
      }

      ident_write(tag->hier, ident_ctx);
      if (tag->kind == TAG_EXPRESSION || tag->kind == TAG_STATE)
         ident_write(tag->func_name, ident_ctx);

      write_u32(tag->num, f);
   }

   for (list_iter(cover_scope_t *, it, s->children))
      cover_write_scope(it, f, ident_ctx, loc_ctx);

   write_u8(CTRL_POP_SCOPE, f);
}

void cover_dump_tags(cover_tagging_t *ctx, fbuf_t *f, cover_dump_t dt,
                     const int32_t *counts)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(ctx->root_scope, counts);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(ctx->root_scope, 0);

   write_u32(ctx->mask, f);
   write_u32(ctx->array_limit, f);
   write_u32(ctx->next_tag, f);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   cover_write_scope(ctx->root_scope, f, ident_ctx, loc_wr);

   write_u8(CTRL_END_OF_FILE, f);

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

static bool cover_should_emit_scope(cover_tagging_t *tagging, tree_t t)
{
   cover_scope_t *ts = tagging->top_scope;
   cover_spec_t *spc = tagging->spec;

   // Block (entity, package instance or block) name
   if (ts->block_name) {
      ident_t ename = ident_until(ts->block_name, '-');

      for (int i = 0; i < spc->block_exclude.count; i++)

         if (ident_glob(ename, AGET(spc->block_exclude, i), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: False, block (Block: %s, Pattern: %s)\n",
                   istr(ts->block_name), AGET(spc->block_exclude, i));
#endif
            return false;
         }

      for (int i = 0; i < tagging->spec->block_include.count; i++)
         if (ident_glob(ename, AGET(spc->block_include, i), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: True, block (Block: %s, Pattern: %s)\n",
                   istr(ts->block_name), AGET(spc->block_include, i));
#endif
            return true;
         }
   }

   // Hierarchy
   for (int i = 0; i < spc->hier_exclude.count; i++)
      if (ident_glob(ts->hier, AGET(spc->hier_exclude, i), -1)) {
#ifdef COVER_DEBUG_EMIT
         printf("Cover emit: False, hierarchy (Hierarchy: %s, Pattern: %s)\n",
                istr(ts->hier), AGET(spc->hier_exclude, i));
#endif
         return false;
      }

   for (int i = 0; i < spc->hier_include.count; i++)
      if (ident_glob(ts->hier, AGET(spc->hier_include, i), -1)) {
#ifdef COVER_DEBUG_EMIT
         printf("Cover emit: True, hierarchy (Hierarchy: %s, Pattern: %s)\n",
                istr(ts->hier), AGET(spc->hier_include, i));
#endif
         return true;
      }

   return false;
}

void cover_push_scope(cover_tagging_t *tagging, tree_t t)
{
   if (tagging == NULL)
      return;
   else if (tagging->root_scope == NULL) {
      cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
      s->name = s->hier = lib_name(lib_work());

      tagging->top_scope = tagging->root_scope = s;
   }

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
      s->sig_pos = ident_len(tagging->top_scope->hier) + 1;
   }
   else if (tree_has_ident(t))
      name = tree_ident(t);
   // Consider everything else as statement
   // Expressions do not get scope pushed, so if scope for e.g.
   // T_FCALL is pushed it will be concurent function call -> Label as statement
   else {
      assert(tagging->top_scope);
      cnt = &tagging->top_scope->stmt_label;
      c = 'S';
   }

   if (c) {
      checked_sprintf(prefix, sizeof(prefix), "_%c%u", c, *cnt);
      (*cnt)++;
   }
   if (name == NULL)
      name = ident_new(prefix);

   s->name       = name;
   s->parent     = tagging->top_scope;
   s->block_name = s->parent->block_name;
   s->loc        = *tree_loc(t);
   s->hier       = ident_prefix(s->parent->hier, name, '.');

   if (s->sig_pos == 0)
      s->sig_pos = tagging->top_scope->sig_pos;

   list_add(&tagging->top_scope->children, s);

   tagging->top_scope = s;

   if (tree_kind(t) == T_BLOCK) {
      tree_t hier = tree_decl(t, 0);
      assert(tree_kind(hier) == T_HIER);

      tree_t unit = tree_ref(hier);
      if (tree_kind(unit) == T_ARCH) {
         s->block_name = ident_rfrom(tree_ident(unit), '.');
         s->type = CSCOPE_INSTANCE;
      }
   }

   s->emit = (tagging->spec == NULL) ? true : cover_should_emit_scope(tagging, t);

#ifdef COVER_DEBUG_SCOPE
   printf("Pushing cover scope: %s\n", istr(s->hier));
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

#ifdef COVER_DEBUG_SCOPE
   printf("Popping cover scope: %s\n", istr(tagging->top_scope->hier));
#endif

   tagging->top_scope = tagging->top_scope->parent;

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
   tagging->next_tag = read_u32(f);
}

static void cover_read_one_tag(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                               ident_rd_ctx_t ident_ctx, cover_tag_t *tag)
{
   tag->kind = read_u8(f);

   tag->tag = read_u32(f);
   tag->data = read_u32(f);
   tag->flags = read_u32(f);
   tag->tree_kind = read_u32(f);
   tag->unrc_msk = read_u32(f);
   tag->excl_msk = 0;

   loc_read(&(tag->loc), loc_rd);
   if (tag->flags & COVER_FLAGS_LHS_RHS_BINS) {
      loc_read(&(tag->loc_lhs), loc_rd);
      loc_read(&(tag->loc_rhs), loc_rd);
   }

   tag->hier = ident_read(ident_ctx);
   if (tag->kind == TAG_EXPRESSION || tag->kind == TAG_STATE)
      tag->func_name = ident_read(ident_ctx);

   tag->num = read_u32(f);
}

static cover_scope_t *cover_read_scope(fbuf_t *f, ident_rd_ctx_t ident_ctx,
                                       loc_rd_ctx_t *loc_ctx)
{
   cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
   s->type = fbuf_get_uint(f);
   s->name = ident_read(ident_ctx);
   s->hier = ident_read(ident_ctx);

   loc_read(&s->loc, loc_ctx);

   if (s->type == CSCOPE_INSTANCE)
      s->block_name = ident_read(ident_ctx);

   const int ntags = fbuf_get_uint(f);
   for (int i = 0; i < ntags; i++) {
      cover_tag_t new;
      cover_read_one_tag(f, loc_ctx, ident_ctx, &new);

      APUSH(s->tags, new);
   }

   for (;;) {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         {
            cover_scope_t *child = cover_read_scope(f, ident_ctx, loc_ctx);
            list_add(&s->children, child);
         }
         break;
      case CTRL_POP_SCOPE:
         return s;
      default:
         fatal_trace("invalid control word %x in cover db", ctrl);
      }
   }
}

cover_tagging_t *cover_read_tags(fbuf_t *f, uint32_t pre_mask)
{
   cover_tagging_t *tagging = xcalloc(sizeof(cover_tagging_t));
   cover_read_header(f, tagging);
   tagging->mask |= pre_mask;

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         tagging->root_scope = cover_read_scope(f, ident_ctx, loc_rd);
         break;
      case CTRL_END_OF_FILE:
         eof = true;
         break;
      default:
         fatal_trace("invalid control word %x in cover db", ctrl);
      }
   } while (!eof);

   ident_read_end(ident_ctx);
   loc_read_end(loc_rd);
   return tagging;
}

static void cover_merge_scope(cover_scope_t *old_s, cover_scope_t *new_s)
{
   // TODO: Could merging be done more efficiently?
   bool found = false;
   for (int i = 0; i < new_s->tags.count; i++) {
      cover_tag_t *new = AREF(new_s->tags, i);

      for (int j = 0; j < old_s->tags.count; j++) {
         cover_tag_t *old = AREF(old_s->tags, j);

         // Compare based on hierarchical path, each
         // statement / branch / signal has unique hierarchical name
         if (new->hier == old->hier) {
            assert(new->kind == old->kind);
#ifdef COVER_DEBUG_MERGE
            printf("Merging coverage tag: %s\n", istr(old->hier));
#endif
            cover_merge_one_tag(old, new->data);

            found = true;
            break;
         }

         // TODO: Append the new tag just before popping hierarchy tag
         //       with longest common prefix of new tag. That will allow to
         //       merge coverage of IPs from different configurations of
         //       generics which form hierarchy differently!
         if (!found)
            warnf("dropping coverage tag: %s", istr(new->hier));
      }
   }

   for (list_iter(cover_scope_t *, new_c, new_s->children)) {
      bool found = false;
      for (list_iter(cover_scope_t *, old_c, old_s->children)) {
         if (new_c->name == old_c->name) {
            cover_merge_scope(old_c, new_c);
            found = true;
            break;
         }
      }

      if (!found)
         warnf("scope %s not found in original database", istr(new_c->name));
   }
}

void cover_merge_tags(fbuf_t *f, cover_tagging_t *tagging)
{
   assert (tagging != NULL);

   cover_read_header(f, tagging);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         {
            cover_scope_t *new = cover_read_scope(f, ident_ctx, loc_rd);
            cover_merge_scope(tagging->root_scope, new);
         }
         break;
      case CTRL_END_OF_FILE:
         eof = true;
         break;
      default:
         fatal_trace("invalid control word %x in cover db", ctrl);
      }
   } while (!eof);

   ident_read_end(ident_ctx);
   loc_read_end(loc_rd);
}

unsigned cover_count_tags(cover_tagging_t *tagging)
{
   if (tagging == NULL)
      return 0;
   else
      return tagging->next_tag;
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
            fatal_at(&ctx.loc, "coverage specification command must "
                               "start with '+' or '-");
         tok++;

         char_array_t *arr;
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
         else if (!strcmp(tok, "fsm_type")) {
            if (include)
               arr = &(tagging->spec->fsm_type_include);
            else
               arr = &(tagging->spec->fsm_type_exclude);
         }
         else
            fatal_at(&ctx.loc, "invalid command: $bold$%s$$", tok);

         char *hier = strtok(NULL, delim);
         if (!hier)
            fatal_at(&ctx.loc, "%s name missing", tok);

         APUSH(*arr, xstrdup(hier));

         char *str = AGET(*arr, arr->count - 1);
         int i = 0;
         while (str[i]) {
            str[i] = toupper_iso88591(str[i]);
            i++;
         }

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
      rt_model_t *m = get_model();                                            \
      const int32_t tag = (uintptr_t)user;                                    \
      int32_t *toggle_mask = get_cover_counter(m, tag);                       \
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

void x_cover_setup_toggle_cb(sig_shared_t *ss, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();
   cover_mask_t op_mask = get_coverage(m)->mask;

   if (is_constant_input(s)) {
      int32_t *toggle_mask = get_cover_counter(m, tag);

      for (int i = 0; i < s->shared.size; i++) {
         // Remember constant driver in run-time data. Unreachable mask not
         // available at run-time.
         (*toggle_mask++) |= COV_FLAG_CONST_DRIVEN;
      }
      return;
   }

   sig_event_fn_t fn = &cover_toggle_cb_0_1;

   if ((op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED) &&
       (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z))
      fn = &cover_toggle_cb_0_1_u_z;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED)
      fn = &cover_toggle_cb_0_1_u;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z)
      fn = &cover_toggle_cb_0_1_z;

   model_set_event_cb(m, s, fn, (void *)(uintptr_t)tag, false);
}

#define READ_STATE(type) tag_index = *((type *)signal_value(s));

static void cover_state_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)
{
   // I-th enum literal is encoded in i-th tag from first tag, that corresponds to enum value.
   int size = signal_size(s);
   int32_t tag_index = 0;
   FOR_ALL_SIZES(size, READ_STATE);

   rt_model_t *m = get_model();
   int32_t *mask = get_cover_counter(m, ((uintptr_t)user) + tag_index);

   *mask |= COV_FLAG_STATE;
}

void x_cover_setup_state_cb(sig_shared_t *ss, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();

   int32_t *mask = get_cover_counter(m, tag);

   // TYPE'left is a default value of enum type that does not
   // cause an event. First tag eeds to be flagged as covered manually.
   *mask |= COV_FLAG_STATE;

   model_set_event_cb(m, s, cover_state_cb, (void *)(uintptr_t)tag, false);
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

   // If bin is not given, exclude all bins of a tag
   if (!bin)
      return allowed;

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

static int cover_exclude_tags(cover_exclude_ctx_t *ctx, cover_tag_t *tag,
                              const char *bin, ident_t hier)
{
   int kind = tag->kind;

   switch (kind) {
   case TAG_STMT:
      if (bin)
         fatal_at(&ctx->loc, "statements do not contain bins, but bin '%s' "
                             "was given for statement: '%s'", bin, istr(hier));

      note_at(&ctx->loc, "excluding statement: '%s'", istr(hier));
      if (tag->data)
         warn_at(&ctx->loc, "statement: '%s' already covered!", istr(hier));

      tag->excl_msk |= 0xFFFFFFFF;
      return 1;

   case TAG_BRANCH:
   case TAG_TOGGLE:
   case TAG_EXPRESSION:
   {
      int32_t bmask = cover_bin_str_to_bmask(ctx, bin, tag);

      char LOCAL *blist = cover_bmask_to_bin_list(bmask);
      note_at(&ctx->loc, "excluding %s: '%s' bins: %s",
               tag_kind_str[tag->kind], istr(hier), blist);

      uint32_t excl_cov = bmask & tag->data;
      if (excl_cov) {
         char LOCAL *blist = cover_bmask_to_bin_list(excl_cov);
         warn_at(&ctx->loc, "%s: '%s' bins: %s already covered!",
                  tag_kind_str[tag->kind], istr(hier), blist);
      }

      tag->excl_msk |= bmask;
      return 1;
   }

   case TAG_STATE:
   {
      int n_states = tag->num;
      cover_tag_t* curr_tag = tag;
      for (int i = 0; i < n_states; i++) {
         ident_t state_name = ident_rfrom(curr_tag->hier, '.');
         if (!bin || !strcmp(istr(state_name), bin)) {
            if (curr_tag->data & COV_FLAG_STATE)
               warn_at(&ctx->loc, "%s: '%s' of '%s' already covered!",
                       tag_kind_str[tag->kind], istr(state_name), istr(hier));
            curr_tag->excl_msk |= COV_FLAG_STATE;
         }
         curr_tag++;
      }

      return n_states;
   }

   default:
      fatal("Unsupported tag kind: %d", kind);
   }

   return 1;
}

static bool cover_exclude_hier(cover_scope_t *s, cover_exclude_ctx_t *ctx,
                               const char *excl_hier, const char *bin)
{
   bool match = false;
   int len = strlen(excl_hier);
   int step;

   for (int i = 0; i < s->tags.count; i += step) {
      cover_tag_t *tag = AREF(s->tags, i);
      ident_t hier = tag->hier;

      // FSM state tags contain bin name as part of hierarchy -> Strip it
      if (tag->kind == TAG_STATE)
         hier = ident_runtil(hier, '.');

      if (ident_glob(hier, excl_hier, len)) {
#ifdef COVER_DEBUG_EXCLUDE
         printf("Applying matching exclude:\n");
         printf("    Tag:        %s\n", istr(hier));
         printf("    Tag flags:  %x\n", tag->flags);
         printf("    Tag data:   %x\n", tag->data);
#endif
         match = true;
         step = cover_exclude_tags(ctx, tag, bin, hier);
      }
      else
         step = 1;
   }

   for (list_iter(cover_scope_t *, it, s->children))
      match |= cover_exclude_hier(it, ctx, excl_hier, bin);

   return match;
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
               excl_hier[i] = toupper_iso88591(excl_hier[i]);
               i++;
            }

            if (!cover_exclude_hier(tagging->root_scope, &ctx, excl_hier, bin))
               warn_at(&ctx.loc, "exluded hierarchy does not match any "
                       "coverage item: '%s'", excl_hier);
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
                                    cover_scope_t *s, const char *title, ...)
{
   fprintf(f, "<!DOCTYPE html>\n"
              "<html>\n"
              "  <head>\n"
              "  <title>");

   va_list ap;
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);

   fprintf(f, "</title>\n"
              "  <style>\n"
              "   header {\n"
              "      padding: 30px;\n"
              "      text-align: center;\n"
              "      font-size: 35px;\n"
              "   }\n"
              "   h2 {\n"
              "      word-wrap: break-word;\n"
              "      width:70%%\n"
              "   }\n"
              "   h3 {\n"
              "      word-wrap: break-word;\n"
              "      margin-bottom: 0px;\n"
              "   }\n"
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
              "   table {\n"
              "     table-layout: fixed;"
              "   }\n"
              "   table, th, td {\n"
              "     border: 2px solid black;\n"
              "     border-collapse: collapse;\n"
              "     word-wrap: break-word;\n"
              "   }\n"
              "   .tabcontent {\n"
              "     display: none;\n"
              "     padding: 0px 0px;\n"
              "     border: 2px solid #ccc;\n"
              "     border-top: none;\n"
              "     word-wrap: break-word;\n"
              "   }\n"
              "   .tab {\n"
              "     overflow: hidden;\n"
              "     border: none;\n"
              "     background-color: none;\n"
              "     margin-left: " MARGIN_LEFT ";\n"
              "     margin-top: 10px;\n"
              "   }\n"
              "   .tab button.active {\n"
              "     background-color: #ccc;\n"
              "   }\n"
              "   .tab button:hover {\n"
              "     background-color: #ddd;\n"
              "   }\n"
              "   .tab button {\n"
              "     background-color: inherit;\n"
              "     float: left;\n"
              "     margin-left: 20px\n"
              "     border: 2px solid black;\n"
              "     cursor: pointer;\n"
              "     padding: 14px 16px;\n"
              "     font-size: 17px;\n"
              "   }\n"
              "   .cbg {\n"
              "     background-color: #bbbbbb;\n"
              "   }\n"
              "   .cbt {\n"
              "     margin-top: 8px;\n"
              "   }\n"
              "   .cbt th {\n"
              "     background-color: #bbbbbb;\n"
              "     text-align: center;\n"
              "    }\n"
              "   .cbt td, .cbt th {\n"
              "     width:50px;\n"
              "     text-align: center;\n"
              "    }\n"
              "   .cbt td + td, .cbt th + th { width:150px; }\n"
              "   .cbt td + td + td, .cbt th + th + th { width:150px; }\n"
              "   .cbt td + td + td + td, .cbt th + th + th + th { width:150px; }\n"
              "  </style>\n"
              "  </head>\n"
              "  <section>\n\n");

   if (!top) {
      fprintf(f, "<nav>\n"
                 "<b>Hierarchy:</b><br>\n");
      int offset = 0;

      ident_t full_hier = s->hier;
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
      fprintf(f, "</nav>\n\n");
   }

   fprintf(f, "<header>");
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);
   fprintf(f, "</header>\n\n");

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "   Instance:&nbsp;%s\n", istr(s->hier));
   else
      fprintf(f, "   Instance:");
   fprintf(f, "</h2>\n\n");

   cover_file_t *src = cover_file(&(s->loc));
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "   File:&nbsp; <a href=\"../../%s\">../../%s</a>\n",
               src->name, src->name);
   else
      fprintf(f, "   File:");
   fprintf(f, "</h2>\n\n");
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

      fprintf(f, "    <td bgcolor=%s>%.1f %% (%d/%d)</td>\n",
              color, perc, hit, total);
      return;
   }

   fprintf(f, "    <td bgcolor=#aaaaaa>N.A.</td>\n");
}

static void cover_print_hierarchy_header(FILE *f)
{
   fprintf(f, "<table style=\"width:70%%;margin-left:" MARGIN_LEFT ";margin-right:auto;\"> \n"
              "  <tr>\n"
              "    <th class=\"cbg\" style=\"width:30%%\">Instance</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Statement</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Branch</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Toggle</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Expression</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">FSM state</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Average</th>\n"
              "  </tr>\n");
}

static void cover_print_hierarchy_footer(FILE *f)
{
   fprintf(f, "</table>\n\n");
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

   fprintf(f, "  <tr>\n"
              "    <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(hier), istr(print_hier));

   cover_print_percents_cell(f, stats->hit_stmts, stats->total_stmts);
   cover_print_percents_cell(f, stats->hit_branches, stats->total_branches);
   cover_print_percents_cell(f, stats->hit_toggles, stats->total_toggles);
   cover_print_percents_cell(f, stats->hit_expressions, stats->total_expressions);
   cover_print_percents_cell(f, stats->hit_states, stats->total_states);

   int avg_total = stats->total_stmts + stats->total_branches +
                   stats->total_toggles + stats->total_expressions +
                   stats->total_states;
   int avg_hit = stats->hit_stmts + stats->hit_branches +
                 stats->hit_toggles + stats->hit_expressions +
                 stats->hit_states;

   cover_print_percents_cell(f, avg_hit, avg_total);

   fprintf(f, "  </tr>\n");

   float perc_stmt = 0.0f;
   float perc_branch = 0.0f;
   float perc_toggle = 0.0f;
   float perc_expr = 0.0f;
   float perc_state = 0.0f;

   if (stats->total_stmts > 0)
      perc_stmt = 100.0 * ((float)stats->hit_stmts) / stats->total_stmts;
   if (stats->total_branches > 0)
      perc_branch = 100.0 * ((float)stats->hit_branches) / stats->total_branches;
   if (stats->total_toggles > 0)
      perc_toggle = 100.0 * ((float)stats->hit_toggles) / stats->total_toggles;
   if (stats->total_expressions > 0)
      perc_expr = 100.0 * ((float)stats->hit_expressions) / stats->total_expressions;
   if (stats->total_states > 0)
      perc_state = 100.0 * ((float)stats->hit_states) / stats->total_states;

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

      if (perc_state > 0)
         notef("     FSM state:     %.1f %% (%d/%d)", perc_state,
               stats->hit_states, stats->total_states);
      else
         notef("     FSM state:     N.A.");
   }
   else if (opt_get_int(OPT_VERBOSE) && !flat) {

      cover_rpt_buf_t *new = xcalloc(sizeof(cover_rpt_buf_t));
      new->tb = tb_new();
      new->prev = ctx->tagging->rpt_buf;
      ctx->tagging->rpt_buf = new;

      tb_printf(new->tb,
         "%*s %-*s %10.1f %% (%d/%d)  %10.1f %% (%d/%d) %10.1f %% (%d/%d) "
         "%10.1f %% (%d/%d) %10.1f %% (%d/%d)",
         ctx->lvl, "", 50-ctx->lvl, istr(ident_rfrom(hier, '.')),
         perc_stmt, stats->hit_stmts, stats->total_stmts,
         perc_branch, stats->hit_branches, stats->total_branches,
         perc_toggle, stats->hit_toggles, stats->total_toggles,
         perc_expr, stats->hit_expressions, stats->total_expressions,
         perc_state, stats->hit_states, stats->total_states);
   }
}

static inline void cover_print_char(FILE *f, char c)
{
   if (c == '\n' || c == ' ')
      fprintf(f, "&nbsp");
   else
      fprintf(f, "%c", c);
}

static void cover_print_single_code_line(FILE *f, loc_t loc, cover_line_t *line)
{
   assert(loc.line_delta == 0);

   int curr_char = 0;
   while (curr_char < strlen(line->text)) {

      // Highlight code location
      if (curr_char == loc.first_column)
         fprintf(f, "<code class=\"cbg\">");

      cover_print_char(f, line->text[curr_char]);

      // Finish code highlight
      if (curr_char == (loc.first_column + loc.column_delta))
         fprintf(f, "</code>");

      curr_char++;
   }
}

static void cover_print_lhs_rhs_arrows(FILE *f, cover_pair_t *pair)
{
   int last = strlen(pair->line->text);
   int curr = 0;

   // Offset by line number width
   int digits = pair->tag->loc.first_line;
   do {
      digits /= 10;
      fprintf(f, "&nbsp");
   } while (digits > 0);
   fprintf(f, "&nbsp");

   loc_t *loc_lhs = &(pair->tag->loc_lhs);
   loc_t *loc_rhs = &(pair->tag->loc_rhs);

   int lhs_beg = loc_lhs->first_column;
   int rhs_beg = loc_rhs->first_column;

   int lhs_end = lhs_beg + loc_lhs->column_delta;
   int rhs_end = rhs_beg + loc_rhs->column_delta;

   int lhs_mid = (lhs_end + lhs_beg) / 2;
   int rhs_mid = (rhs_end + rhs_beg) / 2;

   while (curr < last) {
      if (curr == lhs_mid - 1)
         fprintf(f, "L");
      else if (curr == lhs_mid)
         fprintf(f, "H");
      else if (curr == lhs_mid + 1)
         fprintf(f, "S");

      else if (curr == lhs_beg)
         fprintf(f, "<");
      else if (curr > lhs_beg && curr < lhs_end)
         fprintf(f, "-");
      else if (curr == (lhs_beg + loc_lhs->column_delta))
         fprintf(f, ">");

      else if (curr == rhs_mid - 1)
         fprintf(f, "R");
      else if (curr == rhs_mid)
         fprintf(f, "H");
      else if (curr == rhs_mid + 1)
         fprintf(f, "S");

      else if (curr == rhs_beg)
         fprintf(f, "<");
      else if (curr > rhs_beg && curr < rhs_end)
         fprintf(f, "-");
      else if (curr == (rhs_beg + loc_rhs->column_delta))
         fprintf(f, ">");

      else
         fprintf(f, "&nbsp");

      curr++;
   }
}

static void cover_print_tag_title(FILE *f, cover_pair_t *pair)
{
   tree_kind_t kind = pair->tag->tree_kind;

   fprintf(f, "<h3>");

   switch (pair->tag->kind) {
   case TAG_STMT:
      fprintf(f, "\"%s\" ", tree_kind_str(kind));
      fprintf(f, "statement:");
      break;
   case TAG_BRANCH:
      switch (kind) {
      case T_COND_STMT:
      case T_COND_ASSIGN:
         fprintf(f, "\"if\" / \"when\" / \"else\" condition:");
         break;
      case T_ASSOC:
         fprintf(f, "\"case\" / \"with\" / \"select\" choice:");
         break;
      case T_WHILE:
      case T_EXIT:
      case T_NEXT:
         fprintf(f, "Loop control condition:");
         break;
      default:
         fprintf(f, "Condition:");
         break;
      };
      break;
   case TAG_EXPRESSION:
      fprintf(f, "%s expression", istr(pair->tag->func_name));
      break;
   case TAG_STATE:
      fprintf(f, "\"%s\" FSM with total %d states", istr(pair->tag->func_name),
              pair->tag->num);
      break;
   default:
      break;
   }
   fprintf(f, "</h3>");
}

static void cover_print_code_loc(FILE *f, cover_pair_t *pair)
{
   loc_t loc = pair->tag->loc;
   cover_line_t *curr_line = pair->line;
   cover_line_t *last_line = pair->line + loc.line_delta;

   cover_print_tag_title(f, pair);

   if (loc.line_delta == 0) {
      fprintf(f, "<code>");
      fprintf(f, "%d:", loc.first_line);

      cover_print_single_code_line(f, loc, curr_line);
      if (pair->tag->flags & COVER_FLAGS_LHS_RHS_BINS) {
         fprintf(f, "<br>");
         cover_print_lhs_rhs_arrows(f, pair);
      }
      fprintf(f, "</code>");
   }
   else {
      fprintf(f, "<code>");

      do {
         // Shorten code samples longer than 5 lines
         if (loc.line_delta > 5 &&
             curr_line == pair->line + 2) {
            fprintf(f, "...<br>");
            curr_line = last_line - 1;
            continue;
         }
         else
            fprintf(f, "%lu:", loc.first_line + (curr_line - pair->line));

         int curr_char = 0;
         while (curr_char < curr_line->len) {
            cover_print_char(f, curr_line->text[curr_char]);
            curr_char++;
         }

         fprintf(f, "<br>");
         curr_line++;

      } while (curr_line <= last_line);

      fprintf(f, "</code>");
   }
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

   ident_t hier = tag->hier;

   // State coverage contains bin name (state name) appended to hierarchical path
   if (tag->kind == TAG_STATE) {
      bin_name = istr(ident_rfrom(hier, '.'));
      hier = ident_runtil(hier, '.');
   }

   fprintf(f, "<button onclick=\"GetExclude('exclude %s %s')\" %s>"
              "Copy %sto Clipoard</button>", istr(hier), bin_name,
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
         const char *er = (flag & pair->tag->unrc_msk) ?
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

   fprintf(f, "<tr><th></th>");

   for (int i = 0; i < cols; i++) {
      const char *val = va_arg(argp, const char *);
      fprintf(f, "<th>%s</th>", val);
   }

   if (pkind == PAIR_UNCOVERED)
      fprintf(f, "<th>Exclude Command</th>");

   if (pkind == PAIR_EXCLUDED)
      fprintf(f, "<th>Excluded due to</th>");

   fprintf(f, "</tr>");
}

static void cover_print_bins(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind)
{
   loc_t loc = pair->tag->loc;

   fprintf(f, "<br><table class=\"cbt\">");

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


static int cover_print_fsm_table(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind,
                                 cover_pair_t *last)
{
   // All pairs of a single FSM are consequent. Group them together and print them
   // in a single table.
   ident_t first_prefix = ident_runtil(pair->tag->hier, '.');

   fprintf(f, "<br><table class=\"cbt\">");

   cover_print_bin_header(f, pkind, 1, "State");

   int n_pairs = 0;
   do {
      ident_t state_name = ident_rfrom(pair->tag->hier, '.');
      cover_print_bin(f, pair, COV_FLAG_STATE, pkind, 1, istr(state_name));
      n_pairs++;

      // End of the chain was hit
      if (pair == last)
         break;
      pair++;

      // A tag with different prefix -> Not the same FSM.
      ident_t curr_prefix = ident_runtil(pair->tag->hier, '.');
      if (!ident_starts_with(curr_prefix, first_prefix))
         break;

   } while (1);

   fprintf(f, "</table>");

   return n_pairs;
}


static void cover_print_pairs(FILE *f, cover_pair_t *first, cov_pair_kind_t pkind,
                              int pair_cnt)
{
   if (pair_cnt == 0)
      return;

   cover_pair_t *last = first + pair_cnt - 1;
   cover_pair_t *curr = first;
   int step;

   do {
      step = 1;
      fprintf(f, "    <p>");

      switch (curr->tag->kind) {
      case TAG_STMT:
         if (pkind == PAIR_UNCOVERED)
            cover_print_get_exclude_button(f, curr->tag, 0, false);
         if (pkind == PAIR_EXCLUDED) {
            // No un-reachability on statements so far
            assert(curr->tag->unrc_msk == 0);
            fprintf(f, "<div style=\"float: right\"><b>Excluded due to:</b> Exclude file</div>");
         }

         cover_print_code_loc(f, curr);
         break;

      case TAG_BRANCH:
         cover_print_code_loc(f, curr);
         cover_print_bins(f, curr, pkind);
         break;

      case TAG_TOGGLE:
         if (curr->tag->flags & COV_FLAG_TOGGLE_SIGNAL)
            fprintf(f, "<h3>Signal:</h3>");
         else if (curr->tag->flags & COV_FLAG_TOGGLE_PORT)
            fprintf(f, "<h3>Port:</h3>");

         const char *sig_name = istr(curr->tag->hier);
         sig_name += curr->tag->num;
         fprintf(f, "&nbsp;<code>%s</code>", sig_name);

         cover_print_bins(f, curr, pkind);
         break;

      case TAG_EXPRESSION:
         cover_print_code_loc(f, curr);
         cover_print_bins(f, curr, pkind);
         break;

      case TAG_STATE:
         cover_print_code_loc(f, curr);
         step = cover_print_fsm_table(f, curr, pkind, last);
         break;

      default:
         fatal("unsupported type of code coverage: %d !", curr->tag->kind);
      }

      fprintf(f, "<hr>");
      fprintf(f, "</p>\n");

      curr += step;

   } while (curr <= last);
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
   else if (kind == TAG_STATE)
      fprintf(f, "FSM state");

   fprintf(f, "\" class=\"tabcontent\" style=\"width:68.5%%;margin-left:" MARGIN_LEFT "; "
                          "margin-right:auto; margin-top:10px; border: 2px solid black;\">\n");

   for (cov_pair_kind_t pkind = PAIR_UNCOVERED; pkind < PAIR_LAST; pkind++) {
      int n;
      cover_pair_t *first_pair;

      if (pkind == PAIR_UNCOVERED) {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_UNCOVERED))
            continue;
         first_pair = chn->miss;
         n = chn->n_miss;
      }
      else if (pkind == PAIR_EXCLUDED) {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_EXCLUDED))
            continue;
         first_pair = chn->excl;
         n = chn->n_excl;
      }
      else {
         if (cover_enabled(tagging, COVER_MASK_DONT_PRINT_COVERED))
            continue;
         first_pair = chn->hits;
         n = chn->n_hits;
      }

      fprintf(f, "  <section style=\"background-color:");
      if (pkind == PAIR_UNCOVERED)
         fprintf(f, "#ffcccc;\">\n");
      else if (pkind == PAIR_EXCLUDED)
         fprintf(f, "#d6eaf8;\">\n");
      else
         fprintf(f, "#ccffcc;\">\n");

      fprintf(f, " <h2>");
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
      else if (kind == TAG_STATE)
         fprintf(f, "FSM states:");
      fprintf(f, "</h2>\n");

      fprintf(f, "  <section style=\"padding:0px 10px;\">\n");
      cover_print_pairs(f, first_pair, pkind, n);
      fprintf(f, "  </section>\n\n");
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
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'FSM state')\">FSM state</button>\n"
              "</div>\n\n");

   cover_print_chain(f, ctx->tagging, &(ctx->ch_stmt), TAG_STMT);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_branch), TAG_BRANCH);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_toggle), TAG_TOGGLE);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_expression), TAG_EXPRESSION);
   cover_print_chain(f, ctx->tagging, &(ctx->ch_state), TAG_STATE);

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

static bool cover_bin_unreachable(cover_report_ctx_t *ctx, cover_tag_t *tag,
                                  unsigned flag)
{
   if ((ctx->tagging->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Toggle tags remember unreachability in run-time data. Must check tag kind
   // not to get false unreachability on statement tags. Excludes both bins
   // automatically!
   if (tag->kind == TAG_TOGGLE && ((tag->data & COV_FLAG_CONST_DRIVEN) != 0))
      return true;

   // Expression tags remembed unreachability via dedicated mask
   if (tag->kind == TAG_EXPRESSION && ((flag & tag->unrc_msk) != 0))
      return true;

   return false;
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
   case TAG_STATE:
      flat_total = &(ctx->flat_stats.total_states);
      nested_total = &(ctx->nested_stats.total_states);
      flat_hits = &(ctx->flat_stats.hit_states);
      nested_hits = &(ctx->nested_stats.hit_states);
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
   else if ((tag->excl_msk & flag) || cover_bin_unreachable(ctx, tag, flag)) {
      (*flat_hits)++;
      (*nested_hits)++;
      (*excludes) |= flag;
   }
   else
      (*misses) |= flag;

}


static void cover_report_scope(cover_report_ctx_t *ctx,
                               cover_scope_t *s, const char *dir,
                               FILE *summf, int *skipped)
{
   for (int i = 0; i < s->tags.count; i++) {
      cover_tag_t *tag = &(s->tags.items[i]);

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
         *skipped += cover_append_to_chain(&(ctx->ch_stmt), tag, line,
                                           hits, misses, excludes, limit);
         break;

      case TAG_BRANCH:
         // If/else, when else
         if (tag->flags & COV_FLAG_TRUE && tag->flags & COV_FLAG_FALSE) {
            cover_tag_to_chain(ctx, tag, COV_FLAG_TRUE,
                               &hits, &misses, &excludes);
            cover_tag_to_chain(ctx, tag, COV_FLAG_FALSE,
                               &hits, &misses, &excludes);
            *skipped += cover_append_to_chain(&(ctx->ch_branch), tag, line,
                                              hits, misses, excludes, limit);
         }

         // Case, with select
         if (tag->flags & COV_FLAG_CHOICE) {
            cover_tag_to_chain(ctx, tag, COV_FLAG_CHOICE,
                               &hits, &misses, &excludes);
            *skipped += cover_append_to_chain(&(ctx->ch_branch), tag, line,
                                              hits, misses, excludes, limit);
         }
         break;

      case TAG_TOGGLE:
         cover_tag_to_chain(ctx, tag, COV_FLAG_TOGGLE_TO_1,
                            &hits, &misses, &excludes);
         cover_tag_to_chain(ctx, tag, COV_FLAG_TOGGLE_TO_0,
                            &hits, &misses, &excludes);
         *skipped += cover_append_to_chain(&(ctx->ch_toggle), tag, line,
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

         *skipped += cover_append_to_chain(&(ctx->ch_expression), tag, line,
                                           hits, misses, excludes, limit);
         break;

      case TAG_STATE:
         cover_tag_to_chain(ctx, tag, COV_FLAG_STATE, &hits, &misses, &excludes);
         *skipped += cover_append_to_chain(&(ctx->ch_state), tag, line,
                                           hits, misses, excludes, limit);
         break;

      default:
         fatal("unsupported type of code coverage: %d !", tag->kind);
      }
   }

   cover_report_children(ctx, s, dir, summf, skipped);
}

static void cover_report_hierarchy(cover_report_ctx_t *ctx,
                                   cover_scope_t *s, const char *dir)
{
   char *hier LOCAL = xasprintf("%s/%s.html", dir, istr(s->hier));

   // TODO: Handle escaped identifiers in hierarchy path!
   FILE *f = fopen(hier, "w");
   if (f == NULL)
      fatal("failed to open report file: %s\n", hier);

   INIT_CHAIN(ctx, ch_stmt);
   INIT_CHAIN(ctx, ch_branch);
   INIT_CHAIN(ctx, ch_toggle);
   INIT_CHAIN(ctx, ch_expression);
   INIT_CHAIN(ctx, ch_state);

   cover_print_html_header(f, ctx, false, s, "NVC code coverage report");

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Sub-instances:\n</h2>\n\n");
   cover_print_hierarchy_header(f);

   int skipped = 0;
   cover_report_children(ctx, s, dir, f, &skipped);

   cover_print_hierarchy_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current Instance:\n</h2>\n\n");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, ctx, s->hier, false, true, true);
   cover_print_hierarchy_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h3>\n\n", ctx->tagging->report_item_limit, skipped);
   cover_print_hierarchy_guts(f, ctx);
   cover_print_timestamp(f);

   fclose(f);
}

static void cover_report_children(cover_report_ctx_t *ctx,
                                  cover_scope_t *s, const char *dir,
                                  FILE *summf, int *skipped)
{
   for (list_iter(cover_scope_t *, it, s->children)) {
      if (it->type == CSCOPE_INSTANCE) {
         // Collect coverage of sub-block
         cover_report_ctx_t sub_ctx = {};
         sub_ctx.parent = ctx;
         sub_ctx.tagging = ctx->tagging;
         sub_ctx.lvl = ctx->lvl + 2;

         cover_report_hierarchy(&sub_ctx, it, dir);

         cover_print_hierarchy_summary(summf, &sub_ctx,
                                       it->hier, false, false, false);

         // Add coverage from sub-hierarchies
         ctx->nested_stats.hit_stmts += sub_ctx.nested_stats.hit_stmts;
         ctx->nested_stats.total_stmts += sub_ctx.nested_stats.total_stmts;
         ctx->nested_stats.hit_branches += sub_ctx.nested_stats.hit_branches;
         ctx->nested_stats.total_branches += sub_ctx.nested_stats.total_branches;
         ctx->nested_stats.hit_toggles += sub_ctx.nested_stats.hit_toggles;
         ctx->nested_stats.total_toggles += sub_ctx.nested_stats.total_toggles;
         ctx->nested_stats.hit_expressions += sub_ctx.nested_stats.hit_expressions;
         ctx->nested_stats.total_expressions += sub_ctx.nested_stats.total_expressions;
         ctx->nested_stats.hit_states += sub_ctx.nested_stats.hit_states;
         ctx->nested_stats.total_states += sub_ctx.nested_stats.total_states;
      }
      else
         cover_report_scope(ctx, it, dir, summf, skipped);
   }
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

   cover_scope_t *child0 = list_get(tagging->root_scope->children, 0);

   cover_report_ctx_t top_ctx = {};
   top_ctx.tagging = tagging;
   tagging->report_item_limit = item_limit;
   cover_report_hierarchy(&top_ctx, child0, subdir);

   char *top LOCAL = xasprintf("%s/index.html", path);
   FILE *f = fopen(top, "w");

   cover_print_html_header(f, &top_ctx, true, child0, "NVC code coverage report");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, &top_ctx, child0->hier, true, true, false);
   cover_print_hierarchy_footer(f);
   cover_print_timestamp(f);

   if (opt_get_int(OPT_VERBOSE)) {
      notef("Coverage for sub-hierarchies:");
      printf("%-55s %-20s %-20s %-20s %-20s %-20s\n",
             "Hierarchy", "Statement", "Branch", "Toggle", "Expression", "FSM state");
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

////////////////////////////////////////////////////////////////////////////////
// Cobertura XML export

typedef struct _cobertura_class cobertura_class_t;

typedef struct {
   unsigned lineno;
   unsigned hits;
   bool     branch;
   unsigned bflags;
} cobertura_line_t;

typedef struct _cobertura_class {
   const char        *file;
   ident_t            name;
   cobertura_class_t *next;
   unsigned           nlines;
   unsigned           maxlines;
   cobertura_line_t  *lines;
} cobertura_class_t;

typedef struct {
   hash_t            *class_map;
   cobertura_class_t *classes;
   char              *relative;
} cobertura_report_t;

static cobertura_class_t *cobertura_get_class(cobertura_report_t *report,
                                              ident_t name, const loc_t *loc)
{
   cobertura_class_t *c = hash_get(report->class_map, name);
   if (c == NULL) {
      c = xcalloc(sizeof(cobertura_class_t));
      c->name = name;
      c->file = loc_file_str(loc);
      c->next = report->classes;

      if (is_absolute_path(c->file) && report->relative != NULL) {
         const size_t rlen = strlen(report->relative);
         if (strncmp(report->relative, c->file, rlen) == 0) {
            c->file += rlen;
            while (c->file[0] == DIR_SEP[0] || c->file[0] == '/')
               c->file++;
         }
      }

      report->classes = c;
      hash_put(report->class_map, name, c);
   }

   return c;
}

static cobertura_line_t *cobertura_get_line(cobertura_class_t *class,
                                            const loc_t *loc)
{
   if (class->nlines > 0) {
      cobertura_line_t *last = &(class->lines[class->nlines - 1]);
      if (last->lineno == loc->first_line)  // Most likely
         return last;

      for (int i = 0; i < class->nlines - 1; i++) {
         cobertura_line_t *line = &(class->lines[i]);
         if (line->lineno == loc->first_line)
            return line;
      }
   }

   if (class->nlines == class->maxlines) {
      class->maxlines = MAX(class->maxlines * 2, 100);
      class->lines = xrealloc_array(class->lines, class->maxlines,
                                    sizeof(cobertura_line_t));
   }

   cobertura_line_t *line = &(class->lines[class->nlines++]);
   memset(line, '\0', sizeof(cobertura_line_t));
   line->lineno = loc->first_line;
   return line;
}

static void cobertura_export_scope(cobertura_report_t *report,
                                   cobertura_class_t *class,
                                   cover_scope_t *s)
{
   if (s->block_name != NULL)
      class = cobertura_get_class(report, s->block_name, &s->loc);

   for (int i = 0; i < s->tags.count; i++) {
      cover_tag_t *t = &(s->tags.items[i]);
      switch (t->kind) {
      case TAG_STMT:
         {
            cobertura_line_t *l = cobertura_get_line(class, &(t->loc));
            l->hits += t->data;
         }
         break;
      case TAG_BRANCH:
         {
            cobertura_line_t *l = cobertura_get_line(class, &(t->loc));
            l->branch = true;
            l->bflags |= t->data;
         }
         break;
      default:
         break;
      }
   }

   for (list_iter(cover_scope_t *, it, s->children))
      cobertura_export_scope(report, class, it);
}

static void cobertura_class_stats(const cobertura_class_t *class,
                                  int *nlines, int *hitlines,
                                  int *nbranches, int *hitbranches)
{
   *nlines += class->nlines;
   for (int i = 0; i < class->nlines; i++) {
      const cobertura_line_t *line = &(class->lines[i]);
      if (line->hits > 0)
         (*hitlines)++;
      if (line->branch) {
         (*nbranches)++;
         if ((line->bflags & COV_FLAG_TRUE) && (line->bflags & COV_FLAG_FALSE))
             (*hitbranches)++;
      }
   }
}

static void cobertura_print_class(cobertura_class_t *class, FILE *f)
{
   ident_t ename = ident_until(class->name, '-');
   ident_t aname = ident_from(class->name, '-');

   int nlines = 0, hitlines = 0, nbranches = 0, hitbranches = 0;
   cobertura_class_stats(class, &nlines, &hitlines, &nbranches, &hitbranches);

   const double line_rate = (double)hitlines / (double)nlines;
   const double branch_rate = (double)hitbranches / (double)nbranches;

   fprintf(f, "<class name=\"%s(%s)\" filename=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" >\n",
           istr(ename), istr(aname), class->file, line_rate, branch_rate);
   fprintf(f, "<methods/>\n");

   fprintf(f, "<lines>\n");
   for (int i = 0; i < class->nlines; i++) {
      const cobertura_line_t *line = &(class->lines[i]);
      if (line->branch) {
         int pct = 0;
         if (line->bflags & COV_FLAG_TRUE) pct += 50;
         if (line->bflags & COV_FLAG_FALSE) pct += 50;

         fprintf(f, "<line number=\"%d\" hits=\"%d\" branch=\"true\" "
                 "condition-coverage=\"%d %%\">\n",
                 line->lineno, line->hits, pct);
         fprintf(f, "<conditions>\n");
         fprintf(f, "<condition number=\"0\" type=\"jump\" "
                 "coverage=\"%d %%\"/>\n", pct);
         fprintf(f, "</conditions>\n");
         fprintf(f, "</line>\n");
      }
      else
         fprintf(f, "<line number=\"%d\" hits=\"%d\" branch=\"false\"/>\n",
                 line->lineno, line->hits);
   }
   fprintf(f, "</lines>\n");

   fprintf(f, "</class>\n");
}

void cover_export_cobertura(cover_tagging_t *tagging, FILE *f,
                            const char *relative)
{
   cobertura_report_t report = {
      .class_map = hash_new(64),
      .relative = relative ? realpath(relative, NULL) : NULL,
   };

   cobertura_export_scope(&report, NULL, tagging->root_scope);

   fprintf(f, "<?xml version='1.0' encoding='UTF-8'?>\n");
   fprintf(f, "<!DOCTYPE coverage SYSTEM "
           "'http://cobertura.sourceforge.net/xml/coverage-04.dtd'>\n");

   int nlines = 0, hitlines = 0, nbranches = 0, hitbranches = 0;
   for (cobertura_class_t *it = report.classes; it; it = it->next)
      cobertura_class_stats(it, &nlines, &hitlines, &nbranches, &hitbranches);

   const double line_rate = (double)hitlines / (double)nlines;
   const double branch_rate = (double)hitbranches / (double)nbranches;

   unsigned long timestamp;
   const long override_time = opt_get_int(OPT_COVER_TIMESTAMP);
   if (override_time >= 0)
      timestamp = override_time;
   else
      timestamp = time(NULL);

   fprintf(f, "<coverage version=\"" PACKAGE_STRING "\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" "
           "lines-valid=\"%d\" lines-covered=\"%d\" "
           "branches-valid=\"%d\" branches-covered=\"%d\" "
           "timestamp=\"%lu\">\n",
           line_rate, branch_rate, nlines, hitlines, nbranches, hitbranches,
           timestamp);
   fprintf(f, "<sources>\n");
   fprintf(f, "<source>.</source>\n");
   fprintf(f, "</sources>\n");
   fprintf(f, "<packages>\n");
   fprintf(f, "<package name=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\">\n",
           istr(tagging->root_scope->name), line_rate, branch_rate);

   fprintf(f, "<classes>\n");
   for (cobertura_class_t *it = report.classes; it; it = it->next)
      cobertura_print_class(it, f);
   fprintf(f, "</classes>\n");

   fprintf(f, "</package>\n");
   fprintf(f, "</packages>\n");
   fprintf(f, "</coverage>\n");

   for (cobertura_class_t *it = report.classes, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it->lines);
      free(it);
   }

   free(report.relative);
   hash_free(report.class_map);
}
