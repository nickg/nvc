//
//  Copyright (C) 2013-2024  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-data.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "option.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>
#include <inttypes.h>

//#define COVER_DEBUG_EMIT
//#define COVER_DEBUG_DUMP
//#define COVER_DEBUG_SCOPE
//#define COVER_DEBUG_MERGE

typedef enum {
   CTRL_PUSH_SCOPE,
   CTRL_POP_SCOPE,
   CTRL_END_OF_FILE,
} cov_control_t;

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

bool cover_is_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
   case T_RETURN:
   case T_FOR:
   case T_PCALL:
   case T_PROT_PCALL:
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

bool cover_skip_array_toggle(cover_data_t *data, int a_size)
{
   assert (data);

   // Array is equal to or than configured limit
   if (data->array_limit != 0 && a_size >= data->array_limit)
      return true;

   // Array is multi-dimensional or nested
   if ((!cover_enabled(data, COVER_MASK_TOGGLE_INCLUDE_MEMS)) && data->array_depth > 0)
      return true;

   return false;
}

bool cover_skip_type_state(cover_data_t *data, type_t type)
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
   cover_spec_t *spc = data->spec;

   // Type should be included
   if (spc) {
      for (int i = 0; i < spc->fsm_type_include.count; i++)
         if (ident_glob(name, AGET(spc->fsm_type_include, i), -1)) {
#ifdef COVER_DEBUG_EMIT
            printf("Cover emit: True, fsm-type (Type: %s, Pattern: %s)\n",
                  istr(name), AGET(spc->fsm_type_include, i));
#endif
            return false;
         }
   }

   // By default enums should not included
   if (data->mask & COVER_MASK_FSM_NO_DEFAULT_ENUMS)
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

static cover_src_t get_cover_source(cover_item_kind_t kind, object_t *obj)
{
   tree_t t = tree_from_object(obj);
   if (t != NULL) {
      switch (kind) {
      case COV_ITEM_STMT:
         switch (tree_kind(t)) {
         case T_ASSERT:
            return tree_has_value(t) ? COV_SRC_ASSERT : COV_SRC_REPORT;
         case T_WAIT:
            return COV_SRC_WAIT;
         case T_FOR:
         case T_WHILE:
            return COV_SRC_LOOP_STMT;
         case T_SIGNAL_ASSIGN:
            return COV_SRC_SIGNAL_ASSIGN;
         case T_VAR_ASSIGN:
            return COV_SRC_VAR_ASSIGN;
         case T_IF:
            return COV_SRC_IF_STMT;
         default:
            return COV_SRC_STATEMENT;
         }
      case COV_ITEM_BRANCH:
         switch (tree_kind(t)) {
         case T_COND_STMT:
         case T_COND_ASSIGN:
            return COV_SRC_IF_CONDITION;
         case T_ASSOC:
            return COV_SRC_CASE_CHOICE;
         case T_WHILE:
         case T_EXIT:
         case T_NEXT:
            return COV_SRC_LOOP_CONTROL;
         default:
            return COV_SRC_CONDITION;
         }
      default:
         return COV_SRC_UNKNOWN;
      }
   }

   return COV_SRC_UNKNOWN;
}

const loc_t *get_cover_loc(cover_item_kind_t kind, object_t *obj)
{
   tree_t t = tree_from_object(obj);
   if (t != NULL) {
      // Refer location of test condition instead of branch statement to
      // get accurate test condition location in the coverage report
      if (kind == COV_ITEM_BRANCH && tree_kind(t) != T_ASSOC)
         return tree_loc(tree_value(t));
   }

   return &(obj->loc);
}

cover_item_t *cover_add_item(cover_data_t *data, object_t *obj, ident_t suffix,
                             cover_item_kind_t kind, uint32_t flags)
{
   assert(data != NULL);

   if (!data->top_scope->emit)
      return NULL;

   cover_scope_t *ignore_scope = data->top_scope;
   for (; ignore_scope->type != CSCOPE_INSTANCE && ignore_scope->parent;
        ignore_scope = ignore_scope->parent)
      ;

   const loc_t *loc = get_cover_loc(kind, obj);

   for (int i = 0; i < ignore_scope->ignore_lines.count; i++) {
      line_range_t *lr = &(ignore_scope->ignore_lines.items[i]);
      if (loc->first_line > lr->start && loc->first_line <= lr->end)
         return NULL;
   }

   // Everything creates scope, so name of current item is already given
   // by scope in hierarchy.
   ident_t hier = data->top_scope->hier;
   if (suffix)
      hier = ident_prefix(hier, suffix, '\0');

   ident_t func_name = NULL;
   loc_t loc_lhs = LOC_INVALID, loc_rhs = LOC_INVALID;

   // Expression items do not nest scope, expression name must be created
   if (kind == COV_ITEM_EXPRESSION) {
      char buf[16];
      checked_sprintf(buf, sizeof(buf), "_E%d", data->top_scope->expression_label);
      hier = ident_prefix(hier, ident_new(buf), '.');
      data->top_scope->expression_label++;

      tree_t t = tree_from_object(obj);
      assert(t != NULL);

      // Query LHS/RHS operand locations of binary expressions
      if (flags & COVER_FLAGS_LHS_RHS_BINS) {
         assert(tree_params(t) > 1);
         loc_lhs = *tree_loc(tree_param(t, 0));
         loc_rhs = *tree_loc(tree_param(t, 1));
      }

      func_name = tree_ident(t);
   }

#ifdef COVER_DEBUG_EMIT
   printf("Item: %s\n", istr(hier));
   printf("    First line: %d\n", loc->first_line);
   printf("    First column: %d\n", loc->first_column);
   printf("    Line delta: %d\n", loc->line_delta);
   printf("    Column delta: %d\n", loc->column_delta);
   printf("\n\n");
#endif

   int num = 0;
   if (kind == COV_ITEM_STATE) {
      tree_t t = tree_from_object(obj);
      assert(t != NULL);

      type_t enum_type = tree_type(t);
      assert(type_is_enum(enum_type));
      int64_t low, high;
      folded_bounds(range_of(enum_type, 0), &low, &high);
      num = high - low + 1;

      func_name = ident_rfrom(type_ident(tree_type(t)), '.');
   }
   else
      num = data->top_scope->sig_pos;

   cover_item_t new = {
      .kind       = kind,
      .tag        = data->next_tag++,
      .data       = 0,
      .flags      = flags,
      .excl_msk   = 0,
      .unrc_msk   = 0,
      .loc        = *loc,
      .loc_lhs    = loc_lhs,
      .loc_rhs    = loc_rhs,
      .hier       = hier,
      .func_name  = func_name,
      .num        = num,
      .source     = get_cover_source(kind, obj),
   };

   APUSH(data->top_scope->items, new);

   return AREF(data->top_scope->items, data->top_scope->items.count - 1);
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

   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = &(s->items.items[i]);

      char *path LOCAL = xstrdup(loc_file_str(&item->loc));
      printf("%*s%d: %s %s %s:%d => %x\n", indent + 2, "", item->tag,
             cover_item_kind_str(item->kind), istr(item->hier),
             basename(path), item->loc.first_line, item->data);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_debug_dump(s->children.items[i], indent + 2);
}
LCOV_EXCL_STOP

static void cover_merge_one_item(cover_item_t *item, int32_t data)
{
   switch (item->kind) {
   case COV_ITEM_STMT:
   case COV_ITEM_FUNCTIONAL:
   case COV_ITEM_BRANCH:
      item->data += data;
      break;
   case COV_ITEM_TOGGLE:
   case COV_ITEM_EXPRESSION:
   case COV_ITEM_STATE:
      item->data |= data;
      break;
   default:
      break;
   }
}

static void cover_update_counts(cover_scope_t *s, const int32_t *counts)
{
   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = &(s->items.items[i]);

      const int32_t data = counts ? counts[item->tag] : 0;
      cover_merge_one_item(item, data);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_update_counts(s->children.items[i], counts);
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

   fbuf_put_uint(f, s->items.count);
   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = &(s->items.items[i]);

      write_u8(item->kind, f);
      write_u32(item->tag, f);
      write_u32(item->data, f);
      write_u32(item->flags, f);
      fbuf_put_uint(f, item->source);
      // Do not dump "excl_msk" since it is only filled at
      // report generation time
      write_u32(item->unrc_msk, f);

      loc_write(&(item->loc), loc_ctx);
      if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
         loc_write(&(item->loc_lhs), loc_ctx);
         loc_write(&(item->loc_rhs), loc_ctx);
      }

      ident_write(item->hier, ident_ctx);
      if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE)
         ident_write(item->func_name, ident_ctx);

      write_u32(item->num, f);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_write_scope(s->children.items[i], f, ident_ctx, loc_ctx);

   write_u8(CTRL_POP_SCOPE, f);
}

void cover_dump_items(cover_data_t *data, fbuf_t *f, cover_dump_t dt,
                      const int32_t *counts)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(data->root_scope, counts);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(data->root_scope, 0);

   write_u32(data->mask, f);
   write_u32(data->array_limit, f);
   write_u32(data->next_tag, f);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   cover_write_scope(data->root_scope, f, ident_ctx, loc_wr);

   write_u8(CTRL_END_OF_FILE, f);

   loc_write_end(loc_wr);
   ident_write_end(ident_ctx);
}

cover_data_t *cover_data_init(cover_mask_t mask, int array_limit)
{
   cover_data_t *data = xcalloc(sizeof(cover_data_t));
   data->mask = mask;
   data->array_limit = array_limit;

   return data;
}

bool cover_enabled(cover_data_t *data, cover_mask_t mask)
{
   return data != NULL && (data->mask & mask);
}

static bool cover_should_emit_scope(cover_data_t *data, tree_t t)
{
   cover_scope_t *ts = data->top_scope;
   cover_spec_t *spc = data->spec;

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

      for (int i = 0; i < data->spec->block_include.count; i++)
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

void cover_push_scope(cover_data_t *data, tree_t t)
{
   if (data == NULL)
      return;
   else if (data->root_scope == NULL) {
      cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
      s->name = s->hier = lib_name(lib_work());

      data->top_scope = data->root_scope = s;
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
         assert(data->top_scope);
         cnt = &data->top_scope->branch_label;
         c = 'B';
      }
   }
   // For toggle coverage, remember the position where its name in
   // the hierarchy starts.
   else if (cover_is_toggle_first(t)) {
      assert(tree_has_ident(t));
      name = tree_ident(t);
      s->sig_pos = ident_len(data->top_scope->hier) + 1;
   }
   else if (tree_has_ident(t))
      name = tree_ident(t);
   // Consider everything else as statement
   // Expressions do not get scope pushed, so if scope for e.g.
   // T_FCALL is pushed it will be concurent function call -> Label as statement
   else {
      assert(data->top_scope);
      cnt = &data->top_scope->stmt_label;
      c = 'S';
   }

   if (c) {
      checked_sprintf(prefix, sizeof(prefix), "_%c%u", c, *cnt);
      (*cnt)++;
   }
   if (name == NULL)
      name = ident_new(prefix);

   s->name       = name;
   s->parent     = data->top_scope;
   s->block_name = s->parent->block_name;
   s->loc        = *tree_loc(t);
   s->hier       = ident_prefix(s->parent->hier, name, '.');

   if (s->sig_pos == 0)
      s->sig_pos = data->top_scope->sig_pos;

   APUSH(data->top_scope->children, s);

   data->top_scope = s;

   if (tree_kind(t) == T_BLOCK) {
      tree_t hier = tree_decl(t, 0);
      assert(tree_kind(hier) == T_HIER);

      tree_t unit = tree_ref(hier);
      if (tree_kind(unit) == T_ARCH) {
         s->block_name = ident_rfrom(tree_ident(unit), '.');
         s->type = CSCOPE_INSTANCE;
      }
   }

   s->emit = (data->spec == NULL) ? true : cover_should_emit_scope(data, t);

#ifdef COVER_DEBUG_SCOPE
   printf("Pushing cover scope: %s\n", istr(s->hier));
   printf("Tree_kind: %s\n\n", tree_kind_str(tree_kind(t)));
   printf("Coverage emit: %d\n\n", s->emit);
#endif
}

void cover_pop_scope(cover_data_t *data)
{
   if (data == NULL)
      return;

   assert(data->top_scope != NULL);

   ACLEAR(data->top_scope->ignore_lines);

#ifdef COVER_DEBUG_SCOPE
   printf("Popping cover scope: %s\n", istr(data->top_scope->hier));
#endif

   data->top_scope = data->top_scope->parent;

}

void cover_inc_array_depth(cover_data_t *data)
{
   assert(data != NULL);
   data->array_depth++;
#ifdef COVER_DEBUG_SCOPE
   printf("Adding dimension: %d\n", data->array_depth);
#endif
}

void cover_dec_array_depth(cover_data_t *data)
{
   assert(data != NULL);
   assert(data->array_depth > 0);
   data->array_depth--;
#ifdef COVER_DEBUG_SCOPE
   printf("Subtracting dimension: %d\n", data->array_depth);
#endif
}

static void cover_read_header(fbuf_t *f, cover_data_t *data)
{
   assert(data != NULL);

   data->mask = read_u32(f);
   data->array_limit = read_u32(f);
   data->next_tag = read_u32(f);
}

static void cover_read_one_item(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                                ident_rd_ctx_t ident_ctx, cover_item_t *item)
{
   item->kind = read_u8(f);

   item->tag = read_u32(f);
   item->data = read_u32(f);
   item->flags = read_u32(f);
   item->source = fbuf_get_uint(f);
   item->unrc_msk = read_u32(f);
   item->excl_msk = 0;

   loc_read(&(item->loc), loc_rd);
   if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
      loc_read(&(item->loc_lhs), loc_rd);
      loc_read(&(item->loc_rhs), loc_rd);
   }

   item->hier = ident_read(ident_ctx);
   if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE)
      item->func_name = ident_read(ident_ctx);

   item->num = read_u32(f);
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

   const int nitems = fbuf_get_uint(f);
   for (int i = 0; i < nitems; i++) {
      cover_item_t new;
      cover_read_one_item(f, loc_ctx, ident_ctx, &new);

      APUSH(s->items, new);
   }

   for (;;) {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         {
            cover_scope_t *child = cover_read_scope(f, ident_ctx, loc_ctx);
            APUSH(s->children, child);
         }
         break;
      case CTRL_POP_SCOPE:
         return s;
      default:
         fatal_trace("invalid control word %x in cover db", ctrl);
      }
   }
}

cover_data_t *cover_read_items(fbuf_t *f, uint32_t pre_mask)
{
   cover_data_t *data = xcalloc(sizeof(cover_data_t));
   cover_read_header(f, data);
   data->mask |= pre_mask;

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         data->root_scope = cover_read_scope(f, ident_ctx, loc_rd);
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

   return data;
}

static void cover_merge_scope(cover_scope_t *old_s, cover_scope_t *new_s)
{

   for (int i = 0; i < new_s->items.count; i++) {
      cover_item_t *new = AREF(new_s->items, i);

      bool found = false;
      for (int j = 0; j < old_s->items.count; j++) {
         cover_item_t *old = AREF(old_s->items, j);

         // Compare based on hierarchical path, each
         // coverage item has unique hierarchical name
         if (new->hier == old->hier && (new->flags == old->flags)) {
            assert(new->kind == old->kind);
#ifdef COVER_DEBUG_MERGE
            printf("Merging coverage item: %s\n", istr(old->hier));
#endif
            cover_merge_one_item(old, new->data);

            found = true;
            break;
         }
      }

      // Append the new item to the common scope
      if (!found)
         APUSH(old_s->items, *new);
   }

   for (int i = 0; i < new_s->children.count; i++) {
      cover_scope_t *new_c = new_s->children.items[i];
      bool found = false;
      for (int j = 0; j < old_s->children.count; j++) {
         cover_scope_t *old_c = old_s->children.items[j];
         if (new_c->name == old_c->name) {
            cover_merge_scope(old_c, new_c);
            found = true;
            break;
         }
      }

      if (!found)
         APUSH(old_s->children, new_c);
   }
}

void cover_merge_items(fbuf_t *f, cover_data_t *data)
{
   assert (data != NULL);

   cover_read_header(f, data);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         {
            cover_scope_t *new = cover_read_scope(f, ident_ctx, loc_rd);
            cover_merge_scope(data->root_scope, new);
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

unsigned cover_count_items(cover_data_t *data)
{
   if (data == NULL)
      return 0;
   else
      return data->next_tag;
}

void cover_bmask_to_bin_list(uint32_t bmask, text_buf_t *tb)
{
   bool empty = true;
   for (int i = 0; i < ARRAY_LEN(bin_map); i++) {
      if (bmask & bin_map[i].flag) {
         if (!empty)
            tb_cat(tb, ", ");
         tb_cat(tb, bin_map[i].name);
         empty = false;
      }
   }
}

uint32_t cover_bin_str_to_bmask(const char *bin)
{
   for (int i = 0; i < ARRAY_LEN(bin_map); i++) {
      if (strcmp(bin, bin_map[i].name) == 0)
         return bin_map[i].flag;
   }

   return 0;
}

const char *cover_item_kind_str(cover_item_kind_t kind)
{
   static const char* item_kind_str[] = {
      "statement",
      "branch",
      "toggle",
      "expression",
      "FSM state",
      "cover point",
   };
   assert(kind < ARRAY_LEN(item_kind_str));
   return item_kind_str[kind];
}
