//
//  Copyright (C) 2013-2026  Nick Gasson
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
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "option.h"
#include "printf.h"
#include "tree.h"
#include "psl/psl-node.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>
#include <inttypes.h>

typedef enum {
   CTRL_PUSH_SCOPE,
   CTRL_POP_SCOPE,
   CTRL_END_OF_FILE,
   CTRL_PUSH_UNIT,
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

#define COVER_FILE_MAGIC   0x6e636462   // ASCII "ncdb"
#define COVER_FILE_VERSION 5

static inline unsigned get_next_tag(cover_block_t *b)
{
   assert(b->data == NULL);
   return b->next_tag++;
}

static bool cover_is_branch(tree_t branch)
{
   return tree_kind(branch) == T_CHOICE || tree_kind(branch) == T_COND_STMT;
}

static cover_src_t get_cover_source(cover_item_kind_t kind, object_t *obj)
{
   if (obj == NULL)
      return COV_SRC_UNKNOWN;

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
         case T_CHOICE:
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

   psl_node_t p = psl_from_object(obj);
   if (p != NULL) {
      switch (kind) {
      case COV_ITEM_FUNCTIONAL:
         return COV_SRC_PSL_COVER;
      default:
         return COV_SRC_UNKNOWN;
      }
   }

   return COV_SRC_UNKNOWN;
}

const loc_t get_cover_loc(cover_item_kind_t kind, object_t *obj)
{
   if (obj == NULL)
      return LOC_INVALID;

   tree_t t = tree_from_object(obj);
   if (t != NULL) {
      // Refer location of test condition instead of branch statement to
      // get accurate test condition location in the coverage report
      if (kind == COV_ITEM_BRANCH && tree_kind(t) != T_CHOICE)
         return *tree_loc(tree_value(t));
   }

   return obj->loc;
}

static cover_item_t *cover_add_item(cover_data_t *data, cover_scope_t *cs,
                                    object_t *obj, cover_item_kind_t kind,
                                    uint32_t flags, int consecutive)
{
   // Everything creates scope, so name of current item is already given
   // by scope in hierarchy.
   ident_t hier = cs->hier;

   // Expression items do not nest scope, expression name must be created
   if (kind == COV_ITEM_EXPRESSION) {
      char buf[16];
      checked_sprintf(buf, sizeof(buf), "_E%d", cs->expression_label);
      hier = ident_prefix(hier, ident_new(buf), '.');
      cs->expression_label++;
   }

   int64_t metadata = 0;
   if (kind == COV_ITEM_TOGGLE)
      metadata = cs->sig_pos;

   loc_t loc = get_cover_loc(kind, obj);
   cover_src_t src = get_cover_source(kind, obj);

   cover_item_t *new = pool_malloc_array(data->pool, sizeof(cover_item_t),
                                         consecutive);
   for (int i = 0; i < consecutive; i++) {
      new[i] = (cover_item_t){
         .kind          = kind,
         .tag           = get_next_tag(cs->block),
         .data          = 0,
         .flags         = flags,
         .loc           = loc,
         .loc_lhs       = LOC_INVALID,
         .loc_rhs       = LOC_INVALID,
         .hier          = hier,
         .func_name     = NULL,
         .consecutive   = consecutive - i,
         .atleast       = data->threshold,
         .metadata      = metadata,
         .n_ranges      = 0,
         .ranges        = NULL,
         .source        = src,
      };

      // TODO: only push the first item?
      APUSH(cs->items, &(new[i]));
   }

   return new;
}

///////////////////////////////////////////////////////////////////////////////
// Toggle coverage item emit
///////////////////////////////////////////////////////////////////////////////

static bool cover_is_toggle_first(tree_t decl)
{
   const tree_kind_t kind = tree_kind(decl);
   return kind == T_SIGNAL_DECL || kind == T_PORT_DECL;
}

static void cover_add_array_toggle_items(cover_data_t *data,
                                         cover_scope_t *cs,
                                         type_t type, object_t *obj,
                                         const char *prefix, int curr_dim,
                                         cover_flags_t flags,
                                         cover_item_t **itemp)
{
   int t_dims = dimension_of(type);
   tree_t r = range_of(type, t_dims - curr_dim);

   const int64_t left = assume_int(tree_left(r));
   const int64_t right = assume_int(tree_right(r));
   const int inc = tree_subkind(r) == RANGE_TO ? +1 : -1;

   type_t elem = type_elem(type);
   const bool memory = type_is_array(elem);

   const char *binstr[2] = {
      cover_bmask_to_bin_str(COV_FLAG_TOGGLE_TO_1),
      cover_bmask_to_bin_str(COV_FLAG_TOGGLE_TO_0),
   };

   for (int64_t i = left; i != right + inc; i += inc) {
      char arr_index[64];
      checked_sprintf(arr_index, sizeof(arr_index), "%s(%"PRIi64")", prefix, i);

      // On lowest dimension walk through elements, if elements
      // are arrays, then start new (nested) recursion.
      if (curr_dim == 1) {
         if (memory)
            cover_add_array_toggle_items(data, cs, elem, obj, arr_index,
                                         dimension_of(elem), flags, itemp);
         else {
            cover_item_t *pair = *itemp;
            *itemp += 2;

            pair[0].flags |= COV_FLAG_TOGGLE_TO_1;
            pair[1].flags |= COV_FLAG_TOGGLE_TO_0;

            for (int j = 0; j < 2; j++) {
               char suffix[64];
               checked_sprintf(suffix, sizeof(suffix), "%s.%s",
                               arr_index, binstr[j]);

               pair[j].hier = ident_prefix(pair[j].hier,
                                           ident_new(suffix), '\0');
            }

            pair[0].consecutive = 2;
            pair[1].consecutive = 1;
         }
      }
      else   // Recurse to lower dimension
         cover_add_array_toggle_items(data, cs, type, obj, arr_index,
                                      curr_dim - 1, flags, itemp);
   }
}

static cover_item_t *cover_add_toggle_items(cover_data_t *data,
                                            cover_scope_t *cs,
                                            object_t *obj)
{
   assert(data != NULL);

   type_t type = tree_type(tree_from_object(obj));
   type_t root = type_base_recur(type_elem_recur(type));

   well_known_t known = is_well_known(type_ident(root));
   if (known != W_IEEE_ULOGIC && known != W_IEEE_ULOGIC_VECTOR)
      return NULL;

   cover_flags_t flags = 0;
   if (tree_kind(tree_from_object(obj)) == T_SIGNAL_DECL)
      flags |= COV_FLAG_TOGGLE_SIGNAL;
   else
      flags |= COV_FLAG_TOGGLE_PORT;

   if (type_is_scalar(type)) {
      cover_item_t *pair = cover_add_item(data, cs, obj, COV_ITEM_TOGGLE,
                                          flags, 2);

      pair[0].flags |= COV_FLAG_TOGGLE_TO_1;
      pair[1].flags |= COV_FLAG_TOGGLE_TO_0;

      for (int i = 0; i < 2; i++) {
         ident_t suffix = ident_new(cover_bmask_to_bin_str(pair[i].flags));
         pair[i].hier = ident_prefix(pair[i].hier, suffix, '.');
      }

      return pair;
   }
   else if (!type_const_bounds(type))
      return NULL;   // Not yet supported

   const unsigned total = type_width(type);
   if (total == 0)
      return NULL;

   if (data->array_limit != 0 && total >= data->array_limit)
      return NULL;

   type_t elem = type_elem(type);

   // TODO: perhaps make memory coverage a different coverage type
   const bool memory = type_is_array(elem);
   if (!cover_enabled(data, COVER_MASK_TOGGLE_INCLUDE_MEMS) && memory)
      return NULL;

   cover_item_t *set = cover_add_item(data, cs, obj, COV_ITEM_TOGGLE,
                                      flags, total * 2);

   cover_item_t *p = set;
   const int ndims = dimension_of(type);
   cover_add_array_toggle_items(data, cs, type, obj, "", ndims, flags, &p);
   assert(p == set + total * 2);

   return set;
}

///////////////////////////////////////////////////////////////////////////////
// Branch coverage item emit
///////////////////////////////////////////////////////////////////////////////

static cover_item_t *cover_add_branch_items_for(cover_data_t *data,
                                                cover_scope_t *cs,
                                                object_t *obj)
{
   tree_t b = tree_from_object(obj);

   if (tree_kind(b) == T_CHOICE) {  // Case choice
      cover_item_t *item = cover_add_item(data, cs, obj, COV_ITEM_BRANCH,
                                          COV_FLAG_CHOICE, 1);

      ident_t suffix = ident_new(cover_bmask_to_bin_str(item->flags));
      item->hier = ident_prefix(item->hier, suffix, '.');

      return item;
   }
   else {    // If-else
      cover_item_t *pair = cover_add_item(data, cs, obj, COV_ITEM_BRANCH, 0, 2);

      pair[0].flags |= COV_FLAG_TRUE;
      pair[1].flags |= COV_FLAG_FALSE;

      for (int i = 0; i < 2; i++) {
         ident_t suffix = ident_new(cover_bmask_to_bin_str(pair[i].flags));
         pair[i].hier = ident_prefix(pair[i].hier, suffix, '.');
      }

      return pair;
   }
}

///////////////////////////////////////////////////////////////////////////////
// FSM state coverage item emit
///////////////////////////////////////////////////////////////////////////////

static bool cover_skip_type_state(cover_data_t *data, type_t type)
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

   // Type should be recognized as FSM
   if (spc) {
      for (int i = 0; i < spc->fsm_type_include.count; i++) {
         if (ident_glob(name, AGET(spc->fsm_type_include, i), -1))
            return false;
      }
   }

   // By default enums should not included
   if (data->mask & COVER_MASK_FSM_NO_DEFAULT_ENUMS)
      return true;

   // Type should not be included
   if (spc) {
      for (int i = 0; i < spc->fsm_type_exclude.count; i++)
         if (ident_glob(name, AGET(spc->fsm_type_exclude, i), -1))
            return true;
     }

   return false;
}

static cover_item_t *cover_add_state_items_for(cover_data_t *data,
                                               cover_scope_t *cs,
                                               object_t *obj)
{
   type_t type = tree_type(tree_from_object(obj));

   if (cover_skip_type_state(data, type))
      return NULL;

   int64_t low, high;
   if (!folded_bounds(range_of(type, 0), &low, &high))
      return NULL;

   cover_item_t *set = cover_add_item(data, cs, obj, COV_ITEM_STATE,
                                      COV_FLAG_STATE, high - low + 1);

   // Add single coverage item per enum literal. This is to track
   // literal string in the identifier of the coverage item.
   type_t base = type_base_recur(type);
   assert(type_is_enum(base));
   ident_t itype = type_ident(type);

   for (int64_t i = low; i <= high; i++) {
      ident_t literal = tree_ident(type_enum_literal(base, i));
      ident_t suffix = ident_prefix(ident_new("BIN_STATE"), literal, '.');

      // For FSM State coverage, "func_name" stores name of the FSM Enum type
      cover_item_t *item = &(set[i - low]);
      item->hier = ident_prefix(item->hier, suffix, '.');
      item->func_name = ident_rfrom(itype, '.');
      if (i == low) {
         item->metadata = low;
      }
   }

   return set;
}

///////////////////////////////////////////////////////////////////////////////
// Expression coverage item emit
///////////////////////////////////////////////////////////////////////////////

static cover_item_t *cover_add_expression_xor_items(cover_data_t *data,
                                                    cover_scope_t *cs,
                                                    object_t *obj,
                                                    uint32_t flags)
{
   cover_item_t *set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION,
                                      flags, 4);
   set[0].flags |= COV_FLAG_00;
   set[1].flags |= COV_FLAG_01;
   set[2].flags |= COV_FLAG_10;
   set[3].flags |= COV_FLAG_11;

   return set;
}

static cover_item_t *cover_add_expression_and_items(cover_data_t *data,
                                                    cover_scope_t *cs,
                                                    object_t *obj,
                                                    uint32_t flags)
{
   cover_item_t *set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION,
                                      flags, 3);
   set[0].flags |= COV_FLAG_01;
   set[1].flags |= COV_FLAG_10;
   set[2].flags |= COV_FLAG_11;

   return set;
}

static cover_item_t *cover_add_expression_or_items(cover_data_t *data,
                                                   cover_scope_t *cs,
                                                   object_t *obj,
                                                   uint32_t flags)
{
   cover_item_t *set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION,
                                      flags, 3);
   set[0].flags |= COV_FLAG_00;
   set[1].flags |= COV_FLAG_01;
   set[2].flags |= COV_FLAG_10;

   return set;
}

static cover_item_t *cover_add_expression_items(cover_data_t *data,
                                                cover_scope_t *cs,
                                                object_t *obj)
{
   tree_t t = tree_from_object(obj);
   if (tree_kind(t) == T_PROT_FCALL)
      return NULL;

   assert(tree_kind(t) == T_FCALL);

   cover_flags_t flags = 0;
   cover_item_t *set;

   switch (tree_subkind(tree_ref(t))) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_NOT:
      set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION, 0, 2);
      set[0].flags |= COV_FLAG_FALSE;
      set[1].flags |= COV_FLAG_TRUE;
      break;

   case S_IEEE_XOR:
   case S_IEEE_XNOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
      set = cover_add_expression_xor_items(data, cs, obj, flags);
      break;

   case S_IEEE_OR:
   case S_IEEE_NOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_OR:
   case S_SCALAR_NOR:
      set = cover_add_expression_or_items(data, cs, obj, flags);
      break;

   case S_IEEE_AND:
   case S_IEEE_NAND:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
      set = cover_add_expression_and_items(data, cs, obj, flags);
      break;

   case S_IEEE_MISC:
   case S_IEEE_NOT:
   case S_USER:
      return NULL;

   default:
      should_not_reach_here();
   }

   ident_t func_name = tree_ident(t);
   loc_t loc_lhs = LOC_INVALID, loc_rhs = LOC_INVALID;
   if (set->flags & COVER_FLAGS_LHS_RHS_BINS) {
      loc_lhs = *tree_loc(tree_param(t, 0));
      loc_rhs = *tree_loc(tree_param(t, 1));
   }

   for (int i = 0; i < set->consecutive; i++) {
      ident_t suffix = ident_new(cover_bmask_to_bin_str(set[i].flags));
      set[i].hier = ident_prefix(set[i].hier, suffix, '.');

      set[i].loc_lhs = loc_lhs;
      set[i].loc_rhs = loc_rhs;
      set[i].func_name = func_name;
   }

   return set;
}

///////////////////////////////////////////////////////////////////////////////
// Lower EMIT API
///////////////////////////////////////////////////////////////////////////////

cover_item_t *cover_add_items_for(cover_data_t *data, cover_scope_t *cs,
                                  object_t *obj, cover_item_kind_t kind)
{
   assert(data != NULL);

   if (!cs->emit)
      return NULL;

   const loc_t loc = get_cover_loc(kind, obj);

   // Multiple scopes may be emitted from a single file for generate
   // statements, blocks, etc.
   for (cover_scope_t *ignore_scope = cs; ignore_scope->parent;
        ignore_scope = ignore_scope->parent) {
      if (ignore_scope->block_name == NULL)
         continue;
      else if (ignore_scope->loc.file_ref != loc.file_ref)
         break;

      for (int i = 0; i < ignore_scope->ignore_lines.count; i++) {
         line_range_t *lr = &(ignore_scope->ignore_lines.items[i]);
         if (loc.first_line > lr->start && loc.first_line <= lr->end)
            return NULL;
      }
   }

   switch (kind) {
   case COV_ITEM_STMT:
      return cover_add_item(data, cs, obj, COV_ITEM_STMT, 0, 1);
   case COV_ITEM_BRANCH:
      return cover_add_branch_items_for(data, cs, obj);
   case COV_ITEM_STATE:
      return cover_add_state_items_for(data, cs, obj);
   case COV_ITEM_FUNCTIONAL:
      return cover_add_item(data, cs, obj, COV_ITEM_FUNCTIONAL, 0, 1);
   case COV_ITEM_TOGGLE:
      return cover_add_toggle_items(data, cs, obj);
   case COV_ITEM_EXPRESSION:
      return cover_add_expression_items(data, cs, obj);
   default:
      should_not_reach_here();
   }
}

///////////////////////////////////////////////////////////////////////////////
// Coverage data write/read to covdb, covdb merging and coverage scope handling
///////////////////////////////////////////////////////////////////////////////

void cover_merge_one_item(cover_item_t *item, int32_t data)
{
   switch (item->kind) {
   case COV_ITEM_STMT:
   case COV_ITEM_FUNCTIONAL:
   case COV_ITEM_BRANCH:
   case COV_ITEM_STATE:
   case COV_ITEM_EXPRESSION:
      item->data = saturate_add(item->data, data);
      break;

   // Highest bit of run-time data for COV_ITEM_TOGGLE is used to track
   // unreachability due to being constant driven. If multiple designs
   // like this are merged, the resulting value would underflow and this
   // information would be lost. Similarly, if different designs were
   // merged, where one was unreachable due to being constant driven and
   // the other was driven. So, If the unreachability is detected, enforce
   // its propagation further to the merged database
   case COV_ITEM_TOGGLE:

      if ((item->data & COV_FLAG_UNREACHABLE) || (data & COV_FLAG_UNREACHABLE))
         item->data = COV_FLAG_UNREACHABLE;
      else
         item->data = saturate_add(item->data, data);
      break;

   default:
      break;
   }
}

static void cover_update_counts(cover_scope_t *s)
{
   if (s->block != NULL && s->block->data != NULL) {
      for (int i = 0; i < s->items.count; i++) {
         cover_item_t *item = s->items.items[i];
         cover_merge_one_item(item, s->block->data[item->tag]);
      }
   }

   for (int i = 0; i < s->children.count; i++)
      cover_update_counts(s->children.items[i]);
}

static void cover_write_scope(cover_scope_t *s, fbuf_t *f,
                              ident_wr_ctx_t ident_ctx, loc_wr_ctx_t *loc_ctx)
{
   if (s->block != NULL && s == s->block->self) {
      write_u8(CTRL_PUSH_UNIT, f);

      ident_write(s->block->name, ident_ctx);
      fbuf_put_uint(f, s->block->next_tag);
   }
   else
      write_u8(CTRL_PUSH_SCOPE, f);

   ident_write(s->name, ident_ctx);
   ident_write(s->hier, ident_ctx);
   ident_write(s->block_name, ident_ctx);
   fbuf_put_uint(f, s->kind);
   loc_write(&s->loc, loc_ctx);

   fbuf_put_uint(f, s->items.count);
   for (int i = 0; i < s->items.count; i++) {
      const cover_item_t *item = s->items.items[i];

      fbuf_put_uint(f, item->kind);
      fbuf_put_uint(f, item->tag);
      fbuf_put_uint(f, item->data);
      fbuf_put_uint(f, item->flags);
      fbuf_put_uint(f, item->source);
      fbuf_put_uint(f, item->consecutive);
      fbuf_put_uint(f, item->atleast);
      fbuf_put_uint(f, item->n_ranges);
      fbuf_put_uint(f, item->metadata);

      for (int i = 0; i < item->n_ranges; i++) {
         fbuf_put_uint(f, item->ranges[i].min);
         fbuf_put_uint(f, item->ranges[i].max);
      }

      loc_write(&(item->loc), loc_ctx);
      if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
         loc_write(&(item->loc_lhs), loc_ctx);
         loc_write(&(item->loc_rhs), loc_ctx);
      }

      ident_write(item->hier, ident_ctx);
      if (item->kind == COV_ITEM_EXPRESSION ||
          item->kind == COV_ITEM_STATE ||
          item->kind == COV_ITEM_FUNCTIONAL)
         ident_write(item->func_name, ident_ctx);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_write_scope(s->children.items[i], f, ident_ctx, loc_ctx);

   write_u8(CTRL_POP_SCOPE, f);
}

LCOV_EXCL_START
static void cover_debug_dump(cover_scope_t *s, int indent)
{
   nvc_printf("%*s$!blue$%s$$", indent, "", istr(s->name));
   if (s->block_name != NULL)
      printf(" : %s", istr(s->block_name));
   nvc_printf("\n");

   for (int i = 0; i < s->items.count; i++) {
      const cover_item_t *item = s->items.items[i];

      char *path LOCAL = xstrdup(loc_file_str(&item->loc));
      printf("%*s%d: %s %s %s:%d => %x\n", indent + 2, "", item->tag,
             cover_item_kind_str(item->kind), istr(item->hier),
             basename(path), item->loc.first_line, item->data);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_debug_dump(s->children.items[i], indent + 2);
}
LCOV_EXCL_STOP

void cover_dump_items(cover_data_t *data, fbuf_t *f, cover_dump_t dt)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(data->root_scope);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(data->root_scope, 0);

   write_u32(COVER_FILE_MAGIC, f);
   fbuf_put_uint(f, COVER_FILE_VERSION);

   fbuf_put_uint(f, data->mask);
   fbuf_put_uint(f, data->array_limit);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   cover_write_scope(data->root_scope, f, ident_ctx, loc_wr);

   write_u8(CTRL_END_OF_FILE, f);

   loc_write_end(loc_wr);
   ident_write_end(ident_ctx);
}

cover_data_t *cover_data_init(cover_mask_t mask, int array_limit, int threshold)
{
   cover_data_t *data = xcalloc(sizeof(cover_data_t));
   data->mask = mask;
   data->array_limit = array_limit;
   data->threshold = threshold;
   data->blocks = hash_new(16);
   data->pool = pool_new();

   return data;
}

void cover_data_free(cover_data_t *db)
{
#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(db->pool, &alloc, &npages);
   if (npages > 0)
      debugf("coverage database allocated %zu bytes in %zu pages",
             alloc, npages);
#endif

   hash_free(db->blocks);
   pool_free(db->pool);
   free(db);
}

bool cover_enabled(cover_data_t *data, cover_mask_t mask)
{
   return data != NULL && (data->mask & mask);
}

static bool cover_should_emit_scope(cover_data_t *data, cover_scope_t *cs)
{
   cover_spec_t *spc = data->spec;
   if (spc == NULL)
      return true;

   cover_scope_t *blk = cs;
   for (; blk != NULL && blk->block_name == NULL; blk = blk->parent);
   assert(blk != NULL);

   ident_t ename = ident_until(blk->block_name, '-');

   for (int i = 0; i < spc->block_exclude.count; i++) {
      if (ident_glob(ename, AGET(spc->block_exclude, i), -1))
         return false;
   }

   for (int i = 0; i < data->spec->block_include.count; i++) {
      if (ident_glob(ename, AGET(spc->block_include, i), -1))
         return true;
   }

   // Hierarchy
   for (int i = 0; i < spc->hier_exclude.count; i++) {
      if (ident_glob(cs->hier, AGET(spc->hier_exclude, i), -1))
         return false;
   }

   for (int i = 0; i < spc->hier_include.count; i++) {
      if (ident_glob(cs->hier, AGET(spc->hier_include, i), -1))
         return true;
   }

   return false;
}

cover_scope_t *cover_create_block(cover_data_t *data, ident_t qual,
                                  cover_scope_t *parent, tree_t inst,
                                  tree_t unit, ident_t name)
{
   if (data == NULL)
      return NULL;

   cover_block_t *b = pool_calloc(data->pool, sizeof(cover_block_t));
   b->name = qual;

   assert(hash_get(data->blocks, qual) == NULL);
   hash_put(data->blocks, qual, b);

   if (parent == NULL) {
      assert(data->root_scope == NULL);

      parent = data->root_scope = xcalloc(sizeof(cover_scope_t));
      parent->name = parent->hier = lib_name(lib_work());
   }

   ident_t scope_name;
   if (name != NULL)
      scope_name = name;
   else if (tree_kind(inst) == T_INERTIAL)   // Process without label
      scope_name = ident_sprintf("_S%u", parent->stmt_label++);
   else if (tree_has_ident(inst))
      scope_name = tree_ident(inst);
   else
      scope_name = ident_sprintf("_S%u", parent->stmt_label++);

   ident_t block_name = NULL;
   if (is_design_unit(unit) || is_concurrent_block(unit))
      block_name = ident_rfrom(tree_ident(unit), '.');

   cover_scope_t *s = pool_calloc(data->pool, sizeof(cover_scope_t));
   s->name       = scope_name;
   s->parent     = parent;
   s->block      = b;
   s->sig_pos    = parent->sig_pos;
   s->loc        = *tree_loc(unit);
   s->hier       = ident_prefix(parent->hier, scope_name, '.');
   s->block_name = block_name;
   s->emit       = cover_should_emit_scope(data, s);

   switch (tree_kind(inst)) {
   case T_BLOCK:
      if (name == NULL)
         s->kind = CSCOPE_INSTANCE;
      else
         s->kind = CSCOPE_USER;   // XXX
      break;
   case T_PROCESS:
   case T_INERTIAL:
      s->kind = CSCOPE_PROCESS;
      break;
   case T_PROC_BODY:
   case T_FUNC_BODY:
      s->kind = CSCOPE_SUBPROG;
      break;
   case T_PSL_DIRECT:
      s->kind = CSCOPE_PROPERTY;
      break;
   case T_PACK_INST:
   case T_PACKAGE:
   case T_PACK_BODY:
      s->kind = CSCOPE_PACKAGE;
      break;
   default:
      should_not_reach_here();
   }

   APUSH(parent->children, s);
   b->self = s;

   return b->self;
}

cover_scope_t *cover_create_scope(cover_data_t *data, cover_scope_t *parent,
                                  tree_t t, ident_t name)
{
   if (data == NULL)
      return NULL;

   assert(parent != NULL);
   assert(data->root_scope != NULL);
   assert(!is_design_unit(t));

   cover_scope_t *s = pool_calloc(data->pool, sizeof(cover_scope_t));

   if (cover_is_branch(t)) {
      if (tree_kind(t) == T_CHOICE && !tree_has_name(t))
         name = ident_new("_B_OTHERS");
      else
         name = ident_sprintf("_B%u", parent->branch_label++);
   }
   // For toggle coverage, remember the position where its name in
   // the hierarchy starts.
   else if (cover_is_toggle_first(t)) {
      assert(tree_has_ident(t));
      name = tree_ident(t);
      s->sig_pos = ident_len(parent->hier) + 1;
   }
   else if (tree_kind(t) == T_INERTIAL)   // Process without label
      name = ident_sprintf("_S%u", parent->stmt_label++);
   else if (name == NULL && tree_has_ident(t))
      name = tree_ident(t);
   // Consider everything else as statement
   // Expressions do not get scope pushed, so if scope for e.g.
   // T_FCALL is pushed it will be concurent function call -> Label as statement
   else if (name == NULL)
      name = ident_sprintf("_S%u", parent->stmt_label++);

   s->name   = name;
   s->parent = parent;
   s->block  = parent->block;
   s->loc    = *tree_loc(t);
   s->hier   = ident_prefix(parent->hier, name, '.');
   s->emit   = cover_should_emit_scope(data, s);
   s->kind   = CSCOPE_NONE;

   if (s->sig_pos == 0)
      s->sig_pos = parent->sig_pos;

   APUSH(parent->children, s);
   return s;
}

static void cover_read_header(fbuf_t *f, cover_data_t *data)
{
   assert(data != NULL);

   if (read_u32(f) != COVER_FILE_MAGIC)
      fatal("%s is not a valid coverage database", fbuf_file_name(f));

   const unsigned version = fbuf_get_uint(f);
   if (version != COVER_FILE_VERSION)
      fatal("coverage database %s format version %d is not the expected %d",
            fbuf_file_name(f), version, COVER_FILE_VERSION);

   data->mask        = fbuf_get_uint(f);
   data->array_limit = fbuf_get_uint(f);
}

static void cover_read_one_item(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                                ident_rd_ctx_t ident_ctx, cover_item_t *item)
{
   item->kind        = fbuf_get_uint(f);
   item->tag         = fbuf_get_uint(f);
   item->data        = fbuf_get_uint(f);
   item->flags       = fbuf_get_uint(f);
   item->source      = fbuf_get_uint(f);
   item->consecutive = fbuf_get_uint(f);
   item->atleast     = fbuf_get_uint(f);
   item->n_ranges    = fbuf_get_uint(f);
   item->metadata    = fbuf_get_uint(f);

   if (item->n_ranges > 0)
      item->ranges = xcalloc_array(item->n_ranges, sizeof(cover_range_t));

   for (int i = 0; i < item->n_ranges; i++) {
      item->ranges[i].min = fbuf_get_uint(f);
      item->ranges[i].max = fbuf_get_uint(f);
   }

   loc_read(&(item->loc), loc_rd);
   if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
      loc_read(&(item->loc_lhs), loc_rd);
      loc_read(&(item->loc_rhs), loc_rd);
   }

   item->hier = ident_read(ident_ctx);
   if (item->kind == COV_ITEM_EXPRESSION ||
       item->kind == COV_ITEM_STATE ||
       item->kind == COV_ITEM_FUNCTIONAL)
      item->func_name = ident_read(ident_ctx);
}

static cover_scope_t *cover_read_scope(cover_data_t *db, fbuf_t *f,
                                       ident_rd_ctx_t ident_ctx,
                                       loc_rd_ctx_t *loc_ctx,
                                       cover_block_t *b,
                                       cover_scope_t *parent)
{
   cover_scope_t *s = pool_calloc(db->pool, sizeof(cover_scope_t));
   s->name       = ident_read(ident_ctx);
   s->hier       = ident_read(ident_ctx);
   s->block_name = ident_read(ident_ctx);
   s->kind       = fbuf_get_uint(f);
   s->block      = b;
   s->parent     = parent;

   loc_read(&s->loc, loc_ctx);

   const int nitems = fbuf_get_uint(f);
   cover_item_t *new =
      pool_malloc_array(db->pool, sizeof(cover_item_t), nitems);

   for (int i = 0; i < nitems; i++) {
      cover_read_one_item(f, loc_ctx, ident_ctx, &new[i]);
      APUSH(s->items, &new[i]);
   }

   for (;;) {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_UNIT:
         {
            // FIXME: this check is required for merging
            ident_t name = ident_read(ident_ctx);
            cover_block_t *b = hash_get(db->blocks, name);
            if (b == NULL) {
               b = xcalloc(sizeof(cover_block_t));
               b->name = name;
               b->next_tag = fbuf_get_uint(f);
               b->self = cover_read_scope(db, f, ident_ctx, loc_ctx, b, s);

               hash_put(db->blocks, b->name, b);

               APUSH(s->children, b->self);
            }
            else {
               (void)fbuf_get_uint(f);

               cover_scope_t *child =
                  cover_read_scope(db, f, ident_ctx, loc_ctx, b, s);
               APUSH(s->children, child);
            }
         }
         break;
      case CTRL_PUSH_SCOPE:
         {
            cover_scope_t *child =
               cover_read_scope(db, f, ident_ctx, loc_ctx, b, s);
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
   data->blocks = hash_new(16);
   data->pool = pool_new();

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         data->root_scope =
            cover_read_scope(data, f, ident_ctx, loc_rd, NULL, NULL);
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

static void cover_merge_scope(cover_scope_t *old_s, cover_scope_t *new_s,
                              merge_mode_t mode)
{
   // Most merged cover scopes have equal items.
   // Start from equal item index in old and new scope instead of iterating
   // for each items.
   // If new scope has extra item in the middle (e.g. due to generic sized array)
   // account for offset. Then there is no reiteration upon added items.
   int n_added = 0;
   for (int i = 0; i < new_s->items.count; i++) {
      cover_item_t *new = AGET(new_s->items, i);

      bool found = false;
      int n_old_visits = 0;
      int old_pos = i - n_added;

      do {
         if (n_old_visits == old_s->items.count)
            break;

         cover_item_t *old = AGET(old_s->items, old_pos);

         if ((new->hier == old->hier) && (new->flags == old->flags)) {
            assert(new->kind == old->kind);
            cover_merge_one_item(old, new->data);
            found = true;
            break;
         }

         if (old_pos == old_s->items.count - 1)
            old_pos = 0;
         else
            old_pos++;

         n_old_visits++;
      } while (true);

      if (!found && mode == MERGE_UNION) {
         APUSH(old_s->items, new);
         n_added++;
      }
   }

   for (int i = 0; i < new_s->children.count; i++) {
      cover_scope_t *new_c = new_s->children.items[i];
      bool found = false;
      for (int j = 0; j < old_s->children.count; j++) {
         cover_scope_t *old_c = old_s->children.items[j];
         if (new_c->name == old_c->name) {
            cover_merge_scope(old_c, new_c, mode);
            found = true;
            break;
         }
      }

      if (!found && mode == MERGE_UNION)
         APUSH(old_s->children, new_c);
   }
}

void cover_merge_items(fbuf_t *f, cover_data_t *data, merge_mode_t mode)
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
            cover_scope_t *new =
               cover_read_scope(data, f, ident_ctx, loc_rd, NULL, NULL);
            cover_merge_scope(data->root_scope, new, mode);
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

int32_t *cover_get_counters(cover_data_t *db, ident_t name)
{
   if (db == NULL)
      return NULL;

   cover_block_t *b = hash_get(db->blocks, name);
   if (b == NULL || b->next_tag == 0)
      return NULL;

   if (b->data == NULL)
      b->data = pool_calloc(db->pool, b->next_tag * sizeof(int32_t));

   return b->data;
}

cover_scope_t *cover_get_scope(cover_data_t *db, ident_t name)
{
   if (db == NULL)
      return NULL;

   cover_block_t *b = hash_get(db->blocks, name);
   if (b == NULL)
      return NULL;

   return b->self;
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

const char *cover_bmask_to_bin_str(uint32_t bmask)
{
   // TODO: Smarter way instead of iterating -> Probably OK for such small array
   //       even if called many times!
   for (int i = 0; i < ARRAY_LEN(bin_map); i++)
      if (bmask & bin_map[i].flag)
         return bin_map[i].name;

   should_not_reach_here();
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

bool cover_is_hier(cover_scope_t *s)
{
   switch (s->kind) {
   case CSCOPE_INSTANCE:
   case CSCOPE_PACKAGE:
      return true;
   default:
      return false;
   }
}

bool cover_is_leaf(cover_scope_t *s)
{
   for (int i = 0; i < s->children.count; i++) {
      if (cover_is_hier(s->children.items[i]))
         return false;
   }

   return true;
}

bool cover_bin_unreachable(cover_data_t *data, const cover_item_t *item)
{
   if ((data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Toggle items remember unreachability in run-time data. Must check
   // item kind not to get false unreachability on statement items.
   // Excludes both bins automatically!
   if (item->kind == COV_ITEM_TOGGLE
       && ((item->data & COV_FLAG_UNREACHABLE) != 0))
      return true;

   // Expression items remember unreachability as unreachable mask
   if (item->kind == COV_ITEM_EXPRESSION
       && ((item->flags & COV_FLAG_UNREACHABLE) != 0))
      return true;

   return false;
}
