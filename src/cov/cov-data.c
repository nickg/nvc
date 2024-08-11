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
#include "psl/psl-node.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>
#include <inttypes.h>

//#define COVER_DEBUG_EMIT
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

#define COVER_FILE_MAGIC   0x6e636462   // ASCII "ncdb"
#define COVER_FILE_VERSION 1

static bool cover_is_branch(tree_t branch)
{
   return tree_kind(branch) == T_ASSOC || tree_kind(branch) == T_COND_STMT;
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

static int32_t cover_add_item(cover_data_t *data, object_t *obj, ident_t suffix,
                              cover_item_kind_t kind, uint32_t flags, int consecutive)
{
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

   // Append BIN name to the cover item
   if (kind == COV_ITEM_BRANCH ||kind == COV_ITEM_TOGGLE || kind == COV_ITEM_EXPRESSION)
      hier = ident_prefix(hier, ident_new(cover_bmask_to_bin_str(flags)), '.');

   int64_t metadata = 0;
   if (kind == COV_ITEM_TOGGLE)
      metadata = data->top_scope->sig_pos;

   const loc_t *loc = get_cover_loc(kind, obj);

   cover_item_t new = {
      .kind          = kind,
      .tag           = data->next_tag++,
      .data          = 0,
      .flags         = flags,
      .loc           = *loc,
      .loc_lhs       = loc_lhs,
      .loc_rhs       = loc_rhs,
      .hier          = hier,
      .func_name     = func_name,
      .consecutive   = consecutive,
      .metadata      = metadata,
      .source        = get_cover_source(kind, obj),
   };

#ifdef COVER_DEBUG_EMIT
   printf("Item: %s\n", istr(hier));
   printf("    Kind:          %s\n", cover_item_kind_str(kind));
   printf("    Flags:         0x%x\n", flags);
   printf("    Consecutive:   %d\n", consecutive);
   printf("    Metadata:      %d\n", metadata);
   printf("    Function name: %s\n", istr(func_name));
   printf("    First line:    %d\n", loc->first_line);
   printf("    First column:  %d\n", loc->first_column);
   printf("    Line delta:    %d\n", loc->line_delta);
   printf("    Column delta:  %d\n", loc->column_delta);
   printf("\n\n");
#endif

   APUSH(data->top_scope->items, new);

   return data->top_scope->items.count - 1;
}

///////////////////////////////////////////////////////////////////////////////
// Toggle coverage item emit
///////////////////////////////////////////////////////////////////////////////

static bool cover_is_toggle_first(tree_t decl)
{
   const tree_kind_t kind = tree_kind(decl);
   return kind == T_SIGNAL_DECL || kind == T_PORT_DECL;
}

static bool cover_skip_array_toggle(cover_data_t *data, int a_size)
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

static int32_t cover_add_toggle_items_one_bit(cover_data_t *data, object_t *obj,
                                              ident_t suffix, unsigned int flags)
{
   assert (obj != NULL);

   int32_t first_item_index = cover_add_item(data, obj, suffix, COV_ITEM_TOGGLE,
                                             flags | COV_FLAG_TOGGLE_TO_1, 2);
   cover_add_item(data, obj, suffix, COV_ITEM_TOGGLE, flags | COV_FLAG_TOGGLE_TO_0, 1);

   return first_item_index;
}

static int32_t cover_add_toggle_items(cover_data_t *data, type_t type,
                                      object_t *obj, ident_t prefix, int curr_dim)
{
   assert (data != NULL);

   type_t root = type;

   // Gets well known type for scalar and vectorized version of
   // standard types (std_[u]logic[_vector], signed, unsigned)
   while (type_base_kind(root) == T_ARRAY)
      root = type_elem(root);
   root = type_base_recur(root);

   well_known_t known = is_well_known(type_ident(root));
   if (known != W_IEEE_ULOGIC && known != W_IEEE_ULOGIC_VECTOR)
      return -1;

   unsigned int flags = 0;
   if (tree_kind(tree_from_object(obj)) == T_SIGNAL_DECL)
      flags |= COV_FLAG_TOGGLE_SIGNAL;
   else
      flags |= COV_FLAG_TOGGLE_PORT;

   if (type_is_scalar(type))
      return cover_add_toggle_items_one_bit(data, obj, NULL, flags);
   else if (type_is_unconstrained(type))
      return -1;   // Not yet supported

   int t_dims = dimension_of(type);
   tree_t r = range_of(type, t_dims - curr_dim);
   int64_t low, high;

   if (!folded_bounds(r, &low, &high))
      return -1;

   assert(low <= high);

   if (cover_skip_array_toggle(data, high - low + 1))
      return -1;

   int64_t first, last, i;
   int32_t first_item_index = -1;
   int inc;

   data->array_depth++;
#ifdef COVER_DEBUG_SCOPE
   printf("Added dimension: %d\n", data->array_depth);
#endif

   switch (tree_subkind(r)) {
   case RANGE_DOWNTO:
      i = high;
      first = high;
      last = low;
      inc = -1;
      break;
   case RANGE_TO:
      i = low;
      first = low;
      last = high;
      inc = +1;
      break;
   default:
      fatal_trace("invalid subkind for range: %d", tree_subkind(r));
   }

   while (1) {
      char arr_index[16];
      int32_t tmp = -1;
      checked_sprintf(arr_index, sizeof(arr_index), "(%"PRIi64")", i);
      ident_t arr_suffix =
         ident_prefix(prefix, ident_new(arr_index), '\0');

      // On lowest dimension walk through elements, if elements
      // are arrays, then start new (nested) recursion.
      if (curr_dim == 1) {
         type_t e_type = type_elem(type);
         if (type_is_array(e_type))
            tmp = cover_add_toggle_items(data, e_type, obj, arr_suffix,
                                             dimension_of(e_type));
         else
            tmp = cover_add_toggle_items_one_bit(data, obj, arr_suffix, flags);
      }
      else   // Recurse to lower dimension
         tmp = cover_add_toggle_items(data, type, obj, arr_suffix, curr_dim - 1);

      if (i == first)
         first_item_index = tmp;
      if (i == last)
         break;

      i += inc;
   }

   assert(data->array_depth > 0);
   data->array_depth--;
#ifdef COVER_DEBUG_SCOPE
   printf("Subtracted dimension: %d\n", data->array_depth);
#endif

   return first_item_index;
}

///////////////////////////////////////////////////////////////////////////////
// Branch coverage item emit
///////////////////////////////////////////////////////////////////////////////

static int32_t cover_add_branch_items_for(cover_data_t *data, object_t *obj)
{
   tree_t b = tree_from_object(obj);
   int first_item_index = -1;

   // Case choice
   if (tree_kind(b) == T_ASSOC)
      first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_BRANCH,
                                        COV_FLAG_CHOICE, 1);

   // If-else
   else {
      first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_BRANCH,
                                        COV_FLAG_TRUE, 2);
      cover_add_item(data, obj, NULL, COV_ITEM_BRANCH, COV_FLAG_FALSE, 1);
   }

   return first_item_index;
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

static int32_t cover_add_state_items_for(cover_data_t *data, object_t *obj)
{
   type_t type = tree_type(tree_from_object(obj));

   if (cover_skip_type_state(data, type))
      return -1;

   int64_t low, high;
   if (!folded_bounds(range_of(type, 0), &low, &high))
      return -1;

   // Add single coverage item per enum literal. This is to track
   // literal string in the identifier of the coverage item.
   type_t base = type_base_recur(type);
   assert(type_is_enum(base));
   ident_t itype = type_ident(type);
   int64_t first_item_index = 0;

   for (int64_t i = low; i <= high; i++) {
      tree_t literal = type_enum_literal(base, i);
      ident_t suffix = ident_prefix(ident_new(".BIN_STATE."), tree_ident(literal), '\0');
      int32_t curr_item_index = cover_add_item(data,  obj, suffix, COV_ITEM_STATE,
                                               COV_FLAG_STATE, ((int)(high - i)) + 1);

      // For FSM State coverage, "func_name" stores name of the FSM Enum type
      // TODO: Move this logic into "cover_add_item"
      cover_item_t *item = AREF(data->top_scope->items, curr_item_index);
      item->func_name = ident_rfrom(itype, '.');
      if (i == low) {
         item->metadata = low;
         first_item_index = curr_item_index;
      }
   }

   return first_item_index;
}

///////////////////////////////////////////////////////////////////////////////
// Expression coverage item emit
///////////////////////////////////////////////////////////////////////////////

static int32_t cover_add_expression_xor_items(cover_data_t *data, object_t *obj,
                                              uint32_t flags)
{
   int first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION,
                                         flags | COV_FLAG_00, 4);

   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_01, 3);
   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_10, 2);
   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_11, 1);

   return first_item_index;
}

static int32_t cover_add_expression_and_items(cover_data_t *data, object_t *obj,
                                              uint32_t flags)
{
   int first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION,
                                         flags | COV_FLAG_01, 3);

   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_10, 2);
   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_11, 1);

   return first_item_index;
}

static int32_t cover_add_expression_or_items(cover_data_t *data, object_t *obj,
                                             uint32_t flags)
{
   int first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION,
                                         flags | COV_FLAG_00, 3);

   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_01, 2);
   cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, flags | COV_FLAG_10, 1);

   return first_item_index;
}

static int32_t cover_add_builtin_expression_items(cover_data_t *data, object_t *obj,
                                                  subprogram_kind_t builtin)
{
   // Handle expression result True or False
   switch (builtin) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_NOT:
   {
      int first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION,
                                            COV_FLAG_FALSE, 2);
      cover_add_item(data, obj, NULL, COV_ITEM_EXPRESSION, COV_FLAG_TRUE, 1);

      return first_item_index;
   }

   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
      return cover_add_expression_xor_items(data, obj, 0);

   case S_SCALAR_OR:
   case S_SCALAR_NOR:
      return cover_add_expression_or_items(data, obj, 0);

   case S_SCALAR_AND:
   case S_SCALAR_NAND:
      return cover_add_expression_and_items(data, obj, 0);

   default:
      fatal_trace("unhandled subprogram kind %d in cover_add_builtin_expression_items",
                  builtin);
   }

   return -1;
}

static int32_t cover_add_logic_expression_items(cover_data_t *data, object_t *obj)
{
   tree_t fcall = tree_from_object(obj);
   tree_t decl = tree_ref(fcall);

   if (tree_kind(decl) != T_FUNC_DECL)
      return -1;

   tree_t container = tree_container(decl);
   if (is_well_known(tree_ident(container)) != W_IEEE_1164)
      return -1;

   // Only binary expressions
   if (tree_params(fcall) != 2)
      return -1;

   // Skip arrays -> Matches behavior of VCS and Modelsim
   if (type_is_array(type_param(tree_type(decl), 0)) ||
       type_is_array(type_param(tree_type(decl), 1)))
      return -1;

   ident_t full_name = tree_ident2(decl);

   // Emit items for each of AND, NAND, OR, NOR, XOR, XNOR
   if (ident_starts_with(full_name, well_known(W_IEEE_1164_AND)) ||
       ident_starts_with(full_name, well_known(W_IEEE_1164_NAND)))
      return cover_add_expression_and_items(data, obj, COV_FLAG_EXPR_STD_LOGIC);

   if (ident_starts_with(full_name, well_known(W_IEEE_1164_OR)) ||
       ident_starts_with(full_name, well_known(W_IEEE_1164_NOR)))
      return cover_add_expression_or_items(data, obj, COV_FLAG_EXPR_STD_LOGIC);

   if (ident_starts_with(full_name, well_known(W_IEEE_1164_XOR)) ||
       ident_starts_with(full_name, well_known(W_IEEE_1164_XNOR)))
      return cover_add_expression_xor_items(data, obj, COV_FLAG_EXPR_STD_LOGIC);

   return -1;
}

///////////////////////////////////////////////////////////////////////////////
// Lower EMIT API
///////////////////////////////////////////////////////////////////////////////

cover_item_t *cover_add_items_for(cover_data_t *data, object_t *obj,
                                  cover_item_kind_t kind)
{
   assert(data != NULL);

   if (!data->top_scope->emit)
      return NULL;

   // Skip coverage emit when ignored by spec-file -> Common for all item kinds
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

   // Emit items based on item kind
   assert(obj != NULL);
   tree_t tree = tree_from_object(obj);
   int32_t first_item_index = -1;

   switch (kind) {
   case COV_ITEM_STMT:
      first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_STMT, 0, 1);
      break;

   case COV_ITEM_BRANCH:
      first_item_index = cover_add_branch_items_for(data, obj);
      break;

   case COV_ITEM_STATE:
      first_item_index = cover_add_state_items_for(data, obj);
      break;

   case COV_ITEM_FUNCTIONAL:
      first_item_index = cover_add_item(data, obj, NULL, COV_ITEM_FUNCTIONAL, 0, 1);
      break;

   case COV_ITEM_TOGGLE:
   {
      type_t type = tree_type(tree);
      const int ndims = dimension_of(type);
      first_item_index = cover_add_toggle_items(data, type, obj, NULL, ndims);

      break;
   }
   case COV_ITEM_EXPRESSION:
   {
      assert(tree_kind(tree) == T_FCALL);

      // Choose if to emit for built-in or "std_logic"
      tree_t decl = tree_ref(tree);
      const subprogram_kind_t kind = tree_subkind(decl);

      if (is_open_coded_builtin(kind))
         first_item_index = cover_add_builtin_expression_items(data, obj, kind);
      else
         first_item_index = cover_add_logic_expression_items(data, obj);

      break;
   }

   default:
      fatal("unsupported type of code coverage: %d at 'cover_add_items_for' !", kind);
   }

   return (first_item_index == -1) ? NULL : AREF(data->top_scope->items, first_item_index);
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

      fbuf_put_uint(f, item->kind);
      fbuf_put_uint(f, item->tag);
      fbuf_put_uint(f, item->data);
      fbuf_put_uint(f, item->flags);
      fbuf_put_uint(f, item->source);
      fbuf_put_uint(f, item->consecutive);
      fbuf_put_uint(f, item->metadata);

      loc_write(&(item->loc), loc_ctx);
      if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
         loc_write(&(item->loc_lhs), loc_ctx);
         loc_write(&(item->loc_rhs), loc_ctx);
      }

      ident_write(item->hier, ident_ctx);
      if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE)
         ident_write(item->func_name, ident_ctx);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_write_scope(s->children.items[i], f, ident_ctx, loc_ctx);

   write_u8(CTRL_POP_SCOPE, f);
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

void cover_dump_items(cover_data_t *data, fbuf_t *f, cover_dump_t dt,
                      const int32_t *counts)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(data->root_scope, counts);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(data->root_scope, 0);

   write_u32(COVER_FILE_MAGIC, f);
   fbuf_put_uint(f, COVER_FILE_VERSION);

   fbuf_put_uint(f, data->mask);
   fbuf_put_uint(f, data->array_limit);
   fbuf_put_uint(f, data->next_tag);

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
   data->next_tag    = fbuf_get_uint(f);
}

static void cover_read_one_item(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                                ident_rd_ctx_t ident_ctx, cover_item_t *item)
{
   item->kind   = fbuf_get_uint(f);
   item->tag    = fbuf_get_uint(f);
   item->data   = fbuf_get_uint(f);
   item->flags  = fbuf_get_uint(f);
   item->source = fbuf_get_uint(f);
   item->consecutive = fbuf_get_uint(f);
   item->metadata    = fbuf_get_uint(f);

   loc_read(&(item->loc), loc_rd);
   if (item->flags & COVER_FLAGS_LHS_RHS_BINS) {
      loc_read(&(item->loc_lhs), loc_rd);
      loc_read(&(item->loc_rhs), loc_rd);
   }

   item->hier = ident_read(ident_ctx);
   if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE)
      item->func_name = ident_read(ident_ctx);
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
