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
#include "mask.h"
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
#define COVER_FILE_VERSION 8

static inline unsigned get_next_tag(cover_block_t *b)
{
   if (b == NULL)
      return UINT_MAX;
   else {
      assert(b->data == NULL);
      return b->next_tag++;
   }
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

static cover_bin_t *cover_get_bins(const cover_data_t *db,
                                   const cover_item_t *item)
{
   if (cover_is_null(item->first_bin))
      return NULL;

   assert(item->first_bin.tag == COVER_TAG_BIN);
   assert(item->first_bin.id < db->bins.count);

   return db->bins.items + item->first_bin.id;
}

static const cover_bin_t *cover_bin_data_const(const cover_data_t *db,
                                               cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_BIN);
   assert(obj.id < db->bins.count);
   return db->bins.items + obj.id;
}

static cover_bin_t *cover_bin_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_BIN);
   assert(obj.id < db->bins.count);
   return db->bins.items + obj.id;
}

static const cover_item_t *cover_item_data_const(const cover_data_t *db,
                                                 cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_ITEM);
   assert(obj.id < db->items.count);
   return db->items.items + obj.id;
}

static cover_item_t *cover_item_data(const cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_ITEM);
   assert(obj.id < db->items.count);
   return db->items.items + obj.id;
}

static const cover_range_t *cover_range_data_const(const cover_data_t *db,
                                                   cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_RANGE);
   assert(obj.id < db->ranges.count);
   return db->ranges.items + obj.id;
}

static cover_range_t *cover_range_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_RANGE);
   assert(obj.id < db->ranges.count);
   return db->ranges.items + obj.id;
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

static cover_obj_t cover_alloc_bins(cover_data_t *db, unsigned count)
{
   if (db->bins.count + count > db->bins.limit) {
      const unsigned new_limit =
         MAX(16, MAX(db->bins.limit * 2, db->bins.count + count));
      db->bins.items = xrealloc_array(db->bins.items, new_limit,
                                      sizeof(cover_bin_t));
      db->bins.limit = new_limit;
   }

   cover_obj_t obj = {
      .tag = COVER_TAG_BIN,
      .id = db->bins.count,
   };

   db->bins.count += count;
   return obj;
}

static cover_obj_t cover_alloc_ranges(cover_data_t *db, unsigned count)
{
   if (count == 0)
      return COVER_NULL_OBJ;

   if (db->ranges.count + count > db->ranges.limit) {
      const unsigned new_limit =
         MAX(8, MAX(db->ranges.limit * 2, db->ranges.count + count));
      db->ranges.items = xrealloc_array(db->ranges.items, new_limit,
                                        sizeof(cover_range_t));
      db->ranges.limit = new_limit;
   }

   cover_obj_t obj = {
      .tag = COVER_TAG_RANGE,
      .id = db->ranges.count,
   };

   db->ranges.count += count;
   return obj;
}

static cover_obj_t cover_add_item(cover_data_t *db, cover_scope_t *cs,
                                  object_t *obj, cover_item_kind_t kind,
                                  uint32_t flags, int nbins)
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

   cover_item_t new = {
      .nbins     = nbins,
      .kind      = kind,
      .loc       = loc,
      .loc_lhs   = LOC_INVALID,
      .loc_rhs   = LOC_INVALID,
      .func_name = NULL,
      .atleast   = db->threshold,
      .metadata  = metadata,
      .source    = src,
      .first_bin = cover_alloc_bins(db, nbins),
   };

   cover_bin_t *bins = db->bins.items + new.first_bin.id;
   for (int i = 0; i < nbins; i++) {
      bins[i] = (cover_bin_t){
         .tag   = get_next_tag(cs->block),
         .data  = 0,
         .flags = flags,
         .hier  = hier,
      };
   }

   cover_obj_t item_obj = {
      .tag = COVER_TAG_ITEM,
      .id = db->items.count,
   };

   APUSH(db->items, new);

   APUSH(cs->items, item_obj);
   return item_obj;
}

///////////////////////////////////////////////////////////////////////////////
// Toggle coverage item emit
///////////////////////////////////////////////////////////////////////////////

static int cover_count_toggle_elems(cover_data_t *db, type_t type)
{
   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      int sum = 0;
      for (int i = 0; i < nfields; i++)
         sum += cover_count_toggle_elems(db, tree_type(type_field(type, i)));

      return sum;
   }

   type_t root = type_base_recur(type_elem_recur(type));
   if (is_well_known(type_ident(root)) != W_IEEE_ULOGIC)
      return 0;
   else if (type_is_scalar(type))
      return 1;
   else if (!type_const_bounds(type))
      return 0;   // Not yet supported

   const int width = type_width(type);
   if (db->array_limit != 0 && width >= db->array_limit)
      return 0;

   // TODO: perhaps make memory coverage a different coverage type
   const bool memory = type_is_array(type_elem(type));
   if (!cover_enabled(db, COVER_MASK_TOGGLE_INCLUDE_MEMS) && memory)
      return 0;

   return width;
}

static void cover_add_array_toggle_items(cover_data_t *data,
                                         cover_scope_t *cs,
                                         type_t type,
                                         const char *prefix, int curr_dim,
                                         cover_flags_t flags,
                                         cover_obj_t **binp)
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

   LOCAL_TEXT_BUF tb = tb_new();

   for (int64_t i = left; i != right + inc; i += inc) {
      tb_rewind(tb);
      tb_printf(tb, "%s(%"PRIi64")", prefix, i);

      // On lowest dimension walk through elements, if elements
      // are arrays, then start new (nested) recursion.
      if (curr_dim == 1) {
         if (memory)
            cover_add_array_toggle_items(data, cs, elem, tb_get(tb),
                                         dimension_of(elem), flags, binp);
         else {
            cover_obj_t *pair = *binp;
            *binp += 2;

            cover_put_flags(data, pair[0], COV_FLAG_TOGGLE_TO_1);
            cover_put_flags(data, pair[1], COV_FLAG_TOGGLE_TO_0);

            for (int j = 0; j < 2; j++) {
               ident_t hier = cover_get_ident(data, pair[j], COV_ATTR_HIER);
               cover_put_ident(data, pair[j], COV_ATTR_HIER,
                               ident_sprintf("%s%s.%s", istr(hier),
                                             tb_get(tb), binstr[j]));
            }
         }
      }
      else   // Recurse to lower dimension
         cover_add_array_toggle_items(data, cs, type, tb_get(tb),
                                      curr_dim - 1, flags, binp);
   }
}

static void cover_add_record_toggle_items(cover_data_t *db,
                                          cover_scope_t *cs,
                                          type_t type,
                                          const char *prefix,
                                          cover_flags_t flags,
                                          cover_obj_t **binp,
                                          unsigned *field_idx)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, prefix);
   tb_append(tb, '.');

   const size_t base = tb_len(tb);

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(type, i);
      type_t ftype = tree_type(f);

      tb_trim(tb, base);
      tb_istr(tb, tree_ident(f));

      if (type_is_record(ftype)) {
         cover_add_record_toggle_items(db, cs, ftype,
                                       tb_get(tb), flags, binp, field_idx);
         continue;
      }

      const unsigned fidx = (*field_idx)++;

      // TODO: cache this
      const int count = cover_count_toggle_elems(db, ftype);
      if (count == 0)
         continue;
      else if (type_is_scalar(ftype)) {
         assert(count == 1);

         cover_obj_t *pair = *binp;
         *binp += 2;

         cover_put_flags(db, pair[0], COV_FLAG_TOGGLE_TO_1);
         cover_put_flags(db, pair[1], COV_FLAG_TOGGLE_TO_0);

         for (int i = 0; i < 2; i++) {
            cover_flags_t flags = cover_get_flags(db, pair[i]);
            ident_t hier = cover_get_ident(db, pair[i], COV_ATTR_HIER);

            char suffix[64];
            checked_sprintf(suffix, sizeof(suffix), "%s.%s", tb_get(tb),
                            cover_bmask_to_bin_str(flags));

            cover_put_ident(db, pair[i], COV_ATTR_HIER,
                            ident_prefix(hier, ident_new(suffix), '\0'));

            cover_put_u32(db, pair[i], COV_ATTR_FIELD_IDX, fidx);
         }
      }
      else if (type_is_array(ftype)) {
         cover_obj_t *set = *binp;
         const int ndims = dimension_of(ftype);
         cover_add_array_toggle_items(db, cs, ftype, tb_get(tb), ndims,
                                      flags, binp);

         for (int i = 0; i < count * 2; i++)
            cover_put_u32(db, set[i], COV_ATTR_FIELD_IDX, fidx);
      }
      else
         should_not_reach_here();
   }
}

static cover_obj_t cover_add_toggle_items(cover_data_t *data,
                                          cover_scope_t *cs,
                                          object_t *obj)
{
   assert(data != NULL);

   tree_t decl = tree_from_object(obj);
   type_t type = tree_type(decl);

   const int nelems = cover_count_toggle_elems(data, type);
   if (nelems == 0)
      return COVER_NULL_OBJ;

   cover_flags_t flags = 0;
   if (tree_kind(decl) == T_SIGNAL_DECL)
      flags |= COV_FLAG_TOGGLE_SIGNAL;
   else
      flags |= COV_FLAG_TOGGLE_PORT;

   if (type_is_record(type)) {
      cover_obj_t set = cover_add_item(data, cs, obj, COV_ITEM_TOGGLE,
                                       flags, nelems * 2);

      cover_obj_t *bins LOCAL = xmalloc_array(nelems * 2, sizeof(cover_obj_t));
      cover_rel(data, set, COV_REL_BINS, 0, bins, nelems * 2);

      cover_obj_t *p = bins;
      unsigned field_idx = 0;
      cover_add_record_toggle_items(data, cs, type, "", flags, &p, &field_idx);
      assert(p == bins + nelems * 2);

      return set;
   }

   type_t root = type_base_recur(type_elem_recur(type));

   well_known_t known = is_well_known(type_ident(root));
   if (known != W_IEEE_ULOGIC && known != W_IEEE_ULOGIC_VECTOR)
      return COVER_NULL_OBJ;

   if (type_is_scalar(type)) {
      cover_obj_t pair = cover_add_item(data, cs, obj, COV_ITEM_TOGGLE,
                                        flags, 2);

      cover_obj_t bins[2];
      cover_rel(data, pair, COV_REL_BINS, 0, bins, 2);

      static const cover_flags_t flags[2] = {
         COV_FLAG_TOGGLE_TO_1, COV_FLAG_TOGGLE_TO_0
      };

      for (int i = 0; i < 2; i++) {
         cover_put_flags(data, bins[i], flags[i]);

         ident_t suffix = ident_new(cover_bmask_to_bin_str(flags[i]));
         ident_t prefix = cover_get_ident(data, bins[i], COV_ATTR_HIER);
         cover_put_ident(data, bins[i], COV_ATTR_HIER,
                         ident_prefix(prefix, suffix, '.'));
      }

      return pair;
   }
   else {
      cover_obj_t set = cover_add_item(data, cs, obj, COV_ITEM_TOGGLE,
                                       flags, nelems * 2);

      cover_obj_t *bins LOCAL = xmalloc_array(nelems * 2, sizeof(cover_obj_t));
      cover_rel(data, set, COV_REL_BINS, 0, bins, nelems * 2);

      cover_obj_t *p = bins;
      const int ndims = dimension_of(type);
      cover_add_array_toggle_items(data, cs, type, "", ndims, flags, &p);
      assert(p == bins + nelems * 2);

      return set;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Branch coverage item emit
///////////////////////////////////////////////////////////////////////////////

static cover_obj_t cover_add_branch_items_for(cover_data_t *data,
                                              cover_scope_t *cs,
                                              object_t *obj)
{
   tree_t b = tree_from_object(obj);

   if (tree_kind(b) == T_CHOICE) {  // Case choice
      cover_obj_t item = cover_add_item(data, cs, obj, COV_ITEM_BRANCH,
                                        COV_FLAG_CHOICE, 1);

      cover_obj_t bin0 = cover_at(data, item, COV_REL_BINS, 0);

      ident_t suffix = ident_new(cover_bmask_to_bin_str(COV_FLAG_CHOICE));
      ident_t prefix = cover_get_ident(data, bin0, COV_ATTR_HIER);
      cover_put_ident(data, bin0, COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));

      return item;
   }
   else {    // If-else
      cover_obj_t pair = cover_add_item(data, cs, obj, COV_ITEM_BRANCH, 0, 2);

      static const cover_flags_t bin_flags[] = {
         COV_FLAG_TRUE, COV_FLAG_FALSE
      };

      cover_obj_t bins[2];
      cover_rel(data, pair, COV_REL_BINS, 0, bins, 2);

      for (int i = 0; i < 2; i++) {
         cover_put_flags(data, bins[i], bin_flags[i]);

         ident_t suffix = ident_new(cover_bmask_to_bin_str(bin_flags[i]));
         ident_t prefix = cover_get_ident(data, bins[i], COV_ATTR_HIER);
         cover_put_ident(data, bins[i], COV_ATTR_HIER,
                         ident_prefix(prefix, suffix, '.'));
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

   ident_t name = ident_rfrom(type_ident(type), '.');
   return !cover_should_emit_fsm_type(data, name);
}

static cover_obj_t cover_add_state_items_for(cover_data_t *data,
                                             cover_scope_t *cs,
                                             object_t *obj)
{
   type_t type = tree_type(tree_from_object(obj));

   if (cover_skip_type_state(data, type))
      return COVER_NULL_OBJ;

   int64_t low, high;
   if (!folded_bounds(range_of(type, 0), &low, &high))
      return COVER_NULL_OBJ;

   cover_obj_t set = cover_add_item(data, cs, obj, COV_ITEM_STATE,
                                    COV_FLAG_STATE, high - low + 1);

   // Add single coverage item per enum literal. This is to track
   // literal string in the identifier of the coverage item.
   type_t base = type_base_recur(type);
   assert(type_is_enum(base));
   ident_t itype = type_ident(type);

   cover_obj_t *bins LOCAL = xmalloc_array(high - low + 1, sizeof(cover_obj_t));
   cover_rel(data, set, COV_REL_BINS, 0, bins, high - low + 1);

   for (int64_t i = low; i <= high; i++) {
      ident_t literal = tree_ident(type_enum_literal(base, i));
      ident_t suffix = ident_prefix(ident_new("BIN_STATE"), literal, '.');

      // For FSM State coverage, "func_name" stores name of the FSM Enum type
      ident_t prefix = cover_get_ident(data, bins[i - low], COV_ATTR_HIER);
      cover_put_ident(data, bins[i - low], COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));
   }

   cover_put_ident(data, set, COV_ATTR_FUNC_NAME, ident_rfrom(itype, '.'));
   cover_put_i64(data, set, COV_ATTR_METADATA, low);

   return set;
}

///////////////////////////////////////////////////////////////////////////////
// Expression coverage item emit
///////////////////////////////////////////////////////////////////////////////

static cover_obj_t cover_add_expression_items(cover_data_t *data,
                                              cover_scope_t *cs,
                                              object_t *obj)
{
   tree_t t = tree_from_object(obj);
   if (tree_kind(t) == T_PROT_FCALL)
      return COVER_NULL_OBJ;

   assert(tree_kind(t) == T_FCALL);

   cover_flags_t flags = 0;
   cover_obj_t set, bins[4];
   int nbins;

   switch (tree_subkind(tree_ref(t))) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_NOT:
      set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION, 0, 2);
      nbins = cover_rel(data, set, COV_REL_BINS, 0, bins, ARRAY_LEN(bins));

      cover_put_flags(data, bins[0], COV_FLAG_FALSE);
      cover_put_flags(data, bins[1], COV_FLAG_TRUE);
      break;

   case S_IEEE_XOR:
   case S_IEEE_XNOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
      set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION, flags, 4);
      nbins = cover_rel(data, set, COV_REL_BINS, 0, bins, ARRAY_LEN(bins));

      cover_put_flags(data, bins[0], COV_FLAG_00);
      cover_put_flags(data, bins[1], COV_FLAG_01);
      cover_put_flags(data, bins[2], COV_FLAG_10);
      cover_put_flags(data, bins[3], COV_FLAG_11);
      break;

   case S_IEEE_OR:
   case S_IEEE_NOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_OR:
   case S_SCALAR_NOR:
      set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION, flags, 3);
      nbins = cover_rel(data, set, COV_REL_BINS, 0, bins, ARRAY_LEN(bins));

      cover_put_flags(data, bins[0], COV_FLAG_00);
      cover_put_flags(data, bins[1], COV_FLAG_01);
      cover_put_flags(data, bins[2], COV_FLAG_10);
      break;

   case S_IEEE_AND:
   case S_IEEE_NAND:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
      set = cover_add_item(data, cs, obj, COV_ITEM_EXPRESSION, flags, 3);
      nbins = cover_rel(data, set, COV_REL_BINS, 0, bins, ARRAY_LEN(bins));

      cover_put_flags(data, bins[0], COV_FLAG_01);
      cover_put_flags(data, bins[1], COV_FLAG_10);
      cover_put_flags(data, bins[2], COV_FLAG_11);
      break;

   case S_IEEE_MISC:
   case S_IEEE_NOT:
   case S_USER:
      return COVER_NULL_OBJ;

   default:
      should_not_reach_here();
   }

   if (cover_get_flags(data, bins[0]) & COVER_FLAGS_LHS_RHS_BINS) {
      cover_put_loc(data, set, COV_ATTR_LHS_LOC, *tree_loc(tree_param(t, 0)));
      cover_put_loc(data, set, COV_ATTR_RHS_LOC, *tree_loc(tree_param(t, 1)));
   }

   cover_put_ident(data, set, COV_ATTR_FUNC_NAME, tree_ident(t));

   for (int i = 0; i < nbins; i++) {
      cover_flags_t flags = cover_get_flags(data, bins[i]);
      ident_t suffix = ident_new(cover_bmask_to_bin_str(flags));
      ident_t prefix = cover_get_ident(data, bins[i], COV_ATTR_HIER);
      cover_put_ident(data, bins[i], COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));
   }

   return set;
}

///////////////////////////////////////////////////////////////////////////////
// Lower EMIT API
///////////////////////////////////////////////////////////////////////////////

cover_obj_t cover_add_items_for(cover_data_t *data, cover_scope_t *cs,
                                object_t *obj, cover_item_kind_t kind)
{
   assert(data != NULL);

   if (!cs->emit)
      return COVER_NULL_OBJ;

   const loc_t loc = get_cover_loc(kind, obj);

   // Multiple scopes may be emitted from a single file for generate
   // statements, blocks, etc.
   for (cover_scope_t *ignore_scope = cs; ignore_scope->parent;
        ignore_scope = ignore_scope->parent) {
      for (int i = 0; i < ignore_scope->ignore_lines.count; i++) {
         ignore_range_t *ir = &(ignore_scope->ignore_lines.items[i]);
         if (ir->file_ref != loc.file_ref)
            continue;
         else if (loc.first_line > ir->start && loc.first_line <= ir->end)
            return COVER_NULL_OBJ;
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

void cover_add_ranges(cover_data_t *db, cover_obj_t obj, unsigned count)
{
   cover_bin_t *bin = cover_bin_data(db, obj);
   assert(cover_is_null(bin->first_range));

   bin->first_range = cover_alloc_ranges(db, count);
   bin->n_ranges = count;
}

///////////////////////////////////////////////////////////////////////////////
// Coverage data write/read to covdb, covdb merging and coverage scope handling
///////////////////////////////////////////////////////////////////////////////

void cover_merge_bin(cover_data_t *db, cover_obj_t obj, int32_t data)
{
   cover_bin_t *bin = cover_bin_data(db, obj);

   const bool is_toggle_bin =
      !!(bin->flags & (COV_FLAG_TOGGLE_TO_0 | COV_FLAG_TOGGLE_TO_1));

   if (is_toggle_bin) {
      // Highest bit tracks unreachability#
      if ((bin->data & COV_FLAG_UNREACHABLE) || (data & COV_FLAG_UNREACHABLE))
         bin->data = COV_FLAG_UNREACHABLE;
      else
         bin->data = saturate_add(bin->data, data);
   }
   else
      bin->data = saturate_add(bin->data, data);
}

static void cover_update_counts(cover_data_t *db, cover_scope_t *s)
{
   if (s->block != NULL && s->block->data != NULL) {
      for (int i = 0; i < s->items.count; i++) {
         cover_obj_t item = s->items.items[i];

         cover_obj_t chunk[10];
         int pos = 0, nbins;
         do {
            nbins = cover_rel(db, item, COV_REL_BINS, pos, chunk,
                              ARRAY_LEN(chunk));
            pos += ARRAY_LEN(chunk);

            for (int j = 0; j < MIN(nbins, ARRAY_LEN(chunk)); j++) {
               uint32_t tag = cover_get_tag(db, chunk[j]);
               cover_merge_bin(db, chunk[j], s->block->data[tag]);
            }
         } while (nbins > ARRAY_LEN(chunk));
      }
   }

   for (int i = 0; i < s->children.count; i++)
      cover_update_counts(db, s->children.items[i]);
}

static void cover_write_scope(cover_data_t *db, cover_scope_t *s, fbuf_t *f,
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
      assert(s->items.items[i].tag == COVER_TAG_ITEM);
      fbuf_put_uint(f, s->items.items[i].id);
   }

   for (int i = 0; i < s->children.count; i++)
      cover_write_scope(db, s->children.items[i], f, ident_ctx, loc_ctx);

   write_u8(CTRL_POP_SCOPE, f);
}

LCOV_EXCL_START
static void cover_debug_dump(cover_data_t *db, cover_scope_t *s, int indent)
{
   nvc_printf("%*s$!blue$%s$$", indent, "", istr(s->name));
   if (s->block_name != NULL)
      printf(" : %s", istr(s->block_name));
   nvc_printf("\n");

   for (int i = 0; i < s->items.count; i++) {
      cover_obj_t item = s->items.items[i];

      int nbins = cover_count(db, item, COV_REL_BINS);
      cover_item_kind_t kind = cover_get_kind(db, item);
      loc_t loc = cover_get_loc(db, item, COV_ATTR_LOC);

      for (int j = 0; j < nbins; j++) {
         cover_obj_t bin = cover_at(db, item, COV_REL_BINS, j);

         uint32_t tag = cover_get_u32(db, bin, COV_ATTR_TAG, -1);
         uint32_t data = cover_get_u32(db, bin, COV_ATTR_DATA, -1);
         ident_t hier = cover_get_ident(db, bin, COV_ATTR_HIER);

         if (loc_invalid_p(&loc))
            printf("%*s%d: %s %s <invalid> => %x\n", indent + 2, "",
                   tag, cover_item_kind_str(kind), istr(hier), data);
         else {
            const char *path = loc_file_str(&loc), *basename;
            if ((basename = strrchr(path, '/')))
               path = basename + 1;

            printf("%*s%d: %s %s %s:%d => %x\n", indent + 2, "", tag,
                   cover_item_kind_str(kind), istr(hier),
                   path, loc.first_line, data);
         }
      }
   }

   for (int i = 0; i < s->children.count; i++)
      cover_debug_dump(db, s->children.items[i], indent + 2);
}
LCOV_EXCL_STOP

void cover_write(cover_data_t *db, fbuf_t *f, cover_dump_t dt)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(db, db->root_scope);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(db, db->root_scope, 0);

   write_u32(COVER_FILE_MAGIC, f);
   fbuf_put_uint(f, COVER_FILE_VERSION);

   fbuf_put_uint(f, db->mask);
   fbuf_put_uint(f, db->array_limit);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   fbuf_put_uint(f, db->items.count);
   for (int i = 0; i < db->items.count; i++) {
      const cover_item_t *item = &(db->items.items[i]);

      fbuf_put_uint(f, item->nbins);
      fbuf_put_uint(f, item->kind);
      fbuf_put_uint(f, item->source);
      fbuf_put_uint(f, item->atleast);
      fbuf_put_uint(f, item->metadata);

      loc_write(&(item->loc), loc_wr);

      if (item->kind == COV_ITEM_EXPRESSION
          || item->kind == COV_ITEM_STATE
          || item->kind == COV_ITEM_FUNCTIONAL)
         ident_write(item->func_name, ident_ctx);

      if (item->kind == COV_ITEM_EXPRESSION) {
         loc_write(&(item->loc_lhs), loc_wr);
         loc_write(&(item->loc_rhs), loc_wr);
      }

      if (item->nbins > 0)
         fbuf_put_uint(f, item->first_bin.id);
   }

   fbuf_put_uint(f, db->ranges.count);
   for (int i = 0; i < db->ranges.count; i++) {
      fbuf_put_uint(f, db->ranges.items[i].min);
      fbuf_put_uint(f, db->ranges.items[i].max);
   }

   fbuf_put_uint(f, db->bins.count);
   for (int i = 0; i < db->bins.count; i++) {
      cover_bin_t *bin = &(db->bins.items[i]);

      fbuf_put_uint(f, bin->tag);
      fbuf_put_uint(f, bin->data);
      fbuf_put_uint(f, bin->flags);
      fbuf_put_uint(f, bin->field_idx);
      fbuf_put_uint(f, bin->n_ranges);

      if (bin->n_ranges > 0)
         fbuf_put_uint(f, bin->first_range.id);

      ident_write(bin->hier, ident_ctx);
   }

   cover_write_scope(db, db->root_scope, f, ident_ctx, loc_wr);

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

   free(db->items.items);
   free(db->bins.items);
   free(db->ranges.items);
   hash_free(db->blocks);
   pool_free(db->pool);
   free(db);
}

bool cover_enabled(cover_data_t *data, cover_mask_t mask)
{
   return data != NULL && (data->mask & mask);
}

cover_scope_t *cover_create_block(cover_data_t *db, ident_t qual,
                                  cover_scope_t *parent, tree_t inst,
                                  tree_t unit)
{
   if (db == NULL)
      return NULL;

   cover_block_t *b = pool_calloc(db->pool, sizeof(cover_block_t));
   b->name = qual;

   assert(hash_get(db->blocks, qual) == NULL);
   hash_put(db->blocks, qual, b);

   if (parent == NULL) {
      assert(db->root_scope == NULL);

      parent = db->root_scope = pool_calloc(db->pool, sizeof(cover_scope_t));
      parent->loc = LOC_INVALID;
      parent->name = parent->hier = lib_name(lib_work());
   }

   b->self = cover_create_scope(db, parent, inst, tree_ident(inst));
   b->self->block = b;
   b->self->block_name = ident_rfrom(tree_ident(unit), '.');
   b->self->emit = cover_should_emit_scope(db, b->self);

   return b->self;
}

cover_scope_t *cover_create_user_scope(cover_data_t *db, cover_scope_t *parent,
                                       loc_t loc, ident_t name)
{
   if (db == NULL)
      return NULL;

   assert(parent != NULL);

   cover_scope_t *s = pool_calloc(db->pool, sizeof(cover_scope_t));
   s->name   = name;
   s->parent = parent;
   s->loc    = loc;
   s->hier   = ident_prefix(parent->hier, name, '.');
   s->emit   = cover_should_emit_scope(db, s);
   s->kind   = CSCOPE_USER;

   APUSH(parent->children, s);
   return s;
}

cover_scope_t *cover_create_scope(cover_data_t *db, cover_scope_t *parent,
                                  tree_t t, ident_t name)
{
   if (db == NULL)
      return NULL;

   assert(parent != NULL);
   assert(db->root_scope != NULL);
   assert(!is_design_unit(t));

   cover_scope_t *s = pool_calloc(db->pool, sizeof(cover_scope_t));

   switch (tree_kind(t)) {
   case T_BLOCK:
      s->kind = CSCOPE_INSTANCE;
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
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
      // For toggle coverage, remember the position where its name in
      // the hierarchy starts.
      s->sig_pos = ident_len(parent->hier) + 1;
      break;
   default:
      break;
   }

   s->parent = parent;
   s->block  = parent->block;
   s->name   = name;
   s->loc    = *tree_loc(t);
   s->hier   = ident_prefix(parent->hier, s->name, '.');
   s->emit   = cover_should_emit_scope(db, s);

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
   for (int i = 0; i < nitems; i++) {
      cover_obj_t obj = {
         .tag = COVER_TAG_ITEM,
         .id = fbuf_get_uint(f),
      };
      assert(obj.id < db->items.count);
      APUSH(s->items, obj);
   }

   for (;;) {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_UNIT:
         {
            ident_t name = ident_read(ident_ctx);
            assert(hash_get(db->blocks, name) == NULL);

            cover_block_t *b = pool_calloc(db->pool, sizeof(cover_block_t));
            b->name = name;
            b->next_tag = fbuf_get_uint(f);
            b->self = cover_read_scope(db, f, ident_ctx, loc_ctx, b, s);

            hash_put(db->blocks, b->name, b);

            APUSH(s->children, b->self);
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

cover_data_t *cover_read(fbuf_t *f, uint32_t pre_mask)
{
   cover_data_t *db = xcalloc(sizeof(cover_data_t));
   cover_read_header(f, db);
   db->mask |= pre_mask;
   db->blocks = hash_new(16);
   db->pool = pool_new();

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   db->items.count = db->items.limit = fbuf_get_uint(f);
   db->items.items = xmalloc_array(db->items.count, sizeof(cover_item_t));

   for (int i = 0; i < db->items.count; i++) {
      cover_item_t *item = &(db->items.items[i]);

      item->nbins = fbuf_get_uint(f);
      item->kind = fbuf_get_uint(f);
      item->source = fbuf_get_uint(f);
      item->atleast = fbuf_get_uint(f);
      item->metadata = fbuf_get_uint(f);

      loc_read(&item->loc, loc_rd);

      if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE
          || item->kind == COV_ITEM_FUNCTIONAL)
         item->func_name = ident_read(ident_ctx);

      if (item->kind == COV_ITEM_EXPRESSION) {
         loc_read(&item->loc_lhs, loc_rd);
         loc_read(&item->loc_rhs, loc_rd);
      }
      else {
         item->loc_lhs = LOC_INVALID;
         item->loc_rhs = LOC_INVALID;
      }

      if (item->nbins > 0) {
         item->first_bin.tag = COVER_TAG_BIN;
         item->first_bin.id = fbuf_get_uint(f);
      }
   }

   db->ranges.count = db->ranges.limit = fbuf_get_uint(f);
   db->ranges.items = xmalloc_array(db->ranges.count, sizeof(cover_range_t));

   for (int i = 0; i < db->ranges.count; i++) {
      db->ranges.items[i].min = fbuf_get_uint(f);
      db->ranges.items[i].max = fbuf_get_uint(f);
   }

   db->bins.count = db->bins.limit = fbuf_get_uint(f);
   db->bins.items = xmalloc_array(db->bins.count, sizeof(cover_bin_t));

   for (int i = 0; i < db->bins.count; i++) {
      cover_bin_t *bin = &(db->bins.items[i]);

      bin->tag = fbuf_get_uint(f);
      bin->data = fbuf_get_uint(f);
      bin->flags = fbuf_get_uint(f);
      bin->field_idx = fbuf_get_uint(f);
      bin->n_ranges = fbuf_get_uint(f);

      if (bin->n_ranges > 0) {
         bin->first_range.tag = COVER_TAG_RANGE;
         bin->first_range.id = fbuf_get_uint(f);
      }

      bin->hier = ident_read(ident_ctx);
   }

   bool eof = false;
   do {
      const uint8_t ctrl = read_u8(f);
      switch (ctrl) {
      case CTRL_PUSH_SCOPE:
         db->root_scope =
            cover_read_scope(db, f, ident_ctx, loc_rd, NULL, NULL);
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

   return db;
}

static bool cover_merge_items(cover_data_t *dst_db, const cover_data_t *src_db,
                              cover_obj_t dst, cover_obj_t src)
{
   if (cover_get_kind(dst_db, dst) != cover_get_kind(src_db, src))
      return false;

   int src_nbins = cover_count(src_db, src, COV_REL_BINS);
   int dst_nbins = cover_count(dst_db, dst, COV_REL_BINS);

   LOCAL_BIT_MASK missed;
   mask_init(&missed, src_nbins);
   mask_setall(&missed);

   for (int i = 0; i < src_nbins; i++) {
      cover_obj_t sbin = cover_at(src_db, src, COV_REL_BINS, i);
      cover_flags_t src_flags = cover_get_flags(src_db, sbin);
      ident_t src_hier = cover_get_ident(src_db, sbin, COV_ATTR_HIER);

      // Try the same index first assuming the scopes are identical
      cover_obj_t dbin_i = cover_at(dst_db, dst, COV_REL_BINS, i);
      if (!cover_is_null(dbin_i)) {
         cover_flags_t dst_flags = cover_get_flags(dst_db, dbin_i);
         ident_t dst_hier = cover_get_ident(dst_db, dbin_i, COV_ATTR_HIER);

         if (dst_flags == src_flags && dst_hier == src_hier) {
            uint32_t src_data = cover_get_u32(src_db, sbin, COV_ATTR_DATA, 0);
            cover_merge_bin(dst_db, dbin_i, src_data);
            mask_clear(&missed, i);
            continue;
         }
      }

      for (int j = 0; j < dst_nbins; j++) {
         if (i == j)
            continue;   // Checked above

         cover_obj_t dbin = cover_at(dst_db, dst, COV_REL_BINS, j);
         cover_flags_t dst_flags = cover_get_flags(dst_db, dbin);
         ident_t dst_hier = cover_get_ident(dst_db, dbin, COV_ATTR_HIER);

         if (dst_flags == src_flags && dst_hier == src_hier) {
            uint32_t src_data = cover_get_u32(src_db, sbin, COV_ATTR_DATA, 0);
            cover_merge_bin(dst_db, dbin, src_data);
            mask_clear(&missed, i);
            break;
         }
      }
   }

   const int nmissed = mask_popcount(&missed);

   if (nmissed == 0)
      return true;    // Merged all items
   else if (nmissed == src_nbins)
      return false;   // Unrelated

   // Append the unmerged items to the destination array

   const int new_count = dst_nbins + nmissed;
   cover_obj_t new_first = cover_alloc_bins(dst_db, new_count);

   cover_item_t *dst_data = cover_item_data(dst_db, dst);

   // Allocation may have moved the table
   cover_bin_t *sbins = cover_get_bins(src_db, cover_item_data(src_db, src));
   cover_bin_t *dbins = cover_get_bins(dst_db, dst_data);

   cover_bin_t *nbins = dst_db->bins.items + new_first.id;
   memcpy(nbins, dbins, dst_nbins * sizeof(cover_bin_t));

   cover_bin_t *ptr = nbins + dst_nbins;
   for (size_t i = -1; mask_iter(&missed, &i);)
      *ptr++ = sbins[i];
   assert(ptr == nbins + new_count);

   dst_data->first_bin = new_first;
   dst_data->nbins = new_count;
   return true;
}

static cover_obj_t cover_clone_item(cover_data_t *dst_db,
                                    const cover_data_t *src_db,
                                    cover_obj_t src)
{
   const cover_item_t *src_data = cover_item_data_const(src_db, src);
   cover_item_t copy = *src_data;

   copy.first_bin = cover_alloc_bins(dst_db, src_data->nbins);

   cover_bin_t *dst_bins = cover_get_bins(dst_db, &copy);
   cover_bin_t *src_bins = cover_get_bins(src_db, src_data);
   memcpy(dst_bins, src_bins, src_data->nbins * sizeof(cover_bin_t));

   cover_obj_t item_obj = {
      .tag = COVER_TAG_ITEM,
      .id = dst_db->items.count,
   };

   APUSH(dst_db->items, copy);

   return item_obj;
}

static cover_scope_t *cover_clone_scope(cover_data_t *dst_db,
                                        const cover_data_t *src_db,
                                        const cover_scope_t *src,
                                        cover_scope_t *parent,
                                        cover_block_t *parent_block)
{
   cover_scope_t *copy = pool_malloc(dst_db->pool, sizeof(cover_scope_t));
   memcpy(copy, src, sizeof(cover_scope_t));

   copy->parent = parent;
   copy->items = (cover_array_t)AINIT;
   copy->children = (scope_array_t)AINIT;
   copy->ignore_lines = (ignore_array_t)AINIT;

   cover_block_t *block = parent_block;
   if (src->block != NULL && src->block->self == src) {
      block = pool_malloc(dst_db->pool, sizeof(cover_block_t));
      memcpy(block, src->block, sizeof(cover_block_t));
      block->self = copy;
      block->data = NULL;
      hash_put(dst_db->blocks, block->name, block);
   }
   copy->block = block;

   for (int i = 0; i < src->items.count; i++)
      APUSH(copy->items, cover_clone_item(dst_db, src_db, src->items.items[i]));

   for (int i = 0; i < src->ignore_lines.count; i++)
      APUSH(copy->ignore_lines, src->ignore_lines.items[i]);

   for (int i = 0; i < src->children.count; i++) {
      cover_scope_t *child = cover_clone_scope(dst_db, src_db,
                                               src->children.items[i], copy,
                                               block);
      APUSH(copy->children, child);
   }

   return copy;
}

static void cover_merge_scope(cover_data_t *dst_db,
                              const cover_data_t *src_db,
                              cover_scope_t *dst_s,
                              const cover_scope_t *src_s,
                              merge_mode_t mode)
{
   for (int i = 0; i < src_s->items.count; i++) {
      cover_obj_t src = AGET(src_s->items, i);

      // Try the same index first assuming the scopes are identical
      if (i < dst_s->items.count) {
         cover_obj_t dst = AGET(dst_s->items, i);
         if (cover_merge_items(dst_db, src_db, dst, src))
            continue;
      }

      bool merged = false;
      for (int j = 0; j < dst_s->items.count; j++) {
         if (i == j)
            continue;
         else {
            cover_obj_t dst = AGET(dst_s->items, j);
            if ((merged = cover_merge_items(dst_db, src_db, dst, src)))
               break;
         }
      }

      if (!merged) {
         // TOOD: if mode == MERGE_UNION add to dst_s->items?
      }
   }

   for (int i = 0; i < src_s->children.count; i++) {
      cover_scope_t *new_c = src_s->children.items[i];
      bool found = false;
      for (int j = 0; j < dst_s->children.count; j++) {
         cover_scope_t *old_c = dst_s->children.items[j];
         if (new_c->name == old_c->name) {
            cover_merge_scope(dst_db, src_db, old_c, new_c, mode);
            found = true;
            break;
         }
      }

      if (!found && mode == MERGE_UNION) {
         cover_scope_t *copy = cover_clone_scope(dst_db, src_db, new_c, dst_s,
                                                 dst_s->block);
         APUSH(dst_s->children, copy);
      }
   }
}

void cover_merge(cover_data_t *dst, const cover_data_t *src, merge_mode_t mode)
{
   cover_merge_scope(dst, src, dst->root_scope, src->root_scope, mode);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(dst, dst->root_scope, 0);
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

cover_scope_t *cover_get_child(cover_scope_t *s, ident_t name)
{
   if (s == NULL)
      return NULL;

   for (int i = 0; i < s->children.count; i++) {
      if (s->children.items[i]->name == name)
         return s->children.items[i];
   }

   return NULL;
}

cover_obj_t cover_get_item(cover_data_t *db, cover_scope_t *s,
                           cover_item_kind_t kind, int nth)
{
   if (s == NULL)
      return COVER_NULL_OBJ;

   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = &(db->items.items[s->items.items[i].id]);
      if (item->kind == kind && nth-- == 0)
         return s->items.items[i];
   }

   return COVER_NULL_OBJ;
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

bool cover_bin_unreachable(cover_data_t *data, cover_item_kind_t kind,
                           const cover_bin_t *bin)
{
   if ((data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Toggle items remember unreachability in run-time data. Must check
   // item kind not to get false unreachability on statement items.
   // Excludes both bins automatically!
   if (kind == COV_ITEM_TOGGLE
       && ((bin->data & COV_FLAG_UNREACHABLE) != 0))
      return true;

   // Expression items remember unreachability as unreachable mask
   if (kind == COV_ITEM_EXPRESSION
       && ((bin->flags & COV_FLAG_UNREACHABLE) != 0))
      return true;

   return false;
}

size_t cover_count(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel)
{
   return cover_rel(db, obj, rel, 0, NULL, 0);
}

static size_t cover_rel_contig(unsigned rel_count, cover_obj_t rel_first,
                               unsigned first, cover_obj_t *out, size_t max)
{
   if (first >= rel_count)
      return 0;

   const size_t count = rel_count - first;

   for (int i = 0; i < MIN(count, max); i++) {
      out[i].tag = rel_first.tag;
      out[i].id  = rel_first.id + first + i;
   }

   return count;
}

size_t cover_rel(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel,
                 unsigned first, cover_obj_t *out, size_t max)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (rel) {
         case COV_REL_BINS:
            return cover_rel_contig(item->nbins, item->first_bin,
                                    first, out, max);
         default:
            return 0;
         }
      }
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (rel) {
         case COV_REL_RANGES:
            return cover_rel_contig(bin->n_ranges, bin->first_range,
                                    first, out, max);
         default:
            return 0;
         }
      }
   default:
      return 0;
   }
}

cover_obj_t cover_at(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel,
                     unsigned index)
{
   cover_obj_t result[1];
   if (cover_rel(db, obj, rel, index, result, 1) == 0)
      return COVER_NULL_OBJ;
   else
      return result[0];
}

uint32_t cover_get_u32(const cover_data_t *db, cover_obj_t obj,
                       cover_attr_t attr, uint32_t def)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_KIND:    return item->kind;
         case COV_ATTR_SOURCE:  return item->source;
         case COV_ATTR_ATLEAST: return item->atleast;
         default:               return def;
         }
      }
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_TAG:       return bin->tag;
         case COV_ATTR_FLAGS:     return bin->flags;
         case COV_ATTR_FIELD_IDX: return bin->field_idx;
         case COV_ATTR_DATA:      return bin->data;
         default:                 return def;
         }
      }
   default:
      return def;
   }
}

int64_t cover_get_i64(const cover_data_t *db, cover_obj_t obj,
                      cover_attr_t attr, int64_t def)
{
   switch (obj.tag) {
   case COVER_TAG_RANGE:
      {
         const cover_range_t *r = cover_range_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_MAX: return r->max;
         case COV_ATTR_MIN: return r->min;
         default:           return def;
         }
      }
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_METADATA: return item->metadata;
         default:                return def;
         }
      }
   default:
      return def;
   }
}

ident_t cover_get_ident(const cover_data_t *db, cover_obj_t obj,
                        cover_attr_t attr)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_HIER: return bin->hier;
         default:            return NULL;
         }
      }
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_FUNC_NAME: return item->func_name;
         default:                 return NULL;
         }
      }
   default:
      return NULL;
   }
}

loc_t cover_get_loc(const cover_data_t *db, cover_obj_t obj, cover_attr_t attr)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_LOC:     return item->loc;
         case COV_ATTR_LHS_LOC: return item->loc_lhs;
         case COV_ATTR_RHS_LOC: return item->loc_rhs;
         default:               return LOC_INVALID;
         }
      }
   default:
      return LOC_INVALID;
   }
}

cover_flags_t cover_get_flags(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_FLAGS, 0);
}

uint32_t cover_get_tag(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_TAG, -1);
}

unsigned cover_get_kind(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_KIND, -1);
}

unsigned cover_get_threshold(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_ATLEAST, db->threshold);
}

void cover_put_u32(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   uint32_t value)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         cover_bin_t *bin = cover_bin_data(db, obj);
         switch (attr) {
         case COV_ATTR_FLAGS:
            bin->flags = value;
            return;
         case COV_ATTR_FIELD_IDX:
            bin->field_idx = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_SOURCE:
            item->source = value;
            return;
         case COV_ATTR_ATLEAST:
            item->atleast = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_i64(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   int64_t value)
{
   switch (obj.tag) {
   case COVER_TAG_RANGE:
      {
         cover_range_t *r = cover_range_data(db, obj);
         switch (attr) {
         case COV_ATTR_MAX:
            r->max = value;
            return;
         case COV_ATTR_MIN:
            r->min = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_METADATA:
            item->metadata = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_ident(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                     ident_t value)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         cover_bin_t *bin = cover_bin_data(db, obj);
         switch (attr) {
         case COV_ATTR_HIER:
            bin->hier = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_FUNC_NAME:
            item->func_name = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_loc(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   loc_t value)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_LOC:
            item->loc = value;
            return;
         case COV_ATTR_LHS_LOC:
            item->loc_lhs = value;
            return;
         case COV_ATTR_RHS_LOC:
            item->loc_rhs = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_flags(cover_data_t *db, cover_obj_t obj, cover_flags_t flags)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      cover_bin_data(db, obj)->flags |= flags;
      break;
   default:
      should_not_reach_here();
   }
}
