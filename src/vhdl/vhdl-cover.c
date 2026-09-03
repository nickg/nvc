//
//  Copyright (C) 2026  Nick Gasson
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
#include "ident.h"
#include "tree.h"
#include "type.h"
#include "printf.h"
#include "vhdl/vhdl-phase.h"
#include "vhdl/vhdl-util.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <inttypes.h>

typedef struct _lazy_cscope lazy_cscope_t;

typedef struct _lazy_cscope {
   lazy_cscope_t *parent;
   cover_obj_t    cscope;
   tree_t         tree;
   int            nth;
} lazy_cscope_t;

typedef struct {
   file_ref_t file_ref;
   int        start;
   int        end;
} ignore_range_t;

typedef A(ignore_range_t) ignore_array_t;

typedef struct {
   cover_data_t   *data;
   ignore_array_t  ignore;
} vhdl_cover_t;

static void vhdl_cover_stmts(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent);

static cover_obj_t get_cover_scope(vhdl_cover_t *g, lazy_cscope_t *lcs)
{
   if (!cover_is_null(lcs->cscope))
      return lcs->cscope;
   else {
      cover_obj_t parent = get_cover_scope(g, lcs->parent);
      ident_t name = vhdl_scope_name(lcs->tree, lcs->nth);
      return (lcs->cscope = cover_scope_new(g->data, parent, CSCOPE_NONE, name,
                                            *tree_loc(lcs->tree)));
   }
}

static bool is_coverage_off(vhdl_cover_t *g, loc_t loc)
{
   for (int i = 0; i < g->ignore.count; i++) {
      ignore_range_t *ir = &(g->ignore.items[i]);
      if (ir->file_ref != loc.file_ref)
         continue;
      else if (loc.first_line > ir->start && loc.first_line <= ir->end)
         return true;
   }

   return false;
}

static lazy_cscope_t lazy_cover_scope(tree_t t, lazy_cscope_t *parent, int nth)
{
   lazy_cscope_t lcs = { parent, COVER_NULL_OBJ, t, nth };
   return lcs;
}

static void vhdl_cover_branch(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_BRANCH))
      return;

   loc_t loc;
   if (tree_kind(t) != T_CHOICE) {
      // Refer location of test condition instead of branch statement to
      // get accurate test condition location in the coverage report
      loc = *tree_loc(tree_value(t));
   }
   else
      loc = *tree_loc(t);

   if (is_coverage_off(g, loc))
      return;

   cover_obj_t cs = get_cover_scope(g, parent);

   if (tree_kind(t) == T_CHOICE) {  // Case choice
      cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_BRANCH, 1);
      if (cover_is_null(item))
         return;

      cover_put_loc(g->data, item, COV_ATTR_LOC, loc);
      cover_put_u32(g->data, item, COV_ATTR_SOURCE, COV_SRC_CASE_CHOICE);

      cover_obj_t bin0 = cover_at(g->data, item, COV_REL_BINS, 0);

      cover_put_flags(g->data, bin0, COV_FLAG_CHOICE);

      ident_t suffix = ident_new(cover_bmask_to_bin_str(COV_FLAG_CHOICE));
      ident_t prefix = cover_get_ident(g->data, bin0, COV_ATTR_HIER);
      cover_put_ident(g->data, bin0, COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));
   }
   else {    // If-else
      cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_BRANCH, 2);
      if (cover_is_null(item))
         return;

      cover_src_t src;
      switch (tree_kind(t)) {
      case T_COND_STMT:
      case T_COND_ASSIGN:
         src = COV_SRC_IF_CONDITION;
         break;
      case T_CHOICE:
      case T_WHILE:
      case T_EXIT:
      case T_NEXT:
         src = COV_SRC_LOOP_CONTROL;
         break;
      default:
         src = COV_SRC_CONDITION;
      }

      cover_put_loc(g->data, item, COV_ATTR_LOC, loc);
      cover_put_u32(g->data, item, COV_ATTR_SOURCE, src);

      static const cover_flags_t bin_flags[] = {
         COV_FLAG_TRUE, COV_FLAG_FALSE
      };

      cover_obj_t bins[2];
      cover_rel(g->data, item, COV_REL_BINS, 0, bins, 2);

      for (int i = 0; i < 2; i++) {
         cover_put_flags(g->data, bins[i], bin_flags[i]);

         ident_t suffix = ident_new(cover_bmask_to_bin_str(bin_flags[i]));
         ident_t prefix = cover_get_ident(g->data, bins[i], COV_ATTR_HIER);
         cover_put_ident(g->data, bins[i], COV_ATTR_HIER,
                         ident_prefix(prefix, suffix, '.'));
      }
   }
}

static void vhdl_cover_stmt(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_STMT))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   cover_obj_t cs = get_cover_scope(g, parent);

   cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_STMT, 1);
   if (cover_is_null(item))
      return;

   cover_put_loc(g->data, item, COV_ATTR_LOC, loc);

   cover_src_t src;
   switch (tree_kind(t)) {
   case T_ASSERT:
      src = tree_has_value(t) ? COV_SRC_ASSERT : COV_SRC_REPORT;
      break;
   case T_WAIT:
      src = COV_SRC_WAIT;
      break;
   case T_FOR:
   case T_WHILE:
      src = COV_SRC_LOOP_STMT;
      break;
   case T_SIGNAL_ASSIGN:
      src = COV_SRC_SIGNAL_ASSIGN;
      break;
   case T_VAR_ASSIGN:
      src = COV_SRC_VAR_ASSIGN;
      break;
   case T_IF:
      src = COV_SRC_IF_STMT;
      break;
   default:
      src = COV_SRC_STATEMENT;
      break;
   }

   cover_put_u32(g->data, item, COV_ATTR_SOURCE, src);
}

static void vhdl_cover_expr(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_EXPRESSION))
      return;
   else if (tree_kind(t) != T_FCALL)
      return;
   else if (!vhdl_is_logical(tree_subkind(tree_ref(t))))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   cover_flags_t flags = 0, bin_flags[4] = {};
   int nbins;

   switch (tree_subkind(tree_ref(t))) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_NOT:
      nbins = 2;
      bin_flags[0] = COV_FLAG_FALSE;
      bin_flags[1] = COV_FLAG_TRUE;
      break;

   case S_IEEE_XOR:
   case S_IEEE_XNOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
      nbins = 4;
      bin_flags[0] = COV_FLAG_00;
      bin_flags[1] = COV_FLAG_01;
      bin_flags[2] = COV_FLAG_10;
      bin_flags[3] = COV_FLAG_11;
      break;

   case S_IEEE_OR:
   case S_IEEE_NOR:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_OR:
   case S_SCALAR_NOR:
      nbins = 3;
      bin_flags[0] = COV_FLAG_00;
      bin_flags[1] = COV_FLAG_01;
      bin_flags[2] = COV_FLAG_10;
      break;

   case S_IEEE_AND:
   case S_IEEE_NAND:
      flags = COV_FLAG_EXPR_STD_LOGIC;
      // Fall-through
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
      nbins = 3;
      bin_flags[0] = COV_FLAG_01;
      bin_flags[1] = COV_FLAG_10;
      bin_flags[2] = COV_FLAG_11;
      break;

   case S_IEEE_MISC:
   case S_IEEE_NOT:
   case S_USER:
      return;

   default:
      should_not_reach_here();
   }

   cover_obj_t cs = get_cover_scope(g, parent);

   cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_EXPRESSION, nbins);
   if (cover_is_null(item))
      return;

   cover_obj_t bins[4];
   cover_rel(g->data, item, COV_REL_BINS, 0, bins, ARRAY_LEN(bins));

   if (bin_flags[0] & COVER_FLAGS_LHS_RHS_BINS) {
      cover_put_loc(g->data, item, COV_ATTR_LHS_LOC,
                    *tree_loc(tree_param(t, 0)));
      cover_put_loc(g->data, item, COV_ATTR_RHS_LOC,
                    *tree_loc(tree_param(t, 1)));
   }

   cover_put_ident(g->data, item, COV_ATTR_FUNC_NAME, tree_ident(t));
   cover_put_loc(g->data, item, COV_ATTR_LOC, loc);

   for (int i = 0; i < nbins; i++) {
      cover_put_flags(g->data, bins[i], bin_flags[i] | flags);

      ident_t suffix = ident_new(cover_bmask_to_bin_str(bin_flags[i]));
      ident_t prefix = cover_get_ident(g->data, bins[i], COV_ATTR_HIER);
      cover_put_ident(g->data, bins[i], COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));
   }

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++)
      vhdl_cover_expr(g, tree_value(tree_param(t, i)), parent);
}

static void vhdl_cover_states(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_STATE))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   type_t type = tree_type(t);
   if (!type_is_enum(type))
      return;

   // Ignore enums from built-in libraries
   if (is_well_known(tree_ident(type_container(type))) < NUM_WELL_KNOWN)
      return;

   ident_t name = ident_rfrom(type_ident(type), '.');
   if (!cover_should_emit_fsm_type(g->data, name))
      return;

   int64_t low, high;
   if (!folded_bounds(range_of(type, 0), &low, &high))
      return;

   cover_obj_t cs = get_cover_scope(g, parent);

   const int nbins = high - low + 1;
   cover_obj_t set = cover_item_new(g->data, cs, COV_ITEM_STATE, nbins);
   if (cover_is_null(set))
      return;

   // Add single coverage item per enum literal. This is to track
   // literal string in the identifier of the coverage item.
   type_t base = type_base_recur(type);
   ident_t itype = type_ident(type);

   cover_obj_t *bins LOCAL = xmalloc_array(high - low + 1, sizeof(cover_obj_t));
   cover_rel(g->data, set, COV_REL_BINS, 0, bins, high - low + 1);

   for (int64_t i = low; i <= high; i++) {
      ident_t literal = tree_ident(type_enum_literal(base, i));
      ident_t suffix = ident_prefix(ident_new("BIN_STATE"), literal, '.');

      // For FSM State coverage, "func_name" stores name of the FSM Enum type
      ident_t prefix = cover_get_ident(g->data, bins[i - low], COV_ATTR_HIER);
      cover_put_ident(g->data, bins[i - low], COV_ATTR_HIER,
                      ident_prefix(prefix, suffix, '.'));

      cover_put_flags(g->data, bins[i - low], COV_FLAG_STATE);
   }

   cover_put_ident(g->data, set, COV_ATTR_FUNC_NAME, ident_rfrom(itype, '.'));
   cover_put_i64(g->data, set, COV_ATTR_METADATA, low);
   cover_put_loc(g->data, set, COV_ATTR_LOC, loc);
}

static int vhdl_cover_count_toggle_elems(vhdl_cover_t *g, type_t type)
{
   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      int sum = 0;
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         sum += vhdl_cover_count_toggle_elems(g, ftype);
      }

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
   if (!cover_should_emit_array_toggle(g->data, width))
      return 0;

   // TODO: perhaps make memory coverage a different coverage type
   const bool memory = type_is_array(type_elem(type));
   if (!cover_enabled(g->data, COVER_MASK_TOGGLE_INCLUDE_MEMS) && memory)
      return 0;

   return width;
}

static void vhdl_cover_add_array_toggle_items(vhdl_cover_t *g,
                                              type_t type,
                                              const char *prefix,
                                              int curr_dim,
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
            vhdl_cover_add_array_toggle_items(g, elem, tb_get(tb),
                                              dimension_of(elem), flags, binp);
         else {
            cover_obj_t *pair = *binp;
            *binp += 2;

            cover_put_flags(g->data, pair[0], COV_FLAG_TOGGLE_TO_1 | flags);
            cover_put_flags(g->data, pair[1], COV_FLAG_TOGGLE_TO_0 | flags);

            for (int j = 0; j < 2; j++) {
               ident_t hier = cover_get_ident(g->data, pair[j], COV_ATTR_HIER);
               cover_put_ident(g->data, pair[j], COV_ATTR_HIER,
                               ident_sprintf("%s%s.%s", istr(hier),
                                             tb_get(tb), binstr[j]));
            }
         }
      }
      else   // Recurse to lower dimension
         vhdl_cover_add_array_toggle_items(g, type, tb_get(tb),
                                           curr_dim - 1, flags, binp);
   }
}

static void vhdl_cover_add_record_toggle_items(vhdl_cover_t *g,
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
         vhdl_cover_add_record_toggle_items(g, ftype, tb_get(tb),
                                            flags, binp, field_idx);
         continue;
      }

      const unsigned fidx = (*field_idx)++;

      // TODO: cache this
      const int count = vhdl_cover_count_toggle_elems(g, ftype);
      if (count == 0)
         continue;
      else if (type_is_scalar(ftype)) {
         assert(count == 1);

         cover_obj_t *pair = *binp;
         *binp += 2;

         static const cover_flags_t bin_flags[2] = {
            COV_FLAG_TOGGLE_TO_1, COV_FLAG_TOGGLE_TO_0
         };

         for (int i = 0; i < 2; i++) {
            ident_t hier = cover_get_ident(g->data, pair[i], COV_ATTR_HIER);

            char suffix[64];
            checked_sprintf(suffix, sizeof(suffix), "%s.%s", tb_get(tb),
                            cover_bmask_to_bin_str(bin_flags[i]));

            cover_put_ident(g->data, pair[i], COV_ATTR_HIER,
                            ident_prefix(hier, ident_new(suffix), '\0'));

            cover_put_u32(g->data, pair[i], COV_ATTR_FIELD_IDX, fidx);
            cover_put_flags(g->data, pair[i], flags | bin_flags[i]);
         }
      }
      else if (type_is_array(ftype)) {
         cover_obj_t *set = *binp;
         const int ndims = dimension_of(ftype);
         vhdl_cover_add_array_toggle_items(g, ftype, tb_get(tb), ndims,
                                           flags, binp);

         for (int i = 0; i < count * 2; i++)
            cover_put_u32(g->data, set[i], COV_ATTR_FIELD_IDX, fidx);
      }
      else
         should_not_reach_here();
   }
}

static void vhdl_cover_toggle(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_TOGGLE))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   type_t type = tree_type(t);

   const int nelems = vhdl_cover_count_toggle_elems(g, type);
   if (nelems == 0)
      return;

   cover_obj_t cs = get_cover_scope(g, parent);

   // Remember the position where its name in the hierarchy starts
   ident_t parent_hier = cover_get_ident(g->data, parent->parent->cscope,
                                         COV_ATTR_HIER);
   cover_put_u32(g->data, cs, COV_ATTR_SIG_POS, ident_len(parent_hier) + 1);

   cover_flags_t flags = 0;
   if (tree_kind(t) == T_SIGNAL_DECL)
      flags |= COV_FLAG_TOGGLE_SIGNAL;
   else
      flags |= COV_FLAG_TOGGLE_PORT;

   if (type_is_record(type)) {
      cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_TOGGLE,
                                        nelems * 2);
      if (cover_is_null(item))
         return;

      cover_put_loc(g->data, item, COV_ATTR_LOC, loc);

      cover_obj_t *bins LOCAL = xmalloc_array(nelems * 2, sizeof(cover_obj_t));
      cover_rel(g->data, item, COV_REL_BINS, 0, bins, nelems * 2);

      cover_obj_t *p = bins;
      unsigned field_idx = 0;
      vhdl_cover_add_record_toggle_items(g, type, "", flags, &p, &field_idx);
      assert(p == bins + nelems * 2);
   }

   type_t root = type_base_recur(type_elem_recur(type));

   well_known_t known = is_well_known(type_ident(root));
   if (known != W_IEEE_ULOGIC && known != W_IEEE_ULOGIC_VECTOR)
      return;

   if (type_is_scalar(type)) {
      cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_TOGGLE, 2);
      if (cover_is_null(item))
         return;

      cover_put_loc(g->data, item, COV_ATTR_LOC, loc);

      cover_obj_t bins[2];
      cover_rel(g->data, item, COV_REL_BINS, 0, bins, 2);

      static const cover_flags_t bin_flags[2] = {
         COV_FLAG_TOGGLE_TO_1, COV_FLAG_TOGGLE_TO_0
      };

      for (int i = 0; i < 2; i++) {
         cover_put_flags(g->data, bins[i], bin_flags[i] | flags);

         ident_t suffix = ident_new(cover_bmask_to_bin_str(bin_flags[i]));
         ident_t prefix = cover_get_ident(g->data, bins[i], COV_ATTR_HIER);
         cover_put_ident(g->data, bins[i], COV_ATTR_HIER,
                         ident_prefix(prefix, suffix, '.'));
      }
   }
   else {
      cover_obj_t item = cover_item_new(g->data, cs, COV_ITEM_TOGGLE,
                                        nelems * 2);
      if (cover_is_null(item))
         return;

      cover_put_loc(g->data, item, COV_ATTR_LOC, loc);

      cover_obj_t *bins LOCAL = xmalloc_array(nelems * 2, sizeof(cover_obj_t));
      cover_rel(g->data, item, COV_REL_BINS, 0, bins, nelems * 2);

      cover_obj_t *p = bins;
      const int ndims = dimension_of(type);
      vhdl_cover_add_array_toggle_items(g, type, "", ndims, flags, &p);
      assert(p == bins + nelems * 2);
   }
}

static void vhdl_cover_if(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(t, i);
      lazy_cscope_t lcs = lazy_cover_scope(c, parent, i);

      if (tree_has_value(c)) {
         vhdl_cover_expr(g, tree_value(c), &lcs);
         vhdl_cover_branch(g, c, &lcs);
      }

      vhdl_cover_stmts(g, c, &lcs);
   }
}

static void vhdl_cover_loop(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);
   vhdl_cover_stmts(g, t, parent);
}

static void vhdl_cover_while(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);
   vhdl_cover_branch(g, t, parent);
   vhdl_cover_stmts(g, t, parent);
}

static void vhdl_cover_case(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);
      lazy_cscope_t lcs = lazy_cover_scope(alt, parent, i);

      if (cover_enabled(g->data, COVER_MASK_BRANCH)) {
         const int nchoices = tree_choices(alt);
         for (int j = 0; j < nchoices; j++) {
            tree_t c = tree_choice(alt, j);
            vhdl_cover_branch(g, c, &lcs);
         }
      }

      vhdl_cover_stmts(g, alt, &lcs);
   }
}

static void vhdl_cover_signal_assign(vhdl_cover_t *g, tree_t t,
                                     lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);

   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++)
      vhdl_cover_expr(g, tree_value(tree_waveform(t, i)), parent);
}

static void vhdl_cover_wait(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   if (tree_flags(t) & TREE_F_STATIC_WAIT)
      return;   // Wait inserted by simp

   vhdl_cover_stmt(g, t, parent);
}

static void vhdl_cover_loop_control(vhdl_cover_t *g, tree_t t,
                                    lazy_cscope_t *parent)
{
   vhdl_cover_stmt(g, t, parent);

   if (tree_has_value(t))
      vhdl_cover_branch(g, t, parent);
}

static void vhdl_cover_stmts(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      lazy_cscope_t lcs = lazy_cover_scope(s, parent, i);

      switch (tree_kind(s)) {
      case T_IF:
         vhdl_cover_if(g, s, &lcs);
         break;
      case T_CASE:
         vhdl_cover_case(g, s, &lcs);
         break;
      case T_LOOP:
      case T_FOR:
         vhdl_cover_loop(g, s, &lcs);
         break;
      case T_WHILE:
         vhdl_cover_while(g, s, &lcs);
         break;
      case T_SIGNAL_ASSIGN:
         vhdl_cover_signal_assign(g, s, &lcs);
         break;
      case T_WAIT:
         vhdl_cover_wait(g, s, &lcs);
         break;
      case T_NEXT:
      case T_EXIT:
         vhdl_cover_loop_control(g, s, &lcs);
         break;
      default:
         vhdl_cover_stmt(g, s, &lcs);
         break;
      }
   }
}

static void vhdl_cover_subprogram(vhdl_cover_t *g, tree_t t,
                                  lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent, 0);
   vhdl_cover_stmts(g, t, &lcs);
}

static void vhdl_cover_signal_decl(vhdl_cover_t *g, tree_t t,
                                   lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent, 0);
   vhdl_cover_toggle(g, t, &lcs);
   vhdl_cover_states(g, t, &lcs);
}

static void vhdl_cover_port_decl(vhdl_cover_t *g, tree_t t,
                                 lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent, 0);
   vhdl_cover_toggle(g, t, &lcs);
}

static void vhdl_cover_decls(vhdl_cover_t *g, tree_t t, lazy_cscope_t *parent)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_BODY:
      case T_PROC_BODY:
         vhdl_cover_subprogram(g, d, parent);
         break;
      case T_SIGNAL_DECL:
         vhdl_cover_signal_decl(g, d, parent);
         break;
      default:
         break;
      }
   }
}

static void vhdl_cover_process(vhdl_cover_t *g, tree_t t, ident_t qual,
                               lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent, 0);
   vhdl_cover_stmts(g, t, &lcs);
   vhdl_cover_decls(g, t, &lcs);
}

static void vhdl_cover_inertial(vhdl_cover_t *g, tree_t t, ident_t qual,
                                lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent, 0);
   vhdl_cover_expr(g, tree_value(t), &lcs);
}

static void ignore_from_pragmas(vhdl_cover_t *g, tree_t unit)
{
   if (!is_design_unit(unit))
      return;   // Generate block, etc.

   bool state = true;
   const int npragmas = tree_pragmas(unit);
   for (int i = 0; i < npragmas; i++) {
      tree_t p = tree_pragma(unit, i);
      const pragma_kind_t kind = tree_subkind(p);
      if (kind != PRAGMA_COVERAGE_OFF && kind != PRAGMA_COVERAGE_ON)
         continue;

      if (kind == PRAGMA_COVERAGE_OFF && state) {
         const loc_t *loc = tree_loc(p);
         ignore_range_t ir = {
            loc->file_ref,
            loc->first_line,
            INT_MAX
         };
         APUSH(g->ignore, ir);
         state = false;
      }
      else if (kind == PRAGMA_COVERAGE_ON && !state) {
         assert(g->ignore.count > 0);
         assert(g->ignore.items[g->ignore.count - 1].end == INT_MAX);
         g->ignore.items[g->ignore.count - 1].end = tree_loc(p)->first_line;
         state = true;
      }
   }
}

void vhdl_cover_block(tree_t block, cover_data_t *db, cover_obj_t cs)
{
   assert(tree_kind(block) == T_BLOCK);

   if (db == NULL || cover_is_null(cs))
      return;

   vhdl_cover_t g = {
      .data = db,
   };

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   int nstmts = tree_stmts(block);
   tree_t unit = tree_ref(hier);

   ignore_from_pragmas(&g, unit);

   tree_t container = tree_container(unit);
   if (container != unit)
      ignore_from_pragmas(&g, container);

   lazy_cscope_t lcs = { NULL, cs, block };

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++)
      vhdl_cover_port_decl(&g, tree_port(block, i), &lcs);

   ident_t sym_prefix = tree_ident2(hier);

   const int nparams = tree_params(block);
   for (int i = 0; i < nparams; i++) {
      tree_t actual = tree_value(tree_param(block, i));
      if (tree_kind(actual) == T_INERTIAL) {
         ident_t qual = ident_prefix(sym_prefix, tree_ident(actual), '.');
         vhdl_cover_inertial(&g, actual, qual, &lcs);
      }
   }

   vhdl_cover_decls(&g, block, &lcs);

   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_PROCESS:
         {
            ident_t qual = ident_prefix(sym_prefix, tree_ident(s), '.');
            vhdl_cover_process(&g, s, qual, &lcs);
         }
         break;
      default:
         break;
      }
   }

   ACLEAR(g.ignore);
}

ident_t vhdl_scope_name(tree_t t, int nth)
{
   switch (tree_kind(t)) {
   case T_BLOCK:
   case T_PROCESS:
   case T_PROC_BODY:
   case T_FUNC_BODY:
   case T_PSL_DIRECT:
   case T_PACK_INST:
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
   case T_INERTIAL:
      return tree_ident(t);
   case T_ALTERNATIVE:
      if (tree_choices(t) == 1) {
         tree_t c0 = tree_choice(t, 0);
         if (tree_ranges(c0) == 0 && !tree_has_name(c0))
            return ident_new("_B_OTHERS");
      }
      return ident_sprintf("_B%u", nth);
   case T_COND_STMT:
      return ident_sprintf("_B%u", nth);
   default:
      // Consider everything else as statement
      if (tree_has_ident(t))
         return tree_ident(t);
      else
         return ident_sprintf("_S%u", nth);
   }
}
