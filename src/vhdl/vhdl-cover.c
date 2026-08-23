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

typedef struct _lazy_cscope lazy_cscope_t;

typedef struct _lazy_cscope {
   lazy_cscope_t *parent;
   cover_scope_t *cscope;
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

static cover_scope_t *get_cover_scope(vhdl_cover_t *g, lazy_cscope_t *lcs)
{
   if (lcs->cscope != NULL)
      return lcs->cscope;
   else {
      cover_scope_t *parent = get_cover_scope(g, lcs->parent);
      ident_t name = vhdl_scope_name(lcs->tree, lcs->nth);
      return (lcs->cscope =
              cover_create_scope(g->data, parent, lcs->tree, name));
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
   lazy_cscope_t lcs = { parent, NULL, t, nth };
   return lcs;
}

static void vhdl_cover_branch(vhdl_cover_t *g, tree_t t,
                              lazy_cscope_t *parent)
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

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(g, parent);
   cover_add_items_for(g->data, cs, obj, COV_ITEM_BRANCH);
}

static void vhdl_cover_stmt(vhdl_cover_t *g, tree_t t,
                            lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_STMT))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(g, parent);
   cover_add_items_for(g->data, cs, obj, COV_ITEM_STMT);
}

static void vhdl_cover_expr(vhdl_cover_t *g, tree_t t,
                            lazy_cscope_t *parent)
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

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(g, parent);
   cover_add_items_for(g->data, cs, obj, COV_ITEM_EXPRESSION);

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++)
      vhdl_cover_expr(g, tree_value(tree_param(t, i)), parent);
}

static void vhdl_cover_states(vhdl_cover_t *g, tree_t t,
                              lazy_cscope_t *parent)
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

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(g, parent);
   cover_add_items_for(g->data, cs, obj, COV_ITEM_STATE);
}

static void vhdl_cover_toggle(vhdl_cover_t *g, tree_t t,
                              lazy_cscope_t *parent)
{
   if (!cover_enabled(g->data, COVER_MASK_TOGGLE))
      return;

   loc_t loc = *tree_loc(t);
   if (is_coverage_off(g, loc))
      return;

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(g, parent);
   cover_add_items_for(g->data, cs, obj, COV_ITEM_TOGGLE);
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

static void vhdl_cover_while(vhdl_cover_t *g, tree_t t,
                             lazy_cscope_t *parent)
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

static void vhdl_cover_wait(vhdl_cover_t *g, tree_t t,
                            lazy_cscope_t *parent)
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

static void vhdl_cover_decls(vhdl_cover_t *g, tree_t t,
                             lazy_cscope_t *parent)
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

void vhdl_cover_block(tree_t block, cover_data_t *db, cover_scope_t *cs)
{
   assert(tree_kind(block) == T_BLOCK);

   if (db == NULL || cs == NULL)
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
