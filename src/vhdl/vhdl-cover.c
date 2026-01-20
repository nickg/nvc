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
#include "cov/cov-api.h"
#include "ident.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-phase.h"

#include <assert.h>

typedef struct _lazy_cscope lazy_cscope_t;

typedef struct _lazy_cscope {
   lazy_cscope_t *parent;
   cover_scope_t *cscope;
   tree_t         tree;
} lazy_cscope_t;

static void vhdl_cover_stmts(tree_t t, cover_data_t *db, lazy_cscope_t *parent);

static cover_scope_t *get_cover_scope(cover_data_t *db, lazy_cscope_t *lcs)
{
   if (lcs->cscope != NULL)
      return lcs->cscope;
   else {
      cover_scope_t *parent = get_cover_scope(db, lcs->parent);
      return (lcs->cscope = cover_create_scope(db, parent, lcs->tree, NULL));
   }
}

static lazy_cscope_t lazy_cover_scope(tree_t t, lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = { parent, NULL, t };
   return lcs;
}

static void vhdl_cover_branch(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   if (!cover_enabled(db, COVER_MASK_BRANCH))
      return;

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(db, parent);
   cover_item_t *item = cover_add_items_for(db, cs, obj, COV_ITEM_BRANCH);
   if (item != NULL)
      cover_map_item(cs, obj, item);
}

static void vhdl_cover_stmt(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   if (!cover_enabled(db, COVER_MASK_STMT))
      return;

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(db, parent);
   cover_item_t *item = cover_add_items_for(db, cs, obj, COV_ITEM_STMT);
   if (item != NULL)
      cover_map_item(cs, obj, item);
}

static void vhdl_cover_expr(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   if (!cover_enabled(db, COVER_MASK_EXPRESSION))
      return;
   else if (tree_kind(t) != T_FCALL)
      return;

   switch (tree_subkind(tree_ref(t))) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_GT:
   case S_SCALAR_OR:
   case S_SCALAR_NOR:
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
   case S_SCALAR_NOT:
   case S_IEEE_OR:
   case S_IEEE_NOR:
   case S_IEEE_AND:
   case S_IEEE_NAND:
   case S_IEEE_XOR:
   case S_IEEE_XNOR:
      break;
   default:
      return;
   }

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++)
      vhdl_cover_expr(tree_value(tree_param(t, i)), db, parent);

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(db, parent);
   cover_item_t *item = cover_add_items_for(db, cs, obj, COV_ITEM_EXPRESSION);
   if (item != NULL)
      cover_map_item(cs, obj, item);
}

static void vhdl_cover_states(tree_t t, cover_data_t *db,
                              lazy_cscope_t *parent)
{
   if (!cover_enabled(db, COVER_MASK_STATE))
      return;

   type_t type = tree_type(t);
   if (!type_is_enum(type))
      return;

   // Ignore enums from built-in libraries
   if (is_well_known(tree_ident(type_container(type))) < NUM_WELL_KNOWN)
      return;

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(db, parent);
   cover_item_t *item = cover_add_items_for(db, cs, obj, COV_ITEM_STATE);
   if (item != NULL)
      cover_map_item(cs, obj, item);
}

static void vhdl_cover_toggle(tree_t t, cover_data_t *db,
                              lazy_cscope_t *parent)
{
   if (!cover_enabled(db, COVER_MASK_TOGGLE))
      return;

   type_t type = tree_type(t);
   if (type_is_record(type))
      return;   // TODO: handled during lowering for now

   object_t *obj = tree_to_object(t);
   cover_scope_t *cs = get_cover_scope(db, parent);
   cover_item_t *item = cover_add_items_for(db, cs, obj, COV_ITEM_TOGGLE);
   if (item != NULL)
      cover_map_item(cs, obj, item);
}

static void vhdl_cover_if(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(t, i);
      lazy_cscope_t lcs = lazy_cover_scope(c, parent);

      if (tree_has_value(c)) {
         vhdl_cover_expr(tree_value(c), db, &lcs);
         vhdl_cover_branch(c, db, &lcs);
      }

      vhdl_cover_stmts(c, db, &lcs);
   }
}

static void vhdl_cover_loop(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);
   vhdl_cover_stmts(t, db, parent);
}

static void vhdl_cover_while(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);
   vhdl_cover_branch(t, db, parent);
   vhdl_cover_stmts(t, db, parent);
}

static void vhdl_cover_case(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      if (cover_enabled(db, COVER_MASK_BRANCH)) {
         const int nchoices = tree_choices(alt);
         for (int j = 0; j < nchoices; j++) {
            tree_t c = tree_choice(alt, j);
            lazy_cscope_t lcs = lazy_cover_scope(c, parent);
            vhdl_cover_branch(c, db, &lcs);
         }
      }

      vhdl_cover_stmts(alt, db, parent);
   }
}

static void vhdl_cover_signal_assign(tree_t t, cover_data_t *db,
                                     lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);

   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++)
      vhdl_cover_expr(tree_value(tree_waveform(t, i)), db, parent);
}

static void vhdl_cover_wait(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   if (tree_flags(t) & TREE_F_STATIC_WAIT)
      return;   // Wait inserted by simp

   vhdl_cover_stmt(t, db, parent);
}

static void vhdl_cover_loop_control(tree_t t, cover_data_t *db,
                                    lazy_cscope_t *parent)
{
   vhdl_cover_stmt(t, db, parent);

   if (tree_has_value(t))
      vhdl_cover_branch(t, db, parent);
}

static void vhdl_cover_stmts(tree_t t, cover_data_t *db, lazy_cscope_t *parent)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      lazy_cscope_t lcs = lazy_cover_scope(s, parent);

      switch (tree_kind(s)) {
      case T_IF:
         vhdl_cover_if(s, db, &lcs);
         break;
      case T_CASE:
         vhdl_cover_case(s, db, &lcs);
         break;
      case T_LOOP:
      case T_FOR:
         vhdl_cover_loop(s, db, &lcs);
         break;
      case T_WHILE:
         vhdl_cover_while(s, db, &lcs);
         break;
      case T_SIGNAL_ASSIGN:
         vhdl_cover_signal_assign(s, db, &lcs);
         break;
      case T_WAIT:
         vhdl_cover_wait(s, db, &lcs);
         break;
      case T_NEXT:
      case T_EXIT:
         vhdl_cover_loop_control(s, db, &lcs);
         break;
      default:
         vhdl_cover_stmt(s, db, &lcs);
         break;
      }
   }
}

static void vhdl_cover_subprogram(tree_t t, cover_data_t *db,
                                  cover_scope_t *parent)
{
   cover_scope_t *cs = cover_create_block(db, tree_ident2(t), parent, t,
                                          t, NULL);
   lazy_cscope_t lcs = { NULL, cs, t };
   vhdl_cover_stmts(t, db, &lcs);
}

static void vhdl_cover_signal_decl(tree_t t, cover_data_t *db,
                                   lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent);
   vhdl_cover_toggle(t, db, &lcs);
   vhdl_cover_states(t, db, &lcs);
}

static void vhdl_cover_port_decl(tree_t t, cover_data_t *db,
                                 lazy_cscope_t *parent)
{
   lazy_cscope_t lcs = lazy_cover_scope(t, parent);
   vhdl_cover_toggle(t, db, &lcs);
}

static void vhdl_cover_decls(tree_t t, cover_data_t *db, cover_scope_t *cs)
{
   lazy_cscope_t lcs = { NULL, cs, t };
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_BODY:
      case T_PROC_BODY:
         vhdl_cover_subprogram(d, db, cs);
         break;
      case T_SIGNAL_DECL:
         vhdl_cover_signal_decl(d, db, &lcs);
         break;
      default:
         break;
      }
   }
}

static void vhdl_cover_process(tree_t proc, ident_t qual, cover_data_t *db,
                               cover_scope_t *parent)
{
   cover_scope_t *cs = cover_create_block(db, qual, parent, proc, proc, NULL);
   lazy_cscope_t lcs = { NULL, cs, proc };
   vhdl_cover_stmts(proc, db, &lcs);
   vhdl_cover_decls(proc, db, cs);
}

cover_scope_t *vhdl_cover_block(tree_t block, cover_data_t *db,
                                cover_scope_t *parent)
{
   assert(tree_kind(block) == T_BLOCK);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   int nstmts = tree_stmts(block);
   tree_t unit = tree_ref(hier);

   cover_scope_t *cs;
   if (tree_kind(unit) == T_COMPONENT) {
      if (nstmts == 0)
         return NULL;
      else if (nstmts == 1) {
         // Collapse this coverage scope with the block for the
         // component above
         tree_t inst = tree_stmt(block, 0);
         assert(tree_kind(inst) == T_BLOCK);

         tree_t hier2 = tree_decl(inst, 0);
         assert(tree_kind(hier2) == T_HIER);

         unit = tree_ref(hier2);
         cs = cover_create_block(db, tree_ident(hier2), parent,
                                 block, unit, NULL);
         block = inst;
         nstmts = tree_stmts(block);
         hier = hier2;
      }
      else
         should_not_reach_here();
   }
   else
      cs = cover_create_block(db, tree_ident(hier), parent, block, unit, NULL);

   cover_ignore_from_pragmas(db, cs, unit);

   lazy_cscope_t lcs = { NULL, cs, block };

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++)
      vhdl_cover_port_decl(tree_port(block, i), db, &lcs);

   const int nparams = tree_params(block);
   for (int i = 0; i < nparams; i++) {
      tree_t actual = tree_value(tree_param(block, i));
      if (tree_kind(actual) == T_INERTIAL)
         vhdl_cover_expr(tree_value(actual), db, &lcs);
   }

   vhdl_cover_decls(block, db, cs);

   ident_t sym_prefix = tree_ident2(hier);

   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_PROCESS:
         {
            ident_t qual = ident_prefix(sym_prefix, tree_ident(s), '.');
            vhdl_cover_process(s, qual, db, cs);
         }
         break;
      default:
         break;
      }
   }

   return cs;
}
