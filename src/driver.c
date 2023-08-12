//
//  Copyright (C) 2023  Nick Gasson
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
#include "diag.h"
#include "driver.h"
#include "hash.h"
#include "ident.h"
#include "option.h"
#include "phase.h"
#include "tree.h"

#include <assert.h>
#include <stdlib.h>

#define DRIVER_PAGE_SIZE 128

typedef struct _driver_page driver_page_t;

typedef struct _driver_page {
   driver_page_t *next;
   unsigned       count;
   driver_info_t  members[DRIVER_PAGE_SIZE];
} driver_page_t;

typedef struct _driver_set {
   hash_t        *map;
   driver_page_t *pages;
} driver_set_t;

typedef struct {
   driver_set_t *ds;
   tree_t        proc;
   bool          tentative;
} proc_params_t;

static bool same_tree(tree_t a, tree_t b)
{
   const tree_kind_t akind = tree_kind(a);
   if (akind != tree_kind(b))
      return false;

   switch (akind) {
   case T_REF:
      return tree_ref(a) == tree_ref(b);
   case T_ARRAY_REF:
      {
         if (!same_tree(tree_value(a), tree_value(b)))
            return false;

         const int nparams = tree_params(a);
         assert(nparams == tree_params(b));

         for (int i = 0; i < nparams; i++) {
            tree_t pa = tree_value(tree_param(a, i));
            tree_t pb = tree_value(tree_param(b, i));
            if (!same_tree(pa, pb))
               return false;
         }

         return true;
      }
   case T_ARRAY_SLICE:
      {
         if (!same_tree(tree_value(a), tree_value(b)))
            return false;

         tree_t ra = tree_range(a, 0);
         tree_t rb = tree_range(b, 0);

         const range_kind_t rakind = tree_subkind(ra);
         if (rakind != tree_subkind(rb) || rakind == RANGE_EXPR)
            return false;

         return same_tree(tree_left(ra), tree_left(rb))
            && same_tree(tree_right(ra), tree_right(rb));
      }

   case T_RECORD_REF:
      return tree_ident(a) == tree_ident(b)
         && same_tree(tree_value(a), tree_value(b));
   case T_LITERAL:
      {
         const literal_kind_t alkind = tree_subkind(a);
         if (alkind != tree_subkind(b) || alkind != L_INT)
            return false;
         else
            return tree_ival(a) == tree_ival(b);
      }
   default:
      return false;
   }
}

static driver_info_t *alloc_driver_info(driver_set_t *ds)
{
   if (ds->pages == NULL || ds->pages->count == DRIVER_PAGE_SIZE) {
      driver_page_t *p = xmalloc(sizeof(driver_page_t));
      p->next = ds->pages;
      p->count = 0;

      ds->pages = p;
   }

   return &(ds->pages->members[ds->pages->count++]);
}

static void drives_signal(driver_set_t *ds, tree_t where, tree_t expr,
                          bool tentative)
{
   if (tree_kind(expr) == T_AGGREGATE) {
      const int nassocs = tree_assocs(expr);
      for (int i = 0; i < nassocs; i++)
         drives_signal(ds, where, tree_value(tree_assoc(expr, i)), tentative);

      return;
   }

   tree_t prefix = longest_static_prefix(expr);

   tree_t ref = name_to_ref(prefix);
   if (ref == NULL)
      return;

   tree_t decl = tree_ref(ref);

   if (tree_kind(decl) == T_PARAM_DECL) {
      // Assignment to procedure parameter: this is handled at the call
      // site instead
      return;
   }

   driver_info_t *chain = hash_get(ds->map, where), *last = NULL;
   for (; chain; last = chain, chain = chain->chain_proc) {
      if (chain->where != where || chain->decl != decl)
         continue;
      else if (tree_kind(chain->prefix) == T_REF)
         return;   // Already driving full signal
      else if (same_tree(prefix, chain->prefix))
         return;
   }

   driver_info_t *di = alloc_driver_info(ds);
   di->chain_decl = NULL;
   di->chain_proc = NULL;
   di->decl       = decl;
   di->prefix     = prefix;
   di->where      = where;
   di->tentative  = tentative;

   if (last == NULL)
      hash_put(ds->map, where, di);
   else
      last->chain_proc = di;

   driver_info_t *decl_head = hash_get(ds->map, decl);
   if (decl_head == NULL)
      hash_put(ds->map, decl, di);
   else {
      for (; decl_head->chain_decl; decl_head = decl_head->chain_decl);
      decl_head->chain_decl = di;
   }
}

static void driver_proc_cb(tree_t t, void *ctx)
{
   proc_params_t *params = ctx;

   switch (tree_kind(t)) {
   case T_SIGNAL_ASSIGN:
      drives_signal(params->ds, params->proc, tree_target(t),
                    params->tentative);
      break;

   case T_PCALL:
   case T_PROT_PCALL:
      // LRM 08 section 4.2.2.3: a process statement contains a driver
      // for each actual signal associated with a formal signal
      // parameter of mode out or inout in a subprogram call.
      {
         tree_t decl = tree_ref(t);
         const int nports = tree_ports(decl);
         for (int i = 0; i < nports; i++) {
            tree_t p = tree_port(decl, i);
            if (tree_class(p) != C_SIGNAL)
               continue;

            const port_mode_t mode = tree_subkind(p);
            if (mode == PORT_OUT || mode == PORT_INOUT) {
               tree_t arg = tree_param(t, i);
               assert(tree_subkind(arg) == P_POS);
               drives_signal(params->ds, params->proc, tree_value(arg),
                             params->tentative);
            }
         }
      }
      break;

   default:
      break;
   }
}

static void visit_instance(driver_set_t *ds, tree_t inst, bool tentative)
{
   tree_t unit = primary_unit_of(tree_ref(inst));

   const int nparams = tree_params(inst);
   for (int i = 0; i < nparams; i++) {
      tree_t map = tree_param(inst, i);

      tree_t port;
      if (tree_subkind(map) == P_POS)
         port = tree_port(unit, tree_pos(map));
      else {
         tree_t name = tree_name(map);
         switch (tree_kind(name)) {
         case T_CONV_FUNC:
         case T_TYPE_CONV:
            name = tree_value(name);
            break;
         default:
            break;
         }

         port = tree_ref(name_to_ref(name));
      }

      switch (tree_subkind(port)) {
      case PORT_OUT:
      case PORT_INOUT:
         drives_signal(ds, inst, tree_value(map), tentative);
         break;
      }
   }
}

static void visit_block(driver_set_t *ds, tree_t b, bool tentative)
{
   const int nstmts = tree_stmts(b);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(b, i);
      switch (tree_kind(s)) {
      case T_PROCESS:
         {
            proc_params_t params = {
               .ds = ds,
               .proc = s,
               .tentative = tentative,
            };
            tree_visit(s, driver_proc_cb, &params);
         }
         break;
      case T_BLOCK:
         visit_block(ds, s, tentative);
         break;
      case T_FOR_GENERATE:
         visit_block(ds, s, true);
         break;
      case T_IF_GENERATE:
         {
            const int nconds = tree_conds(s);
            for (int i = 0; i < nconds; i++)
               visit_block(ds, tree_cond(s, i), true);
         }
         break;
      case T_CASE_GENERATE:
         {
            const int nalts = tree_stmts(s);
            for (int i = 0; i < nalts; i++) {
               tree_t a = tree_stmt(s, i);
               assert(tree_kind(a) == T_ALTERNATIVE);
               visit_block(ds, a, true);
            }
         }
         break;
      case T_INSTANCE:
         visit_instance(ds, s, tentative);
         break;
      default:
         break;
      }
   }

}

driver_set_t *find_drivers(tree_t block)
{
   driver_set_t *ds = xcalloc(sizeof(driver_set_t));
   ds->map = hash_new(128);

   visit_block(ds, block, false);

   if (opt_get_int(OPT_DRIVER_VERBOSE))
      dump_drivers(ds);

   return ds;
}

void free_drivers(driver_set_t *ds)
{
   for (driver_page_t *p = ds->pages, *tmp; p; p = tmp) {
      tmp = p->next;
      free(p);
   }

   hash_free(ds->map);
   free(ds);
}

driver_info_t *get_drivers(driver_set_t *ds, tree_t what)
{
   return hash_get(ds->map, what);
}

LCOV_EXCL_START
void dump_drivers(driver_set_t *ds)
{
   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN; hash_iter(ds->map, &it, &key, &value); ) {
      tree_t tree = (tree_t)key;
      driver_info_t *di = value;

      const tree_kind_t kind = tree_kind(tree);
      if (kind != T_PROCESS && kind != T_INSTANCE)
         continue;

      printf("%s: { ", istr(tree_ident(tree)));
      for (; di; di = di->chain_proc) {
         vhdl_dump(di->prefix, 0);
         if (di->tentative)
            printf("?");
         if (di->chain_proc)
            printf(", ");
      }
      printf(" }\n");
   }
}
LCOV_EXCL_STOP
