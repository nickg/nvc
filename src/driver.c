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
#include "common.h"
#include "diag.h"
#include "driver.h"
#include "hash.h"
#include "ident.h"
#include "mask.h"
#include "option.h"
#include "phase.h"
#include "tree.h"
#include "type.h"

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
                          tree_t view, bool tentative)
{
   if (tree_kind(expr) == T_AGGREGATE) {
      const int nassocs = tree_assocs(expr);
      for (int i = 0; i < nassocs; i++) {
         tree_t e = tree_value(tree_assoc(expr, i));
         drives_signal(ds, where, e, NULL, tentative);
      }

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
   di->view       = view;
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
                    NULL, params->tentative);
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
                             NULL, params->tentative);
            }
            else if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW) {
               tree_t arg = tree_param(t, i);
               assert(tree_subkind(arg) == P_POS);
               drives_signal(params->ds, params->proc, tree_value(arg),
                             tree_value(p), params->tentative);
            }
         }
      }
      break;

   default:
      break;
   }
}

static void visit_port_map(driver_set_t *ds, tree_t unit, tree_t inst,
                           bool tentative)
{
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
         drives_signal(ds, inst, tree_value(map), NULL, tentative);
         break;
      case PORT_ARRAY_VIEW:
      case PORT_RECORD_VIEW:
         drives_signal(ds, inst, tree_value(map), tree_value(port), tentative);
         break;
      }
   }
}

static void visit_instance(driver_set_t *ds, tree_t inst, bool tentative)
{
   tree_t unit = primary_unit_of(tree_ref(inst));
   visit_port_map(ds, unit, inst, tentative);
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
         visit_port_map(ds, s, s, tentative);
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

bool has_unique_driver(driver_set_t *ds, tree_t what)
{
   // True if signal has exactly one driver for each sub-element

   driver_info_t *di = get_drivers(ds, what);

   if (di == NULL)
      return false;

   // First pass: look for assignments to the full signal

   bool saw_ref = false, saw_multiple = false;
   tree_t proc = di->where;
   for (driver_info_t *it = di; it; it = it->chain_decl) {
      if (it->tentative)
         return false;
      else if (it->where != proc)
         saw_multiple = true;
      else if (tree_kind(it->prefix) == T_REF)
         saw_ref = true;
   }

   if (saw_ref && !saw_multiple)
      return true;

   type_t type = tree_type(what);
   int64_t length = 0, left = 0, right = 0;
   range_kind_t rkind = RANGE_ERROR;
   if (type_is_array(type) && dimension_of(type) == 1) {
      tree_t r = range_of(type, 0);
      if (folded_length(r, &length)) {
         left = assume_int(tree_left(r));
         right = assume_int(tree_right(r));
         rkind = tree_subkind(r);
      }
   }
   else if (type_is_record(type))
      length = type_fields(type);

   if (length == 0)
      return false;

   // Second pass: look for assignments to disjoint sub-elements

   bit_mask_t mask = {};
   mask_init(&mask, length);

   bool covered = false;
   for (driver_info_t *it = di; it; it = it->chain_decl) {
      const tree_kind_t kind = tree_kind(it->prefix);
      if (kind != T_ARRAY_REF && kind != T_ARRAY_SLICE && kind != T_RECORD_REF)
         goto out_free;

      tree_t value = tree_value(it->prefix);
      if (tree_kind(value) != T_REF)
         goto out_free;

      assert(tree_ref(value) == what);

      switch (kind) {
      case T_ARRAY_REF:
         {
            assert(tree_params(it->prefix) == 1);
            tree_t p0 = tree_value(tree_param(it->prefix, 0));

            int64_t ival;
            if (!folded_int(p0, &ival))
               goto out_free;

            const int zero = rkind == RANGE_TO ? ival - left : ival - right;
            if (mask_test(&mask, zero))
               goto out_free;

            mask_set(&mask, zero);
         }
         break;

      case T_ARRAY_SLICE:
         {
            tree_t r = tree_range(it->prefix, 0);

            int64_t low, high;
            if (!folded_bounds(r, &low, &high) || high < low)
               goto out_free;

            const int count = high - low + 1;
            const int zero = rkind == RANGE_TO ? low - left : low - right;

            if (mask_test_range(&mask, zero, count))
               goto out_free;

            mask_set_range(&mask, zero, count);
         }
         break;

      case T_RECORD_REF:
         {
            const int pos = tree_pos(tree_ref(it->prefix));

            if (mask_test(&mask, pos))
               goto out_free;

            mask_set(&mask, pos);
         }
         break;

      default:
         goto out_free;
      }
   }

   covered = (mask_popcount(&mask) == length);

 out_free:
   mask_free(&mask);
   return covered;
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
