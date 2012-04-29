//
//  Copyright (C) 2011  Nick Gasson
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

#include "phase.h"
#include "util.h"

#include <assert.h>
#include <inttypes.h>

static int errors = 0;

static void whole_array_driver(tree_t decl, tree_t proc)
{
   type_t type = tree_type(decl);

   int64_t low, high;
   range_bounds(type_dim(type, 0), &low, &high);

   for (int i = 0; i < high - low + 1; i++) {
      bool already = false;
      for (unsigned j = 0; j < tree_sub_drivers(decl, i); j++) {
         if (tree_sub_driver(decl, i, j) == proc)
            already = true;
      }

      if (!already)
         tree_add_sub_driver(decl, i, proc);
   }
}

static void whole_signal_driver(tree_t ref, tree_t proc)
{
   tree_t decl = tree_ref(ref);

   type_t type = tree_type(decl);
   if (type_kind(type) == T_CARRAY || type_kind(type) == T_UARRAY) {
      // Break an array driver into a driver for each sub-element
      whole_array_driver(decl, proc);
      return;
   }

   for (unsigned i = 0; i < tree_drivers(decl); i++) {
      if (tree_driver(decl, i) == proc)
         return;
   }

   if (tree_drivers(decl) > 0) {
      // TODO: check for resolution function
      error_at(tree_loc(decl),
               "signal %s has multiple drivers (not supported)",
               istr(tree_ident(decl)));
      errors++;
   }

   tree_add_driver(decl, proc);
}

static void part_signal_driver(tree_t ref, tree_t proc)
{
   tree_t value = tree_value(ref);
   assert(tree_kind(value) == T_REF);

   tree_t decl = tree_ref(value);
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   type_t type = tree_type(decl);
   assert(type_kind(type) == T_CARRAY);

   int64_t type_low, type_high;
   range_bounds(type_dim(type, 0), &type_low, &type_high);

   int64_t slice_low, slice_high;
   bool no_static_prefix;

   switch (tree_kind(ref)) {
   case T_ARRAY_SLICE:
      {
         range_t sr = tree_range(ref);

         no_static_prefix =
            (tree_kind(sr.left) != T_LITERAL
             || tree_kind(sr.right) != T_LITERAL);

         if (!no_static_prefix)
            range_bounds(sr, &slice_low, &slice_high);
      }
      break;

   case T_ARRAY_REF:
      {
         param_t p = tree_param(ref, 0);
         assert(p.kind == P_POS);

         no_static_prefix = (tree_kind(p.value) != T_LITERAL);

         if (!no_static_prefix)
            slice_low = slice_high = assume_int(p.value);
      }
      break;

   default:
      assert(false);
   }

   if (no_static_prefix) {
      // Longest static prefix is whole signal
      whole_signal_driver(ref, proc);
      return;
   }

   for (unsigned elem = slice_low; elem <= slice_high; elem++) {
      assert(elem >= type_low && elem <= type_high);

      bool already_driver = false;
      for (unsigned j = 0; j < tree_sub_drivers(decl, elem); j++) {
         if (tree_sub_driver(decl, elem, j) == proc)
            already_driver = true;
      }

      if (already_driver)
         continue;

      if (tree_sub_drivers(decl, elem) > 0 || tree_drivers(decl) > 0) {
         // TODO: check for resolution function
         error_at(tree_loc(decl),
                  "element %u of signal %s has multiple drivers"
                  " (not supported)", elem, istr(tree_ident(decl)));
         errors++;
      }

      tree_add_sub_driver(decl, elem, proc);
   }
}

static void proc_visit_cb(tree_t t, void *context)
{
   assert(tree_kind(t) == T_SIGNAL_ASSIGN);

   tree_t proc = (tree_t)context;

   tree_t target = tree_target(t);
   switch (tree_kind(target)) {
   case T_REF:
      whole_signal_driver(target, proc);
      break;
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      part_signal_driver(target, proc);
      break;
   default:
      assert(false);
   }
}

static void drivers_from_process(tree_t t)
{
   assert(tree_kind(t) == T_PROCESS);

   tree_visit_only(t, proc_visit_cb, (void *)t, T_SIGNAL_ASSIGN);
}

void driver_extract(tree_t top)
{
   assert(tree_kind(top) == T_ELAB);

   for (unsigned i = 0; i < tree_stmts(top); i++)
      drivers_from_process(tree_stmt(top, i));
}

int driver_errors(void)
{
   return errors;
}
