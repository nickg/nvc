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

static void whole_signal_driver(tree_t ref, tree_t proc)
{
   tree_t decl = tree_ref(ref);

   // TODO: check for array

   for (unsigned i = 0; i < tree_drivers(decl); i++) {
      if (tree_driver(decl, i) == proc)
         return;
   }

   // TODO: check for sub-drivers here as well
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
   tree_t decl = tree_ref(ref);
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   type_t type = tree_type(decl);
   assert(type_kind(type) == T_CARRAY);

   int64_t low, high;
   range_bounds(type_dim(type, 0), &low, &high);

   param_t p = tree_param(ref, 0);
   assert(p.kind == P_POS);

   if (tree_kind(p.value) != T_LITERAL) {
      // Longest static prefix is whole signal
      whole_signal_driver(ref, proc);
      return;
   }

   int64_t elem = assume_int(p.value);
   assert(elem >= low && elem <= high);

   printf("low=%"PRIu64" high=%"PRIu64" elem=%"PRIu64"\n", low, high, elem);

   for (int i = 0; i < high - low + 1; i++) {
      for (unsigned j = 0; j < tree_sub_drivers(decl, i); j++) {
         if (tree_sub_driver(decl, i, j) == proc)
            return;
      }
   }

   if (tree_sub_drivers(decl, elem - low) > 0 || tree_drivers(decl) > 0) {
      // TODO: check for resolution function
      error_at(tree_loc(decl),
               "element %"PRIu64" of signal %s has multiple drivers"
               " (not supported)", elem, istr(tree_ident(decl)));
      errors++;
   }

   tree_add_sub_driver(decl, elem - low, proc);
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
