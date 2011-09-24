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

static int errors = 0;

static void proc_visit_cb(tree_t t, void *context)
{
   assert(tree_kind(t) == T_SIGNAL_ASSIGN);

   tree_t decl = tree_ref(tree_target(t));

   tree_t proc = (tree_t)context;

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
