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

#include "util.h"
#include "phase.h"

#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////////
// Delete processes that contain just a single wait statement
//
//   process is
//   begin
//     wait on x;
//   end process;
//
// Such processes can result from optimising away assignments to OPEN outputs.
//

static tree_t opt_delete_wait_only_fn(tree_t t, void *ctx)
{
   if (tree_kind(t) != T_PROCESS)
      return t;

   if ((tree_stmts(t) == 1) && (tree_kind(tree_stmt(t, 0)) == T_WAIT))
      return NULL;
   else
      return t;
}

static void opt_delete_wait_only(tree_t top)
{
   tree_rewrite(top, opt_delete_wait_only_fn, NULL);
}

////////////////////////////////////////////////////////////////////////////////
// Tag procedures that never wait.
//
// This avoids generating lots of unnecessary resumption code in the simple
// case where a procedure never waits
//

static void opt_may_wait_fn(tree_t t, void *ctx)
{
   bool *may_wait = ctx;

   tree_kind_t kind = tree_kind(t);
   if (kind == T_WAIT)
      *may_wait = true;
   else if (kind == T_PCALL) {
      if (!tree_attr_int(tree_ref(t), ident_new("never_waits"), 0))
         *may_wait = true;
   }
}

static void opt_tag_simple_procedure_fn(tree_t t, void *ctx)
{
   bool may_wait = false;
   tree_visit(t, opt_may_wait_fn, &may_wait);
   if (!may_wait)
      tree_add_attr_int(t, ident_new("never_waits"), 1);
}

static void opt_tag_simple_procedures(tree_t top)
{
   tree_visit_only(top, opt_tag_simple_procedure_fn, NULL, T_PROC_BODY);
}

////////////////////////////////////////////////////////////////////////////////

void opt(tree_t top)
{
   if (tree_kind(top) == T_ELAB)
      opt_delete_wait_only(top);

   opt_tag_simple_procedures(top);
}
