//
//  Copyright (C) 2011-2014  Nick Gasson
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

static ident_t never_waits_i;
static ident_t elide_bounds_i;
static ident_t range_var_i;

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
      if (!tree_attr_int(tree_ref(t), never_waits_i, 0))
         *may_wait = true;
   }
}

static void opt_tag_simple_procedure(tree_t t)
{
   bool may_wait = false;
   tree_visit(t, opt_may_wait_fn, &may_wait);
   if (!may_wait)
      tree_add_attr_int(t, never_waits_i, 1);
}

////////////////////////////////////////////////////////////////////////////////
// Tag array variables that are returned from functions
//
// This is used to avoid allocating extra memory for the result
//

static void opt_tag_return_array(tree_t t)
{
   if (!tree_has_value(t))
      return;

   tree_t value = tree_value(t);

   if (tree_kind(value) != T_REF)
      return;

   type_t type = tree_type(value);

   if (!type_is_array(type))
      return;

   tree_t decl = tree_ref(value);

   if (tree_kind(decl) != T_VAR_DECL)
      return;

   tree_add_attr_int(decl, ident_new("returned"), 1);
}

////////////////////////////////////////////////////////////////////////////////
// If an array reference index is a reference to an induction variable with
// the same range as the array then elide bounds checking at runtime
//
// for i in x'range loop
//   x(i) := f(...);
// end loop;
//
// The bounds check on x(i) always succeeds
//

static void opt_elide_array_ref_bounds(tree_t t)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) != T_REF)
      return;

   tree_t decl = tree_ref(value);

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t index = tree_value(tree_param(t, i));
      if (tree_kind(index) != T_REF)
         return;

      tree_t index_decl = tree_ref(index);

      tree_t range_var = tree_attr_tree(index_decl, range_var_i);
      if (range_var == NULL)
         return;

      if (range_var != decl)
         return;
   }

   tree_add_attr_int(t, elide_bounds_i, 1);
}

////////////////////////////////////////////////////////////////////////////////

static void opt_tag(tree_t t, void *ctx)
{
   switch (tree_kind(t)) {
   case T_PROC_BODY:
      opt_tag_simple_procedure(t);
      break;

   case T_RETURN:
      opt_tag_return_array(t);
      break;

   case T_ARRAY_REF:
      opt_elide_array_ref_bounds(t);
      break;

   default:
      break;
   }
}

void opt(tree_t top)
{
   never_waits_i  = ident_new("never_waits");
   elide_bounds_i = ident_new("elide_bounds");
   range_var_i    = ident_new("range_var");

   if (tree_kind(top) == T_ELAB)
      opt_delete_wait_only(top);

   tree_visit(top, opt_tag, NULL);
}
