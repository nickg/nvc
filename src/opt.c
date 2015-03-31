//
//  Copyright (C) 2011-2015  Nick Gasson
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
#include <assert.h>

static ident_t never_waits_i;
static ident_t elide_bounds_i;
static ident_t range_var_i;
static ident_t last_value_i;
static ident_t builtin_i;

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
      tree_t decl = tree_ref(t);
      if (tree_attr_str(decl, builtin_i))
         ;
      else if (!tree_attr_int(decl, never_waits_i, 0))
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
// Identify any potential use of the 'LAST_VALUE attribute
//

static void opt_tag_last_value_attr_ref(tree_t t)
{
   if (icmp(tree_ident(t), "LAST_VALUE")) {
      tree_t signal = tree_ref(tree_name(t));
      if (tree_kind(signal) != T_SIGNAL_DECL)
         return;

      // A signal in a package will not have nets assigned yet so we cannot
      // optimise out 'LAST_VALUE
      if (tree_nets(signal) > 0)
         tree_add_attr_int(signal, last_value_i, 1);
   }
}

static void opt_tag_last_value_fcall(tree_t t)
{
   tree_t decl = tree_ref(t);

   // A regular subprogram call may pass parameters as class signal which
   // could access 'LAST_VALUE in the body

   const int nports = tree_ports(decl);
   for (int i = 0; i < nports; i++) {
      tree_t port = tree_port(decl, i);
      if (tree_class(port) != C_SIGNAL)
         continue;

      tree_t value = tree_value(tree_param(t, i));
      tree_kind_t kind;
      while ((kind = tree_kind(value)) != T_REF) {
         assert((kind == T_ARRAY_REF) || (kind == T_ARRAY_SLICE));
         value = tree_value(value);
      }

      tree_add_attr_int(tree_ref(value), last_value_i, 1);
   }
}

////////////////////////////////////////////////////////////////////////////////

static void opt_tag(tree_t t, void *ctx)
{
   switch (tree_kind(t)) {
   case T_PROC_BODY:
      opt_tag_simple_procedure(t);
      break;

   case T_ARRAY_REF:
      opt_elide_array_ref_bounds(t);
      break;

   case T_FCALL:
   case T_PCALL:
      opt_tag_last_value_fcall(t);
      break;

   case T_ATTR_REF:
      opt_tag_last_value_attr_ref(t);
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
   builtin_i      = ident_new("builtin");
   last_value_i   = ident_new("last_value");

   if (tree_kind(top) == T_ELAB)
      opt_delete_wait_only(top);

   tree_visit(top, opt_tag, NULL);
}
