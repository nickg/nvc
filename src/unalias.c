//
//  Copyright (C) 2012  Nick Gasson
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
#include "common.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>

static tree_t get_ref(tree_t decl)
{
   tree_t var = tree_new(T_REF);
   tree_set_ident(var, tree_ident(decl));
   tree_set_type(var, tree_type(decl));
   tree_set_ref(var, decl);

   return var;
}

static tree_t unalias_index(tree_t decl, tree_t index)
{
   tree_t base_decl = tree_ref(tree_value(decl));
   assert(tree_kind(base_decl) != T_ALIAS);

   type_t alias_type = tree_type(decl);
   type_t base_type  = tree_type(base_decl);

   assert(type_dims(alias_type) == 1);  // TODO: multi-dimensional arrays

   range_t alias_r = type_dim(alias_type, 0);

   type_t ptype = tree_type(index);

   tree_t off = call_builtin("sub", ptype, index, alias_r.left, NULL);

   switch (type_kind(base_type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      // The transformation is a constant offset of indices
      {
         range_t base_r  = type_dim(base_type, 0);
         if (alias_r.kind == base_r.kind) {
            // Range in same direction
            return call_builtin("add", ptype, base_r.left, off, NULL);
         }
         else {
            // Range in opposite direction
            return call_builtin("sub", ptype, base_r.left, off, NULL);
         }
      }
      break;

   case T_UARRAY:
      // The transformation must be computed at runtime
      {
         tree_t ref = get_ref(base_decl);
         tree_t base_left = call_builtin("uarray_left", ptype, ref, NULL);

         tree_t rkind_lit = tree_new(T_LITERAL);
         tree_set_subkind(rkind_lit, L_INT);
         tree_set_ival(rkind_lit, alias_r.kind);
         tree_set_type(rkind_lit, ptype);

         // Call dircmp builtin which multiplies its third argument
         // by -1 if the direction of the first argument is not equal
         // to the direction of the second
         tree_t off_dir = call_builtin("uarray_dircmp", ptype,
                                       ref, rkind_lit, off, NULL);

         return call_builtin("add", ptype, base_left, off_dir, NULL);
      }
      break;

   default:
      assert(false);
   }
}

static tree_t unalias_array_slice(tree_t t)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) != T_REF)
      return t;

   tree_t decl = tree_ref(value);

   if (tree_kind(decl) == T_ALIAS) {
      tree_t base_decl = tree_ref(tree_value(decl));
      type_t base_type = tree_type(base_decl);

      switch (type_kind(base_type)) {
      case T_SUBTYPE:
      case T_CARRAY:
      case T_UARRAY:
         {
            range_t slice_r = tree_range(t);
            slice_r.left  = unalias_index(decl, slice_r.left);
            slice_r.right = unalias_index(decl, slice_r.right);

            type_change_dim(tree_type(t), 0, slice_r);

            tree_set_range(t, slice_r);
            tree_set_value(t, get_ref(base_decl));
         }
         break;
      default:
         assert(false);
      }
   }

   return t;
}

static tree_t unalias_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) != T_REF)
      return t;

   tree_t decl = tree_ref(value);

   if (tree_kind(decl) == T_ALIAS) {
      // Generate code to map from alias indexing to the
      // indexing of the underlying array

      tree_t base_decl = tree_ref(tree_value(decl));

      tree_t new = tree_new(T_ARRAY_REF);
      tree_set_loc(new, tree_loc(t));
      tree_set_value(new, get_ref(base_decl));
      tree_set_type(new, type_elem(tree_type(base_decl)));

      tree_t p = tree_param(t, 0);
      tree_set_value(p, unalias_index(decl, tree_value(p)));
      tree_add_param(new, p);

      return new;
   }
   else
      return t;
}

static tree_t unalias_tree(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_ARRAY_REF:
      return unalias_array_ref(t);
   case T_ARRAY_SLICE:
      return unalias_array_slice(t);
   default:
      return t;
   }
}

void unalias(tree_t top)
{
   tree_rewrite(top, unalias_tree, NULL);
}
