//
//  Copyright (C) 2024  Nick Gasson
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
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

static tree_t select_name(tree_t where, ident_t id)
{
   const int ndecls = tree_decls(where);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(where, i);
      if (tree_ident(d) == id)
         return d;
   }

   const int ngenerics = tree_generics(where);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(where, i);
      if (tree_ident(g) == id)
         return g;
   }

   switch (tree_kind(where)) {
   case T_PACKAGE:
      return select_name(body_of(where), id);
   case T_PACK_BODY:
      return NULL;
   default:
      break;
   }

   const int nstmts = tree_stmts(where);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(where, i);
      if (tree_ident(s) == id)
         return s;
   }

   const int nports = tree_ports(where);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(where, i);
      if (tree_ident(p) == id)
         return p;
   }

   return NULL;
}

void x_bind_external(tree_t name, jit_scalar_t *result)
{
   assert(tree_kind(name) == T_EXTERNAL_NAME);

   tree_t p1 = tree_part(name, 1), top;
   ident_t path = ident_prefix(lib_name(lib_work()), tree_ident(p1), '.');

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      top = tree_container(name);
   else
      top = root_scope(m)->where;

   tree_t root = top;
   if (tree_kind(top) == T_ELAB)
      root = tree_stmt(top, 0);

   tree_t where = root, next = NULL;
   const int nparts = tree_parts(name);

   for (int i = 0; i < nparts; i++, where = next, next = NULL) {
      tree_t pe = tree_part(name, i);
      assert(tree_kind(pe) == T_PATH_ELT);

      ident_t id = NULL;
      switch (tree_subkind(pe)) {
      case PE_ABSOLUTE:
         {
            tree_t entity = tree_part(name, ++i);
            assert(tree_subkind(entity) == PE_SIMPLE);

            if (root == NULL || tree_ident(root) != tree_ident(entity))
               jit_msg(tree_loc(entity), DIAG_FATAL, "%s is not the name of "
                       "the root of the design hierarchy",
                       istr(tree_ident(entity)));

            next = root;
         }
         continue;
      case PE_SIMPLE:
         id = tree_ident(pe);
         break;
      case PE_GENERATE:
         {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_istr(tb, tree_ident(pe));
            tb_printf(tb, "(%"PRIi64")", assume_int(tree_value(pe)));

            id = ident_new(tb_get(tb));
         }
         break;
      case PE_LIBRARY:
         {
            lib_t lib = lib_require(tree_ident(pe));

            pe = tree_part(name, ++i);
            assert(tree_subkind(pe) == PE_SIMPLE);

            ident_t qual = ident_prefix(lib_name(lib), tree_ident(pe), '.');

            tree_t pack = lib_get(lib, qual);
            if (pack == NULL || !is_package(pack))
               jit_msg(tree_loc(pe), DIAG_FATAL, "%s is not a package",
                       istr(qual));

            next = pack;
         }
         continue;
      default:
         should_not_reach_here();
      }

      if (tree_kind(where) == T_BLOCK) {
         tree_t hier = tree_decl(where, 0);
         assert(tree_kind(hier) == T_HIER);

         if (tree_subkind(hier) == T_COMPONENT) {
            // Skip over implicit block for component declaration
            where = tree_stmt(where, 0);
            assert(tree_kind(where) == T_BLOCK);
            hier = tree_decl(where, 0);
            path = ident_prefix(path, tree_ident(where), '.');
         }
      }
      else if (!is_package(where)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(pe));
         diag_printf(d, "%s is not a concurrent region",
                     istr(tree_ident(where)));
         diag_hint(d, tree_loc(where), "location of %s",
                   istr(tree_ident(where)));
         diag_emit(d);
         jit_abort_with_status(1);
      }

      if ((next = select_name(where, id)) == NULL) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(pe));
         diag_printf(d, "external name %s not found",
                     istr(tree_ident(tree_part(name, nparts - 1))));
         diag_hint(d, tree_loc(pe), "name %s not found inside %s", istr(id),
                   istr(tree_ident(where)));

         if (m == NULL)
            diag_hint(d, NULL, "an object cannot be referenced by an "
                      "external name until it has been elaborated");

         diag_emit(d);
         jit_abort_with_status(1);
      }

      if (i + 1 < nparts)
         path = ident_prefix(path, id, '.');
   }

   if (class_of(where) != tree_class(name)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(name));
      diag_printf(d, "class of object %s is not %s", istr(tree_ident(where)),
                  class_str(tree_class(name)));
      diag_hint(d, tree_loc(name), "external name with class %s",
                class_str(tree_class(name)));
      diag_hint(d, tree_loc(where), "declaration of %s as %s",
                istr(tree_ident(where)), class_str(class_of(where)));
      diag_emit(d);
      jit_abort_with_status(1);
   }

   type_t type = tree_type(where);
   type_t expect = tree_type(name);

   if (!type_eq(type, expect)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(name));
      diag_printf(d, "type of %s %s is not %s",
                  class_str(tree_class(name)), istr(tree_ident(where)),
                  type_pp2(expect, type));
      diag_hint(d, tree_loc(name), "external name with type %s",
                type_pp2(expect, type));
      diag_hint(d, tree_loc(where), "declaration of %s with type %s",
                istr(tree_ident(where)), type_pp2(type, expect));
      diag_emit(d);
      jit_abort_with_status(1);
   }

   jit_t *j = jit_for_thread();

   jit_handle_t handle = jit_compile(j, path);
   (void)jit_link(j, handle);   // Package may not be loaded

   void *ptr = jit_get_frame_var(j, handle, tree_ident(where));

   if (type_is_array(type)) {
      int pos = 0;
      if (tree_class(name) == C_SIGNAL) {
         result[pos++].pointer = *(sig_shared_t **)ptr;
         result[pos++].integer = *(int32_t *)(ptr + 8);
      }
      else
         result[pos++].pointer = ptr;

      if (type_const_bounds(type)) {
         const int ndims = dimension_of(type);
         for (int i = 0; i < ndims; i++) {
            tree_t r = range_of(type, i);
            const range_kind_t dir = tree_subkind(r);
            const int64_t left = assume_int(tree_left(r));
            const int64_t right = assume_int(tree_right(r));
            const int64_t length =
               MAX(0, dir == RANGE_TO ? right - left + 1 : left - right + 1);

            result[pos++].integer = left;
            result[pos++].integer = dir == RANGE_TO ? length : ~length;
         }
      }
      else {
         const int ndims = dimension_of(type);
         for (int i = 0; i < ndims * 2; i++, pos++)
            result[pos].integer = ((int64_t *)ptr)[pos];
      }
   }
   else if (tree_class(name) == C_SIGNAL && type_is_homogeneous(type)) {
      result[0].pointer = *(sig_shared_t **)ptr;
      result[1].integer = *(int32_t *)(ptr + 8);
   }
   else
      result[0].pointer = ptr;
}
