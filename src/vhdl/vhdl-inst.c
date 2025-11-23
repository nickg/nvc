//
//  Copyright (C) 2025 Nick Gasson
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
#include "ident.h"
#include "inst.h"
#include "tree.h"
#include "vhdl/vhdl-phase.h"

#include <assert.h>
#include <inttypes.h>

typedef struct {
   tree_t  genvar;
   ident_t prefix;
} generate_pred_ctx_t;

static bool elab_copy_genvar_cb(tree_t t, void *_ctx)
{
   const generate_pred_ctx_t *ctx = _ctx;
   switch (tree_kind(t)) {
   case T_REF:
      return tree_ref(t) == ctx->genvar;
   case T_FUNC_DECL:
   case T_PROC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         return false;
      // Fall-through
   case T_FUNC_BODY:
   case T_PROC_BODY:
      return ident_starts_with(tree_ident2(t), ctx->prefix);
   default:
      return false;
   }
}

tree_t vhdl_generate_instance(tree_t t, ident_t prefix, ident_t dotted)
{
   assert(tree_kind(t) == T_FOR_GENERATE);

   tree_t g = tree_decl(t, 0);
   assert(tree_kind(g) == T_GENERIC_DECL);

   generate_pred_ctx_t pred_ctx = {
      .genvar = g,
      .prefix = prefix,
   };

   ident_t prefixes[] = { prefix };
   tree_t roots[] = { t, g };
   copy_with_renaming(roots, ARRAY_LEN(roots), elab_copy_genvar_cb, NULL,
                      &pred_ctx, dotted, prefixes, ARRAY_LEN(prefixes));

   return roots[0];
}
