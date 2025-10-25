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
#include "common.h"
#include "ident.h"
#include "inst.h"
#include "tree.h"
#include "phase.h"
#include "vhdl/vhdl-phase.h"

#include <assert.h>
#include <inttypes.h>

typedef struct {
   tree_t  genvar;
   ident_t prefix;
} generate_pred_ctx_t;

static void map_generics(tree_t out, tree_t entity, tree_t bind)
{
   const int ngenerics = tree_generics(entity);
   const int ngenmaps = tree_genmaps(bind);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(entity, i);
      tree_add_generic(out, g);

      tree_t map = NULL;
      if (i < ngenmaps) {
         map = tree_genmap(bind, i);
         assert(tree_subkind(map) == P_POS);
         assert(tree_pos(map) == i);
      }
      else if (tree_has_value(g)) {
         map = tree_new(T_PARAM);
         tree_set_loc(map, tree_loc(g));
         tree_set_subkind(map, P_POS);
         tree_set_pos(map, i);
         tree_set_value(map, tree_value(g));
      }

      if (map == NULL)
         error_at(tree_loc(bind), "missing value for generic %s with no "
                  "default", istr(tree_ident(g)));
      else
         tree_add_genmap(out, map);
   }
}

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

tree_t vhdl_component_instance(tree_t comp, tree_t inst, ident_t dotted)
{
   assert(tree_kind(comp) == T_COMPONENT);
   assert(tree_kind(inst) == T_INSTANCE);

   tree_t spec = tree_spec(inst);
   assert(tree_kind(spec) == T_SPEC);

   tree_t roots[] = { comp, spec };
   new_instance(roots, ARRAY_LEN(roots), dotted, NULL, 0);

   tree_t comp_copy = roots[0], spec_copy = roots[1];

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, dotted);

   map_generics(b, comp_copy, inst);

   const int nports = tree_ports(comp_copy);
   for (int i = 0; i < nports; i++)
      tree_add_port(b, tree_port(comp_copy, i));

   if (!tree_has_value(spec_copy))
      return b;

   tree_t bind = tree_value(spec_copy);

   tree_t unit = tree_ref(bind);
   tree_t primary = primary_unit_of(unit);

   tree_t sub = tree_new(T_INSTANCE);
   tree_set_loc(sub, tree_loc(spec));
   tree_set_ident(sub, ident_rfrom(tree_ident(primary), '.'));

   const int ndecls = tree_decls(spec_copy);
   if (ndecls > 0) {
      tree_t cfg = tree_new(T_CONFIGURATION);
      tree_set_ident(cfg, dotted);
      tree_set_ident2(cfg, tree_ident(bind));
      tree_set_primary(cfg, primary);

      for (int i = 0; i < ndecls; i++)
         tree_add_decl(cfg, tree_decl(spec_copy, i));

      tree_set_ident2(sub, dotted);
      tree_set_class(sub, C_CONFIGURATION);
      tree_set_ref(sub, cfg);
   }
   else {
      tree_set_ident2(sub, tree_ident(bind));
      tree_set_class(sub, C_ENTITY);
      tree_set_ref(sub, unit);
   }

   const int ngenmaps = tree_genmaps(bind);
   for (int i = 0; i < ngenmaps; i++)
      tree_add_genmap(sub, tree_genmap(bind, i));

   const int nparams = tree_params(bind);
   for (int i = 0; i < nparams; i++)
      tree_add_param(sub, tree_param(bind, i));

   tree_add_stmt(b, sub);

   return b;
}
