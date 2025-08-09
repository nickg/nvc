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
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>

static vlog_node_t bind_parameter(vlog_node_t decl, int nth, vlog_node_t inst)
{
   vlog_node_t value = NULL;
   if (vlog_has_value(decl))
      value = vlog_value(decl);

   if (inst != NULL) {
      vlog_node_t p;
      const int nparams = vlog_params(inst);
      if (nth < nparams && !vlog_has_ident((p = vlog_param(inst, nth))))
         value = vlog_value(p);
      else {
         ident_t id = vlog_ident(decl);
         for (int i = 0; i < nparams; i++) {
            p = vlog_param(inst, i);
            if (vlog_has_ident(p) && vlog_ident(p) == id) {
               value = vlog_value(p);
               break;
            }
         }
      }
   }

   if (value == NULL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(inst));
      diag_printf(d, "missing value for parameter %s", istr(vlog_ident(decl)));
      diag_hint(d, vlog_loc(decl), "parameter declared here");
      diag_emit(d);
      return NULL;
   }

   vlog_node_t local = vlog_new(V_LOCALPARAM);
   vlog_set_loc(local, vlog_loc(decl));
   vlog_set_ident(local, vlog_ident(decl));
   vlog_set_value(local, value);
   vlog_set_type(local, vlog_type(decl));

   return local;
}

static bool should_copy_pred(vlog_node_t v, void *ctx)
{
   switch (vlog_kind(v)) {
   case V_REF:
      return vlog_kind(vlog_ref(v)) == V_PARAM_DECL;
   default:
      return false;
   }
}

static vlog_node_t fixup_refs_cb(vlog_node_t v, void *ctx)
{
   hash_t *map = ctx;

   if (vlog_kind(v) == V_REF) {
      vlog_node_t new = hash_get(map, vlog_ref(v));
      if (new != NULL)
         vlog_set_ref(v, new);
   }

   return v;
}

vlog_node_t vlog_new_instance(vlog_node_t mod, vlog_node_t inst, ident_t prefix)
{
   ident_t suffix = vlog_ident(vlog_stmt(inst, 0));

   vlog_node_t v = vlog_new(V_INST_BODY);
   vlog_set_ident(v, ident_prefix(prefix, suffix, '.'));
   vlog_set_ident2(v, vlog_ident2(mod));
   vlog_set_loc(v, vlog_loc(inst));

   vlog_node_t copy = vlog_copy(mod, should_copy_pred, NULL);

   hash_t *map = hash_new(16);

   const int nports = vlog_ports(copy);
   for (int i = 0; i < nports; i++)
      vlog_add_port(v, vlog_port(copy, i));

   int pidx = 0;
   const int ndecls = vlog_decls(copy);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(copy, i);
      switch (vlog_kind(d)) {
      case V_PARAM_DECL:
         {
            vlog_node_t local = bind_parameter(d, pidx++, inst);
            if (local != NULL) {
               vlog_add_decl(v, local);
               hash_put(map, d, local);
            }
         }
         break;
      default:
         vlog_add_decl(v, d);
         break;
      }
   }

   if (inst != NULL) {
      const int nparams = vlog_params(inst);
      if (pidx < nparams)
         error_at(vlog_loc(inst), "module %s has %d parameter%s but %d "
                  "values given", istr(vlog_ident2(mod)), pidx,
                  pidx != 1 ? "s" : "", nparams);
   }

   const int nstmts = vlog_stmts(copy);
   for (int i = 0; i < nstmts; i++)
      vlog_add_stmt(v, vlog_stmt(copy, i));

   vlog_rewrite(v, fixup_refs_cb, map);
   vlog_simp(v);

   hash_free(map);
   return v;
}
