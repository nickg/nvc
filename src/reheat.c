//
//  Copyright (C) 2026  Nick Gasson
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
#include "cov/cov-api.h"
#include "hash.h"
#include "hier.h"
#include "ident.h"
#include "lower.h"
#include "phase.h"
#include "rt/model.h"
#include "tree.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"
#include "vhdl/vhdl-phase.h"

#include <assert.h>

typedef struct _reheat_ctx reheat_ctx_t;

typedef struct _reheat_ctx {
   const reheat_ctx_t *parent;
   rt_model_t         *model;
   mir_context_t      *mir;
   unit_registry_t    *registry;
   cover_data_t       *cover;
   lower_unit_t       *lowered;
   rt_scope_t         *scope;
   ident_t             dotted;
   ident_t             cloned;
   ident_t             inst_alias;
   hset_t             *instances;
} reheat_ctx_t;

// Thin wrapper around hier_scope_alias() for this module's ctx type.
// The rule lives in hier.h; we just project the fields.
static inline ident_t vlog_scope_alias(const reheat_ctx_t *c)
{
   hier_scope_t s = { c->inst_alias, c->cloned, c->dotted };
   return hier_scope_alias(&s);
}

static void reheat_inherit(reheat_ctx_t *ctx, const reheat_ctx_t *parent)
{
   ctx->parent    = parent;
   ctx->model     = parent->model;
   ctx->mir       = parent->mir;
   ctx->registry  = parent->registry;
   ctx->cover     = parent->cover;
   ctx->instances = parent->instances;
}

static void reheat_block(tree_t b, const reheat_ctx_t *parent)
{
   reheat_ctx_t ctx = {};
   reheat_inherit(&ctx, parent);

   tree_t hier = tree_decl(b, 0);
   assert(tree_kind(hier) == T_HIER);

   ctx.dotted = tree_ident(hier);
   ctx.cloned = tree_ident2(hier);

   if (tree_subkind(hier) == T_VERILOG) {
      vlog_node_t body = tree_vlog(tree_ref(hier));

      // Compute the per-instance alias: parent_alias.label.  Use
      // vlog_scope_alias() so the resulting ident matches the one
      // elab_resolve_one_hier_ref stores on I_IDENT2 and the one
      // vlog_lower_block registers.  Skip when the composed alias
      // equals the shared module name (no clone needed) or the
      // dotted path (mixed VHDL/Verilog wrapper case).
      ident_t inst_alias = NULL;
      if (parent->cloned != NULL) {
         ident_t label = tree_ident(b);
         ident_t alias = ident_prefix(vlog_scope_alias(parent), label, '.');
         if (alias != vlog_ident(body) && alias != ctx.dotted)
            inst_alias = alias;
      }
      ctx.inst_alias = inst_alias;

      if (!hset_contains(ctx.instances, body)) {
         vlog_lower_instance(ctx.mir, body, parent->cloned, b);
         hset_insert(ctx.instances, body);
      }

      vlog_lower_block(ctx.mir, vlog_scope_alias(parent),
                       vlog_scope_alias(&ctx), b);
   }
   else {
      cover_scope_t *cs = cover_get_scope(ctx.cover, ctx.dotted);
      ctx.lowered = lower_instance(ctx.registry, parent->lowered, ctx.cover,
                                   cs, b);
   }

   ctx.scope = create_scope(ctx.model, b, parent->scope);

   const int nstmts = tree_stmts(b);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(b, i);
      if (tree_kind(s) == T_BLOCK)
         reheat_block(s, &ctx);
   }
}

void reheat(tree_t top, unit_registry_t *ur, mir_context_t *mc,
            cover_data_t *cover, rt_model_t *m)
{
   assert(tree_kind(top) == T_ELAB);

   rt_scope_t *root = create_scope(m, top, NULL);

   reheat_ctx_t ctx = {
      .model     = m,
      .scope     = root,
      .cover     = cover,
      .registry  = ur,
      .mir       = mc,
      .instances = hset_new(16),
   };

   reheat_block(tree_stmt(top, 0), &ctx);

   hset_free(ctx.instances);
}
