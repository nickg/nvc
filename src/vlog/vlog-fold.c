//
//  Copyright (C) 2026 Nick Gasson
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
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"
#include "jit/jit.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

typedef struct {
   mir_context_t *mir;
   jit_t         *jit;
   unsigned       depth;
} fold_ctx_t;

static void *fold_make_node_cb(jit_scalar_t *result, void *arg)
{
   mir_unit_t *mu = arg;

   mir_type_t type = mir_get_result(mu);

   switch (mir_get_class(mu, type)) {
   case MIR_TYPE_VEC2:
      return NULL;   // TODO
   case MIR_TYPE_VEC4:
      {
         const int size = mir_get_size(mu, type);

         vlog_node_t n = vlog_new(V_NUMBER);
         vlog_set_number(n, number_from_jit(size, result[0], result[1]));
         return n;
      }
   default:
      return NULL;  // TODO
   }
}

static bool is_folded(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REAL:
   case V_STRING:
   case V_NUMBER:
      return true;
   case V_REF:
      return vlog_kind(vlog_ref(v)) == V_LOCALPARAM;
   default:
      return false;
   }
}

static vlog_node_t fold_call_thunk(vlog_node_t v, fold_ctx_t *ctx)
{
   if (ctx->depth > 0)
      return NULL;   // Do not fold expressions in inner blocks

   mir_unit_t *mu = vlog_lower_thunk(ctx->mir, NULL, v);

   vlog_node_t result = jit_call_thunk2(ctx->jit, mu, NULL,
                                        fold_make_node_cb, mu);

   mir_unit_free(mu);
   return result;
}

static vlog_node_t fold_localparam(vlog_node_t v, fold_ctx_t *ctx)
{
   vlog_node_t value = vlog_value(v);
   if (is_folded(value))
      return v;

   vlog_node_t result = fold_call_thunk(v, ctx);
   if (result != NULL)
      vlog_set_value(v, result);

   return v;
}

static vlog_node_t fold_dimension(vlog_node_t v, fold_ctx_t *ctx)
{
   vlog_node_t left = vlog_left(v);
   if (!is_folded(left)) {
      vlog_node_t result = fold_call_thunk(left, ctx);
      if (result != NULL)
         vlog_set_left(v, result);
   }

   vlog_node_t right = vlog_right(v);
   if (!is_folded(right)) {
      vlog_node_t result = fold_call_thunk(right, ctx);
      if (result != NULL)
         vlog_set_right(v, result);
   }

   return v;
}

static vlog_node_t fold_part_select(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_subkind(v) == V_RANGE_CONST) {
      vlog_node_t left = vlog_left(v);
      if (!is_folded(left)) {
         vlog_node_t left_fold = fold_call_thunk(left, ctx);
         if (left_fold != NULL)
            vlog_set_left(v, left_fold);
      }
   }

   vlog_node_t right = vlog_right(v);
   if (!is_folded(right)) {
      vlog_node_t right_fold = fold_call_thunk(right, ctx);
      if (right_fold != NULL)
         vlog_set_right(v, right_fold);
   }

   return v;
}

static vlog_node_t fold_concat(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_has_value(v)) {
      vlog_node_t value = vlog_value(v);
      if (!is_folded(value)) {
         vlog_node_t result = fold_call_thunk(value, ctx);
         if (result != NULL)
            vlog_set_value(v, result);
      }
   }

   return v;
}

static void vlog_fold_pre_cb(vlog_node_t v, void *context)
{
   fold_ctx_t *ctx = context;

   switch (vlog_kind(v)) {
   case V_GEN_BLOCK:
   case V_FOR_GENERATE:
   case V_IF_GENERATE:
      ctx->depth++;
      break;
   default:
      break;
   }
}

static vlog_node_t vlog_fold_post_cb(vlog_node_t v, void *context)
{
   fold_ctx_t *ctx = context;

   switch (vlog_kind(v)) {
   case V_LOCALPARAM:
      return fold_localparam(v, context);
   case V_DIMENSION:
      return fold_dimension(v, context);
   case V_PART_SELECT:
      return fold_part_select(v, context);
   case V_CONCAT:
      return fold_concat(v, context);
   case V_GEN_BLOCK:
   case V_FOR_GENERATE:
   case V_IF_GENERATE:
      assert(ctx->depth > 0);
      ctx->depth--;
      return v;
   default:
      return v;
   }
}

void vlog_fold(vlog_node_t mod, mir_context_t *mc, jit_t *j)
{
   fold_ctx_t ctx = {
      .mir = mc,
      .jit = j,
   };

   vlog_lower_const_funcs(mc, mod);
   vlog_rewrite(mod, vlog_fold_pre_cb, vlog_fold_post_cb, &ctx);
}
