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
#include "jit/jit.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "printf.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

typedef struct {
   mir_context_t *mir;
   jit_t         *jit;
   unsigned       depth;
} fold_ctx_t;

typedef enum {
   FOLD_CONST,
   FOLD_POSSIBLE,
   FOLD_LATER,
   FOLD_NON_CONST,
} fold_state_t;

static fold_state_t get_fold_state(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REAL:
   case V_STRING:
   case V_NUMBER:
      return FOLD_CONST;
   case V_REF:
      {
         vlog_node_t d = vlog_ref(v);
         switch (vlog_kind(d)) {
         case V_LOCALPARAM:
            return get_fold_state(vlog_value(d));
         case V_PARAM_DECL:
         case V_GENVAR_DECL:
            return FOLD_LATER;
         default:
            return FOLD_NON_CONST;
         }
      }
   case V_BINARY:
      {
         fold_state_t left = get_fold_state(vlog_left(v));
         fold_state_t right = get_fold_state(vlog_right(v));
         fold_state_t state = MAX(left, right);
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_UNARY:
   case V_MIN_TYP_MAX:
      {
         fold_state_t state = get_fold_state(vlog_value(v));
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_USER_FCALL:
      if (!(vlog_flags(vlog_ref(v)) & VLOG_F_CONST))
         return FOLD_NON_CONST;
      // Fall-through
   case V_SYS_FCALL:
      {
         fold_state_t state = FOLD_CONST;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i)));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_COND_EXPR:
      {
         fold_state_t value = get_fold_state(vlog_value(v));
         fold_state_t left = get_fold_state(vlog_left(v));
         fold_state_t right = get_fold_state(vlog_right(v));
         fold_state_t state = MAX(value, MAX(left, right));
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_CONCAT:
      {
         fold_state_t state = FOLD_CONST;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i)));

         if (vlog_has_value(v))
            state = MAX(state, get_fold_state(vlog_value(v)));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   default:
      return FOLD_NON_CONST;
   }
}

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
         const bool issigned = mir_get_signed(mu, type);

         number_t n = number_from_jit(size, issigned, result[0], result[1]);

         vlog_node_t v = vlog_new(V_NUMBER);
         vlog_set_number(v, n);
         return v;
      }
   case MIR_TYPE_REAL:
      {
         vlog_node_t v = vlog_new(V_REAL);
         vlog_set_dval(v, result[0].real);
         return v;
      }
   default:
      return NULL;  // TODO
   }
}

static vlog_node_t fold_call_thunk(vlog_node_t v, fold_ctx_t *ctx)
{
   if (ctx->depth > 1)
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
   switch (get_fold_state(value)) {
   case FOLD_CONST:
      break;
   case FOLD_POSSIBLE:
      {
         vlog_node_t result = fold_call_thunk(v, ctx);
         if (result == NULL)
            return v;

         vlog_set_value(v, (value = result));
      }
      break;
   case FOLD_LATER:
      return v;
   case FOLD_NON_CONST:
      should_not_reach_here();
   }

   vlog_node_t vtype = vlog_type(v);
   if (!is_implicit_data_type(vtype) || vlog_ranges(vtype) > 0)
      return v;

   vlog_node_t dt = vlog_new(V_DATA_TYPE);
   vlog_set_loc(dt, vlog_loc(v));

   int width = 32;
   bool issigned = true;
   const vlog_kind_t kind = vlog_kind(value);
   if (kind == V_REAL)
      vlog_set_subkind(dt, DT_REAL);
   else {
      width = vlog_width(value);
      issigned = vlog_is_signed(value);

      if (width == 32 && issigned) {
         vlog_set_subkind(dt, DT_INTEGER);
         vlog_set_flags(dt, VLOG_F_SIGNED);
      }
      else {
         vlog_set_subkind(dt, DT_LOGIC);
         if (issigned)
            vlog_set_flags(dt, VLOG_F_SIGNED);
      }
   }

   if (width != 1) {
      vlog_node_t left = vlog_new(V_NUMBER);
      vlog_set_number(left, number_from_int(width - 1));

      vlog_node_t right = vlog_new(V_NUMBER);
      vlog_set_number(right, number_from_int(0));

      vlog_node_t r = vlog_new(V_DIMENSION);
      vlog_set_subkind(r, V_DIM_PACKED);
      vlog_set_left(r, left);
      vlog_set_right(r, right);

      vlog_add_range(dt, r);
   }

   vlog_set_type(v, dt);
   return v;
}

static vlog_node_t fold_dimension(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_subkind(v) == V_DIM_UNSIZED)
      return v;

   vlog_node_t left = vlog_left(v);
   if (get_fold_state(left) == FOLD_POSSIBLE) {
      vlog_node_t result = fold_call_thunk(left, ctx);
      if (result != NULL)
         vlog_set_left(v, result);
   }

   vlog_node_t right = vlog_right(v);
   if (get_fold_state(right) == FOLD_POSSIBLE) {
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
      if (get_fold_state(left) == FOLD_POSSIBLE) {
         vlog_node_t left_fold = fold_call_thunk(left, ctx);
         if (left_fold != NULL)
            vlog_set_left(v, left_fold);
      }
   }

   vlog_node_t right = vlog_right(v);
   if (get_fold_state(right) == FOLD_POSSIBLE) {
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
      if (get_fold_state(value) == FOLD_POSSIBLE) {
         vlog_node_t result = fold_call_thunk(value, ctx);
         if (result != NULL)
            vlog_set_value(v, result);
      }
   }

   return v;
}

static vlog_node_t fold_if_generate(vlog_node_t v, fold_ctx_t *ctx)
{
   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      if (vlog_has_value(c)) {
         vlog_node_t value = vlog_value(c);
         switch (get_fold_state(value)) {
         case FOLD_CONST:
            break;
         case FOLD_POSSIBLE:
            {
               vlog_node_t result = fold_call_thunk(value, ctx);
               if (result == NULL)
                  return v;

               vlog_set_value(c, (value = result));
            }
            break;
         case FOLD_LATER:
            return v;
         case FOLD_NON_CONST:
            should_not_reach_here();
         }

         int64_t ival;
         if (!vlog_get_const(value, &ival) || ival == 0)
            continue;
      }

      return vlog_stmts(c) == 1 ? vlog_stmt(c, 0) : NULL;
   }

   return NULL;   // None of the conditions are true
}

static void vlog_fold_pre_cb(vlog_node_t v, void *context)
{
   fold_ctx_t *ctx = context;

   switch (vlog_kind(v)) {
   case V_GEN_BLOCK:
   case V_FOR_GENERATE:
   case V_IF_GENERATE:
   case V_MODULE:
   case V_INST_BODY:
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
   case V_IF_GENERATE:
      assert(ctx->depth > 0);
      ctx->depth--;
      return fold_if_generate(v, context);
   case V_GEN_BLOCK:
   case V_FOR_GENERATE:
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
