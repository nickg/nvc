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
#include "hash.h"
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
   vlog_node_t    func;
   vlog_node_t    nonconst;
   hset_t        *funcdeps;
   unsigned       later;
   bool           retry;
} fold_ctx_t;

typedef enum {
   FOLD_CONST,
   FOLD_POSSIBLE,
   FOLD_LATER,
   FOLD_NON_CONST,
} fold_state_t;

static fold_state_t get_fold_state(vlog_node_t v, fold_ctx_t *ctx)
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
            return get_fold_state(vlog_value(d), ctx);
         case V_PARAM_DECL:
         case V_GENVAR_DECL:
            ctx->later++;
            return FOLD_LATER;
         case V_ENUM_NAME:
            return FOLD_CONST;
         default:
            ctx->nonconst = d;
            return FOLD_NON_CONST;
         }
      }
   case V_BIT_SELECT:
      {
         fold_state_t state = get_fold_state(vlog_value(v), ctx);

         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i), ctx));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_PART_SELECT:
      {
         fold_state_t value = get_fold_state(vlog_value(v), ctx);
         fold_state_t left = get_fold_state(vlog_left(v), ctx);
         fold_state_t right = get_fold_state(vlog_right(v), ctx);
         fold_state_t state = MAX(value, MAX(left, right));
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_BINARY:
      {
         fold_state_t left = get_fold_state(vlog_left(v), ctx);
         fold_state_t right = get_fold_state(vlog_right(v), ctx);
         fold_state_t state = MAX(left, right);
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_UNARY:
   case V_MIN_TYP_MAX:
      {
         fold_state_t state = get_fold_state(vlog_value(v), ctx);
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_USER_FCALL:
      {
         vlog_node_t d = vlog_ref(v);
         const vlog_flags_t flags = vlog_flags(d);
         if (!(flags & VLOG_F_CONST)) {
            ctx->nonconst = d;
            return FOLD_NON_CONST;
         }
         else if (!(flags & VLOG_F_FOLDED)) {
            hset_insert(ctx->funcdeps, d);
            ctx->later++;
            return FOLD_LATER;
         }

         fold_state_t state = FOLD_CONST;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i), ctx));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_SYS_FCALL:
      {
         // See 1800-2023 section 11.2.1 for list of constant system functions
         switch (vlog_subkind(v)) {
         case V_SYSTF_CLOG2:
         case V_SYSTF_SIGNED:
         case V_SYSTF_UNSIGNED:
         case V_SYSTF_RTOI:
         case V_SYSTF_SQRT:
         case V_SYSTF_CEIL:
         case V_SYSTF_FLOOR:
            break;
         default:
            ctx->nonconst = v;
            return FOLD_NON_CONST;
         }

         fold_state_t state = FOLD_CONST;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i), ctx));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_COND_EXPR:
      {
         fold_state_t value = get_fold_state(vlog_value(v), ctx);
         fold_state_t left = get_fold_state(vlog_left(v), ctx);
         fold_state_t right = get_fold_state(vlog_right(v), ctx);
         fold_state_t state = MAX(value, MAX(left, right));
         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   case V_CONCAT:
      {
         fold_state_t state = FOLD_CONST;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            state = MAX(state, get_fold_state(vlog_param(v, i), ctx));

         if (vlog_has_value(v))
            state = MAX(state, get_fold_state(vlog_value(v), ctx));

         return state == FOLD_CONST ? FOLD_POSSIBLE : state;
      }
   default:
      ctx->nonconst = NULL;
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
   mir_unit_t *mu = vlog_lower_thunk(ctx->mir, NULL, v);

   vlog_node_t result = jit_call_thunk2(ctx->jit, mu, NULL,
                                        fold_make_node_cb, mu);

   mir_unit_free(mu);
   return result;
}

static void fold_report_non_const(vlog_node_t v, fold_ctx_t *ctx)
{
   if (ctx->nonconst == NULL)
      error_at(vlog_loc(v), "expression is not a constant");
   else {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "cannot ");
      switch (vlog_kind(ctx->nonconst)) {
      case V_FUNC_DECL:
         diag_printf(d, "call non-constant user function");
         break;
      case V_SYS_FCALL:
         diag_printf(d, "call system function");
         break;
      case V_VAR_DECL:
         diag_printf(d, "reference variable");
         break;
      case V_NET_DECL:
         diag_printf(d, "reference net");
         break;
      default:
         diag_printf(d, "reference");
         break;
      }
      diag_printf(d, " '%pi' in constant expression",
                  vlog_ident(ctx->nonconst));

      if (vlog_kind(ctx->nonconst) != V_SYS_FCALL)
         diag_hint(d, vlog_loc(ctx->nonconst), "%pi declared here",
                   vlog_ident(ctx->nonconst));

      diag_emit(d);
   }
}

static vlog_node_t fold_const_expr(vlog_node_t v, fold_ctx_t *ctx)
{
   switch (get_fold_state(v, ctx)) {
   case FOLD_CONST:
      return v;
   case FOLD_POSSIBLE:
      return fold_call_thunk(v, ctx);
   case FOLD_LATER:
      return NULL;
   case FOLD_NON_CONST:
      fold_report_non_const(v, ctx);
      return NULL;
   default:
      should_not_reach_here();
   }
}

static vlog_node_t fold_localparam(vlog_node_t v, fold_ctx_t *ctx)
{
   vlog_node_t value = vlog_value(v);
   switch (get_fold_state(value, ctx)) {
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
      fold_report_non_const(v, ctx);
      return v;
   }

   vlog_node_t vtype = vlog_type(v);
   if (!is_implicit_data_type(vtype) || vlog_ranges(vtype) > 0) {
      // Explicitly typed localparam: expand an unbased unsized literal to
      // the declared width
      if (vlog_kind(value) == V_NUMBER
          && vlog_subkind(value) == V_NUMBER_UNBASED) {
         const unsigned w = vlog_size(vtype);
         const bool s = !!(vlog_flags(vtype) & VLOG_F_SIGNED);
         vlog_set_value(v, vlog_fill_unbased(value, w, s));
      }
      return v;
   }

   vlog_node_t dt = vlog_new(V_DATA_TYPE);
   vlog_set_loc(dt, vlog_loc(v));

   int width = 32;
   bool issigned = true;
   const vlog_kind_t kind = vlog_kind(value);
   if (kind == V_REAL)
      vlog_set_subkind(dt, DT_REAL);
   else if (kind == V_NUMBER && vlog_subkind(value) == V_NUMBER_UNBASED) {
      // Self-determined unbased unsized literal takes the default integer
      // width as a four-state logic vector
      width = 32;
      issigned = false;
      vlog_set_value(v, vlog_fill_unbased(value, width, issigned));
      vlog_set_subkind(dt, DT_LOGIC);
   }
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

   vlog_node_t left = fold_const_expr(vlog_left(v), ctx);
   if (left != NULL)
      vlog_set_left(v, left);

   vlog_node_t right = fold_const_expr(vlog_right(v), ctx);
   if (right != NULL)
      vlog_set_right(v, right);

   return v;
}

static vlog_node_t fold_param_decl(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_has_value(v)) {
      vlog_node_t value = vlog_value(v);
      if (get_fold_state(value, ctx) == FOLD_NON_CONST)
         fold_report_non_const(value, ctx);
   }

   return v;
}

static vlog_node_t fold_enum_decl(vlog_node_t v, fold_ctx_t *ctx)
{
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t di = vlog_decl(v, i);
      assert(vlog_has_value(di));

      vlog_node_t value = vlog_value(di);
      if (vlog_kind(value) != V_NUMBER)
         continue;

      number_t n = vlog_number(value);

      for (int j = 0; j < i; j++) {
         vlog_node_t dj = vlog_decl(v, j), cmp = vlog_value(dj);
         if (vlog_kind(cmp) != V_NUMBER)
            continue;
         else if (number_equal(vlog_number(cmp), n)) {
            error_at(vlog_loc(di), "enum names '%pi' and '%pi' both have "
                     "value %"PRIi64, vlog_ident(dj), vlog_ident(di),
                     number_integer(n));
            return v;
         }
      }
   }

   return v;
}

static vlog_node_t fold_part_select(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_subkind(v) == V_RANGE_CONST) {
      vlog_node_t left = fold_const_expr(vlog_left(v), ctx);
      if (left != NULL)
         vlog_set_left(v, left);
   }

   vlog_node_t right = fold_const_expr(vlog_right(v), ctx);
   if (right != NULL)
      vlog_set_right(v, right);

   return v;
}

static vlog_node_t fold_const_value(vlog_node_t v, fold_ctx_t *ctx)
{
   if (vlog_has_value(v)) {
      vlog_node_t value = fold_const_expr(vlog_value(v), ctx);
      if (value != NULL)
         vlog_set_value(v, value);
   }

   return v;
}

static vlog_node_t fold_if_generate(vlog_node_t v, fold_ctx_t *ctx)
{
   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      if (vlog_has_value(c)) {
         vlog_node_t value = fold_const_expr(vlog_value(c), ctx);
         if (value == NULL)
            return v;

         vlog_set_value(c, value);

         int64_t ival;
         if (!vlog_get_const(value, &ival) || ival == 0)
            continue;
      }

      return vlog_stmts(c) == 1 ? vlog_stmt(c, 0) : NULL;
   }

   return NULL;   // None of the conditions are true
}

static vlog_node_t fold_func_decl(vlog_node_t v, fold_ctx_t *ctx)
{
   assert(ctx->func == v);
   ctx->func = NULL;

   if (ctx->later == 0) {
      vlog_set_flags(v, VLOG_F_FOLDED);
      ctx->retry |= hset_contains(ctx->funcdeps, v);
   }

   return v;
}

static vlog_node_t fold_ref(vlog_node_t v, fold_ctx_t *ctx)
{
   if (ctx->func != NULL) {
      // Do not evalulate constant functions which reference unbound or
      // unfolded parameters
      (void)get_fold_state(v, ctx);
   }

   return v;
}

static void vlog_fold_pre_cb(vlog_node_t v, void *context)
{
   fold_ctx_t *ctx = context;

   if (vlog_kind(v) == V_FUNC_DECL) {
      ctx->func = v;
      ctx->later = 0;
   }
}

static vlog_node_t vlog_fold_post_cb(vlog_node_t v, void *context)
{
   switch (vlog_kind(v)) {
   case V_LOCALPARAM:
      return fold_localparam(v, context);
   case V_PARAM_DECL:
      return fold_param_decl(v, context);
   case V_ENUM_DECL:
      return fold_enum_decl(v, context);
   case V_DIMENSION:
      return fold_dimension(v, context);
   case V_PART_SELECT:
      return fold_part_select(v, context);
   case V_CONCAT:
   case V_PARAM_ASSIGN:
   case V_ENUM_NAME:
   case V_DEFPARAM:
      return fold_const_value(v, context);
   case V_IF_GENERATE:
      return fold_if_generate(v, context);
   case V_FUNC_DECL:
      return fold_func_decl(v, context);
   case V_REF:
      return fold_ref(v, context);
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

   const int base_errors = error_count();
   do {
      ctx.funcdeps = hset_new(4);
      ctx.retry = false;
      ctx.later = 0;
      vlog_rewrite(mod, vlog_fold_pre_cb, vlog_fold_post_cb, &ctx);
      hset_free(ctx.funcdeps);
   } while (ctx.retry && error_count() == base_errors);
}
