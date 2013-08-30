//
//  Copyright (C) 2013  Nick Gasson
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
#include <stdlib.h>

#define MAX_BUILTIN_ARGS 2
#define VTABLE_SZ        16
#define MAX_ITERS        1000

static ident_t std_bool_i = NULL;
static ident_t builtin_i  = NULL;
static ident_t result_i   = NULL;

typedef struct vtable vtable_t;
typedef struct vtframe vtframe_t;

struct vtframe {
   struct {
      ident_t name;
      tree_t  value;
   } binding[VTABLE_SZ];

   size_t     size;
   vtframe_t *down;
};

struct vtable {
   vtframe_t *top;
   bool       failed;
};

static void eval_stmt(tree_t t, vtable_t *v);
static tree_t eval_expr(tree_t t, vtable_t *v);

static bool debug = false;

#define eval_error(t, ...) do {                 \
      if (debug)                                \
         warn_at(tree_loc(t),  __VA_ARGS__);    \
      v->failed = true;                         \
      return;                                   \
   } while (0);

static void vtable_push(vtable_t *v)
{
   vtframe_t *f = xmalloc(sizeof(vtframe_t));
   f->size = 0;
   f->down = v->top;

   v->top = f;
}

static void vtable_pop(vtable_t *v)
{
   vtframe_t *f = v->top;
   v->top = f->down;
   free(f);
}

static void vtable_bind(vtable_t *v, ident_t name, tree_t value)
{
   vtframe_t *f = v->top;
   if (f == NULL)
      return;

   for (size_t i = 0; i < f->size; i++) {
      if (f->binding[i].name == name) {
         f->binding[i].value = value;
         return;
      }
   }

   assert(f->size < VTABLE_SZ);
   f->binding[f->size].name  = name;
   f->binding[f->size].value = value;
   ++(f->size);
}

static tree_t vtframe_get(vtframe_t *f, ident_t name)
{
   if (f == NULL)
      return NULL;
   else {
      for (size_t i = 0; i < f->size; i++) {
         if (f->binding[i].name == name)
            return f->binding[i].value;
      }
      return vtframe_get(f->down, name);
   }
}

static tree_t vtable_get(vtable_t *v, ident_t name)
{
   return vtframe_get(v->top, name);
}

static bool folded_agg(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         literal_t dummy;
         switch (tree_subkind(a)) {
         case A_NAMED:
            if (!folded_int(tree_name(a), &dummy))
               return false;
            break;
         case A_RANGE:
            {
               range_t r = tree_range(a);
               if (!folded_int(r.left, &dummy)
                   || !folded_int(r.right, &dummy))
                  return false;
            }
            break;
         default:
            break;
         }
      }
      return true;
   }
   else
      return false;
}

static bool folded(tree_t t)
{
   tree_kind_t kind = tree_kind(t);
   if (kind == T_LITERAL)
      return true;
   else if (kind == T_AGGREGATE)
      return folded_agg(t);
   else if (kind == T_REF)
      return folded_bool(t, NULL);
   else
      return false;
}

static tree_t get_bool_lit(tree_t t, bool v)
{
   tree_t fdecl = tree_ref(t);
   assert(tree_kind(fdecl) == T_FUNC_DECL);

   static type_t bool_type = NULL;
   if (bool_type == NULL) {
      lib_t std = lib_find("std", true, true);
      assert(std != NULL);

      tree_t standard = lib_get(std, ident_new("STD.STANDARD"));
      assert(standard != NULL);

      const int ndecls = tree_decls(standard);
      for (int i = 0; (i < ndecls) && (bool_type == NULL); i++) {
         tree_t d = tree_decl(standard, i);
         if (tree_ident(d) == std_bool_i)
            bool_type = tree_type(d);
      }
      assert(bool_type != NULL);
   }

   tree_t lit = type_enum_literal(bool_type, v ? 1 : 0);

   tree_t b = tree_new(T_REF);
   tree_set_loc(b, tree_loc(t));
   tree_set_ref(b, lit);
   tree_set_type(b, bool_type);
   tree_set_ident(b, tree_ident(lit));

   return b;
}

static tree_t eval_fcall_log(tree_t t, ident_t builtin, bool *args)
{
   if (icmp(builtin, "not"))
      return get_bool_lit(t, !args[0]);
   else if (icmp(builtin, "and"))
      return get_bool_lit(t, args[0] && args[1]);
   else if (icmp(builtin, "nand"))
      return get_bool_lit(t, !(args[0] && args[1]));
   else if (icmp(builtin, "or"))
      return get_bool_lit(t, args[0] || args[1]);
   else if (icmp(builtin, "nor"))
      return get_bool_lit(t, !(args[0] || args[1]));
   else if (icmp(builtin, "xor"))
      return get_bool_lit(t, args[0] ^ args[1]);
   else if (icmp(builtin, "xnor"))
      return get_bool_lit(t, !(args[0] ^ args[1]));
   else
      return t;
}

static tree_t eval_fcall_real(tree_t t, ident_t builtin, literal_t *args)
{
   const int lkind = args[0].kind;  // Assume all types checked same
   assert(lkind == L_REAL);

   if (icmp(builtin, "mul")) {
      return get_real_lit(t, args[0].r * args[1].r);
   }
   else if (icmp(builtin, "div")) {
      return get_real_lit(t, args[0].r / args[1].r);
   }
   else if (icmp(builtin, "add")) {
      return get_real_lit(t, args[0].r + args[1].r);
   }
   else if (icmp(builtin, "sub")) {
      return get_real_lit(t, args[0].r - args[1].r);
   }
   else if (icmp(builtin, "neg")) {
      return get_real_lit(t, -args[0].r);
   }
   else if (icmp(builtin, "identity")) {
      return get_real_lit(t, args[0].r);
   }
   else if (icmp(builtin, "eq")) {
      return get_bool_lit(t, args[0].r == args[1].r);
   }
   else if (icmp(builtin, "neq")) {
      return get_bool_lit(t, args[0].r != args[1].r);
   }
   else if (icmp(builtin, "gt")) {
      return get_bool_lit(t, args[0].r > args[1].r);
   }
   else if (icmp(builtin, "lt")) {
      return get_bool_lit(t, args[0].r < args[1].r);
   }
   else
      return t;
}

static tree_t eval_fcall_int(tree_t t, ident_t builtin, literal_t *args)
{
   const int lkind = args[0].kind;  // Assume all types checked same
   assert(lkind == L_INT);

   if (icmp(builtin, "mul")) {
      return get_int_lit(t, args[0].i * args[1].i);
   }
   else if (icmp(builtin, "div")) {
      return get_int_lit(t, args[0].i / args[1].i);
   }
   else if (icmp(builtin, "add")) {
      return get_int_lit(t, args[0].i + args[1].i);
   }
   else if (icmp(builtin, "sub")) {
      return get_int_lit(t, args[0].i - args[1].i);
   }
   else if (icmp(builtin, "neg")) {
      return get_int_lit(t, -args[0].i);
   }
   else if (icmp(builtin, "identity")) {
      return get_int_lit(t, args[0].i);
   }
   else if (icmp(builtin, "eq")) {
      return get_bool_lit(t, args[0].i == args[1].i);
   }
   else if (icmp(builtin, "neq")) {
      return get_bool_lit(t, args[0].i != args[1].i);
   }
   else if (icmp(builtin, "gt")) {
      return get_bool_lit(t, args[0].i > args[1].i);
   }
   else if (icmp(builtin, "lt")) {
      return get_bool_lit(t, args[0].i < args[1].i);
   }
   else if (icmp(builtin, "leq"))
      return get_bool_lit(t, args[0].i <= args[1].i);
   else if (icmp(builtin, "geq"))
      return get_bool_lit(t, args[0].i >= args[1].i);
   else
      return t;
}

static tree_t eval_fcall_agg(tree_t t, ident_t builtin)
{
   bool agg_low  = icmp(builtin, "agg_low");
   bool agg_high = icmp(builtin, "agg_high");

   if (agg_low || agg_high) {
      int64_t low = INT64_MAX, high = INT64_MIN;
      tree_t p = tree_param(t, 0);
      tree_t value = tree_value(p);
      const int nassocs = tree_assocs(value);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(value, i);
         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               int64_t tmp = assume_int(tree_name(a));
               if (tmp < low) low = tmp;
               if (tmp > high) high = tmp;
            }
            break;

         case A_RANGE:
            {
               int64_t low_r, high_r;
               range_bounds(tree_range(a), &low_r, &high_r);
               if (low_r < low) low = low_r;
               if (high_r > high) high = high_r;
            }
            break;

         default:
            assert(false);
         }
      }

      return get_int_lit(t, agg_low ? low : high);
   }
   else
      return t;
}

static void eval_func_body(tree_t t, vtable_t *v)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t decl = tree_decl(t, i);
      if (tree_kind(decl) == T_VAR_DECL)
         vtable_bind(v, tree_ident(decl), eval_expr(tree_value(decl), v));
   }

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      eval_stmt(tree_stmt(t, i), v);
      if (v->failed || vtable_get(v, result_i))
         return;
   }
}

static tree_t eval_fcall(tree_t t, vtable_t *v)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin == NULL) {
      if (tree_kind(decl) != T_FUNC_BODY)
         return t;

      // Only evaluating scalar functions is supported at the moment
      if (type_is_array(tree_type(t)))
         return t;

      vtable_push(v);

      const int nports = tree_ports(decl);
      for (int i = 0; i < nports; i++) {
         tree_t port  = tree_port(decl, i);
         tree_t value = tree_value(tree_param(t, i));

         if (!folded(value)) {
            vtable_pop(v);
            return t;    // Cannot fold this
         }
         else
            vtable_bind(v, tree_ident(port), value);
      }

      eval_func_body(decl, v);
      tree_t result = vtable_get(v, result_i);
      vtable_pop(v);

      return ((result != NULL) && folded(result)) ? result : t;
   }

   if (icmp(builtin, "length")) {
      tree_t dim   = tree_value(tree_param(t, 0));
      tree_t array = tree_value(tree_param(t, 1));

      if (tree_kind(array) != T_REF)
         return t;   // Cannot fold this

      literal_t dim_i;
      if ((type_kind(tree_type(array)) != T_UARRAY)
          && folded_int(dim, &dim_i)) {
         range_t r = type_dim(tree_type(array), dim_i.i - 1);
         if (tree_kind(r.left) == T_LITERAL
             && tree_kind(r.right) == T_LITERAL) {
            int64_t low, high;
            range_bounds(r, &low, &high);
            return get_int_lit(t, (high < low) ? 0 : high - low + 1);
         }
      }
   }

   const bool uarray_left  = icmp(builtin, "uarray_left");
   const bool uarray_right = icmp(builtin, "uarray_right");

   if (uarray_left || uarray_right) {
      tree_t array = tree_value(tree_param(t, 0));
      assert(tree_kind(array) == T_REF);

      tree_t decl = tree_ref(array);
      type_t array_type = tree_type(decl);

      if (type_kind(array_type) == T_UARRAY)
         return t;   // Cannot fold this

      range_t dim0 = type_dim(array_type, 0);

      return uarray_left ? dim0.left : dim0.right;
   }

   if (tree_params(t) > MAX_BUILTIN_ARGS)
      return t;

   bool can_fold_int  = true;
   bool can_fold_log  = true;
   bool can_fold_agg  = true;
   bool can_fold_real = true;
   literal_t largs[MAX_BUILTIN_ARGS];
   bool bargs[MAX_BUILTIN_ARGS];
   for (unsigned i = 0; i < tree_params(t); i++) {
      tree_t p = tree_param(t, i);
      tree_t val = eval_expr(tree_value(p), v);
      can_fold_int  = can_fold_int && folded_int(val, &largs[i]);
      can_fold_log  = can_fold_log && folded_bool(val, &bargs[i]);
      can_fold_agg  = can_fold_agg && folded_agg(val);
      can_fold_real = can_fold_real && folded_real(val, &largs[i]);
   }

   if (can_fold_int)
      return eval_fcall_int(t, builtin, largs);
   else if (can_fold_log)
      return eval_fcall_log(t, builtin, bargs);
   else if (can_fold_agg)
      return eval_fcall_agg(t, builtin);
   else if (can_fold_real)
      return eval_fcall_real(t, builtin, largs);
   else
      return t;
}

static tree_t eval_ref(tree_t t, vtable_t *v)
{
   tree_t binding = vtable_get(v, tree_ident(tree_ref(t)));
   return (binding != NULL) ? binding : t;
}

static tree_t eval_expr(tree_t t, vtable_t *v)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      return eval_fcall(t, v);
   case T_REF:
      return eval_ref(t, v);
   default:
      return t;
   }
}

static void eval_return(tree_t t, vtable_t *v)
{
   if (tree_has_value(t))
      vtable_bind(v, result_i, eval_expr(tree_value(t), v));
}

static void eval_if(tree_t t, vtable_t *v)
{
   tree_t cond = eval_expr(tree_value(t), v);
   bool cond_b;
   if (!folded_bool(cond, &cond_b))
      eval_error(cond, "cannot constant fold expression");

   if (cond_b) {
      const int nstmts = tree_stmts(t);
      for (int i = 0; i < nstmts; i++)
         eval_stmt(tree_stmt(t, i), v);
   }
   else {
      const int nstmts = tree_else_stmts(t);
      for (int i = 0; i < nstmts; i++)
         eval_stmt(tree_else_stmt(t, i), v);
   }
}

static void eval_while(tree_t t, vtable_t *v)
{
   int iters = 0;
   tree_t value = tree_value(t);
   for (;;) {
      tree_t cond = eval_expr(value, v);
      bool cond_b;
      if (!folded_bool(cond, &cond_b))
         eval_error(value, "cannot constant fold expression");

      if (!cond_b)
         break;
      else if (++iters == MAX_ITERS) {
         warn_at(tree_loc(t), "iteration limit exceeded");
         v->failed = true;
         return;
      }

      const int nstmts = tree_stmts(t);
      for (int i = 0; i < nstmts; i++)
         eval_stmt(tree_stmt(t, i), v);
   }
}

static void eval_var_assign(tree_t t, vtable_t *v)
{
   tree_t target = tree_target(t);
   if (tree_kind(target) != T_REF)
      eval_error(target, "cannot evaluate this target");

   tree_t value = tree_value(t);
   tree_t updated = eval_expr(value, v);
   if (!folded(updated))
      eval_error(value, "cannot constant fold expression");

   vtable_bind(v, tree_ident(tree_ref(target)), updated);
}

static void eval_stmt(tree_t t, vtable_t *v)
{
   switch (tree_kind(t)) {
   case T_RETURN:
      eval_return(t, v);
      break;
   case T_WHILE:
      eval_while(t, v);
      break;
   case T_IF:
      eval_if(t, v);
      break;
   case T_VAR_ASSIGN:
      eval_var_assign(t, v);
      break;
   default:
      eval_error(t, "cannot evaluate statement %s",
                 tree_kind_str(tree_kind(t)));
   }
}

static void eval_intern_strings(void)
{
   // Intern some commonly used strings

   std_bool_i = ident_new("STD.STANDARD.BOOLEAN");
   builtin_i  = ident_new("builtin");
}

tree_t eval(tree_t fcall)
{
   assert(tree_kind(fcall) == T_FCALL);

   static bool have_interned = false;
   if (!have_interned) {
      eval_intern_strings();
      have_interned = true;

      debug = (getenv("NVC_EVAL_DEBUG") != NULL);
   }

   vtable_t vt = {
      .top    = NULL,
      .failed = false
   };
   tree_t r = eval_fcall(fcall, &vt);
   return vt.failed ? fcall : r;
}
