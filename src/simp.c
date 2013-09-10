//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include <inttypes.h>

#define MAX_BUILTIN_ARGS 2

static int errors = 0;

#define simp_error(t, ...) \
   { errors++; error_at(tree_loc(t), __VA_ARGS__); return t; }
#define simp_error_bool(t, ...) \
   { errors++; error_at(tree_loc(t), __VA_ARGS__); return false; }

static tree_t simp_tree(tree_t t, void *context);
static void simp_build_wait(tree_t ref, void *context);

static tree_t simp_call_args(tree_t t)
{
   // Replace named arguments with positional ones

   tree_t decl = tree_ref(t);

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   int last_pos = -1;
   for (int i = 0; i < nparams; i++) {
      if (tree_subkind(tree_param(t, i)) == P_POS)
         last_pos = i;
   }

   if (last_pos == nparams - 1)
      return t;    // No named arguments

   tree_t new = tree_new(tree_kind(t));
   tree_set_loc(new, tree_loc(t));
   tree_set_ident(new, tree_ident(t));
   tree_set_ref(new, tree_ref(t));

   if (tree_kind(t) == T_FCALL)
      tree_set_type(new, tree_type(t));
   else if (tree_kind(t) == T_PCALL)
      tree_set_ident2(new, tree_ident2(t));

   for (int i = 0; i <= last_pos; i++)
      tree_add_param(new, tree_param(t, i));

   for (int i = last_pos + 1; i < nports; i++) {
      tree_t port  = tree_port(decl, i);
      ident_t name = tree_ident(port);

      bool found = false;
      for (int j = last_pos + 1; (j < nparams) && !found; j++) {
         tree_t p = tree_param(t, j);
         assert(tree_subkind(p) == P_NAMED);

         tree_t ref = tree_name(p);
         assert(tree_kind(ref) == T_REF);

         if (name == tree_ident(ref)) {
            tree_t q = tree_new(T_PARAM);
            tree_set_subkind(q, P_POS);
            tree_set_loc(q, tree_loc(p));
            tree_set_value(q, tree_value(p));

            tree_add_param(new, q);
            found = true;
         }
      }
      assert(found);
   }

   return new;
}

static tree_t simp_fcall(tree_t t)
{
   return eval(simp_call_args(t));
}

static tree_t simp_pcall(tree_t t)
{
   return simp_call_args(t);
}

static tree_t simp_ref(tree_t t)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (type_is_array(tree_type(decl)))
         return t;
      else
         return tree_value(decl);

   case T_UNIT_DECL:
   case T_ALIAS:
      return tree_value(decl);

   default:
      return t;
   }
}

static tree_t simp_attr_ref(tree_t t)
{
   if (tree_has_value(t))
      return tree_value(t);
   else {
      tree_t decl = tree_ref(t);
      assert(tree_kind(decl) == T_FUNC_DECL);

      ident_t builtin = tree_attr_str(decl, ident_new("builtin"));
      assert(builtin != NULL);

      // Convert attributes like 'EVENT to function calls
      tree_t fcall = tree_new(T_FCALL);
      tree_set_loc(fcall, tree_loc(t));
      tree_set_type(fcall, tree_type(t));
      tree_set_ident(fcall, tree_ident(t));
      tree_set_ref(fcall, decl);

      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++)
         tree_add_param(fcall, tree_param(t, i));

      return simp_fcall(fcall);
   }
}

static tree_t simp_array_ref(tree_t t)
{
   tree_t value = tree_value(t);

   const int nparams = tree_params(t);

   int64_t indexes[nparams];
   bool can_fold = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);
      can_fold = can_fold && folded_int(tree_value(p), &indexes[i]);
   }

   if (!can_fold)
      return t;

   if (!tree_has_type(value))
      return t;

   type_t value_type = tree_type(value);

   // Check bounds at compile time
   if (type_kind(value_type) != T_UARRAY) {
      for (int i = 0; i < nparams; i++) {
         range_t b = type_dim(value_type, i);

         if ((b.kind != RANGE_TO) && (b.kind != RANGE_DOWNTO))
            continue;

         int64_t left, right;
         if (folded_int(b.left, &left) && folded_int(b.right, &right)) {
            const int64_t low  = (b.kind == RANGE_TO) ? left : right;
            const int64_t high = (b.kind == RANGE_TO) ? right : left;
            if (indexes[i] < low || indexes[i] > high)
               simp_error(t, "array index %"PRIi64" out of bounds "
                          "%"PRIi64" %s %"PRIi64, indexes[0], left,
                          (b.kind == RANGE_TO) ? "to" : "downto", right);
         }
      }
   }

   if (tree_kind(value) != T_REF)
      return t;   // Cannot fold nested array references

   tree_t decl = tree_ref(value);

   if (nparams > 1)
      return t;  // Cannot constant fold multi-dimensional arrays

   assert(nparams == 1);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      {
         tree_t v = tree_value(decl);
         if (tree_kind(v) != T_AGGREGATE)
            return t;

         range_t bounds = type_dim(tree_type(decl), 0);
         int64_t low, high;
         range_bounds(bounds, &low, &high);

         const bool to = (bounds.kind == RANGE_TO);
         const int64_t index = indexes[0];

         const int nassocs = tree_assocs(v);
         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(v, i);
            switch (tree_subkind(a)) {
            case A_POS:
               {
                  const int pos = tree_pos(a);
                  if ((to && (pos + low == index))
                      || (!to && (high - pos == index)))
                     return tree_value(a);
               }
               break;

            case A_OTHERS:
               return tree_value(a);

            case A_RANGE:
               {
                  range_t r = tree_range(a);
                  const int64_t left  = assume_int(r.left);
                  const int64_t right = assume_int(r.right);

                  if ((to && (index >= left) && (index <= right))
                      || (!to && (index <= left) && (index >= right)))
                     return tree_value(a);
               }
               break;

            case A_NAMED:
               if (assume_int(tree_name(a)) == indexes[0])
                  return tree_value(a);
               break;
            }
         }

         assert(false);
      }
   default:
      return t;
   }
}

static tree_t simp_array_slice(tree_t t)
{
   tree_t value = tree_value(t);

   if (!tree_has_type(value))
      return t;

   type_t value_type = tree_type(value);

   // Check bounds at compile time
   if (type_kind(value_type) != T_UARRAY) {
      range_t b = type_dim(value_type, 0);
      range_t r = tree_range(t);

      if ((b.kind != RANGE_TO) && (b.kind != RANGE_DOWNTO))
         return t;
      else if ((r.kind != RANGE_TO) && (r.kind != RANGE_DOWNTO))
         return t;

      int64_t b_left, r_left;
      bool left_error = false;
      if (folded_int(b.left, &b_left) && folded_int(r.left, &r_left))
         left_error = ((b.kind == RANGE_TO) && (r_left < b_left))
            || ((b.kind == RANGE_DOWNTO) && (r_left > b_left));

      int64_t b_right, r_right;
      bool right_error = false;
      if (folded_int(b.right, &b_right) && folded_int(r.right, &r_right))
         right_error = ((b.kind == RANGE_TO) && (r_right > b_right))
            || ((b.kind == RANGE_DOWNTO) && (r_right < b_right));

      if (left_error || right_error)
         simp_error(t, "slice %s index %"PRIi64" out of bounds "
                    "%"PRIi64" %s %"PRIi64, left_error ? "left" : "right",
                    left_error ? r_left : r_right, b_left,
                    (b.kind == RANGE_TO) ? "to" : "downto", b_right);
   }

   return t;
}

static tree_t simp_process(tree_t t)
{
   // Replace sensitivity list with a "wait on" statement
   if (tree_triggers(t) > 0) {
      tree_t p = tree_new(T_PROCESS);
      tree_set_ident(p, tree_ident(t));
      tree_set_loc(p, tree_loc(t));

      for (unsigned i = 0; i < tree_decls(t); i++)
         tree_add_decl(p, tree_decl(t, i));

      for (unsigned i = 0; i < tree_stmts(t); i++)
         tree_add_stmt(p, tree_stmt(t, i));

      tree_t w = tree_new(T_WAIT);
      tree_set_ident(w, tree_ident(p));
      for (unsigned i = 0; i < tree_triggers(t); i++)
         tree_add_trigger(w, tree_trigger(t, i));
      tree_add_stmt(p, w);

      return p;
   }
   else
      return t;
}

static tree_t simp_wait(tree_t t)
{
   // LRM 93 section 8.1
   // If there is no sensitivity list supplied generate one from the
   // condition clause

   if (tree_has_value(t) && (tree_triggers(t) == 0))
      tree_visit_only(tree_value(t), simp_build_wait, t, T_REF);

   // Rewrite until clause as a loop
   if (tree_has_value(t)) {
      tree_t until = tree_value(t);
      tree_set_value(t, NULL);

      ident_t label = ident_uniq("until_loop");

      tree_t exit = tree_new(T_EXIT);
      tree_set_ident(exit, ident_uniq("until_exit"));
      tree_set_value(exit, until);
      tree_set_ident2(exit, label);

      tree_t loop = tree_new(T_WHILE);
      tree_add_stmt(loop, t);
      tree_add_stmt(loop, exit);
      tree_set_ident(loop, label);

      return loop;
   }
   else
      return t;
}

static tree_t simp_if(tree_t t)
{
   bool value_b;
   if (folded_bool(tree_value(t), &value_b)) {
      if (value_b) {
         // If statement always executes so replace with then part
         if (tree_stmts(t) == 1)
            return tree_stmt(t, 0);
         else {
            tree_t b = tree_new(T_BLOCK);
            tree_set_ident(b, tree_ident(t));
            for (unsigned i = 0; i < tree_stmts(t); i++)
               tree_add_stmt(b, tree_stmt(t, i));
            return b;
         }
      }
      else {
         // If statement never executes so replace with else part
         if (tree_else_stmts(t) == 1)
            return tree_else_stmt(t, 0);
         else if (tree_else_stmts(t) == 0)
            return NULL;   // Delete it
         else {
            tree_t b = tree_new(T_BLOCK);
            tree_set_ident(b, tree_ident(t));
            for (unsigned i = 0; i < tree_else_stmts(t); i++)
               tree_add_stmt(b, tree_else_stmt(t, i));
            return b;
         }
      }
   }
   else
      return t;
}

static tree_t simp_while(tree_t t)
{
   bool value_b;
   if (!tree_has_value(t))
      return t;
   else if (folded_bool(tree_value(t), &value_b) && !value_b) {
      // Condition is false so loop never executes
      return NULL;
   }
   else
      return t;
}

static tree_t simp_for(tree_t t)
{
   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(t));

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      tree_add_decl(b, tree_decl(t, i));

   tree_t decl = tree_decl(t, 0);

   tree_t var = tree_new(T_REF);
   tree_set_ident(var, tree_ident(decl));
   tree_set_type(var, tree_type(decl));
   tree_set_ref(var, decl);

   range_t r = tree_range(t);

   tree_t test = NULL;
   switch (r.kind) {
   case RANGE_TO:
      test = call_builtin("leq", NULL, r.left, r.right, NULL);
      break;
   case RANGE_DOWNTO:
      test = call_builtin("geq", NULL, r.left, r.right, NULL);
      break;
   case RANGE_DYN:
   case RANGE_RDYN:
      break;
   default:
      assert(false);
   }

   tree_t container = b;
   if (test != NULL) {
      container = tree_new(T_IF);
      tree_set_ident(container, ident_uniq("null_check"));
      tree_set_value(container, test);

      tree_add_stmt(b, container);
   }

   tree_t init = tree_new(T_VAR_ASSIGN);
   tree_set_ident(init, ident_uniq("init"));
   tree_set_target(init, var);
   tree_set_value(init, (r.kind == RANGE_RDYN) ? r.right : r.left);

   ident_t label = tree_ident(t);
   tree_t wh = tree_new(T_WHILE);
   tree_set_ident(wh, label);
   tree_set_loc(wh, tree_loc(t));

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      tree_add_stmt(wh, tree_stmt(t, i));

   tree_t cmp = call_builtin("eq", NULL, var,
                             (r.kind == RANGE_RDYN ? r.left : r.right), NULL);

   tree_t exit = tree_new(T_EXIT);
   tree_set_ident(exit, ident_uniq("for_exit"));
   tree_set_value(exit, cmp);
   tree_set_ident2(exit, label);

   tree_t next;
   if ((r.kind == RANGE_DYN) || (r.kind == RANGE_RDYN)) {
      assert(tree_kind(r.left) == T_FCALL);
      tree_t p = tree_param(r.left, 0);

      tree_t asc = call_builtin("uarray_asc", NULL, tree_value(p), NULL);
      next = tree_new(T_IF);
      tree_set_value(next, asc);
      tree_set_ident(next, ident_uniq("for_next"));

      tree_t succ = call_builtin((r.kind == RANGE_DYN) ? "succ" : "pred",
                                 tree_type(decl), var, NULL);

      tree_t a1 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(a1, ident_uniq("for_next_asc"));
      tree_set_target(a1, var);
      tree_set_value(a1, succ);

      tree_t pred = call_builtin((r.kind == RANGE_DYN) ? "pred" : "succ",
                                 tree_type(decl), var, NULL);

      tree_t a2 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(a2, ident_uniq("for_next_dsc"));
      tree_set_target(a2, var);
      tree_set_value(a2, pred);

      tree_add_stmt(next, a1);
      tree_add_else_stmt(next, a2);
   }
   else {
      tree_t call;
      switch (r.kind) {
      case RANGE_TO:
         call = call_builtin("succ", tree_type(decl), var, NULL);
         break;
      case RANGE_DOWNTO:
         call = call_builtin("pred", tree_type(decl), var, NULL);
         break;
      default:
         assert(false);
      }

      next = tree_new(T_VAR_ASSIGN);
      tree_set_ident(next, ident_uniq("for_next"));
      tree_set_target(next, var);
      tree_set_value(next, call);
   }

   tree_add_stmt(wh, exit);
   tree_add_stmt(wh, next);

   tree_add_stmt(container, init);
   tree_add_stmt(container, wh);

   return b;
}

static void simp_build_wait(tree_t ref, void *context)
{
   // Add each signal referenced in an expression to the trigger
   // list for a wait statement

   tree_t wait = context;

   tree_t decl = tree_ref(ref);
   tree_kind_t kind = tree_kind(decl);
   if (kind == T_SIGNAL_DECL || kind == T_PORT_DECL) {
      // Check for duplicates
      for (unsigned i = 0; i < tree_triggers(wait); i++) {
         if (tree_ref(tree_trigger(wait, i)) == decl)
            return;
      }

      tree_add_trigger(wait, ref);
   }
}

static tree_t simp_cassign(tree_t t)
{
   // Replace concurrent assignments with a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));

   tree_t w = tree_new(T_WAIT);
   tree_set_ident(w, ident_new("cassign"));

   tree_t container = p;  // Where to add new statements
   void (*add_stmt)(tree_t, tree_t) = tree_add_stmt;

   for (unsigned i = 0; i < tree_conds(t); i++) {
      tree_t c = tree_cond(t, i);

      if (tree_has_value(c)) {
         // Replace this with an if statement
         tree_t i = tree_new(T_IF);
         tree_set_value(i, tree_value(c));
         tree_set_ident(i, ident_uniq("cond"));

         tree_visit_only(tree_value(c), simp_build_wait, w, T_REF);

         (*add_stmt)(container, i);

         container = i;
         add_stmt  = tree_add_stmt;
      }

      tree_t s = tree_new(T_SIGNAL_ASSIGN);
      tree_set_loc(s, tree_loc(t));
      tree_set_target(s, tree_target(t));
      tree_set_ident(s, tree_ident(t));
      tree_set_reject(s, tree_reject(c));

      for (unsigned i = 0; i < tree_waveforms(c); i++) {
         tree_t wave = tree_waveform(c, i);
         tree_add_waveform(s, wave);
         tree_visit_only(wave, simp_build_wait, w, T_REF);
      }

      (*add_stmt)(container, s);

      if (tree_has_value(c)) {
         // Add subsequent statements to the else part
         add_stmt = tree_add_else_stmt;
      }
   }

   tree_add_stmt(p, w);
   return p;
}

static bool simp_check_bounds(tree_t i, range_kind_t kind, const char *what,
                              int64_t low, int64_t high)
{
   int64_t folded;
   if (folded_int(i, &folded)) {
      if (folded < low || folded > high)
         simp_error_bool(i, "%s index %"PRIi64" out of bounds %"PRIi64
                         " %s %"PRIi64, what, folded,
                         (kind == RANGE_TO) ? low : high,
                         (kind == RANGE_TO) ? "to" : "downto",
                         (kind == RANGE_TO) ? high : low);
   }
   return true;
}

static tree_t simp_aggregate(tree_t t)
{
   type_t type = tree_type(t);
   if (!type_is_array(type))
      return t;

   assert(type_kind(type) != T_UARRAY);

   // Find the tightest bounds for the index

   int64_t low  = -INT64_MAX;
   int64_t high = INT64_MAX;

   range_t r = type_dim(type, 0);
   if ((tree_kind(r.left) == T_LITERAL) && (tree_kind(r.right) == T_LITERAL))
      range_bounds(r, &low, &high);

   if (type_kind(type) == T_SUBTYPE) {
      type_t base = type_base(type);

      type_t index = (type_kind(base) == T_UARRAY)
         ? type_index_constr(base, 0) : base;
      range_t base_r = type_dim(index, 0);

      if ((tree_kind(base_r.left) == T_LITERAL)
          && (tree_kind(base_r.right) == T_LITERAL)) {
         int64_t base_low, base_high;
         range_bounds(base_r, &base_low, &base_high);

         low  = MAX(low, base_low);
         high = MIN(high, base_high);
      }
   }

   if ((low == -INT64_MAX) && (high == INT64_MAX))
      return t;

   // Check for out of bounds indexes

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      switch (tree_subkind(a)) {
      case A_NAMED:
         if (!simp_check_bounds(tree_name(a), r.kind, "aggregate", low, high))
            return t;
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a);
            if (!simp_check_bounds(r.left, r.kind, "aggregate", low, high)
                || !simp_check_bounds(r.right, r.kind, "aggregate", low, high))
               return t;
         }
         break;

      default:
         break;
      }
   }

   return t;
}

static tree_t simp_select(tree_t t)
{
   // Replace a select statement with a case inside a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));

   tree_t w = tree_new(T_WAIT);
   tree_set_ident(w, ident_new("select_wait"));

   tree_t c = tree_new(T_CASE);
   tree_set_ident(c, ident_new("select_case"));
   tree_set_loc(c, tree_loc(t));
   tree_set_value(c, tree_value(t));

   tree_visit_only(tree_value(t), simp_build_wait, w, T_REF);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_add_assoc(c, a);

      if (tree_subkind(a) == A_NAMED)
         tree_visit_only(tree_name(a), simp_build_wait, w, T_REF);

      tree_t value = tree_value(a);

      const int nwaveforms = tree_waveforms(value);
      for (int j = 0; j < nwaveforms; j++)
         tree_visit_only(tree_waveform(value, j), simp_build_wait, w, T_REF);
   }

   tree_add_stmt(p, c);
   tree_add_stmt(p, w);
   return p;
}

static tree_t simp_cpcall(tree_t t)
{
   t = simp_call_args(t);

   tree_t process = tree_new(T_PROCESS);
   tree_set_ident(process, tree_ident(t));
   tree_set_loc(process, tree_loc(t));

   tree_t wait = tree_new(T_WAIT);
   tree_set_ident(wait, ident_new("pcall_wait"));

   tree_t pcall = tree_new(T_PCALL);
   tree_set_ident(pcall, ident_new("pcall"));
   tree_set_ident2(pcall, tree_ident2(t));
   tree_set_loc(pcall, tree_loc(t));
   tree_set_ref(pcall, tree_ref(t));

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);

      // Only add IN and INOUT parameters to sensitivity list
      tree_t port = tree_port(tree_ref(t), i);
      port_mode_t mode = tree_subkind(port);
      const bool sensitive =
         (tree_class(port) == C_SIGNAL)
         && ((mode == PORT_IN) || (mode == PORT_INOUT));
      if (sensitive)
         tree_visit_only(tree_value(p), simp_build_wait, wait, T_REF);

      tree_add_param(pcall, p);
   }

   tree_add_stmt(process, pcall);
   tree_add_stmt(process, wait);

   return process;
}

static tree_t simp_cassert(tree_t t)
{
   tree_t process = tree_new(T_PROCESS);
   tree_set_ident(process, tree_ident(t));
   tree_set_loc(process, tree_loc(t));

   tree_t wait = tree_new(T_WAIT);
   tree_set_ident(wait, ident_new("assert_wait"));

   tree_t a = tree_new(T_ASSERT);
   tree_set_ident(a, ident_new("assert_wrap"));
   tree_set_loc(a, tree_loc(t));
   tree_set_value(a, tree_value(t));
   tree_set_severity(a, tree_severity(t));
   tree_set_message(a, tree_message(t));

   tree_visit_only(tree_value(t), simp_build_wait, wait, T_REF);

   tree_add_stmt(process, a);
   tree_add_stmt(process, wait);

   return process;
}

static tree_t simp_qualified(tree_t t)
{
   // Not needed by the code generator
   return tree_value(t);
}

static tree_t simp_type_conv(tree_t t)
{
   // Simple conversions performed at compile time

   tree_t value = tree_value(tree_param(t, 0));

   type_t from = tree_type(value);
   type_t to   = tree_type(t);

   type_kind_t from_k = type_kind(from);
   type_kind_t to_k   = type_kind(to);

   if ((from_k == T_INTEGER) && (to_k == T_REAL)) {
      int64_t l;
      if (folded_int(value, &l))
         return get_real_lit(t, (double)l);
   }
   else if ((from_k == T_REAL) && (to_k == T_INTEGER)) {
      double l;
      if (folded_real(value, &l))
         return get_int_lit(t, (int)l);
   }

   return t;
}

static tree_t simp_if_generate(tree_t t)
{
   bool value_b;
   if (folded_bool(tree_value(t), &value_b)) {
      if (value_b) {
         tree_t b = tree_new(T_BLOCK);
         tree_set_ident(b, tree_ident(t));

         const int ndecls = tree_decls(t);
         for (int i = 0; i < ndecls; i++)
            tree_add_decl(b, tree_decl(t, i));

         const int nstmts = tree_stmts(t);
         for (int i = 0; i < nstmts; i++)
            tree_add_stmt(b, tree_stmt(t, i));

         return b;
      }
      else
         return NULL;
   }

   return t;
}

static tree_t simp_decl(tree_t t)
{
   type_t type = tree_type(t);

   if (type_is_array(type) && (type_kind(type) != T_UARRAY)) {
      // Check folded range does not violate index constraints

      const int ndims = type_dims(type);
      for (int i = 0; i < ndims; i++) {
         range_t dim = type_dim(type, i);

         type_t cons = tree_type(dim.left);

         if (type_kind(cons) == T_ENUM)
            continue;    // TODO: checking for enum constraints

         range_t bounds = type_dim(cons, 0);

         int64_t dim_left, bounds_left;
         if (folded_int(dim.left, &dim_left)
             && folded_int(bounds.left, &bounds_left)) {
            if (dim_left < bounds_left)
               simp_error(dim.left, "left index %"PRIi64" violates "
                          "constraint %s", dim_left, type_pp(cons));
         }

         int64_t dim_right, bounds_right;
         if (folded_int(dim.right, &dim_right)
             && folded_int(bounds.right, &bounds_right)) {
            if (dim_right > bounds_right)
               simp_error(dim.right, "right index %"PRIi64" violates "
                          "constraint %s", dim_right, type_pp(cons));
         }
      }

   }

   return t;
}

static tree_t simp_tree(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_PROCESS:
      return simp_process(t);
   case T_ARRAY_REF:
      return simp_array_ref(t);
   case T_ARRAY_SLICE:
      return simp_array_slice(t);
   case T_ATTR_REF:
      return simp_attr_ref(t);
   case T_FCALL:
      return simp_fcall(t);
   case T_PCALL:
      return simp_pcall(t);
   case T_REF:
      return simp_ref(t);
   case T_IF:
      return simp_if(t);
   case T_WHILE:
      return simp_while(t);
   case T_FOR:
      return simp_for(t);
   case T_CASSIGN:
      return simp_cassign(t);
   case T_AGGREGATE:
      return simp_aggregate(t);
   case T_SELECT:
      return simp_select(t);
   case T_WAIT:
      return simp_wait(t);
   case T_NULL:
      return NULL;   // Delete it
   case T_CPCALL:
      return simp_cpcall(t);
   case T_CASSERT:
      return simp_cassert(t);
   case T_QUALIFIED:
      return simp_qualified(t);
   case T_TYPE_CONV:
      return simp_type_conv(t);
   case T_IF_GENERATE:
      return simp_if_generate(t);
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_VAR_DECL:
      return simp_decl(t);
   default:
      return t;
   }
}

void simplify(tree_t top)
{
   tree_rewrite(top, simp_tree, NULL);
}

int simplify_errors(void)
{
   return errors;
}
