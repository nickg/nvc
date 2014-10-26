//
//  Copyright (C) 2014  Nick Gasson
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
#include "phase.h"
#include "vcode.h"
#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>

static ident_t builtin_i;
static ident_t foreign_i;
static ident_t vcode_var_i;
static bool    verbose = false;

static vcode_reg_t lower_expr(tree_t expr);
static vcode_type_t lower_bounds(type_t type);

static vcode_type_t lower_type(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type)) {
         const int ndims = type_dims(type);
         assert(ndims > 0);
         vcode_type_t bounds[ndims];
         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);
            int64_t low, high;
            range_bounds(r, &low, &high);
            bounds[i] = vtype_int(low, high);
         }

         type_t elem = type_elem(type);
         return vtype_carray(bounds, ndims, lower_type(elem),
                             lower_bounds(elem));
      }
      else
         return lower_type(type_base(type));

   case T_PHYSICAL:
   case T_INTEGER:
      {
         range_t r = type_dim(type, 0);
         int64_t low, high;
         range_bounds(r, &low, &high);
         return vtype_int(low, high);
      }

   case T_ENUM:
      return vtype_int(0, type_enum_literals(type) - 1);

   default:
      fatal("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }
}

static vcode_type_t lower_bounds(type_t type)
{
   if (type_is_scalar(type) && type_kind(type) == T_SUBTYPE) {
      range_t r = type_dim(type, 0);
      int64_t low, high;
      range_bounds(r, &low, &high);
      return vtype_int(low, high);
   }
   else
      return lower_type(type);
}

static vcode_reg_t lower_func_arg(tree_t fcall, int nth)
{
   assert(nth < tree_params(fcall));

   tree_t param = tree_param(fcall, nth);

   assert(tree_subkind(param) == P_POS);
   assert(tree_pos(param) == nth);

   return lower_expr(tree_value(param));
}

static vcode_reg_t lower_builtin(tree_t fcall, ident_t builtin)
{
   if (icmp(builtin, "eq"))
      return emit_cmp(VCODE_CMP_EQ,
                      lower_func_arg(fcall, 0),
                      lower_func_arg(fcall, 1));
   else if (icmp(builtin, "mul"))
      return emit_mul(lower_func_arg(fcall, 0), lower_func_arg(fcall, 1));
   else if (icmp(builtin, "add"))
      return emit_add(lower_func_arg(fcall, 0), lower_func_arg(fcall, 1));
   else
      fatal_at(tree_loc(fcall), "cannot lower builtin %s", istr(builtin));
}

static vcode_reg_t lower_fcall(tree_t fcall)
{
   tree_t decl = tree_ref(fcall);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin != NULL)
      return lower_builtin(fcall, builtin);

   tree_t foreign = tree_attr_tree(decl, foreign_i);
   ident_t name = tree_ident(decl);
   if (foreign != NULL) {
      if (tree_kind(foreign) != T_LITERAL)
         fatal_at(tree_loc(decl), "foreign attribute must have string "
                  "literal value");

      const int nchars = tree_chars(foreign);
      char buf[nchars + 1];
      for (int i = 0; i < nchars; i++)
         buf[i] = tree_pos(tree_ref(tree_char(foreign, i)));
      buf[nchars] = '\0';

      name = ident_new(buf);
   }

   const int nargs = tree_params(fcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = lower_func_arg(fcall, i);

   return emit_fcall(name, lower_type(tree_type(fcall)), args, nargs);
}

static vcode_reg_t lower_literal(tree_t lit)
{
   switch (tree_subkind(lit)) {
   case L_INT:
      return emit_const(lower_type(tree_type(lit)), tree_ival(lit));

   default:
      fatal_at(tree_loc(lit), "cannot lower literal kind %d",
               tree_subkind(lit));
   }
}

static vcode_var_t lower_get_var(tree_t decl)
{
   vcode_var_t var = tree_attr_int(decl, vcode_var_i, VCODE_INVALID_VAR);
   if (var == VCODE_INVALID_VAR) {
      vcode_dump();
      fatal_trace("missing vcode var for %s", istr(tree_ident(decl)));
   }

   return var;
}

static vcode_reg_t lower_ref(tree_t ref)
{
   tree_t decl = tree_ref(ref);
   return emit_load(lower_get_var(decl));
}

static vcode_reg_t lower_aggregate(tree_t expr)
{
   const int64_t values[] = { 1, 2, 3, 4 };
   return emit_const_array(lower_type(tree_type(expr)), values, ARRAY_LEN(values));
}

static vcode_reg_t lower_expr(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
      return lower_fcall(expr);
   case T_LITERAL:
      return lower_literal(expr);
   case T_REF:
      return lower_ref(expr);
   case T_AGGREGATE:
      return lower_aggregate(expr);
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static void lower_assert(tree_t stmt)
{
   emit_assert(lower_expr(tree_value(stmt)));
}

static void lower_wait(tree_t wait)
{
   vcode_reg_t delay = VCODE_INVALID_REG;
   if (tree_has_delay(wait))
      delay = lower_expr(tree_delay(wait));

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);
}

static void lower_var_assign(tree_t stmt)
{
   vcode_reg_t value = lower_expr(tree_value(stmt));

   tree_t target = tree_target(stmt);
   assert(tree_kind(target) == T_REF);

   tree_t decl = tree_ref(target);
   assert(type_is_scalar(tree_type(decl)));

   emit_bounds(value, lower_bounds(tree_type(decl)));
   emit_store(value, lower_get_var(decl));
}

static void lower_stmt(tree_t stmt)
{
   switch (tree_kind(stmt)) {
   case T_ASSERT:
      lower_assert(stmt);
      break;
   case T_WAIT:
      lower_wait(stmt);
      break;
   case T_VAR_ASSIGN:
      lower_var_assign(stmt);
      break;
   default:
      fatal_at(tree_loc(stmt), "cannot lower statement kind %s",
               tree_kind_str(tree_kind(stmt)));
   }
}

static void lower_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t vtype = lower_type(type);
         vcode_type_t vbounds = lower_bounds(type);
         vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl));
         tree_add_attr_int(decl, vcode_var_i, var);

         if (tree_has_value(decl)) {
            vcode_reg_t value = lower_expr(tree_value(decl));
            emit_bounds(value, vbounds);
            emit_store(value, var);
         }
      }
      break;
   default:
      fatal_at(tree_loc(decl), "cannot lower decl kind %s",
               tree_kind_str(tree_kind(decl)));
   }
}

static void lower_process(tree_t proc)
{
   vcode_unit_t vu = emit_process(tree_ident(proc));

   const int ndecls = tree_decls(proc);
   for (int i = 0; i < ndecls; i++)
      lower_decl(tree_decl(proc, i));

   vcode_block_t start_bb = emit_block();
   emit_jump(start_bb);
   vcode_select_block(start_bb);

   const int nstmts = tree_stmts(proc);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(proc, i));

   if (!vcode_block_finished())
      emit_jump(start_bb);

   vcode_opt();

   if (verbose)
      vcode_dump();

   assert(!tree_has_code(proc));
   tree_set_code(proc, vu);

   for (int i = 0; i < ndecls; i++)
      tree_remove_attr(tree_decl(proc, i), vcode_var_i);
}

static void lower_elab(tree_t unit)
{
   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      assert(tree_kind(s) == T_PROCESS);
      lower_process(s);
   }
}

void lower_unit(tree_t unit)
{
   builtin_i   = ident_new("builtin");
   foreign_i   = ident_new("FOREIGN");
   vcode_var_i = ident_new("vcode_var");

   verbose = (getenv("NVC_LOWER_VERBOSE") != NULL);

   switch (tree_kind(unit)) {
   case T_ELAB:
      lower_elab(unit);
      break;
   default:
      fatal("cannot lower to level unit kind %s to vcode",
            tree_kind_str(tree_kind(unit)));
   }

   vcode_close();
}
