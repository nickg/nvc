//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "hash.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "vcode.h"

#include <stdlib.h>
#include <assert.h>
#include <float.h>

typedef struct {
   mir_block_t *blocks;
   mir_value_t *map;
   mir_value_t *vars;
   ihash_t     *types;
} mir_import_t;

static mir_type_t import_type(mir_unit_t *mu, mir_import_t *imp,
                              vcode_type_t vtype)
{
   void *ptr = ihash_get(imp->types, vtype);
   if (ptr != NULL)
      return (mir_type_t){ .bits = (uintptr_t)ptr };

   mir_type_t type;
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
      type = mir_int_type(mu, vtype_low(vtype), vtype_high(vtype));
      break;
   case VCODE_TYPE_REAL:
      type = mir_real_type(mu, -DBL_MAX, DBL_MAX);
      break;
   case VCODE_TYPE_OFFSET:
      type = mir_offset_type(mu);
      break;
   case VCODE_TYPE_SIGNAL:
      type = mir_signal_type(mu, import_type(mu, imp, vtype_base(vtype)));
      break;
   case VCODE_TYPE_UARRAY:
      type = mir_uarray_type(mu, vtype_dims(vtype),
                             import_type(mu, imp, vtype_elem(vtype)));
      break;
   case VCODE_TYPE_CARRAY:
      type = mir_carray_type(mu, vtype_size(vtype),
                             import_type(mu, imp, vtype_elem(vtype)));
      break;
   case VCODE_TYPE_CONTEXT:
      type = mir_context_type(mu, vtype_name(vtype));
      break;
   case VCODE_TYPE_ACCESS:
      type = mir_access_type(mu, import_type(mu, imp, vtype_pointed(vtype)));
      break;
   case VCODE_TYPE_OPAQUE:
      type = mir_opaque_type(mu);
      break;
   case VCODE_TYPE_CLOSURE:
      {
         mir_type_t atype = mir_opaque_type(mu);
         mir_type_t rtype = import_type(mu, imp, vtype_base(vtype));
         type = mir_closure_type(mu, atype, rtype);
      }
      break;
   case VCODE_TYPE_RECORD:
      {
         const int nfields = vtype_fields(vtype);
         mir_type_t *fields LOCAL = xmalloc_array(nfields, sizeof(mir_type_t));
         for (int i = 0; i < nfields; i++)
            fields[i] = import_type(mu, imp, vtype_field(vtype, i));

         type = mir_record_type(mu, vtype_name(vtype), fields, nfields);
      }
      break;
   case VCODE_TYPE_POINTER:
      type = mir_pointer_type(mu, import_type(mu, imp, vtype_pointed(vtype)));
      break;
   case VCODE_TYPE_FILE:
      type = mir_file_type(mu, import_type(mu, imp, vtype_base(vtype)));
      break;
   case VCODE_TYPE_CONVERSION:
      type = mir_conversion_type(mu);
      break;
   case VCODE_TYPE_TRIGGER:
      type = mir_trigger_type(mu);
      break;
   case VCODE_TYPE_DEBUG_LOCUS:
      type = mir_locus_type(mu);
      break;
   case VCODE_TYPE_RESOLUTION:
      type = mir_resolution_type(mu, import_type(mu, imp, vtype_base(vtype)));
      break;
   default:
      vcode_dump();
      fatal_trace("cannot import type kind %d", vtype_kind(vtype));
   }

   ihash_put(imp->types, vtype, (void *)(uintptr_t)type.bits);
   return type;
}

static void import_package_init(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t context = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 0)
      context = imp->map[vcode_get_arg(op, 0)];

   ident_t name = vcode_get_func(op);
   imp->map[vcode_get_result(op)] = mir_build_package_init(mu, name, context);
}

static void import_link_package(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t context = mir_build_link_package(mu, vcode_get_ident(op));
   imp->map[vcode_get_result(op)] = context;
}

static void import_link_var(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));

   mir_value_t context = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vtype);
   ident_t name = vcode_get_ident(op);

   imp->map[result] = mir_build_link_var(mu, context, name, type);
}

static void import_return(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t result = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 0)
      result = imp->map[vcode_get_arg(op, 0)];

   mir_build_return(mu, result);
}

static void import_fcall(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int nargs = vcode_count_args(op);
   mir_value_t *args LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      args[i] = imp->map[vcode_get_arg(op, i)];

   ident_t func = vcode_get_func(op);
   vcode_reg_t result = vcode_get_result(op);
   if (result == VCODE_INVALID_REG)
      mir_build_fcall(mu, func, MIR_NULL_TYPE, MIR_NULL_STAMP, args, nargs);
   else {
      mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
      imp->map[result] = mir_build_fcall(mu, func, type, MIR_NULL_STAMP,
                                         args, nargs);
   }
}

static void import_pcall(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int nargs = vcode_count_args(op);
   mir_value_t *args LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      args[i] = imp->map[vcode_get_arg(op, i)];

   ident_t func = vcode_get_func(op);
   mir_block_t resume = imp->blocks[vcode_get_target(op, 0)];

   mir_build_pcall(mu, func, resume, args, nargs);
}

static void import_resume(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_build_resume(mu, vcode_get_func(op));
}

static void import_const(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_const(mu, type, vcode_get_value(op));
}

static void import_const_real(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_const_real(mu, type, vcode_get_real(op));
}

static void import_const_array(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   const int nargs = vcode_count_args(op);
   mir_value_t *elts LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      elts[i] = imp->map[vcode_get_arg(op, i)];

   imp->map[result] = mir_const_array(mu, type, elts, nargs);
}

static void import_const_rep(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   unsigned count = vcode_get_value(op);

   imp->map[result] = mir_build_const_rep(mu, type, value, count);
}

static void import_const_record(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   const int nargs = vcode_count_args(op);
   mir_value_t *elts LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      elts[i] = imp->map[vcode_get_arg(op, i)];

   imp->map[result] = mir_const_record(mu, type, elts, nargs);
}

static void import_cmp(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const mir_cmp_t cmp = (mir_cmp_t)vcode_get_cmp(op);
   mir_value_t left = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_cmp(mu, cmp, left, right);
}

static void import_debug_locus(mir_unit_t *mu, mir_import_t *imp, int op)
{
   object_t *obj = vcode_get_object(op);
   imp->map[vcode_get_result(op)] = mir_build_locus(mu, obj);
}

static void import_assert(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value    = imp->map[vcode_get_arg(op, 0)];
   mir_value_t severity = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus    = imp->map[vcode_get_arg(op, 4)];

   // TODO: returning VCODE_INVALID_REG sucks - why not have optional args?
   mir_value_t msg = MIR_NULL_VALUE, length = MIR_NULL_VALUE;
   if (vcode_get_arg(op, 2) != VCODE_INVALID_REG) {
      msg    = imp->map[vcode_get_arg(op, 2)];
      length = imp->map[vcode_get_arg(op, 3)];
   }

   mir_value_t hint_left = MIR_NULL_VALUE, hint_right = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 5) {
      hint_left  = imp->map[vcode_get_arg(op, 5)];
      hint_right = imp->map[vcode_get_arg(op, 6)];
   }

   mir_build_assert(mu, value, msg, length, severity, locus,
                    hint_left, hint_right);
}

static void import_report(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t severity = imp->map[vcode_get_arg(op, 0)];
   mir_value_t message  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t length   = imp->map[vcode_get_arg(op, 2)];
   mir_value_t locus    = imp->map[vcode_get_arg(op, 3)];

   mir_build_report(mu, message, length, severity, locus);
}

static void import_wait(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_block_t btarget = imp->blocks[vcode_get_target(op, 0)];
   mir_build_wait(mu, btarget);
}

static void import_jump(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_block_t btarget = imp->blocks[vcode_get_target(op, 0)];
   mir_build_jump(mu, btarget);
}

static void import_cond(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t test   = imp->map[vcode_get_arg(op, 0)];
   mir_block_t btrue  = imp->blocks[vcode_get_target(op, 0)];
   mir_block_t bfalse = imp->blocks[vcode_get_target(op, 1)];

   mir_build_cond(mu, test, btrue, bfalse);
}

static void import_case(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_block_t def   = imp->blocks[vcode_get_target(op, 0)];

   const int nargs = vcode_count_args(op);

   mir_value_t *cases LOCAL = xmalloc_array(nargs - 1, sizeof(mir_value_t));
   mir_block_t *blocks LOCAL = xmalloc_array(nargs - 1, sizeof(mir_block_t));

   for (int i = 1; i < nargs; i++) {
      cases[i - 1]  = imp->map[vcode_get_arg(op, i)];
      blocks[i - 1] = imp->blocks[vcode_get_target(op, i)];
   }

   mir_build_case(mu, value, def, cases, blocks, nargs - 1);
}

static void import_init_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t count = imp->map[vcode_get_arg(op, 0)];
   mir_value_t size  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t value = imp->map[vcode_get_arg(op, 2)];
   mir_value_t flags = imp->map[vcode_get_arg(op, 3)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 4)];

   mir_value_t offset = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 5)
      offset = imp->map[vcode_get_arg(op, 5)];

   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vtype_base(vcode_reg_type(result)));

   imp->map[result] = mir_build_init_signal(mu, type, count, size,
                                            value, flags, locus, offset);
}

static void import_implicit_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t count   = imp->map[vcode_get_arg(op, 0)];
   mir_value_t size    = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus   = imp->map[vcode_get_arg(op, 2)];
   mir_value_t kind    = imp->map[vcode_get_arg(op, 3)];
   mir_value_t closure = imp->map[vcode_get_arg(op, 4)];
   mir_value_t delay   = imp->map[vcode_get_arg(op, 5)];

   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vtype_base(vcode_reg_type(result)));

   imp->map[result] = mir_build_implicit_signal(mu, type, count, size, locus,
                                                kind, closure, delay);
}

static void import_drive_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];

   mir_build_drive_signal(mu, target, count);
}

static void import_sched_waveform(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t value  = imp->map[vcode_get_arg(op, 2)];
   mir_value_t reject = imp->map[vcode_get_arg(op, 3)];
   mir_value_t after  = imp->map[vcode_get_arg(op, 4)];

   mir_build_sched_waveform(mu, target, count, value, reject, after);
}

static void import_resolved(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t nets = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_resolved(mu, nets);
}

static void import_address_of(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t nets = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_address_of(mu, nets);
}

static void import_store(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t var = imp->vars[vcode_get_address(op)];
   mir_build_store(mu, var, imp->map[vcode_get_arg(op, 0)]);
}

static void import_store_indirect(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t var   = imp->map[vcode_get_arg(op, 1)];
   mir_build_store(mu, var, value);
}

static void import_load(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t var = imp->vars[vcode_get_address(op)];
   imp->map[vcode_get_result(op)] = mir_build_load(mu, var);
}

static void import_load_indirect(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t var = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_load(mu, var);
}

static void import_context_upref(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int hops = vcode_get_hops(op);
   imp->map[vcode_get_result(op)] = mir_build_context_upref(mu, hops);
}

static void import_var_upref(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int hops = vcode_get_hops(op);
   vcode_var_t var = vcode_get_address(op);
   imp->map[vcode_get_result(op)] = mir_build_var_upref(mu, hops, var);
}

static void import_wrap(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t data = imp->map[vcode_get_arg(op, 0)];

   const int nargs = vcode_count_args(op);
   assert((nargs - 1) % 3 == 0);

   const int ndims = (nargs - 1) / 3;
   mir_dim_t *dims LOCAL = xmalloc_array(ndims, sizeof(mir_dim_t));
   for (int i = 0, pos = 1; i < ndims; i++, pos += 3) {
      dims[i].left  = imp->map[vcode_get_arg(op, pos + 0)];
      dims[i].right = imp->map[vcode_get_arg(op, pos + 1)];
      dims[i].dir   = imp->map[vcode_get_arg(op, pos + 2)];
   }

   imp->map[vcode_get_result(op)] = mir_build_wrap(mu, data, dims, ndims);
}

static void import_unwrap(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_unwrap(mu, arg);
}

static void import_uarray_len(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   const int dim = vcode_get_dim(op);
   imp->map[vcode_get_result(op)] = mir_build_uarray_len(mu, arg, dim);
}

static void import_uarray_left(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   const int dim = vcode_get_dim(op);
   imp->map[vcode_get_result(op)] = mir_build_uarray_left(mu, arg, dim);
}

static void import_uarray_right(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   const int dim = vcode_get_dim(op);
   imp->map[vcode_get_result(op)] = mir_build_uarray_right(mu, arg, dim);
}

static void import_uarray_dir(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   const int dim = vcode_get_dim(op);
   imp->map[vcode_get_result(op)] = mir_build_uarray_dir(mu, arg, dim);
}

static void import_add(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_add(mu, type, left, right);
}

static void import_trap_add(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_trap_add(mu, type, left, right, locus);
}

static void import_sub(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_sub(mu, type, left, right);
}

static void import_trap_sub(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_trap_sub(mu, type, left, right, locus);
}

static void import_mul(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_mul(mu, type, left, right);
}

static void import_trap_mul(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_trap_mul(mu, type, left, right, locus);
}

static void import_div(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_div(mu, type, left, right);
}

static void import_mod(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_mod(mu, type, left, right);
}

static void import_rem(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_rem(mu, type, left, right);
}

static void import_exp(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_exp(mu, type, left, right);
}

static void import_trap_exp(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_trap_exp(mu, type, left, right, locus);
}

static void import_or(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_or(mu, left, right);
}

static void import_nor(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   mir_value_t or = mir_build_or(mu, left, right);
   imp->map[vcode_get_result(op)] = mir_build_not(mu, or);
}

static void import_xor(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_xor(mu, left, right);
}

static void import_xnor(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   mir_value_t xor = mir_build_xor(mu, left, right);
   imp->map[vcode_get_result(op)] = mir_build_not(mu, xor);
}

static void import_and(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_and(mu, left, right);
}

static void import_nand(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];

   mir_value_t and = mir_build_and(mu, left, right);
   imp->map[vcode_get_result(op)] = mir_build_not(mu, and);
}

static void import_event(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_event_flag(mu, signal, count);
}

static void import_active(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_active_flag(mu, signal, count);
}

static void import_driving(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_driving_flag(mu, signal, count);
}

static void import_not(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_not(mu, arg);
}

static void import_cast(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vcode_get_type(op));
   imp->map[vcode_get_result(op)] = mir_build_cast(mu, type, arg);
}

static void import_neg(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_build_neg(mu, type, arg);
}

static void import_trap_neg(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_build_trap_neg(mu, type, arg, locus);
}

static void import_abs(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_build_abs(mu, type, arg);
}

static void import_index(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t var = imp->vars[vcode_get_address(op)];
   imp->map[vcode_get_result(op)] = var;
}

static void import_copy(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t dst = imp->map[vcode_get_arg(op, 0)];
   mir_value_t src = imp->map[vcode_get_arg(op, 1)];

   mir_value_t count = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 2)
      count = imp->map[vcode_get_arg(op, 2)];

   mir_build_copy(mu, dst, src, count);
}

static void import_memset(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t dst   = imp->map[vcode_get_arg(op, 0)];
   mir_value_t value = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_set(mu, dst, value, count);
}

static void import_array_ref(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t array  = imp->map[vcode_get_arg(op, 0)];
   mir_value_t offset = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_array_ref(mu, array, offset);
}

static void import_table_ref(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int nargs = vcode_count_args(op);
   assert(nargs >= 2);

   mir_value_t *args LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      args[i] = imp->map[vcode_get_arg(op, i)];

   imp->map[vcode_get_result(op)] = mir_build_table_ref(mu, args[0], args[1],
                                                        args + 2, nargs - 2);
}

static void import_record_ref(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t record = imp->map[vcode_get_arg(op, 0)];
   const int field = vcode_get_field(op);
   imp->map[vcode_get_result(op)] = mir_build_record_ref(mu, record, field);
}

static void import_range_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t left  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t right = imp->map[vcode_get_arg(op, 2)];
   mir_value_t dir   = imp->map[vcode_get_arg(op, 3)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 4)];
   mir_value_t hint  = imp->map[vcode_get_arg(op, 5)];

   mir_build_range_check(mu, value, left, right, dir, locus, hint);
}

static void import_sched_event(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   mir_build_sched_event(mu, signal, count);
}

static void import_clear_event(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count  = imp->map[vcode_get_arg(op, 1)];
   mir_build_clear_event(mu, signal, count);
}

static void import_alloc(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t count  = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type    = import_type(mu, imp, vcode_get_type(op));
   vcode_reg_t result = vcode_get_result(op);
   imp->map[result] = mir_build_alloc(mu, type, MIR_NULL_STAMP, count);
}

static void import_range_length(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left   = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t dir    = imp->map[vcode_get_arg(op, 2)];
   vcode_reg_t result = vcode_get_result(op);
   imp->map[result] = mir_build_range_length(mu, left, right, dir);
}

static void import_range_null(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left   = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t dir    = imp->map[vcode_get_arg(op, 2)];
   vcode_reg_t result = vcode_get_result(op);
   imp->map[result] = mir_build_range_null(mu, left, right, dir);
}

static void import_null(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));
   imp->map[result] = mir_build_null(mu, type);
}

static void import_new(mir_unit_t *mu, mir_import_t *imp, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
   mir_type_t type = import_type(mu, imp, vtype);

   mir_value_t count = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 0)
      count = imp->map[vcode_get_arg(op, 0)];

   imp->map[result] = mir_build_new(mu, type, MIR_NULL_STAMP, count);
}

static void import_all(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_all(mu, arg);
}

static void import_deallocate(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t arg = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = mir_get_type(mu, arg);
   mir_build_store(mu, arg, mir_build_null(mu, mir_get_pointer(mu, type)));
}

static void import_null_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t ptr = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];
   mir_build_null_check(mu, ptr, locus);
}

static void import_zero_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];
   mir_build_zero_check(mu, value, locus);
}

static void import_length_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t llen = imp->map[vcode_get_arg(op, 0)];
   mir_value_t rlen = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];

   mir_value_t dim = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 3)
      dim = imp->map[vcode_get_arg(op, 3)];

   mir_build_length_check(mu, llen, rlen, locus, dim);
}

static void import_index_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t left  = imp->map[vcode_get_arg(op, 1)];
   mir_value_t right = imp->map[vcode_get_arg(op, 2)];
   mir_value_t dir   = imp->map[vcode_get_arg(op, 3)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 4)];
   mir_value_t hint  = imp->map[vcode_get_arg(op, 5)];

   mir_build_index_check(mu, value, left, right, dir, locus, hint);
}

static void import_dir_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t dir   = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];

   mir_build_dir_check(mu, value, dir, locus);
}

static void import_alias_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];

   mir_build_alias_signal(mu, signal, locus);
}

static void import_map_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t src = imp->map[vcode_get_arg(op, 0)];
   mir_value_t dst = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_map_signal(mu, src, dst, count);
}

static void import_map_const(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t src = imp->map[vcode_get_arg(op, 0)];
   mir_value_t dst = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_map_const(mu, src, dst, count);
}

static void import_map_implicit(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t src = imp->map[vcode_get_arg(op, 0)];
   mir_value_t dst = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_map_implicit(mu, src, dst, count);
}

static void import_bind_foreign(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t spec = imp->map[vcode_get_arg(op, 0)];
   mir_value_t length = imp->map[vcode_get_arg(op, 1)];

   mir_value_t locus = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 2)
      locus = imp->map[vcode_get_arg(op, 2)];

   mir_build_bind_foreign(mu, spec, length, locus);
}

static void import_bind_external(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t locus = imp->map[vcode_get_arg(op, 0)];
   ident_t scope = vcode_get_ident(op);

   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
   mir_type_t type = import_type(mu, imp, vtype);

   const int nargs = vcode_count_args(op) - 1;
   mir_value_t *args LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      args[i] = imp->map[vcode_get_arg(op, i + 1)];

   imp->map[result] = mir_build_bind_external(mu, locus, scope, type,
                                              MIR_NULL_STAMP, args, nargs);
}

static void import_unreachable(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t locus = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 0)
      locus = imp->map[vcode_get_arg(op, 0)];

   mir_build_unreachable(mu, locus);
}

static void import_select(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t test = imp->map[vcode_get_arg(op, 0)];
   mir_value_t tval = imp->map[vcode_get_arg(op, 1)];
   mir_value_t fval = imp->map[vcode_get_arg(op, 2)];

   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_select(mu, type, test, tval, fval);
}

static void import_closure(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t context = imp->map[vcode_get_arg(op, 0)];
   ident_t func = vcode_get_func(op);

   mir_type_t atype = MIR_NULL_TYPE;
   vcode_reg_t result = vcode_get_result(op);
   mir_type_t rtype = import_type(mu, imp, vtype_base(vcode_reg_type(result)));

   imp->map[result] = mir_build_closure(mu, func, context, atype, rtype);
}

static void import_resolution_wrapper(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t closure = imp->map[vcode_get_arg(op, 0)];
   mir_value_t nlits = imp->map[vcode_get_arg(op, 1)];

   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vtype_base(vcode_reg_type(result)));

   imp->map[result] = mir_build_resolution_wrapper(mu, type, closure, nlits);
}

static void import_resolve_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t resolution = imp->map[vcode_get_arg(op, 1)];
   mir_build_resolve_signal(mu, signal, resolution);
}

static void import_transfer_signal(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t source = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];
   mir_value_t reject = imp->map[vcode_get_arg(op, 3)];
   mir_value_t after = imp->map[vcode_get_arg(op, 4)];

   mir_build_transfer_signal(mu, target, source, count, reject, after);
}

static void import_file_open(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t file = imp->map[vcode_get_arg(op, 0)];
   mir_value_t name = imp->map[vcode_get_arg(op, 1)];
   mir_value_t length = imp->map[vcode_get_arg(op, 2)];
   mir_value_t kind = imp->map[vcode_get_arg(op, 3)];

   mir_value_t status = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 4)
      status = imp->map[vcode_get_arg(op, 4)];

   mir_build_file_open(mu, file, name, length, kind, status);
}

static void import_file_read(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t file = imp->map[vcode_get_arg(op, 0)];
   mir_value_t ptr = imp->map[vcode_get_arg(op, 1)];

   mir_value_t inlen = MIR_NULL_VALUE, outlen = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 2) {
      inlen = imp->map[vcode_get_arg(op, 2)];

      if (vcode_count_args(op) > 3)
         outlen = imp->map[vcode_get_arg(op, 3)];
   }

   mir_build_file_read(mu, file, ptr, inlen, outlen);
}

static void import_file_write(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t file = imp->map[vcode_get_arg(op, 0)];
   mir_value_t value = imp->map[vcode_get_arg(op, 1)];

   mir_value_t length = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 2)
      length = imp->map[vcode_get_arg(op, 2)];

   mir_build_file_write(mu, file, value, length);
}

static void import_port_conversion(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t driving = imp->map[vcode_get_arg(op, 0)];

   mir_value_t effective = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1)
      effective = imp->map[vcode_get_arg(op, 1)];

   vcode_reg_t result = vcode_get_result(op);
   imp->map[result] = mir_build_port_conversion(mu, driving, effective);
}

static void import_convert_in(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t conv = imp->map[vcode_get_arg(op, 0)];
   mir_value_t nets = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_convert_in(mu, conv, nets, count);
}

static void import_convert_out(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t conv = imp->map[vcode_get_arg(op, 0)];
   mir_value_t nets = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];

   mir_build_convert_out(mu, conv, nets, count);
}

static void import_driving_value(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];

   mir_value_t count = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1)
      count = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_driving_value(mu, signal, count);
}

static void import_put_conversion(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t cf = imp->map[vcode_get_arg(op, 0)];
   mir_value_t target = imp->map[vcode_get_arg(op, 1)];
   mir_value_t count = imp->map[vcode_get_arg(op, 2)];
   mir_value_t values = imp->map[vcode_get_arg(op, 3)];

   mir_build_put_conversion(mu, cf, target, count, values);
}

static void import_force(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count = imp->map[vcode_get_arg(op, 1)];
   mir_value_t values = imp->map[vcode_get_arg(op, 2)];

   mir_build_force(mu, target, count, values);
}

static void import_release(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count = imp->map[vcode_get_arg(op, 1)];

   mir_build_release(mu, target, count);
}

static void import_disconnect(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t target = imp->map[vcode_get_arg(op, 0)];
   mir_value_t count = imp->map[vcode_get_arg(op, 1)];
   mir_value_t reject = imp->map[vcode_get_arg(op, 2)];
   mir_value_t after = imp->map[vcode_get_arg(op, 3)];

   mir_build_disconnect(mu, target, count, reject, after);
}

static void import_exponent_check(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t exp = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];

   mir_build_exponent_check(mu, exp, locus);
}

static void import_process_init(mir_unit_t *mu, mir_import_t *imp, int op)
{
   ident_t name = vcode_get_func(op);
   mir_value_t locus = imp->map[vcode_get_arg(op, 0)];

   mir_build_process_init(mu, name, locus);
}

static void import_cover_stmt(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_build_cover_stmt(mu, vcode_get_tag(op));
}

static void import_cover_branch(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_build_cover_branch(mu, vcode_get_tag(op));
}

static void import_cover_expr(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_build_cover_expr(mu, vcode_get_tag(op));
}

static void import_cover_toggle(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_build_cover_toggle(mu, signal, vcode_get_tag(op));
}

static void import_cover_state(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   mir_value_t low = imp->map[vcode_get_arg(op, 1)];
   mir_build_cover_state(mu, signal, low, vcode_get_tag(op));
}

static void import_array_scope(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t locus = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vcode_get_type(op));
   mir_build_array_scope(mu, locus, type);
}

static void import_package_scope(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t locus = imp->map[vcode_get_arg(op, 0)];
   mir_build_package_scope(mu, locus);
}

static void import_record_scope(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t locus = imp->map[vcode_get_arg(op, 0)];
   mir_type_t type = import_type(mu, imp, vcode_get_type(op));
   mir_build_record_scope(mu, locus, type);
}

static void import_pop_scope(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_build_pop_scope(mu);
}

static void import_cmp_trigger(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_cmp_trigger(mu, left, right);
}

static void import_function_trigger(mir_unit_t *mu, mir_import_t *imp, int op)
{
   const int nargs = vcode_count_args(op);
   mir_value_t *args LOCAL = xmalloc_array(nargs, sizeof(mir_value_t));
   for (int i = 0; i < nargs; i++)
      args[i] = imp->map[vcode_get_arg(op, i)];

   ident_t func = vcode_get_func(op);
   vcode_reg_t result = vcode_get_result(op);
   imp->map[result] = mir_build_function_trigger(mu, func, args, nargs);
}

static void import_or_trigger(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t left = imp->map[vcode_get_arg(op, 0)];
   mir_value_t right = imp->map[vcode_get_arg(op, 1)];
   imp->map[vcode_get_result(op)] = mir_build_or_trigger(mu, left, right);
}

static void import_add_trigger(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t trigger = imp->map[vcode_get_arg(op, 0)];
   mir_build_add_trigger(mu, trigger);
}

static void import_protected_init(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t context = imp->map[vcode_get_arg(op, 0)];

   mir_value_t path_name = MIR_NULL_VALUE, inst_name = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1) {
      path_name = imp->map[vcode_get_arg(op, 1)];
      inst_name = imp->map[vcode_get_arg(op, 2)];
   }

   vcode_reg_t result = vcode_get_result(op);
   mir_type_t type = import_type(mu, imp, vcode_reg_type(result));

   imp->map[result] = mir_build_protected_init(mu, type, context,
                                               path_name, inst_name);
}

static void import_instance_name(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t kind = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_instance_name(mu, kind);
}

static void import_last_event(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];

   mir_value_t count = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1)
      count = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_last_event(mu, signal, count);
}

static void import_last_active(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];

   mir_value_t count = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1)
      count = imp->map[vcode_get_arg(op, 1)];

   imp->map[vcode_get_result(op)] = mir_build_last_active(mu, signal, count);
}

static void import_last_value(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t signal = imp->map[vcode_get_arg(op, 0)];
   imp->map[vcode_get_result(op)] = mir_build_last_value(mu, signal);
}

static void import_enter_state(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t state = imp->map[vcode_get_arg(op, 0)];

   mir_value_t strong = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 1)
      strong = imp->map[vcode_get_arg(op, 1)];

   mir_build_enter_state(mu, state, strong);
}

static void import_reflect_value(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_value_t context = imp->map[vcode_get_arg(op, 1)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 2)];

   mir_value_t bounds = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 3)
      bounds = imp->map[vcode_get_arg(op, 3)];

   imp->map[vcode_get_result(op)] =
      mir_build_reflect_value(mu, value, context, locus, bounds);
}

static void import_reflect_subtype(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t context = imp->map[vcode_get_arg(op, 0)];
   mir_value_t locus = imp->map[vcode_get_arg(op, 1)];

   mir_value_t bounds = MIR_NULL_VALUE;
   if (vcode_count_args(op) > 2)
      bounds = imp->map[vcode_get_arg(op, 2)];

   imp->map[vcode_get_result(op)] =
      mir_build_reflect_subtype(mu, context, locus, bounds);
}

static void import_debug_out(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t value = imp->map[vcode_get_arg(op, 0)];
   mir_build_debug_out(mu, value);
}

static void import_sched_process(mir_unit_t *mu, mir_import_t *imp, int op)
{
   mir_value_t delay = imp->map[vcode_get_arg(op, 0)];
   mir_build_sched_process(mu, delay);
}

static void import_block(mir_unit_t *mu, mir_import_t *imp)
{
   const int nops = vcode_count_ops();
   for (int i = 0; i < nops; i++) {
      mir_set_loc(mu, vcode_get_loc(i));

      switch (vcode_get_op(i)) {
      case VCODE_OP_COMMENT:
         break;
      case VCODE_OP_PACKAGE_INIT:
         import_package_init(mu, imp, i);
         break;
      case VCODE_OP_LINK_PACKAGE:
         import_link_package(mu, imp, i);
         break;
      case VCODE_OP_LINK_VAR:
         import_link_var(mu, imp, i);
         break;
      case VCODE_OP_RETURN:
         import_return(mu, imp, i);
         break;
      case VCODE_OP_FCALL:
         import_fcall(mu, imp, i);
         break;
      case VCODE_OP_PCALL:
         import_pcall(mu, imp, i);
         break;
      case VCODE_OP_RESUME:
         import_resume(mu, imp, i);
         break;
      case VCODE_OP_CONST:
         import_const(mu, imp, i);
         break;
      case VCODE_OP_CONST_REAL:
         import_const_real(mu, imp, i);
         break;
      case VCODE_OP_CONST_ARRAY:
         import_const_array(mu, imp, i);
         break;
      case VCODE_OP_CONST_REP:
         import_const_rep(mu, imp, i);
         break;
      case VCODE_OP_CONST_RECORD:
         import_const_record(mu, imp, i);
         break;
      case VCODE_OP_CMP:
         import_cmp(mu, imp, i);
         break;
      case VCODE_OP_DEBUG_LOCUS:
         import_debug_locus(mu, imp, i);
         break;
      case VCODE_OP_ASSERT:
         import_assert(mu, imp, i);
         break;
      case VCODE_OP_REPORT:
         import_report(mu, imp, i);
         break;
      case VCODE_OP_WAIT:
         import_wait(mu, imp, i);
         break;
      case VCODE_OP_JUMP:
         import_jump(mu, imp, i);
         break;
      case VCODE_OP_COND:
         import_cond(mu, imp, i);
         break;
      case VCODE_OP_INIT_SIGNAL:
         import_init_signal(mu, imp, i);
         break;
      case VCODE_OP_IMPLICIT_SIGNAL:
         import_implicit_signal(mu, imp, i);
         break;
      case VCODE_OP_DRIVE_SIGNAL:
         import_drive_signal(mu, imp, i);
         break;
      case VCODE_OP_SCHED_WAVEFORM:
         import_sched_waveform(mu, imp, i);
         break;
      case VCODE_OP_RESOLVED:
         import_resolved(mu, imp, i);
         break;
      case VCODE_OP_ADDRESS_OF:
         import_address_of(mu, imp, i);
         break;
      case VCODE_OP_STORE:
         import_store(mu, imp, i);
         break;
      case VCODE_OP_STORE_INDIRECT:
         import_store_indirect(mu, imp, i);
         break;
      case VCODE_OP_LOAD:
         import_load(mu, imp, i);
         break;
      case VCODE_OP_LOAD_INDIRECT:
         import_load_indirect(mu, imp, i);
         break;
      case VCODE_OP_CONTEXT_UPREF:
         import_context_upref(mu, imp, i);
         break;
      case VCODE_OP_VAR_UPREF:
         import_var_upref(mu, imp, i);
         break;
      case VCODE_OP_WRAP:
         import_wrap(mu, imp, i);
         break;
      case VCODE_OP_UNWRAP:
         import_unwrap(mu, imp, i);
         break;
      case VCODE_OP_UARRAY_LEN:
         import_uarray_len(mu, imp, i);
         break;
      case VCODE_OP_UARRAY_LEFT:
         import_uarray_left(mu, imp, i);
         break;
      case VCODE_OP_UARRAY_RIGHT:
         import_uarray_right(mu, imp, i);
         break;
      case VCODE_OP_UARRAY_DIR:
         import_uarray_dir(mu, imp, i);
         break;
      case VCODE_OP_ADD:
         import_add(mu, imp, i);
         break;
      case VCODE_OP_TRAP_ADD:
         import_trap_add(mu, imp, i);
         break;
      case VCODE_OP_SUB:
         import_sub(mu, imp, i);
         break;
      case VCODE_OP_TRAP_SUB:
         import_trap_sub(mu, imp, i);
         break;
      case VCODE_OP_MUL:
         import_mul(mu, imp, i);
         break;
      case VCODE_OP_TRAP_MUL:
         import_trap_mul(mu, imp, i);
         break;
      case VCODE_OP_DIV:
         import_div(mu, imp, i);
         break;
      case VCODE_OP_MOD:
         import_mod(mu, imp, i);
         break;
      case VCODE_OP_REM:
         import_rem(mu, imp, i);
         break;
      case VCODE_OP_EXP:
         import_exp(mu, imp, i);
         break;
      case VCODE_OP_TRAP_EXP:
         import_trap_exp(mu, imp, i);
         break;
      case VCODE_OP_OR:
         import_or(mu, imp, i);
         break;
      case VCODE_OP_NOR:
         import_nor(mu, imp, i);
         break;
      case VCODE_OP_XOR:
         import_xor(mu, imp, i);
         break;
      case VCODE_OP_XNOR:
         import_xnor(mu, imp, i);
         break;
      case VCODE_OP_AND:
         import_and(mu, imp, i);
         break;
      case VCODE_OP_NAND:
         import_nand(mu, imp, i);
         break;
      case VCODE_OP_EVENT:
         import_event(mu, imp, i);
         break;
      case VCODE_OP_ACTIVE:
         import_active(mu, imp, i);
         break;
      case VCODE_OP_DRIVING:
         import_driving(mu, imp, i);
         break;
      case VCODE_OP_NOT:
         import_not(mu, imp, i);
         break;
      case VCODE_OP_CAST:
         import_cast(mu, imp, i);
         break;
      case VCODE_OP_NEG:
         import_neg(mu, imp, i);
         break;
      case VCODE_OP_TRAP_NEG:
         import_trap_neg(mu, imp, i);
         break;
      case VCODE_OP_ABS:
         import_abs(mu, imp, i);
         break;
      case VCODE_OP_INDEX:
         import_index(mu, imp, i);
         break;
      case VCODE_OP_COPY:
         import_copy(mu, imp, i);
         break;
      case VCODE_OP_MEMSET:
         import_memset(mu, imp, i);
         break;
      case VCODE_OP_ARRAY_REF:
         import_array_ref(mu, imp, i);
         break;
      case VCODE_OP_TABLE_REF:
         import_table_ref(mu, imp, i);
         break;
      case VCODE_OP_RECORD_REF:
         import_record_ref(mu, imp, i);
         break;
      case VCODE_OP_RANGE_CHECK:
         import_range_check(mu, imp, i);
         break;
      case VCODE_OP_SCHED_EVENT:
         import_sched_event(mu, imp, i);
         break;
      case VCODE_OP_CLEAR_EVENT:
         import_clear_event(mu, imp, i);
         break;
      case VCODE_OP_ALLOC:
         import_alloc(mu, imp, i);
         break;
      case VCODE_OP_RANGE_LENGTH:
         import_range_length(mu, imp, i);
         break;
      case VCODE_OP_RANGE_NULL:
         import_range_null(mu, imp, i);
         break;
      case VCODE_OP_NULL:
         import_null(mu, imp, i);
         break;
      case VCODE_OP_NEW:
         import_new(mu, imp, i);
         break;
      case VCODE_OP_ALL:
         import_all(mu, imp, i);
         break;
      case VCODE_OP_DEALLOCATE:
         import_deallocate(mu, imp, i);
         break;
      case VCODE_OP_ZERO_CHECK:
         import_zero_check(mu, imp, i);
         break;
      case VCODE_OP_NULL_CHECK:
         import_null_check(mu, imp, i);
         break;
      case VCODE_OP_LENGTH_CHECK:
         import_length_check(mu, imp, i);
         break;
      case VCODE_OP_INDEX_CHECK:
         import_index_check(mu, imp, i);
         break;
      case VCODE_OP_DIR_CHECK:
         import_dir_check(mu, imp, i);
         break;
      case VCODE_OP_ALIAS_SIGNAL:
         import_alias_signal(mu, imp, i);
         break;
      case VCODE_OP_MAP_SIGNAL:
         import_map_signal(mu, imp, i);
         break;
      case VCODE_OP_MAP_CONST:
         import_map_const(mu, imp, i);
         break;
      case VCODE_OP_MAP_IMPLICIT:
         import_map_implicit(mu, imp, i);
         break;
      case VCODE_OP_CASE:
         import_case(mu, imp, i);
         break;
      case VCODE_OP_BIND_FOREIGN:
         import_bind_foreign(mu, imp, i);
         break;
      case VCODE_OP_BIND_EXTERNAL:
         import_bind_external(mu, imp, i);
         break;
      case VCODE_OP_UNREACHABLE:
         import_unreachable(mu, imp, i);
         break;
      case VCODE_OP_SELECT:
         import_select(mu, imp, i);
         break;
      case VCODE_OP_CLOSURE:
         import_closure(mu, imp, i);
         break;
      case VCODE_OP_RESOLUTION_WRAPPER:
         import_resolution_wrapper(mu, imp, i);
         break;
      case VCODE_OP_RESOLVE_SIGNAL:
         import_resolve_signal(mu, imp, i);
         break;
      case VCODE_OP_TRANSFER_SIGNAL:
         import_transfer_signal(mu, imp, i);
         break;
      case VCODE_OP_FILE_OPEN:
         import_file_open(mu, imp, i);
         break;
      case VCODE_OP_FILE_READ:
         import_file_read(mu, imp, i);
         break;
      case VCODE_OP_FILE_WRITE:
         import_file_write(mu, imp, i);
         break;
      case VCODE_OP_PORT_CONVERSION:
         import_port_conversion(mu, imp, i);
         break;
      case VCODE_OP_CONVERT_IN:
         import_convert_in(mu, imp, i);
         break;
      case VCODE_OP_CONVERT_OUT:
         import_convert_out(mu, imp, i);
         break;
      case VCODE_OP_DRIVING_VALUE:
         import_driving_value(mu, imp, i);
         break;
      case VCODE_OP_PUT_CONVERSION:
         import_put_conversion(mu, imp, i);
         break;
      case VCODE_OP_FORCE:
         import_force(mu, imp, i);
         break;
      case VCODE_OP_RELEASE:
         import_release(mu, imp, i);
         break;
      case VCODE_OP_DISCONNECT:
         import_disconnect(mu, imp, i);
         break;
      case VCODE_OP_EXPONENT_CHECK:
         import_exponent_check(mu, imp, i);
         break;
      case VCODE_OP_PROCESS_INIT:
         import_process_init(mu, imp, i);
         break;
      case VCODE_OP_COVER_STMT:
         import_cover_stmt(mu, imp, i);
         break;
      case VCODE_OP_COVER_BRANCH:
         import_cover_branch(mu, imp, i);
         break;
      case VCODE_OP_COVER_EXPR:
         import_cover_expr(mu, imp, i);
         break;
      case VCODE_OP_COVER_TOGGLE:
         import_cover_toggle(mu, imp, i);
         break;
      case VCODE_OP_COVER_STATE:
         import_cover_state(mu, imp, i);
         break;
      case VCODE_OP_ARRAY_SCOPE:
         import_array_scope(mu, imp, i);
         break;
      case VCODE_OP_PACKAGE_SCOPE:
         import_package_scope(mu, imp, i);
         break;
      case VCODE_OP_RECORD_SCOPE:
         import_record_scope(mu, imp, i);
         break;
      case VCODE_OP_POP_SCOPE:
         import_pop_scope(mu, imp, i);
         break;
      case VCODE_OP_CMP_TRIGGER:
         import_cmp_trigger(mu, imp, i);
         break;
      case VCODE_OP_FUNCTION_TRIGGER:
         import_function_trigger(mu, imp, i);
         break;
      case VCODE_OP_OR_TRIGGER:
         import_or_trigger(mu, imp, i);
         break;
      case VCODE_OP_ADD_TRIGGER:
         import_add_trigger(mu, imp, i);
         break;
      case VCODE_OP_PROTECTED_INIT:
         import_protected_init(mu, imp, i);
         break;
      case VCODE_OP_PROTECTED_FREE:
         break;   // No-op
      case VCODE_OP_INSTANCE_NAME:
         import_instance_name(mu, imp, i);
         break;
      case VCODE_OP_LAST_EVENT:
         import_last_event(mu, imp, i);
         break;
      case VCODE_OP_LAST_ACTIVE:
         import_last_active(mu, imp, i);
         break;
      case VCODE_OP_LAST_VALUE:
         import_last_value(mu, imp, i);
         break;
      case VCODE_OP_ENTER_STATE:
         import_enter_state(mu, imp, i);
         break;
      case VCODE_OP_REFLECT_SUBTYPE:
         import_reflect_subtype(mu, imp, i);
         break;
      case VCODE_OP_REFLECT_VALUE:
         import_reflect_value(mu, imp, i);
         break;
      case VCODE_OP_DEBUG_OUT:
         import_debug_out(mu, imp, i);
         break;
      case VCODE_OP_SCHED_PROCESS:
         import_sched_process(mu, imp, i);
         break;
      default:
         vcode_dump_with_mark(i, NULL, NULL);
         fatal_trace("cannot import vcode op %s",
                     vcode_op_string(vcode_get_op(i)));
      }
   }
}

mir_unit_t *mir_import(mir_context_t *mc, vcode_unit_t vu)
{
   static const mir_unit_kind_t kind_map[] = {
      [VCODE_UNIT_FUNCTION] = MIR_UNIT_FUNCTION,
      [VCODE_UNIT_INSTANCE] = MIR_UNIT_INSTANCE,
      [VCODE_UNIT_PROCEDURE] = MIR_UNIT_PROCEDURE,
      [VCODE_UNIT_PROCESS] = MIR_UNIT_PROCESS,
      [VCODE_UNIT_PACKAGE] = MIR_UNIT_PACKAGE,
      [VCODE_UNIT_THUNK] = MIR_UNIT_THUNK,
      [VCODE_UNIT_PROTECTED] = MIR_UNIT_PROTECTED,
      [VCODE_UNIT_PROPERTY] = MIR_UNIT_PROPERTY,
   };

   const mir_unit_kind_t kind = kind_map[vcode_unit_kind(vu)];

   mir_shape_t *parent = NULL;
   vcode_unit_t context = vcode_unit_context(vu);
   if (context != NULL) {
      ident_t name = vcode_unit_name(context);
      if ((parent = mir_get_shape(mc, name)) == NULL) {
         mir_unit_t *parent_mu = mir_import(mc, context);
         mir_put_unit(mc, parent_mu);

         parent = mir_get_shape(mc, name);
         assert(parent != NULL);
      }
   }

   ident_t name = vcode_unit_name(vu);
   object_t *obj = vcode_unit_object(vu);

   mir_unit_t *mu = mir_unit_new(mc, name, obj, kind, parent);

   vcode_select_unit(vu);

   const int nblocks = vcode_count_blocks();
   const int nregs = vcode_count_regs();
   const int nvars = vcode_count_vars();

   mir_import_t imp = {};
   imp.blocks = xcalloc_array(nblocks, sizeof(mir_block_t));
   imp.map    = xcalloc_array(nregs, sizeof(mir_value_t));
   imp.vars   = xcalloc_array(nvars, sizeof(mir_value_t));
   imp.types  = ihash_new(64);

   switch (kind) {
   case MIR_UNIT_FUNCTION:
      {
         vcode_type_t vresult = vcode_unit_result(vu);
         if (vresult != VCODE_INVALID_TYPE)
            mir_set_result(mu, import_type(mu, &imp, vresult));
      }
      // Fall-through
   case MIR_UNIT_PROCEDURE:
   case MIR_UNIT_PROTECTED:
   case MIR_UNIT_PROPERTY:
      {
         const int nparams = vcode_count_params();
         for (int i = 0; i < nparams; i++) {
            vcode_reg_t preg = vcode_param_reg(i);
            mir_type_t type = import_type(mu, &imp, vcode_param_type(i));
            ident_t name = vcode_param_name(i);
            imp.map[preg] = mir_add_param(mu, type, MIR_NULL_STAMP, name);
         }
      }
      break;
   case MIR_UNIT_THUNK:
      mir_set_result(mu, import_type(mu, &imp, vcode_unit_result(vu)));
      break;
   default:
      break;
   }

   for (int i = 0; i < nvars; i++) {
      mir_type_t type = import_type(mu, &imp, vcode_var_type(i));
      imp.vars[i] = mir_add_var(mu, type, MIR_NULL_STAMP, vcode_var_name(i),
                                (mir_var_flags_t)vcode_var_flags(i));
   }

   imp.blocks[0] = mir_get_cursor(mu, NULL);
   for (int i = 1; i < nblocks; i++)
      imp.blocks[i] = mir_add_block(mu);

   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);
      mir_set_cursor(mu, imp.blocks[i], MIR_APPEND);
      import_block(mu, &imp);
   }

   ihash_free(imp.types);
   free(imp.blocks);
   free(imp.map);
   free(imp.vars);

   mir_optimise(mu, MIR_PASS_O0);
   return mu;
}
