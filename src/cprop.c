//
//  Copyright (C) 2021-2022  Nick Gasson
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

#include "cprop.h"
#include "enode.h"
#include "phase.h"
#include "array.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

typedef enum { VM_SIGNAL, VM_CONST } var_map_kind_t;

typedef struct {
   vcode_var_t    var;
   vcode_unit_t   unit;
   ident_t        name;
   var_map_kind_t kind;
   union {
      struct {
         e_node_t signal;
         unsigned offset;
      }; // VM_SIGNAL
      struct {
         int64_t  cval;
      }; // VM_CONST
   };
} var_map_t;

struct cprop_vars_s {
   A(var_map_t) maps;
   A(unsigned)  mark_stack;
};

static int cprop_dump_reg(vcode_reg_t what, const cprop_state_t *regs)
{
   switch (regs[what].tag) {
   case CP_NONE:
      return printf("!");
   case CP_CONST:
      return printf("%"PRIi64, regs[what].cval);
   case CP_UNKNOWN:
      return printf("?");
   case CP_SCALE:
      {
         int nchars = cprop_dump_reg(regs[what].base, regs);
         nchars += printf("*");
         nchars += cprop_dump_reg(regs[what].scale, regs);
         return nchars;
      }
   case CP_OFFSET:
      {
         int nchars = cprop_dump_reg(regs[what].base, regs);
         nchars += printf("+");
         nchars += cprop_dump_reg(regs[what].offset.reg, regs);
         if (regs[what].offset.stride != 1)
            nchars += printf("*%u", regs[what].offset.stride);
         return nchars;
      }
   case CP_SIGNAL:
      {
         const cprop_signal_t *s = &(regs[what].signal);
         if (s->offset == 0)
            return printf("%s", istr(e_ident(s->enode)));
         else
            return printf("%s+%u", istr(e_ident(s->enode)), s->offset);
      }
   case CP_FIELD:
      {
         int nchars = cprop_dump_reg(regs[what].base, regs);
         nchars += printf("+%"PRIi64, regs[what].cval);
         return nchars;
      }
   default:
      return 0;
   }
}

static int cprop_dump_fn(vcode_dump_reason_t why, int what, void *context)
{
   cprop_state_t *regs = (cprop_state_t *)context;

   if (why != VCODE_DUMP_REG)
      return 0;

   assert(what != VCODE_INVALID_REG);

   switch (regs[what].tag) {
   case CP_NONE:
      return color_printf("$!red$(!)$$");
   case CP_CLOSURE:
      return color_printf("$!magenta$(%s)$$", istr(regs[what].func));
   case CP_CONST:
   case CP_SIGNAL:
   case CP_SCALE:
   case CP_OFFSET:
   case CP_FIELD:
   case CP_UNKNOWN:
      {
         int nchars = color_printf("$!magenta$(");
         nchars += cprop_dump_reg(what, regs);
         nchars += color_printf(")$$");
         return nchars;
      }
   }

   return 0;
}

void cprop_dump(int op, cprop_state_t *regs)
{
   vcode_dump_with_mark(op, cprop_dump_fn, regs);
}

void cprop_get_signal(vcode_reg_t target, vcode_reg_t count_reg,
                      cprop_state_t *regs, unsigned *offset, unsigned *stride,
                      unsigned *count, e_node_t *signal)
{
   switch (regs[target].tag) {
   case CP_SIGNAL:
      *offset += regs[target].signal.offset;
      *signal = regs[target].signal.enode;
      break;
   case CP_OFFSET:
      cprop_get_signal(regs[target].base, count_reg, regs, offset, stride,
                       count, signal);

      const unsigned subsigs = regs[target].offset.stride;
      if (regs[regs[target].offset.reg].tag == CP_CONST)
         *offset += regs[regs[target].offset.reg].cval * subsigs;
      else {
         cprop_get_signal(regs[target].offset.reg, count_reg, regs, offset,
                          stride, count, signal);
         *stride *= subsigs;
      }
      break;
   case CP_SCALE:
      cprop_get_signal(regs[target].base, count_reg, regs, offset, stride,
                       count, signal);

      if (regs[regs[target].scale].tag == CP_CONST)
         *stride = regs[regs[target].scale].cval;
      else
         cprop_get_signal(regs[target].scale, count_reg, regs, offset,
                          stride, count, signal);
      break;
   case CP_UNKNOWN:
      *stride = 1;
      break;
   case CP_FIELD:
      cprop_get_signal(regs[target].base, count_reg, regs, offset, stride,
                       count, signal);
      *offset += regs[target].cval;
      break;
   default:
      cprop_dump(-1, regs);
      fatal_trace("unexpected reg state for r%d on RHS of target", target);
   }

   if (count_reg != VCODE_INVALID_REG && regs[count_reg].tag == CP_CONST)
      *count = regs[count_reg].cval;
   else {
      *stride = 1;
      *count = 1;
   }
}

static void cprop_store_var(cprop_vars_t *vars, int op, cprop_state_t *regs)
{
   vcode_var_t var = vcode_get_address(op);
   vcode_var_flags_t flags = vcode_var_flags(var);
   vcode_reg_t arg0 = vcode_get_arg(op, 0);

   var_map_t map = {
      .var   = var,
      .name  = (flags & VAR_GLOBAL) ? vcode_var_name(var) : NULL,
      .unit  = vcode_active_unit()
   };

   if ((flags & VAR_CONST) && regs[arg0].tag == CP_CONST) {
      map.kind = VM_CONST;
      map.cval = regs[arg0].cval;
   }
   else if (flags & VAR_SIGNAL) {
      unsigned offset = 0, stride = 0, count = 0;
      e_node_t signal = NULL;

      cprop_get_signal(arg0, VCODE_INVALID_REG, regs,
                       &offset, &stride, &count, &signal);

      if (signal == NULL)
         return;

      map.kind   = VM_SIGNAL;
      map.signal = signal;
      map.offset = offset;
   }
   else
      return;    // Not a constant

   APUSH(vars->maps, map);
}

static var_map_t *cprop_find_var(cprop_vars_t *vars, vcode_var_t var,
                                 vcode_unit_t unit)
{
   for (int i = vars->maps.count - 1; i >= 0; i--) {
      var_map_t *m = &(vars->maps.items[i]);
      if (m->var == var && m->unit == unit)
         return m;
   }

   return NULL;
}

static void cprop_link_var(cprop_vars_t *vars, int op, cprop_state_t *regs)
{
   ident_t name = vcode_get_ident(op);

   vcode_reg_t result = vcode_get_result(op);
   regs[result].tag = CP_UNKNOWN;

   for (int i = 0; i < vars->maps.count; i++) {
      var_map_t *m = &(vars->maps.items[i]);
      if (m->name == name) {
         switch (m->kind) {
         case VM_SIGNAL:
            regs[result].tag = CP_SIGNAL;
            regs[result].signal.enode  = m->signal;
            regs[result].signal.offset = m->offset;
            break;
         case VM_CONST:
            regs[result].tag  = CP_CONST;
            regs[result].cval = m->cval;
            break;
         }
         break;
      }
   }
}

static unsigned cprop_count_subsignals(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_REAL:
      return 1;
   case VCODE_TYPE_CARRAY:
      return vtype_size(vtype) * cprop_count_subsignals(vtype_elem(vtype));
   case VCODE_TYPE_RECORD:
      {
         unsigned sum = 0;
         const int nfields = vtype_fields(vtype);
         for (int i = 0; i < nfields; i++)
            sum += cprop_count_subsignals(vtype_field(vtype, i));
         return sum;
      }
   default:
      fatal_trace("cannot handle vtype kind %d in cprop_count_subsignals",
                  vtype_kind(vtype));
   }
}

void cprop(cprop_req_t *req)
{
   const int nregs = vcode_count_regs();
   cprop_state_t *regs LOCAL = xcalloc_array(nregs, sizeof(cprop_state_t));

   const vunit_kind_t kind = vcode_unit_kind();
   if (kind == VCODE_UNIT_FUNCTION || kind == VCODE_UNIT_PROCEDURE) {
      const int nparams = vcode_count_params();
      for (int i = 0; i < nparams; i++)
         regs[i].tag = CP_UNKNOWN;
   }

   const int nblocks = vcode_count_blocks();
   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);

      const int nops = vcode_count_ops();
      for (int op = 0; op < nops; op++) {
         const vcode_op_t kind = vcode_get_op(op);
         switch (kind) {
         case VCODE_OP_SCHED_WAVEFORM:
         case VCODE_OP_DISCONNECT:
            if (req->sched_waveform)
               (*req->sched_waveform)(op, regs, req->context);
            break;

         case VCODE_OP_SCHED_EVENT:
            if (req->sched_event)
               (*req->sched_event)(op, regs, req->context);
            break;

         case VCODE_OP_SCHED_STATIC:
            if (req->sched_static)
               (*req->sched_static)(op, regs, req->context);
            break;

         case VCODE_OP_CONST:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);
               regs[result].tag  = CP_CONST;
               regs[result].cval = vcode_get_value(op);
            }
            break;

         case VCODE_OP_SUB:
         case VCODE_OP_ADD:
         case VCODE_OP_MUL:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               vcode_reg_t arg1 = vcode_get_arg(op, 1);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (regs[arg0].tag == CP_CONST && regs[arg1].tag == CP_CONST) {
                  regs[result].tag = CP_CONST;
                  switch (vcode_get_op(op)) {
                  case VCODE_OP_ADD:
                     regs[result].cval = regs[arg0].cval + regs[arg1].cval;
                     break;
                  case VCODE_OP_SUB:
                     regs[result].cval = regs[arg0].cval - regs[arg1].cval;
                     break;
                  case VCODE_OP_MUL:
                     regs[result].cval = regs[arg0].cval * regs[arg1].cval;
                     break;
                  default:
                     break;
                  }
               }
               else if (kind == VCODE_OP_MUL
                        && vcode_reg_kind(result) == VCODE_TYPE_OFFSET) {
                  regs[result].tag   = CP_SCALE;
                  regs[result].base  = arg0;
                  regs[result].scale = arg1;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_ARRAY_REF:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               vcode_reg_t arg1 = vcode_get_arg(op, 1);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (vcode_reg_kind(result) == VCODE_TYPE_SIGNAL) {
                  const unsigned stride =
                     cprop_count_subsignals(vtype_base(vcode_reg_type(arg0)));
                  regs[result].tag           = CP_OFFSET;
                  regs[result].base          = arg0;
                  regs[result].offset.reg    = arg1;
                  regs[result].offset.stride = stride;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_NOT:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (regs[arg0].tag == CP_CONST) {
                  regs[result].tag  = CP_CONST;
                  regs[result].cval = !regs[arg0].cval;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_RECORD_REF:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (vcode_reg_kind(result) == VCODE_TYPE_SIGNAL) {
                  vcode_type_t rtype = vtype_base(vcode_reg_type(arg0));
                  assert(vtype_kind(rtype) == VCODE_TYPE_RECORD);

                  unsigned off = 0;
                  const int nth = vcode_get_field(op);
                  for (int i = 0; i < nth; i++)
                     off += cprop_count_subsignals(vtype_field(rtype, i));

                  regs[result].tag  = CP_FIELD;
                  regs[result].base = arg0;
                  regs[result].cval = off;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_CAST:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               vtype_kind_t kind = vtype_kind(vcode_get_type(op));
               const bool is_integer =
                  kind == VCODE_TYPE_INT || kind == VCODE_TYPE_OFFSET;

               if (regs[arg0].tag == CP_CONST && is_integer) {
                  regs[result].tag  = CP_CONST;
                  regs[result].cval = regs[arg0].cval;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_VAR_UPREF:
            {
               vcode_var_t var = vcode_get_address(op);

               vcode_state_t state;
               vcode_state_save(&state);

               const int hops = vcode_get_hops(op);

               vcode_unit_t unit = vcode_active_unit();
               for (int i = 0; i < hops; i++)
                  vcode_select_unit((unit = vcode_unit_context()));

               vcode_state_restore(&state);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               var_map_t *map;
               if (req->vars && (map = cprop_find_var(req->vars, var, unit))) {
                  switch (map->kind) {
                  case VM_SIGNAL:
                     regs[result].tag = CP_SIGNAL;
                     regs[result].signal.enode  = map->signal;
                     regs[result].signal.offset = map->offset;
                     break;
                  case VM_CONST:
                     regs[result].tag  = CP_CONST;
                     regs[result].cval = map->cval;
                     break;
                  }
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_LOAD:
            {
               vcode_var_t var = vcode_get_address(op);
               ident_t name = vcode_var_name(var);
               vcode_var_flags_t flags = vcode_var_flags(var);

               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (!!(flags & VAR_SIGNAL) && req->find_signal) {
                  e_node_t e = (*req->find_signal)(name, 0, req->context);
                  regs[result].tag = CP_SIGNAL;
                  regs[result].signal.enode  = e;
                  regs[result].signal.offset = 0;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_LINK_SIGNAL:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               if (req->find_signal) {
                  ident_t name = vcode_get_ident(op);
                  e_node_t e = (*req->find_signal)(name, 0, req->context);
                  regs[result].tag = CP_SIGNAL;
                  regs[result].signal.enode  = e;
                  regs[result].signal.offset = 0;
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_MAP_SIGNAL:
            if (req->map_signal)
               (*req->map_signal)(op, regs, req->context);
            break;

         case VCODE_OP_INIT_SIGNAL:
            if (req->init_signal)
               (*req->init_signal)(op, regs, req->context);
            break;

         case VCODE_OP_IMPLICIT_SIGNAL:
            if (req->implicit_signal)
               (*req->implicit_signal)(op, regs, req->context);
            break;

         case VCODE_OP_DRIVE_SIGNAL:
            if (req->drive_signal)
               (*req->drive_signal)(op, regs, req->context);
            break;

         case VCODE_OP_PCALL:
            if (req->pcall)
               (*req->pcall)(op, regs, req->context);
            break;

         case VCODE_OP_LAST_VALUE:
            if (req->last_value)
               (*req->last_value)(op, regs, req->context);
            regs[vcode_get_result(op)].tag = CP_UNKNOWN;
            break;

         case VCODE_OP_EVENT:
         case VCODE_OP_ACTIVE:
         case VCODE_OP_DRIVING:
            if (req->signal_flag)
               (*req->signal_flag)(op, regs, req->context);
            break;

         case VCODE_OP_STORE:
            if (req->vars)
               cprop_store_var(req->vars, op, regs);
            break;

         case VCODE_OP_LINK_VAR:
            if (req->vars)
               cprop_link_var(req->vars, op, regs);
            else
               regs[vcode_get_result(op)].tag = CP_UNKNOWN;
            break;

         case VCODE_OP_LOAD_INDIRECT:
         case VCODE_OP_UNWRAP:
         case VCODE_OP_WRAP:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               regs[result] = regs[vcode_get_arg(op, 0)];
            }
            break;

         case VCODE_OP_CMP:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               vcode_reg_t arg1 = vcode_get_arg(op, 1);
               if (regs[arg0].tag == CP_CONST && regs[arg1].tag == CP_CONST) {
                  regs[result].tag = CP_CONST;
                  switch (vcode_get_cmp(op)) {
                  case VCODE_CMP_LT:
                     regs[result].cval = regs[arg0].cval < regs[arg1].cval;
                     break;
                  case VCODE_CMP_GT:
                     regs[result].cval = regs[arg0].cval > regs[arg1].cval;
                     break;
                  case VCODE_CMP_EQ:
                     regs[result].cval = regs[arg0].cval == regs[arg1].cval;
                     break;
                  case VCODE_CMP_NEQ:
                     regs[result].cval = regs[arg0].cval != regs[arg1].cval;
                     break;
                  default:
                     vcode_dump_with_mark(op, cprop_dump_fn, regs);
                     fatal_trace("cannot evaluate comparison");
                  }
               }
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_SELECT:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               vcode_reg_t arg1 = vcode_get_arg(op, 1);
               vcode_reg_t arg2 = vcode_get_arg(op, 2);

               if (regs[arg0].tag == CP_CONST)
                  regs[result] = regs[arg0].cval ? regs[arg1] : regs[arg2];
               else
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         case VCODE_OP_LENGTH_CHECK:
            {
               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               vcode_reg_t arg1 = vcode_get_arg(op, 1);

               if (regs[arg0].tag == CP_CONST)
                  regs[arg1] = regs[arg0];
               else if (regs[arg1].tag == CP_CONST)
                  regs[arg0] = regs[arg1];
            }
            break;

         case VCODE_OP_UARRAY_LEN:
           {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               vcode_reg_t arg0 = vcode_get_arg(op, 0);
               if (regs[arg0].tag == CP_SIGNAL) {
                  regs[result].tag  = CP_CONST;
                  regs[result].cval = e_width(regs[arg0].signal.enode);
               }
               else
                  regs[result].tag = CP_UNKNOWN;
           }
           break;

         case VCODE_OP_FCALL:
            {
               vcode_reg_t result = vcode_get_result(op);
               if (result != VCODE_INVALID_REG)
                  regs[result].tag = CP_UNKNOWN;

               if (req->fcall)
                  (*req->fcall)(op, regs, req->context);
               break;
            }
            break;

         case VCODE_OP_CLOSURE:
            {
               vcode_reg_t result = vcode_get_result(op);
               assert(result != VCODE_INVALID_REG);

               regs[result].tag  = CP_CLOSURE;
               regs[result].func = vcode_get_func(op);
            }
            break;

         case VCODE_OP_RETURN:
         case VCODE_OP_COMMENT:
         case VCODE_OP_WAIT:
         case VCODE_OP_JUMP:
         case VCODE_OP_ASSERT:
         case VCODE_OP_REPORT:
         case VCODE_OP_TEMP_STACK_RESTORE:
         case VCODE_OP_COND:
         case VCODE_OP_STORE_INDIRECT:
         case VCODE_OP_COPY:
         case VCODE_OP_CASE:
         case VCODE_OP_NULL_CHECK:
         case VCODE_OP_INDEX_CHECK:
         case VCODE_OP_RANGE_CHECK:
         case VCODE_OP_RESUME:
         case VCODE_OP_FILE_OPEN:
         case VCODE_OP_FILE_READ:
         case VCODE_OP_FILE_CLOSE:
         case VCODE_OP_FILE_WRITE:
         case VCODE_OP_DEALLOCATE:
         case VCODE_OP_MEMSET:
         case VCODE_OP_COVER_COND:
         case VCODE_OP_COVER_STMT:
         case VCODE_OP_PROTECTED_FREE:
         case VCODE_OP_DEBUG_OUT:
            break;

         case VCODE_OP_RESOLVED:
         case VCODE_OP_CONST_ARRAY:
         case VCODE_OP_TEMP_STACK_MARK:
         case VCODE_OP_UARRAY_LEFT:
         case VCODE_OP_UARRAY_RIGHT:
         case VCODE_OP_UARRAY_DIR:
         case VCODE_OP_MOD:
         case VCODE_OP_ALLOCA:
         case VCODE_OP_INDEX:
         case VCODE_OP_AND:
         case VCODE_OP_OR:
         case VCODE_OP_XOR:
         case VCODE_OP_NOR:
         case VCODE_OP_XNOR:
         case VCODE_OP_NAND:
         case VCODE_OP_LAST_EVENT:
         case VCODE_OP_LAST_ACTIVE:
         case VCODE_OP_CONST_REAL:
         case VCODE_OP_CONST_RECORD:
         case VCODE_OP_ADDRESS_OF:
         case VCODE_OP_NULL:
         case VCODE_OP_DIV:
         case VCODE_OP_NEW:
         case VCODE_OP_DRIVING_VALUE:
         case VCODE_OP_NEG:
         case VCODE_OP_EXP:
         case VCODE_OP_ABS:
         case VCODE_OP_REM:
         case VCODE_OP_ALL:
         case VCODE_OP_ENDFILE:
         case VCODE_OP_RANGE_NULL:
         case VCODE_OP_RESOLUTION_WRAPPER:
         case VCODE_OP_PROTECTED_INIT:
         case VCODE_OP_CONTEXT_UPREF:
         case VCODE_OP_CONST_REP:
         case VCODE_OP_LINK_PACKAGE:
         case VCODE_OP_DEBUG_LOCUS:
            {
               vcode_reg_t result = vcode_get_result(op);
               if (result != VCODE_INVALID_REG)
                  regs[result].tag = CP_UNKNOWN;
            }
            break;

         default:
            vcode_dump_with_mark(op, cprop_dump_fn, regs);
            fatal_trace("cannot constant propagate vcode op %s",
                        vcode_op_string(vcode_get_op(op)));
         }
      }
   }

   const char *verbose = getenv("NVC_CPROP_VERBOSE");
   if (verbose && *verbose != '\0')
      vcode_dump_with_mark(-1, cprop_dump_fn, regs);
}

cprop_vars_t *cprop_vars_new(void)
{
   cprop_vars_t *vars = xcalloc(sizeof(cprop_vars_t));
   return vars;
}

void cprop_vars_free(cprop_vars_t *vars)
{
   ACLEAR(vars->maps);
   ACLEAR(vars->mark_stack);
   free(vars);
}

void cprop_vars_enter(cprop_vars_t *vars)
{
   APUSH(vars->mark_stack, vars->maps.count);
}

void cprop_vars_leave(cprop_vars_t *vars)
{
   const unsigned mark = APOP(vars->mark_stack);
   ATRIM(vars->maps, mark);
}
