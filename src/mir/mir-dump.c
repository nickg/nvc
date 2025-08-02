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
#include "ident.h"
#include "mir/mir-node.h"
#include "mir/mir-priv.h"
#include "mir/mir-structs.h"
#include "mir/mir-unit.h"
#include "thread.h"
#include "tree.h"
#include "vlog/vlog-node.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <float.h>
#include <stdlib.h>

// LCOV_EXCL_START /////////////////////////////////////////////////////////////

const char *mir_op_string(mir_op_t op)
{
   static const char *map[] = {
      [MIR_OP_ADD] = "add",
      [MIR_OP_RETURN] = "return",
      [MIR_OP_CONST] = "const",
      [MIR_OP_COMMENT] = "comment",
      [MIR_OP_STORE] = "store",
      [MIR_OP_LOAD] = "load",
      [MIR_OP_CONST_REAL] = "const real",
      [MIR_OP_JUMP] = "jump",
      [MIR_OP_COND] = "cond",
      [MIR_OP_CMP] = "cmp",
      [MIR_OP_PHI] = "phi",
      [MIR_OP_AND] = "and",
      [MIR_OP_OR] = "or",
      [MIR_OP_XOR] = "xor",
      [MIR_OP_CONSUME] = "consume",
      [MIR_OP_SUB] = "sub",
      [MIR_OP_MUL] = "mul",
      [MIR_OP_DIV] = "div",
      [MIR_OP_MOD] = "mod",
      [MIR_OP_EXP] = "exp",
      [MIR_OP_FCALL] = "fcall",
      [MIR_OP_PCALL] = "pcall",
      [MIR_OP_RESUME] = "resume",
      [MIR_OP_WRAP] = "wrap",
      [MIR_OP_UNWRAP] = "unwrap",
      [MIR_OP_NOT] = "not",
      [MIR_OP_RESOLVED] = "resolved",
      [MIR_OP_LAST_VALUE] = "last value",
      [MIR_OP_LOCUS] = "debug locus",
      [MIR_OP_INIT_SIGNAL] = "init signal",
      [MIR_OP_IMPLICIT_SIGNAL] = "implicit signal",
      [MIR_OP_SELECT] = "select",
      [MIR_OP_REM] = "rem",
      [MIR_OP_WAIT] = "wait",
      [MIR_OP_LINK_PACKAGE] = "link package",
      [MIR_OP_PACKAGE_INIT] = "package init",
      [MIR_OP_PROTECTED_INIT] = "protected init",
      [MIR_OP_ASSERT] = "assert",
      [MIR_OP_CONTEXT_UPREF] = "context upref",
      [MIR_OP_UARRAY_LEN] = "uarray len",
      [MIR_OP_UARRAY_LEFT] = "uarray left",
      [MIR_OP_UARRAY_RIGHT] = "uarray right",
      [MIR_OP_UARRAY_DIR] = "uarray dir",
      [MIR_OP_TRAP_ADD] = "trap add",
      [MIR_OP_TRAP_SUB] = "trap sub",
      [MIR_OP_TRAP_MUL] = "trap mul",
      [MIR_OP_TRAP_EXP] = "trap exp",
      [MIR_OP_CONST_ARRAY] = "const array",
      [MIR_OP_CONST_REP] = "const rep",
      [MIR_OP_CONST_RECORD] = "const record",
      [MIR_OP_CONST_VEC] = "const vec",
      [MIR_OP_ARRAY_REF] = "array ref",
      [MIR_OP_RECORD_REF] = "record ref",
      [MIR_OP_ADDRESS_OF] = "address of",
      [MIR_OP_VAR_UPREF] = "var upref",
      [MIR_OP_SCHED_WAVEFORM] = "sched waveform",
      [MIR_OP_DRIVE_SIGNAL] = "drive signal",
      [MIR_OP_EVENT] = "event",
      [MIR_OP_ACTIVE] = "active",
      [MIR_OP_CAST] = "cast",
      [MIR_OP_NEG] = "neg",
      [MIR_OP_TRAP_NEG] = "trap neg",
      [MIR_OP_ABS] = "abs",
      [MIR_OP_COPY] = "copy",
      [MIR_OP_REPORT] = "report",
      [MIR_OP_RANGE_CHECK] = "range check",
      [MIR_OP_INDEX_CHECK] = "index check",
      [MIR_OP_SCHED_EVENT] = "sched event",
      [MIR_OP_CLEAR_EVENT] = "clear event",
      [MIR_OP_ALLOC] = "alloc",
      [MIR_OP_RANGE_LENGTH] = "range length",
      [MIR_OP_RANGE_NULL] = "range null",
      [MIR_OP_SET] = "set",
      [MIR_OP_NULL] = "null",
      [MIR_OP_ALL] = "all",
      [MIR_OP_NEW] = "new",
      [MIR_OP_NULL_CHECK] = "null check",
      [MIR_OP_LENGTH_CHECK] = "length check",
      [MIR_OP_ZERO_CHECK] = "zero check",
      [MIR_OP_EXPONENT_CHECK] = "exponent check",
      [MIR_OP_ALIAS_SIGNAL] = "alias signal",
      [MIR_OP_MAP_SIGNAL] = "map signal",
      [MIR_OP_MAP_CONST] = "map const",
      [MIR_OP_MAP_IMPLICIT] = "map implicit",
      [MIR_OP_CASE] = "case",
      [MIR_OP_BIND_FOREIGN] = "bind foreign",
      [MIR_OP_BIND_EXTERNAL] = "bind external",
      [MIR_OP_UNREACHABLE] = "unreachable",
      [MIR_OP_RESOLUTION_WRAPPER] = "resolution wrapper",
      [MIR_OP_CLOSURE] = "closure",
      [MIR_OP_RESOLVE_SIGNAL] = "resolve signal",
      [MIR_OP_TRANSFER_SIGNAL] = "transfer signal",
      [MIR_OP_FILE_OPEN] = "file open",
      [MIR_OP_FILE_READ] = "file read",
      [MIR_OP_FILE_WRITE] = "file write",
      [MIR_OP_PORT_CONVERSION] = "port conversion",
      [MIR_OP_CONVERT_IN] = "convert in",
      [MIR_OP_CONVERT_OUT] = "convert out",
      [MIR_OP_PUT_CONVERSION] = "put conversion",
      [MIR_OP_LINK_VAR] = "link var",
      [MIR_OP_DRIVING_VALUE] = "driving value",
      [MIR_OP_FORCE] = "force",
      [MIR_OP_RELEASE] = "release",
      [MIR_OP_DISCONNECT] = "disconnect",
      [MIR_OP_PROCESS_INIT] = "process init",
      [MIR_OP_COVER_STMT] = "cover stmt",
      [MIR_OP_COVER_BRANCH] = "cover branch",
      [MIR_OP_COVER_TOGGLE] = "cover toggle",
      [MIR_OP_COVER_EXPR] = "cover expr",
      [MIR_OP_COVER_STATE] = "cover state",
      [MIR_OP_RECORD_SCOPE] = "record scope",
      [MIR_OP_ARRAY_SCOPE] = "array scope",
      [MIR_OP_PACKAGE_SCOPE] = "package scope",
      [MIR_OP_POP_SCOPE] = "pop scope",
      [MIR_OP_ADD_TRIGGER] = "add trigger",
      [MIR_OP_FUNCTION_TRIGGER] = "function trigger",
      [MIR_OP_OR_TRIGGER] = "or trigger",
      [MIR_OP_CMP_TRIGGER] = "cmp trigger",
      [MIR_OP_LEVEL_TRIGGER] = "level trigger",
      [MIR_OP_INSTANCE_NAME] = "instance name",
      [MIR_OP_LAST_EVENT] = "last event",
      [MIR_OP_LAST_ACTIVE] = "last active",
      [MIR_OP_DRIVING] = "driving",
      [MIR_OP_ENTER_STATE] = "enter state",
      [MIR_OP_DEPOSIT_SIGNAL] = "deposit signal",
      [MIR_OP_SYSCALL] = "syscall",
      [MIR_OP_REFLECT_VALUE] = "reflect value",
      [MIR_OP_REFLECT_SUBTYPE] = "reflect subtype",
      [MIR_OP_DEBUG_OUT] = "debug out",
      [MIR_OP_PACK] = "pack",
      [MIR_OP_UNPACK] = "unpack",
      [MIR_OP_BINARY] = "vector binary",
      [MIR_OP_UNARY] = "vector unary",
      [MIR_OP_DIR_CHECK] = "dir check",
      [MIR_OP_INSERT] = "insert",
      [MIR_OP_TEST] = "test",
   };

   return map[op];
}

static void mir_dump_tab(int col, int to_col)
{
   if (col >= to_col)
      printf(" ");
   else
      printf("%*.s", to_col - col, "");
}

static void mir_dump_comment(int col)
{
   mir_dump_tab(col, 40);
   color_printf("$cyan$// ");
}

static int mir_pretty_print_int(int64_t n)
{
   if (n == INT64_MAX)
      return printf("2^63-1");
   else if (n == INT64_MIN)
      return printf("-2^63");
   else if (n == INT32_MAX)
      return printf("2^31-1");
   else if (n == INT32_MIN)
      return printf("-2^31");
   else
      return printf("%"PRIi64, n);
}

static int mir_dump_value(mir_unit_t *mu, mir_value_t value,
                          const mir_annotate_t *cb, void *ctx)
{
   int col = 0;
   switch (value.tag) {
   case MIR_TAG_NULL:
      col += color_printf("$red$null$$");
      break;
   case MIR_TAG_PARAM:
      col += color_printf("$magenta$%s$$",
                          istr(mir_param_data(mu, value)->name));
      break;
   case MIR_TAG_VAR:
      col += color_printf("$magenta$%s$$",
                          istr(mir_var_data(mu, value)->name));
      break;
   case MIR_TAG_NODE:
      col += color_printf("$green$%%%u$$", value.id);
      break;
   case MIR_TAG_BLOCK:
      col += color_printf("$yellow$%u$$", value.id);
      break;
   case MIR_TAG_CONST:
      {
         int64_t intg = 0;
         mir_get_const(mu, value, &intg);
         col += color_printf("$green$#%"PRIi64"$$", intg);
      }
      break;
   case MIR_TAG_LINKAGE:
      col += color_printf("$magenta$%s$$", istr(mu->linkage.items[value.id]));
      break;
   case MIR_TAG_ENUM:
      col += color_printf("%u", value.id);
      break;
   default:
      col += color_printf("$red$invalid$$");
      break;
   }

   if (cb->value != NULL)
      col += (*cb->value)(mu, value, ctx);

   return col;
}

static void mir_dump_one_type(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case _MIR_INVALID_TYPE:
      break;

   case MIR_TYPE_INT:
      if (td->u.intg.low != td->u.intg.high) {
         mir_pretty_print_int(td->u.intg.low);
         printf("..");
         mir_pretty_print_int(td->u.intg.high);
      }
      else
         mir_pretty_print_int(td->u.intg.low);
      break;

   case MIR_TYPE_REAL:
      if (td->u.real.low == -DBL_MAX && td->u.real.high == DBL_MAX)
         printf("%%");
      else if (td->u.real.low == td->u.real.high)
         printf("%f", td->u.real.low);
      else
         printf("%f..%f", td->u.real.low, td->u.real.high);
      break;

   case MIR_TYPE_OFFSET:
      printf("#");
      break;

   case MIR_TYPE_LOCUS:
      printf("D<>");
      break;

   case MIR_TYPE_POINTER:
      printf("@<");
      mir_dump_one_type(mu, td->u.pointer);
      printf(">");
      break;

   case MIR_TYPE_UARRAY:
      {
         printf("[");
         for (int i = 0; i < td->u.uarray.dims; i++)
            printf("%s*", i > 0 ? ", " : "");
         printf("] : ");
         mir_dump_one_type(mu, td->u.uarray.elem);
      }
      break;

   case MIR_TYPE_SIGNAL:
      printf("$<");
      mir_dump_one_type(mu, td->u.signal.base);
      printf(">");
      break;

   case MIR_TYPE_ACCESS:
      printf("A<");
      mir_dump_one_type(mu, td->u.access.to);
      printf(">");
      break;

   case MIR_TYPE_CONTEXT:
      printf("P<%s>", istr(td->u.context));
      break;

   case MIR_TYPE_CARRAY:
      printf("[%u] : ", td->u.carray.size);
      mir_dump_one_type(mu, td->u.carray.elem);
      break;

   case MIR_TYPE_RECORD:
      printf("%s{}", istr(td->u.record.name));
      break;

   case MIR_TYPE_CLOSURE:
      printf("C<");
      mir_dump_one_type(mu, td->u.closure.rtype);
      printf(">");
      break;

   case MIR_TYPE_RESOLUTION:
      printf("R<");
      mir_dump_one_type(mu, td->u.base);
      printf(">");
      break;

   case MIR_TYPE_FILE:
      printf("F<");
      mir_dump_one_type(mu, td->u.base);
      printf(">");
      break;

   case MIR_TYPE_CONVERSION:
      printf("X<>");
      break;

   case MIR_TYPE_TRIGGER:
      printf("T<>");
      break;

   case MIR_TYPE_OPAQUE:
      printf("?");
      break;

   case MIR_TYPE_VEC2:
      printf("Vec2%s<%u>", td->u.vec.issigned ? "S" : "", td->u.vec.size);
      break;

   case MIR_TYPE_VEC4:
      printf("Vec4%s<%u>", td->u.vec.issigned ? "S" : "", td->u.vec.size);
      break;
   }
}

static void mir_dump_type(mir_unit_t *mu, int col, mir_type_t type)
{
   if (mir_is_null(type))
      return;

   mir_dump_comment(col);
   mir_dump_one_type(mu, type);
}

static void mir_dump_stamp(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp)
{
   if (mir_is_null(type) || mir_is_null(stamp) || mir_is_top(mu, type, stamp))
      return;

   printf(" => ");

   for (;;) {
      const stamp_data_t *sd = mir_stamp_data(mu, stamp);
      switch (sd->kind) {
      case _MIR_INVALID_STAMP:
         return;

      case MIR_STAMP_INT:
         if (sd->u.intg.low != sd->u.intg.high) {
            mir_pretty_print_int(sd->u.intg.low);
            printf("..");
            mir_pretty_print_int(sd->u.intg.high);
         }
         else
            mir_pretty_print_int(sd->u.intg.low);
         return;

      case MIR_STAMP_REAL:
         if (sd->u.real.low == -DBL_MAX && sd->u.real.high == DBL_MAX)
            printf("%%");
         else if (sd->u.real.low == sd->u.real.high)
            printf("%f", sd->u.real.low);
         else
            printf("%f..%f", sd->u.real.low, sd->u.real.high);
         return;

      case MIR_STAMP_POINTER:
         {
            static const char *map[] = {
               "", "const", "stack", "local", "global", ""
            };
            printf("%s", map[sd->u.pointer.memory]);

            if (mir_is_null(sd->u.pointer.elem))
               return;

            printf(", ");
            stamp = sd->u.pointer.elem;
         }
         break;
      }
   }
}

static int mir_dump_const_array(mir_unit_t *mu, mir_value_t value,
                                    const mir_annotate_t *cb, void *ctx)
{
   node_data_t *n = mir_node_data(mu, value);

   bool is_string = true;

   const type_data_t *td = mir_type_data(mu, mir_get_elem(mu, n->type));
   if (td->class != MIR_TYPE_INT || td->u.intg.low < 0 || td->u.intg.high > 255)
      is_string = false;

   mir_stamp_t elem = mir_stamp_elem(mu, n->stamp);
   if (!mir_is_null(elem)) {
      const stamp_data_t *sd = mir_stamp_data(mu, elem);
      if (sd->kind != MIR_STAMP_INT || !isprint(sd->u.intg.low)
          || !isprint(sd->u.intg.high))
         is_string = false;
   }

   int col = printf(" := const ");

   if (is_string) {
      col += color_printf("$green$\"");
      for (int i = 0; i < n->nargs; i++) {
         mir_value_t elt = mir_get_arg(mu, value, i);
         int64_t cval;
         if (mir_get_const(mu, elt, &cval))
            col += printf("%c", (char)cval);
         else
            should_not_reach_here();
      }
      col += color_printf("\"$$");
      return col;
   }

   col += printf("[");
   for (int i = 0; i < n->nargs; i++) {
      mir_value_t elt = mir_get_arg(mu, value, i);
      if (i > 0) col += printf(",");
      col += mir_dump_value(mu, elt, cb, ctx);
   }
   col += printf("]");
   return col;
}

static int mir_dump_arg(mir_unit_t *mu, mir_value_t node, int nth,
                        const mir_annotate_t *cb, void *ctx)
{
   mir_value_t arg = mir_get_arg(mu, node, nth);
   return mir_dump_value(mu, arg, cb, ctx);
}

static int mir_dump_strength(mir_unit_t *mu, mir_value_t value)
{
   assert(value.tag == MIR_TAG_ENUM);

   static const char *names[] = {
      "highz", "small", "medium", "weak", "large", "pull", "strong", "supply"
   };
   return color_printf("$green$(%s1,%s0)$$", names[STRENGTH1(value.id)],
                       names[STRENGTH0(value.id)]);
}

static int mir_dump_vector_op(mir_unit_t *mu, mir_value_t value)
{
   assert(value.tag == MIR_TAG_ENUM);

   switch (value.id) {
   case MIR_VEC_BIT_AND:  return printf(" & ");
   case MIR_VEC_BIT_OR:   return printf(" | ");
   case MIR_VEC_BIT_XOR:  return printf(" ^ ");
   case MIR_VEC_LOG_AND:  return printf(" && ");
   case MIR_VEC_LOG_OR:   return printf(" || ");
   case MIR_VEC_BIT_NOT:  return printf(" ~ ");
   case MIR_VEC_LOG_NOT:  return printf(" ! ");
   case MIR_VEC_LT:       return printf(" < ");
   case MIR_VEC_LEQ:      return printf(" <= ");
   case MIR_VEC_GT:       return printf(" > ");
   case MIR_VEC_GEQ:      return printf(" >= ");
   case MIR_VEC_LOG_EQ:   return printf(" == ");
   case MIR_VEC_LOG_NEQ:  return printf(" != ");
   case MIR_VEC_CASE_EQ:  return printf(" === ");
   case MIR_VEC_CASE_NEQ: return printf(" !== ");
   case MIR_VEC_CASEX_EQ: return printf(" casex == ");
   case MIR_VEC_ADD:      return printf(" + ");
   case MIR_VEC_SUB:      return printf(" - ");
   case MIR_VEC_MUL:      return printf(" * ");
   case MIR_VEC_SLL:      return printf(" << ");
   case MIR_VEC_SRL:      return printf(" >> ");
   default: return printf(" <%d> ", value.id);
   }
}

static int mir_dump_locus(object_t *obj)
{
   int col = color_printf("$magenta$");

   tree_t t = tree_from_object(obj);
   if (t != NULL)
      col += printf("%s@", tree_kind_str(tree_kind(t)));

   vlog_node_t v = vlog_from_object(obj);
   if (v != NULL)
      col += printf("%s@", vlog_kind_str(vlog_kind(v)));

   col += color_printf("%p$$", obj);
   return col;
}

static int mir_dump_dim(mir_unit_t *mu, mir_value_t node, int nth,
                        const mir_annotate_t *cb, void *ctx)
{
   mir_value_t left = mir_get_arg(mu, node, nth + 0);
   mir_value_t right = mir_get_arg(mu, node, nth + 1);
   mir_value_t dir = mir_get_arg(mu, node, nth + 2);

   int col = mir_dump_value(mu, left, cb, ctx);

   int64_t cdir;
   if (mir_get_const(mu, dir, &cdir)
       && (cdir == RANGE_TO || cdir == RANGE_DOWNTO)) {
      col += printf(" %s ", cdir == RANGE_TO ? "to" : "downto");
      col += mir_dump_value(mu, right, cb, ctx);
   }
   else {
      col += printf(" ");
      col += mir_dump_value(mu, right, cb, ctx);
      col += printf(" ");
      col += mir_dump_value(mu, dir, cb, ctx);
   }

   return col;
}


static void mir_visit_records(mir_unit_t *mu, mir_type_t type, ihash_t **seen)
{
   const type_data_t *td = mir_type_data(mu, type);

   switch (td->class) {
   case MIR_TYPE_RECORD:
      {
         if (*seen == NULL) {
            printf("Types\n");
            *seen = ihash_new(16);
         }
         else if (ihash_get(*seen, type.bits) != NULL)
            break;

         int col = 0;
         col += color_printf("  $magenta$%s$$", istr(td->u.record.name));
         mir_dump_tab(col, 40);
         color_printf("$cyan${");
         for (int i = 0; i < td->u.record.count; i++) {
            if (i > 0) printf(", ");
            mir_dump_one_type(mu, td->u.record.fields[i]);
         }
         color_printf("}$$\n");

         ihash_put(*seen, type.bits, (void *)-1);
      }
      break;
   case MIR_TYPE_POINTER:
      mir_visit_records(mu, td->u.pointer, seen);
      break;
   case MIR_TYPE_ACCESS:
      mir_visit_records(mu, td->u.access.to, seen);
      break;
   default:
      break;
   }
}

static void mir_dump_records(mir_unit_t *mu)
{
   ihash_t *seen = NULL;

   for (int i = 0; i < mu->params.count; i++)
      mir_visit_records(mu, mu->params.items[i].type, &seen);

   for (int i = 0; i < mu->vars.count; i++)
      mir_visit_records(mu, mu->vars.items[i].type, &seen);

   for (int i = 0; i < mu->num_nodes; i++) {
      if (!mir_is_null(mu->nodes[i].type))
         mir_visit_records(mu, mu->nodes[i].type, &seen);
   }

   if (seen != NULL) ihash_free(seen);
}

void mir_annotate(mir_unit_t *mu, const mir_annotate_t *cb, void *ctx)
{
   static nvc_lock_t lock = 0;
   SCOPED_LOCK(lock);

   printf("\n");
   if (mu->name != NULL)
      color_printf("Name       $cyan$%s$$\n", istr(mu->name));
   color_printf("Kind       $cyan$");
   switch (mu->kind) {
   case MIR_UNIT_PROCESS:     printf("process"); break;
   case MIR_UNIT_FUNCTION:    printf("function"); break;
   case MIR_UNIT_INSTANCE:    printf("instance"); break;
   case MIR_UNIT_PROCEDURE:   printf("procedure"); break;
   case MIR_UNIT_PACKAGE:     printf("package"); break;
   case MIR_UNIT_THUNK:       printf("thunk"); break;
   case MIR_UNIT_PROPERTY:    printf("property"); break;
   case MIR_UNIT_PROTECTED:   printf("protected"); break;
   }
   color_printf("$$\n");
   if (mu->parent != NULL)
      color_printf("Context    $cyan$%s$$\n", istr(mu->parent->name));

   mir_dump_records(mu);

   if (mu->vars.count > 0) {
      printf("Variables\n");

      for (int i = 0; i < mu->vars.count; i++) {
         const var_data_t *vd = &(mu->vars.items[i]);
         int col = printf("  ");
         col += color_printf("$magenta$%s$$", istr(vd->name));
         mir_dump_type(mu, col, vd->type);
         mir_dump_stamp(mu, vd->type, vd->stamp);
         if (vd->flags & MIR_VAR_SIGNAL) printf(", signal");
         if (vd->flags & MIR_VAR_HEAP)   printf(", heap");
         if (vd->flags & MIR_VAR_CONST)  printf(", constant");
         if (vd->flags & MIR_VAR_TEMP)   printf(", temp");
         color_printf("$$\n");
      }
   }

   if (!mir_is_null(mu->result)) {
      color_printf("Result     $cyan$");
      mir_dump_one_type(mu, mu->result);
      color_printf("$$\n");
   }

   if (mu->params.count > 0) {
      printf("Parameters\n");

      for (int i = 0; i < mu->params.count; i++) {
         const param_data_t *pd = &(mu->params.items[i]);
         int col = color_printf("  $magenta$%s$$", istr(pd->name));
         mir_dump_type(mu, col, pd->type);
         mir_dump_stamp(mu, pd->type, pd->stamp);
         color_printf("$$\n");
      }
   }

   if (mu->linkage.count > 0) {
      printf("Linkage\n");

      for (int i = 0; i < mu->linkage.count; i++)
         color_printf("  $magenta$%s$$\n", istr(mu->linkage.items[i]));
   }

   if (mu->blocks.count > 0)
      printf("Begin\n");

   for (int i = 0; i < mu->blocks.count; i++) {
      const block_data_t *b = &(mu->blocks.items[i]);
      for (int j = 0; j < b->num_nodes; j++) {
         int col = 0;
         if (j == 0) {
            col += color_printf("  $yellow$%2d:$$ ", i);
            if (cb->begin_block != NULL) {
               mir_block_t b = { .tag = MIR_TAG_BLOCK, .id = i };
               col += (*cb->begin_block)(mu, b, col, ctx);
            }
         }
         else
            col += printf("      ");

         const node_data_t *n = &(mu->nodes[b->nodes[j]]);
         const mir_value_t result = { .tag = MIR_TAG_NODE, .id = b->nodes[j] };

         switch (n->op) {
         case MIR_OP_ADD:
         case MIR_OP_SUB:
         case MIR_OP_MUL:
         case MIR_OP_DIV:
         case MIR_OP_AND:
         case MIR_OP_OR:
         case MIR_OP_XOR:
         case MIR_OP_MOD:
         case MIR_OP_REM:
         case MIR_OP_EXP:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               switch (n->op) {
               case MIR_OP_ADD:  col += printf(" + "); break;
               case MIR_OP_SUB:  col += printf(" - "); break;
               case MIR_OP_MUL:  col += printf(" * "); break;
               case MIR_OP_DIV:  col += printf(" / "); break;
               case MIR_OP_AND:  col += printf(" && "); break;
               case MIR_OP_OR:   col += printf(" || "); break;
               case MIR_OP_XOR:  col += printf(" ^ "); break;
               case MIR_OP_MOD:  col += printf(" %% "); break;
               case MIR_OP_REM:  col += printf(" %% "); break;
               case MIR_OP_EXP:  col += printf(" ** "); break;
               default: break;
               }
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_TRAP_ADD:
         case MIR_OP_TRAP_SUB:
         case MIR_OP_TRAP_MUL:
         case MIR_OP_TRAP_EXP:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               switch (n->op) {
               case MIR_OP_TRAP_ADD: col += printf(" + "); break;
               case MIR_OP_TRAP_SUB: col += printf(" - "); break;
               case MIR_OP_TRAP_MUL: col += printf(" * "); break;
               case MIR_OP_TRAP_EXP: col += printf(" ** "); break;
               default: break;
               }
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               col += printf(" locus ");
               col += mir_dump_value(mu, n->args[2], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_NOT:
         case MIR_OP_ALLOC:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_RETURN:
            {
               printf("%s", mir_op_string(n->op));
               if (n->nargs > 0) {
                  printf(" ");
                  mir_dump_value(mu, n->args[0], cb, ctx);
               }
            }
            break;

         case MIR_OP_CONST:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s %"PRIi64" ", mir_op_string(n->op),
                             n->iconst);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONST_REAL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := const %g ", n->dconst);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONST_VEC:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := const 0x%"PRIx64", 0x%"PRIx64 , n->bits[0],
                             n->bits[1]);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_STORE:
         case MIR_OP_COPY:
         case MIR_OP_SET:
            {
               mir_dump_value(mu, n->args[0], cb, ctx);
               printf(" := %s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[1], cb, ctx);
               if (n->nargs > 2) {
                  printf(" count ");
                  mir_dump_value(mu, n->args[2], cb, ctx);
               }
            }
            break;

         case MIR_OP_NEW:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               if (n->nargs > 0) {
                  col += printf(" count ");
                  col += mir_dump_value(mu, n->args[0], cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_LOAD:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_JUMP:
         case MIR_OP_ADD_TRIGGER:
         case MIR_OP_DEBUG_OUT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
            }
            break;

         case MIR_OP_COND:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
               printf(" then ");
               mir_dump_value(mu, n->args[1], cb, ctx);
               printf(" else ");
               mir_dump_value(mu, n->args[2], cb, ctx);
            }
            break;

         case MIR_OP_CASE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" default ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               for (int i = 2; i < n->nargs; i += 2) {
                  printf(" [");
                  mir_dump_arg(mu, result, i, cb, ctx);
                  printf(" ");
                  mir_dump_arg(mu, result, i + 1, cb, ctx);
                  printf("]");
               }
            }
            break;

         case MIR_OP_WAIT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
               if (n->nargs > 1) {
                  printf(" for ");
                  mir_dump_value(mu, n->args[1], cb, ctx);
               }
            }
            break;

         case MIR_OP_SELECT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" then ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               col += printf(" else ");
               col += mir_dump_value(mu, n->args[2], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CMP:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               switch (n->args[0].id) {
               case MIR_CMP_EQ:  col += printf(" == "); break;
               case MIR_CMP_NEQ: col += printf(" != "); break;
               case MIR_CMP_LT:  col += printf(" < "); break;
               case MIR_CMP_GT:  col += printf(" > "); break;
               case MIR_CMP_LEQ: col += printf(" <= "); break;
               case MIR_CMP_GEQ: col += printf(" >= "); break;
               }
               col += mir_dump_value(mu, n->args[2], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_PHI:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s", mir_op_string(n->op));
               for (int i = 0; i < n->nargs; i += 2) {
                  col += printf(" [");
                  col += mir_dump_value(mu, n->args[i], cb, ctx);
                  col += printf(" ");
                  col += mir_dump_value(mu, n->args[i+1], cb, ctx);
                  col += printf("]");
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONSUME:
         case MIR_OP_RESUME:
         case MIR_OP_INSTANCE_NAME:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
            }
            break;

         case MIR_OP_FCALL:
         case MIR_OP_FUNCTION_TRIGGER:
            {
               if (!mir_is_null(n->type)) {
                  col += mir_dump_value(mu, result, cb, ctx);
                  col += printf(" := ");
               }
               col += printf("%s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" ");
               for (int i = 1; i < n->nargs; i++) {
                  if (i > 1) col += printf(", ");
                  col += mir_dump_arg(mu, result, i, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_PCALL:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" ");
               for (int i = 2; i < n->nargs; i++) {
                  if (i > 2) col += printf(", ");
                  col += mir_dump_arg(mu, result, i, cb, ctx);
               }
               printf(" resume ");
               mir_dump_arg(mu, result, 0, cb, ctx);
            }
            break;

         case MIR_OP_SYSCALL:
            {
               if (!mir_is_null(n->type)) {
                  col += mir_dump_value(mu, result, cb, ctx);
                  col += printf(" := ");
               }
               col += printf("%s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               for (int i = 2; i < n->nargs; i++) {
                  col += printf("%s ", i > 2 ? "," : "");
                  col += mir_dump_arg(mu, result, i, cb, ctx);
               }
               col += color_printf(" locus ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_WRAP:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" [");
               for (int i = 1; i < n->nargs; i += 3) {
                  if (i > 1) col += printf(", ");
                  col += mir_dump_dim(mu, result, i, cb, ctx);
               }
               col += printf("]");
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_UNWRAP:
         case MIR_OP_RESOLVED:
         case MIR_OP_LAST_VALUE:
         case MIR_OP_CAST:
         case MIR_OP_NEG:
         case MIR_OP_ABS:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_UARRAY_LEN:
         case MIR_OP_UARRAY_DIR:
         case MIR_OP_UARRAY_LEFT:
         case MIR_OP_UARRAY_RIGHT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" dim ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_LOCUS:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += color_printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_locus(n->locus);
               mir_dump_type(mu, col, n->type);
            }
            break;

         case MIR_OP_INIT_SIGNAL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += color_printf(" := %s count ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" size ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += printf(" value ");
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               col += printf(" flags ");
               col += mir_dump_arg(mu, result, 3, cb, ctx);
               col += printf(" locus ");
               col += mir_dump_arg(mu, result, 4, cb, ctx);
               if (n->nargs > 5) {
                  col += printf(" offset ");
                  col += mir_dump_arg(mu, result, 5, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_IMPLICIT_SIGNAL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += color_printf(" := %s count ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" size ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += printf(" locus ");
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               col += printf(" kind ");
               col += mir_dump_arg(mu, result, 3, cb, ctx);
               col += printf(" closure ");
               col += mir_dump_arg(mu, result, 4, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_PACKAGE_INIT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += color_printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               if (n->nargs > 1) {
                  col += printf(" context ");
                  col += mir_dump_value(mu, n->args[1], cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_SCHED_WAVEFORM:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" values ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" reject ");
               mir_dump_arg(mu, result, 3, cb, ctx);
               printf(" after ");
               mir_dump_arg(mu, result, 4, cb, ctx);
            }
            break;

         case MIR_OP_DRIVE_SIGNAL:
         case MIR_OP_RELEASE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
               printf(" count ");
               mir_dump_value(mu, n->args[1], cb, ctx);
            }
            break;

         case MIR_OP_DISCONNECT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" reject ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" after ");
               mir_dump_arg(mu, result, 3, cb, ctx);
            }
            break;

         case MIR_OP_EVENT:
         case MIR_OP_ACTIVE:
         case MIR_OP_DRIVING:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" count ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_ASSERT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, mir_get_arg(mu, result, 0), cb, ctx);
               mir_value_t msg = mir_get_arg(mu, result, 2);
               if (!mir_is_null(msg)) {
                  printf(" report ");
                  mir_dump_value(mu, msg, cb, ctx);
                  printf(" length ");
                  mir_dump_value(mu, mir_get_arg(mu, result, 3), cb, ctx);
               }
               printf(" severity ");
               mir_dump_value(mu, mir_get_arg(mu, result, 1), cb, ctx);
               printf(" locus ");
               mir_dump_value(mu, mir_get_arg(mu, result, 4), cb, ctx);
               if (n->nargs > 5) {
                  printf(" hint ");
                  mir_dump_value(mu, mir_get_arg(mu, result, 5), cb, ctx);
                  printf(" ");
                  mir_dump_value(mu, mir_get_arg(mu, result, 6), cb, ctx);
               }
            }
            break;

         case MIR_OP_REPORT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, mir_get_arg(mu, result, 1), cb, ctx);
               printf(" length ");
               mir_dump_value(mu, mir_get_arg(mu, result, 2), cb, ctx);
               printf(" severity ");
               mir_dump_value(mu, mir_get_arg(mu, result, 0), cb, ctx);
               printf(" locus ");
               mir_dump_value(mu, mir_get_arg(mu, result, 3), cb, ctx);
            }
            break;

         case MIR_OP_CONTEXT_UPREF:
         case MIR_OP_ADDRESS_OF:
         case MIR_OP_LINK_PACKAGE:
         case MIR_OP_ALL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_LINK_VAR:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               ident_t name = mir_get_name(mu, n->args[2]);
               col += color_printf(" $magenta$%s$$", istr(name));
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_VAR_UPREF:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               ident_t link = mir_get_name(mu, n->args[1]);
               if (link == NULL)
                  col += color_printf(", $red$invalid$$");
               else {
                  mir_shape_t *s = mir_get_shape(mu->context, link);
                  ident_t name = s->slots[n->args[2].id].name;
                  col += color_printf(", $magenta$%s$$", istr(name));
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_ARRAY_REF:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" offset ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_RECORD_REF:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" field ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONST_ARRAY:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += mir_dump_const_array(mu, result, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONST_REP:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := const [");
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf("]*");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONST_RECORD:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := const {");
               for (int i = 0; i < n->nargs; i++) {
                  mir_value_t elt = mir_get_arg(mu, result, i);
                  if (i > 0) col += printf(",");
                  col += mir_dump_value(mu, elt, cb, ctx);
               }
               col += printf("}");
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_INDEX_CHECK:
         case MIR_OP_RANGE_CHECK:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" left ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" right ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" dir ");
               mir_dump_arg(mu, result, 3, cb, ctx);
               printf(" locus ");
               mir_value_t locus = mir_get_arg(mu, result, 4);
               mir_dump_value(mu, locus, cb, ctx);
               mir_value_t hint = mir_get_arg(mu, result, 5);
               if (mir_equals(locus, hint)) {
                  printf(" hint ");
                  mir_dump_value(mu, hint, cb, ctx);
               }
            }
            break;

         case MIR_OP_DIR_CHECK:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" == ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" locus ");
               mir_dump_arg(mu, result, 2, cb, ctx);
            }
            break;

         case MIR_OP_LENGTH_CHECK:
            {
               printf("%s left ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" == right ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" locus ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               if (n->nargs > 3) {
                  printf(" dim ");
                  mir_dump_arg(mu, result, 3, cb, ctx);
               }
            }
            break;

         case MIR_OP_NULL_CHECK:
         case MIR_OP_ZERO_CHECK:
         case MIR_OP_EXPONENT_CHECK:
         case MIR_OP_ALIAS_SIGNAL:
         case MIR_OP_TRAP_NEG:
         case MIR_OP_PROCESS_INIT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" locus ");
               mir_dump_arg(mu, result, 1, cb, ctx);
            }
            break;

         case MIR_OP_SCHED_EVENT:
         case MIR_OP_CLEAR_EVENT:
            {
               printf("%s on ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               if (n->nargs > 1) {
                  printf(" count " );
                  mir_dump_arg(mu, result, 1, cb, ctx);
               }
            }
            break;

         case MIR_OP_RANGE_LENGTH:
         case MIR_OP_RANGE_NULL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s left ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" right ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += printf(" dir ");
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_NULL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s", mir_op_string(n->op));
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_MAP_SIGNAL:
         case MIR_OP_MAP_CONST:
         case MIR_OP_MAP_IMPLICIT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" to ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 2, cb, ctx);
            }
            break;

         case MIR_OP_CLOSURE:
         case MIR_OP_PROTECTED_INIT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" context ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               if (n->nargs >= 4) {
                  col += printf(" path ");
                  col += mir_dump_arg(mu, result, 2, cb, ctx);
                  col += printf(" instance ");
                  col += mir_dump_arg(mu, result, 3, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_RESOLVE_SIGNAL:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" resolution ");
               mir_dump_arg(mu, result, 1, cb, ctx);
            }
            break;

         case MIR_OP_RESOLUTION_WRAPPER:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" nlits ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_TRANSFER_SIGNAL:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" to ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" reject ");
               mir_dump_arg(mu, result, 3, cb, ctx);
               printf(" after ");
               mir_dump_arg(mu, result, 4, cb, ctx);
            }
            break;

         case MIR_OP_FILE_OPEN:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" name ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" length ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" kind ");
               mir_dump_arg(mu, result, 3, cb, ctx);
               if (n->nargs > 4) {
                  printf(" status ");
                  mir_dump_arg(mu, result, 4, cb, ctx);
               }
            }
            break;

         case MIR_OP_FILE_READ:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" ptr ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               if (n->nargs > 2) {
                  printf(" inlen ");
                  mir_dump_arg(mu, result, 2, cb, ctx);
                  if (n->nargs > 3) {
                     printf(" outlen ");
                     mir_dump_arg(mu, result, 3, cb, ctx);
                  }
               }
            }
            break;

         case MIR_OP_FILE_WRITE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" value ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               if (n->nargs > 2) {
                  printf(" length ");
                  mir_dump_arg(mu, result, 2, cb, ctx);
               }
            }
            break;

         case MIR_OP_PORT_CONVERSION:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               if (n->nargs > 1) {
                  col += printf(" effective ");
                  col += mir_dump_arg(mu, result, 1, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_CONVERT_IN:
         case MIR_OP_CONVERT_OUT:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" signal ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 2, cb, ctx);
            }
            break;

         case MIR_OP_PUT_CONVERSION:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" signal ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 2, cb, ctx);
               printf(" values ");
               mir_dump_arg(mu, result, 3, cb, ctx);
            }
            break;

         case MIR_OP_FORCE:
         case MIR_OP_DEPOSIT_SIGNAL:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" count ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               printf(" values ");
               mir_dump_arg(mu, result, 2, cb, ctx);
            }
            break;

         case MIR_OP_UNREACHABLE:
         case MIR_OP_POP_SCOPE:
            {
               printf("%s", mir_op_string(n->op));
               if (n->nargs > 0) {
                  printf(" ");
                  mir_dump_arg(mu, result, 0, cb, ctx);
               }
            }
            break;

         case MIR_OP_BIND_FOREIGN:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_arg(mu, result, 0, cb, ctx);
               printf(" length ");
               mir_dump_arg(mu, result, 1, cb, ctx);
               if (n->nargs > 2) {
                  printf(" locus ");
                  mir_dump_arg(mu, result, 2, cb, ctx);
               }
            }
            break;

         case MIR_OP_DRIVING_VALUE:
         case MIR_OP_LAST_ACTIVE:
         case MIR_OP_LAST_EVENT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               if (n->nargs > 1) {
                  col += printf(" count ");
                  col += mir_dump_value(mu, n->args[1], cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_BIND_EXTERNAL:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" scope ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }

         case MIR_OP_COVER_STMT:
         case MIR_OP_COVER_BRANCH:
         case MIR_OP_COVER_EXPR:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
            }
            break;

         case MIR_OP_COVER_TOGGLE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[1], cb, ctx);
               printf(" signal ");
               mir_dump_value(mu, n->args[0], cb, ctx);
            }
            break;

         case MIR_OP_COVER_STATE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[2], cb, ctx);
               printf(" signal ");
               mir_dump_value(mu, n->args[0], cb, ctx);
               printf(" low ");
               mir_dump_value(mu, n->args[1], cb, ctx);
            }
            break;

         case MIR_OP_PACKAGE_SCOPE:
         case MIR_OP_ARRAY_SCOPE:
         case MIR_OP_RECORD_SCOPE:
            {
               col += printf("%s locus ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               mir_dump_type(mu, col, n->type);
            }
            break;

         case MIR_OP_OR_TRIGGER:
         case MIR_OP_CMP_TRIGGER:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               if (n->op == MIR_OP_OR_TRIGGER)
                  col += printf(" || ");
               else
                  col += printf(" == ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_LEVEL_TRIGGER:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_value(mu, n->args[0], cb, ctx);
               col += printf(" count ");
               col += mir_dump_value(mu, n->args[1], cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_ENTER_STATE:
            {
               printf("%s ", mir_op_string(n->op));
               mir_dump_value(mu, n->args[0], cb, ctx);
               if (n->nargs > 1) {
                  printf(" strong ");
                  mir_dump_value(mu, n->args[1], cb, ctx);
               }
            }
            break;

         case MIR_OP_REFLECT_VALUE:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" context ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += printf(" locus ");
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               if (n->nargs > 3) {
                  col += printf(" bounds ");
                  col += mir_dump_arg(mu, result, 3, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_REFLECT_SUBTYPE:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s context ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" locus ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               if (n->nargs > 2) {
                  col += printf(" bounds ");
                  col += mir_dump_arg(mu, result, 2, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_UNPACK:
            {
               if (!mir_is_null(n->type)) {
                  col += mir_dump_value(mu, result, cb, ctx);
                  col += printf(" := ");
               }
               col += printf("%s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" strength ");
               col += mir_dump_strength(mu, mir_get_arg(mu, result, 1));
               if (n->nargs > 2) {
                  col += printf(" into ");
                  col += mir_dump_arg(mu, result, 2, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_PACK:
         case MIR_OP_TEST:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_BINARY:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := vector ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += mir_dump_vector_op(mu, n->args[0]);
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_UNARY:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := vector");
               col += mir_dump_vector_op(mu, n->args[0]);
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

         case MIR_OP_INSERT:
            {
               col += mir_dump_value(mu, result, cb, ctx);
               col += printf(" := %s ", mir_op_string(n->op));
               col += mir_dump_arg(mu, result, 0, cb, ctx);
               col += printf(" into ");
               col += mir_dump_arg(mu, result, 1, cb, ctx);
               col += printf(" at ");
               col += mir_dump_arg(mu, result, 2, cb, ctx);
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;

#ifdef DEBUG
         case MIR_OP_COMMENT:
            {
               const char *str = tb_get(mu->comments) + n->args[0].id;
               color_printf("$cyan$// %s$$ ", str);
            }
            break;
#endif

         case _MIR_DELETED_OP:
            col += color_printf("$red$deleted$$");
            break;

         default:
            {
               col += color_printf("$red$0x%02x$$ ", n->op);
               for (int k = 0; k < n->nargs; k++) {
                  if (k > 0) col += printf(", ");
                  mir_value_t arg = mir_get_arg(mu, result, k);
                  col += mir_dump_value(mu, arg, cb, ctx);
               }
               mir_dump_type(mu, col, n->type);
               mir_dump_stamp(mu, n->type, n->stamp);
            }
            break;
         }

         const bool mark =
            !mir_is_null(mu->cursor.block) && i == mu->cursor.block.id
             && (j == mu->cursor.pos
                 || (mu->cursor.pos >= b->num_nodes && j == b->num_nodes - 1));

         if (mark)
            color_printf("\t $red$<----$$");

         color_printf("$$\n");

         if (cb->end_node != NULL) {
            mir_block_t b = { .tag = MIR_TAG_BLOCK, .id = i };
            (*cb->end_node)(mu, b, result, ctx);
         }
      }

      if (b->num_nodes == 0)
         color_printf("  $yellow$%2d:$$ $red$Empty basic block$$\n", i);
   }

   printf("\n");
   fflush(stdout);
}

void mir_dump(mir_unit_t *mu)
{
   const mir_annotate_t cb = {};
   mir_annotate(mu, &cb, NULL);
}

// LCOV_EXCL_STOP //////////////////////////////////////////////////////////////
