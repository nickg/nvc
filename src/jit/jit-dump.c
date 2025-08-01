//
//  Copyright (C) 2022-2025  Nick Gasson
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
#include "jit/jit-priv.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "object.h"
#include "psl/psl-node.h"
#include "vlog/vlog-node.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>

// LCOV_EXCL_START /////////////////////////////////////////////////////////////

typedef struct {
   ihash_t     *labels;
   int          next_label;
   int          next_ir;
   int          lpend;
   jit_t       *jit;
   jit_func_t  *func;
   jit_cfg_t   *cfg;
   jit_label_t  label;
   bool         cpool;
} jit_dump_t;

const char *jit_op_name(jit_op_t op)
{
   if (op >= __MACRO_BASE) {
      static const char *names[] = {
         "$COPY", "$GALLOC", "$EXIT", "$FEXP", "$EXP", "$BZERO",
         "$GETPRIV", "$PUTPRIV", "$LALLOC", "$SALLOC", "$CASE",
         "$TRIM", "$MOVE", "$MEMSET", "$REEXEC", "$SADD", "$PACK",
         "$UNPACK",
      };
      assert(op - __MACRO_BASE < ARRAY_LEN(names));
      return names[op - __MACRO_BASE];
   }
   else {
      static const char *names[] = {
         "SEND", "RECV", "ADD", "RET", "TRAP", "ULOAD", "STORE", "JUMP", "CMP",
         "CSET", "SUB", "MOV", "FADD", "MUL", "FMUL", "CALL", "NEG", "LOAD",
         "CSEL", "LEA", "NOT", "DIV", "FDIV", "SCVTF", "FNEG", "FCVTNS",
         "FCMP", "AND", "OR", "XOR", "FSUB", "REM", "DEBUG", "NOP", "ASR",
         "SHL", "CLAMP", "CCMP", "FCCMP", "SHR",
      };
      assert(op < ARRAY_LEN(names));
      return names[op];
   }
}

const char *jit_cc_name(jit_cc_t cc)
{
   static const char *names[] = {
      "", ".T", ".F", ".EQ", ".NE", ".LT", ".GE", ".GT", ".LE",
      ".O", ".NO", ".C", ".NC"
   };
   assert(cc < ARRAY_LEN(names));
   return names[cc];
}

const char *jit_exit_name(jit_exit_t exit)
{
   static const char *names[] = {
      "INDEX_FAIL", "OVERFLOW", "NULL_DEREF", "LENGTH_FAIL", "UNREACHABLE",
      "DIV_ZERO", "EXPONENT_FAIL", "REPORT", "ASSERT_FAIL", "RANGE_FAIL",
      "FUNC_WAIT", "INIT_SIGNAL", "DRIVE_SIGNAL", "SCHED_WAVEFORM",
      "SCHED_PROCESS", "TEST_EVENT", "TEST_ACTIVE", "SCHED_EVENT",
      "FILE_OPEN", "FILE_READ", "FILE_WRITE", "DEBUG_OUT",
      "ALIAS_SIGNAL", "MAP_SIGNAL", "MAP_CONST", "RESOLVE_SIGNAL",
      "LAST_EVENT", "LAST_ACTIVE", "DISCONNECT", "FORCE",
      "RELEASE", "PUSH_SCOPE", "POP_SCOPE", "IMPLICIT_SIGNAL", "DRIVING",
      "DRIVING_VALUE", "CLAIM_TLAB", "COVER_TOGGLE", "PROCESS_INIT",
      "CLEAR_EVENT", "ENTER_STATE", "REFLECT_VALUE", "REFLECT_SUBTYPE",
      "FUNCTION_TRIGGER", "ADD_TRIGGER", "TRANSFER_SIGNAL",
      "PORT_CONVERSION", "CONVERT_IN", "CONVERT_OUT", "BIND_FOREIGN",
      "OR_TRIGGER", "CMP_TRIGGER", "INSTANCE_NAME", "DEPOSIT_SIGNAL",
      "MAP_IMPLICIT", "BIND_EXTERNAL", "SYSCALL", "PUT_CONVERSION",
      "DIR_FAIL", "LEVEL_TRIGGER", "ENABLE_TRIGGER", "DISABLE_TRIGGER",
   };
   assert(exit < ARRAY_LEN(names));
   return names[exit];
}

static int jit_dump_label(jit_dump_t *d, jit_label_t label)
{
   void *map = ihash_get(d->labels, label);
   if (map == NULL)
      ihash_put(d->labels, label, (map = (void *)(uintptr_t)++(d->next_label)));

   return printf("L%d", (int)(uintptr_t)map);
}

static int jit_dump_reg(jit_reg_t reg)
{
   if (reg == JIT_REG_INVALID)
      return printf("RINVALID");
   else
      return printf("R%d", reg);
}

static int jit_dump_value(jit_dump_t *d, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      return jit_dump_reg(value.reg);
   case JIT_VALUE_INT64:
      if (value.int64 < 4096)
         return printf("#%"PRIi64, value.int64);
      else
         return printf("#0x%"PRIx64, value.int64);
   case JIT_VALUE_DOUBLE:
      return printf("%%%g", value.dval);
   case JIT_ADDR_CPOOL:
      return printf("[CP+%"PRIi64"]", value.int64);
   case JIT_ADDR_REG:
      {
         int num = printf("[");
         num += jit_dump_reg(value.reg);
         if (value.disp != 0)
            num += printf("+%d", value.disp);
         num += printf("]");
         return num;
      }
   case JIT_ADDR_ABS:
      return printf("[#%016"PRIx64"]", value.int64);
   case JIT_ADDR_COVER:
      return printf("[$TAG:%"PRIi64"]", value.int64);
   case JIT_VALUE_LABEL:
      if (value.label == JIT_LABEL_INVALID)
         return printf("???");
      else
         return jit_dump_label(d, value.label);
   case JIT_VALUE_HANDLE:
      if (value.handle == JIT_HANDLE_INVALID)
         return printf("<\?\?\?>");
      else
         return printf("<%s>", istr(jit_get_func(d->jit, value.handle)->name));
   case JIT_VALUE_EXIT:
      return printf("%s", jit_exit_name(value.exit));
   case JIT_VALUE_LOC:
      return printf("<%s:%d>", loc_file_str(&value.loc), value.loc.first_line);
   case JIT_VALUE_LOCUS:
      {
         object_t *obj = value.locus;
         switch (obj->tag) {
         case OBJECT_TAG_TREE:
            return printf("%s@%p", tree_kind_str(obj->kind), obj);
         case OBJECT_TAG_VLOG:
            return printf("%s@%p", vlog_kind_str(obj->kind), obj);
         case OBJECT_TAG_PSL:
            return printf("%s@%p", psl_kind_str(obj->kind), obj);
         default:
            return printf("%d@%p", obj->kind, obj);
         }
      }
   case JIT_VALUE_VPOS:
      return printf("%u:%u", value.vpos.block, value.vpos.op);
   case JIT_VALUE_INVALID:
      return printf("???");
   }

   return 0;
}

static void jit_dump_regset(jit_dump_t *d, bit_mask_t *m)
{
   printf("{");
   for (int i = 0, nth = 0; i < d->func->nregs + 1; i++) {
      if (mask_test(m, i)) {
         if (nth++ > 0)
            fputc(',', stdout);
         if (i == d->func->nregs)
            fputs("FLAGS", stdout);
         else
            printf("R%d", i);
      }
   }
   printf("}");
}

static void jit_dump_ir(jit_dump_t *d, jit_ir_t *ir)
{
   if (d->cfg != NULL) {
      const int pos = ir - d->func->irbuf;
      jit_block_t *bb = jit_block_for(d->cfg, pos);
      if (pos == bb->first) {
         printf("\t;; BB%"PRIiPTR, bb - d->cfg->blocks);

         if (bb->in.count > 0) {
            printf(" in:");
            for (int i = 0; i < bb->in.count; i++)
               printf("%s%d", i > 0 ? "," : "", jit_get_edge(&bb->in, i));

            if (mask_popcount(&bb->livein) > 0) {
               printf(" ");
               jit_dump_regset(d, &bb->livein);
            }
         }

         if (bb->out.count > 0) {
            printf(" out:");
            for (int i = 0; i < bb->out.count; i++)
               printf("%s%d", i > 0 ? "," : "", jit_get_edge(&bb->out, i));

            if (mask_popcount(&bb->liveout) > 0) {
               printf(" ");
               jit_dump_regset(d, &bb->liveout);
            }
         }

         if (mask_popcount(&bb->varkill) > 0) {
            printf(" kill:");
            jit_dump_regset(d, &bb->varkill);
         }

         if (bb->aborts)
            printf(" aborts");
         else if (bb->returns)
            printf(" returns");

         printf("\n");
      }
   }

   int col = 0;
   col += printf("\t%s%s", jit_op_name(ir->op), jit_cc_name(ir->cc));
   if (ir->size != JIT_SZ_UNSPEC)
      col += printf(".%d", 1 << (3 + ir->size));

   col += printf("%*.s", (int)MAX(0, 15 - col), "");

   if (ir->result != JIT_REG_INVALID)
      col += printf("R%d", ir->result);

   if (ir->arg1.kind != JIT_VALUE_INVALID) {
      if (ir->result != JIT_REG_INVALID) col += printf(", ");
      col += jit_dump_value(d, ir->arg1);

      if (ir->arg2.kind != JIT_VALUE_INVALID) {
         col += printf(", ");
         col += jit_dump_value(d, ir->arg2);
      }
   }

   printf("\n");
}

static void jit_do_dump(jit_dump_t *d, jit_func_t *f)
{
   printf("------------------------------------------------------------\n");
   printf("%s:\n", istr(f->name));

   if (f->framesz > 0)
      printf("\t;; Frame size: %u bytes\n", f->framesz);

   d->labels = ihash_new(128);

   for (int i = 0; i < f->nirs; i++) {
      if (i == d->label)
         color_printf("$!red$");
      if (f->irbuf[i].target) {
         jit_dump_label(d, i);
         printf(":");
      }
      jit_dump_ir(d, &(f->irbuf[i]));
      if (i == d->label)
         color_printf("$$");
   }

   ihash_free(d->labels);

   if (d->cpool && f->cpoolsz > 0) {
      printf("\n");
      jit_hexdump(f->cpool, f->cpoolsz, 16, NULL, "\t");
   }

   printf("------------------------------------------------------------\n\n");
   fflush(stdout);
}

void jit_dump_with_mark(jit_func_t *f, jit_label_t label)
{
   jit_dump_t d = {
      .jit   = f->jit,
      .func  = f,
      .label = label,
   };
   jit_do_dump(&d, f);
}

void jit_dump_with_cfg(jit_func_t *f, jit_cfg_t *cfg)
{
   jit_dump_t d = {
      .jit   = f->jit,
      .func  = f,
      .label = JIT_LABEL_INVALID,
      .cfg   = cfg,
   };
   jit_do_dump(&d, f);
}

void jit_dump(jit_func_t *f)
{
   jit_dump_t d = {
      .jit   = f->jit,
      .func  = f,
      .label = JIT_LABEL_INVALID,
      .cpool = true,
   };
   jit_do_dump(&d, f);
}

static void jit_interleaved_cb(mir_unit_t *mu, mir_block_t b, mir_value_t n,
                               void *ctx)
{
   jit_dump_t *d = ctx;

   for (; d->next_ir < d->func->nirs; d->next_ir++) {
      jit_ir_t *ir = &(d->func->irbuf[d->next_ir]);
      if (ir->op == J_DEBUG) {
         if (ir->target)
            d->lpend = d->next_ir;
         if (mir_is_null(b) || mir_is_null(n))
            continue;   // Flush
         else if (ir->arg2.vpos.block != b.id || ir->arg2.vpos.op != n.id)
            break;
      }
      else {
         color_printf("$#42$");
         if (ir->target || d->lpend >= 0) {
            jit_dump_label(d, d->lpend < 0 ? d->next_ir : d->lpend);
            printf(":");
            d->lpend = -1;
         }
         jit_dump_ir(d, ir);
         color_printf("$$");
      }
   }
}

static const mir_annotate_t jit_annot = {
   .end_node = jit_interleaved_cb,
};

void jit_dump_interleaved(jit_func_t *f, mir_unit_t *mu)
{
   jit_dump_t d = {
      .labels = ihash_new(128),
      .jit    = f->jit,
      .func   = f,
      .lpend  = -1,
   };

   mir_set_cursor(mu, MIR_NULL_BLOCK, 0);
   mir_annotate(mu, &jit_annot, &d);

   jit_interleaved_cb(mu, MIR_NULL_BLOCK, MIR_NULL_VALUE, &d);

   ihash_free(d.labels);

   if (f->cpoolsz > 0) {
      jit_hexdump(f->cpool, f->cpoolsz, 16, NULL, "\t");
      printf("\n");
   }
}

__attribute__((no_sanitize_address))
void jit_hexdump(const unsigned char *data, size_t sz, int blocksz,
                 const void *highlight, const char *prefix)
{
   for (int i = 0, ndups = 0; i < sz; i += blocksz) {
      if (i > 0 && i + blocksz < sz) {
         bool dup = true;
         for (int j = 0; j < blocksz; j++) {
            if (data[i + j] != data[i - blocksz + j]) {
               dup = false;
               break;
            }
         }

         if (dup) {
            ndups++;
            continue;
         }
      }

      if (ndups > 0) {
         printf("%s\t... skipped %d identical bytes ...\n",
                prefix, ndups * blocksz);
         ndups = 0;
      }

      fputs(prefix, stdout);

      for (int j = 0; j < blocksz; j++) {
         if (i + j < sz) {
            if (highlight != NULL)
               fputc(&(data[i + j]) == highlight ? '>' : ' ', stdout);
            printf("%02x", data[i + j]);
            if (highlight == NULL)
               fputc(' ', stdout);
         }
         else
            printf("   ");
         if (j % 4 == 3)
            printf(" ");
      }
      printf("|");
      for (int j = 0; j < blocksz; j++) {
         if (i + j < sz) {
            const int byte = data[i + j];
            printf("%c", isprint(byte) ? byte : '.');
         }
         else
            printf(" ");
      }
      printf("|\n");
   }
}

// LCOV_EXCL_STOP //////////////////////////////////////////////////////////////
