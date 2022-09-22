//
//  Copyright (C) 2022  Nick Gasson
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

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>

typedef struct {
   ihash_t    *labels;
   int         next_label;
   jit_t      *jit;
   jit_func_t *func;
} jit_dump_t;

const char *jit_op_name(jit_op_t op)
{
   if (op >= __MACRO_BASE) {
      static const char *names[] = {
         "$COPY", "$GALLOC", "$EXIT", "$FEXP", "$EXP", "$BZERO",
         "$FFICALL", "$GETPRIV", "$PUTPRIV",
      };
      assert(op - __MACRO_BASE < ARRAY_LEN(names));
      return names[op - __MACRO_BASE];
   }
   else {
      static const char *names[] = {
         "SEND", "RECV", "ADD", "RET", "TRAP", "ULOAD", "STORE", "JUMP", "CMP",
         "CSET", "SUB", "MOV", "FADD", "MUL", "FMUL", "CALL", "NEG", "LOAD",
         "CSEL", "LEA", "NOT", "DIV", "FDIV", "SCVTF", "FNEG", "FCVTNS",
         "FCMP", "AND", "OR", "XOR", "FSUB", "REM",
      };
      assert(op < ARRAY_LEN(names));
      return names[op];
   }
}

const char *jit_exit_name(jit_exit_t exit)
{
   static const char *names[] = {
      "INDEX_FAIL", "OVERFLOW", "NULL_DEREF", "LENGTH_FAIL", "UNREACHABLE",
      "DIV_ZERO", "EXPONENT_FAIL", "REPORT", "ASSERT_FAIL", "INT_TO_STRING",
      "REAL_TO_STRING", "RANGE_FAIL", "FUNC_WAIT", "INIT_SIGNAL",
      "DRIVE_SIGNAL", "SCHED_WAVEFORM", "SCHED_PROCESS", "TEST_EVENT",
      "TEST_ACTIVE", "INIT_SIGNALS", "SCHED_EVENT", "NOW", "FILE_OPEN",
      "FILE_CLOSE", "FILE_READ", "FILE_WRITE", "ENDFILE", "FILE_FLUSH",
      "STRCONVI", "STRCONVR", "CANON_VALUE", "DEBUG_OUT",
      "ALIAS_SIGNAL", "MAP_SIGNAL", "MAP_CONST"
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

static int jit_dump_value(jit_dump_t *d, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      return printf("R%d", value.reg);
   case JIT_VALUE_INT64:
      if (value.int64 < 4096)
         return printf("#%"PRIi64, value.int64);
      else
         return printf("#0x%"PRIx64, value.int64);
   case JIT_VALUE_DOUBLE:
      return printf("%%%g", value.dval);
   case JIT_ADDR_FRAME:
      return printf("[FP+%"PRIi64"]", value.int64);
   case JIT_ADDR_CPOOL:
      return printf("[CP+%"PRIi64"]", value.int64);
   case JIT_ADDR_REG:
      {
         int num = printf("[R%d", value.reg);
         if (value.disp != 0)
            num += printf("+%d", value.disp);
         num += printf("]");
         return num;
      }
   case JIT_ADDR_ABS:
      return printf("[#%016"PRIx64"]", value.int64);
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
   case JIT_VALUE_INVALID:
      return printf("???");
   }

   return 0;
}

static void jit_dump_ir(jit_dump_t *d, jit_ir_t *ir)
{
   int col = 0;
   col += printf("\t%s", jit_op_name(ir->op));
   switch (ir->cc) {
   case JIT_CC_NONE: break;
   case JIT_CC_T:  col += printf(".T"); break;
   case JIT_CC_F:  col += printf(".F"); break;
   case JIT_CC_EQ: col += printf(".EQ"); break;
   case JIT_CC_NE: col += printf(".NE"); break;
   case JIT_CC_LT: col += printf(".LT"); break;
   case JIT_CC_GT: col += printf(".GT"); break;
   case JIT_CC_LE: col += printf(".LE"); break;
   case JIT_CC_GE: col += printf(".GE"); break;
   case JIT_CC_O:  col += printf(".O");  break;
   case JIT_CC_NO: col += printf(".NO"); break;
   case JIT_CC_C:  col += printf(".C");  break;
   case JIT_CC_NC: col += printf(".NC"); break;
   }
   if (ir->size != JIT_SZ_UNSPEC)
      col += printf(".%d", 1 << (2 + ir->size));

   while (col < 15)
      col += printf(" ");

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

   jit_cfg_t *cfg = d->func->cfg;
   if (cfg != NULL) {
      const int pos = ir - d->func->irbuf;
      jit_block_t *bb = jit_block_for(cfg, pos);
      if (pos == bb->first) {
         printf(" %*c;; BB%"PRIiPTR, MAX(40 - col, 0), ' ',
                bb - d->func->cfg->blocks);

         if (bb->in.count > 0) {
            printf(" in:");
            for (int i = 0; i < bb->in.count; i++)
               printf("%s%d", i > 0 ? "," : "", bb->in.edges[i]);
         }

         if (bb->out.count > 0) {
            printf(" out:");
            for (int i = 0; i < bb->out.count; i++)
               printf("%s%d", i > 0 ? "," : "", bb->out.edges[i]);
         }

         if (bb->aborts)
            printf(" aborts");
         else if (bb->returns)
            printf(" returns");
      }
   }

   printf("\n");
}

void jit_dump_with_mark(jit_func_t *f, jit_label_t label, bool cpool)
{
   printf("------------------------------------------------------------\n");
   printf("%s:\n", istr(f->name));

   if (f->framesz > 0)
      printf("\t;; Frame size: %u bytes\n", f->framesz);

   jit_dump_t d = {
      .labels = ihash_new(128),
      .jit    = f->jit,
      .func   = f,
   };

   for (int i = 0; i < f->nirs; i++) {
      if (i == label)
         color_printf("$!red$");
      if (f->irbuf[i].target) {
         jit_dump_label(&d, i);
         printf(":");
      }
      jit_dump_ir(&d, &(f->irbuf[i]));
      if (i == label)
         color_printf("$$");
   }

   ihash_free(d.labels);

   if (cpool && f->cpoolsz > 0) {
      printf("\n");
      jit_hexdump(f->cpool, f->cpoolsz, 16, NULL, "\t");
   }

   printf("------------------------------------------------------------\n\n");
}

void jit_dump(jit_func_t *f)
{
   jit_dump_with_mark(f, JIT_LABEL_INVALID, true);
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
