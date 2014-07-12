//
//  Copyright (C) 2011-2014  Nick Gasson
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

#ifndef _RT_H
#define _RT_H

#include "ident.h"
#include "prim.h"

#include <stdint.h>

typedef struct watch watch_t;

typedef void (*sig_event_fn_t)(uint64_t, tree_t, watch_t *, void *user);

typedef enum {
   BOUNDS_ARRAY_TO,
   BOUNDS_ARRAY_DOWNTO,
   BOUNDS_ENUM,
   BOUNDS_TYPE_TO,
   BOUNDS_TYPE_DOWNTO,
   BOUNDS_ARRAY_SIZE,
   BOUNDS_INDEX_TO,
   BOUNDS_INDEX_DOWNTO,
} bounds_kind_t;

typedef enum {
   BIT_SHIFT_SLL,
   BIT_SHIFT_SRL,
   BIT_SHIFT_SLA,
   BIT_SHIFT_SRA,
   BIT_SHIFT_ROL,
   BIT_SHIFT_ROR,
} bit_shift_kind_t;

typedef enum {
   BIT_VEC_NOT,
   BIT_VEC_AND,
   BIT_VEC_OR,
   BIT_VEC_XOR,
   BIT_VEC_XNOR,
   BIT_VEC_NAND,
   BIT_VEC_NOR
} bit_vec_op_kind_t;

typedef enum {
   NET_F_ACTIVE = (1 << 0),
   NET_F_EVENT  = (1 << 1)
} net_flags_t;

typedef enum {
   SCHED_SEQUENTIAL = (1 << 0),
   SCHED_STATIC     = (1 << 1)
} sched_flags_t;

void rt_batch_exec(tree_t e, uint64_t stop_time, tree_rd_ctx_t ctx,
                   const char *vhpi_plugins);
void rt_slave_exec(tree_t e, tree_rd_ctx_t ctx);
watch_t *rt_set_event_cb(tree_t s, sig_event_fn_t fn, void *user);
size_t rt_signal_value(watch_t *w, uint64_t *buf, size_t max, bool last);
size_t rt_string_value(watch_t *w, const char *map, char *buf, size_t max);
uint64_t rt_now(unsigned *deltas);

void jit_init(ident_t top);
void jit_shutdown(void);
void *jit_fun_ptr(const char *name, bool required);
void *jit_var_ptr(const char *name, bool required);
void jit_bind_fn(const char *name, void *ptr);

void shell_run(struct tree *e, struct tree_rd_ctx *ctx);

text_buf_t *pprint(struct tree *t, const uint64_t *values, size_t len);

void vcd_init(const char *file, struct tree *top);
void vcd_restart(void);

void lxt_init(const char *file, struct tree *top);
void lxt_restart(void);

void fst_init(const char *file, tree_t top);
void fst_restart(void);

void wave_include_glob(const char *glob);
void wave_exclude_glob(const char *glob);
void wave_include_file(const char *base);
bool wave_should_dump(tree_t decl);

#endif  // _RT_H
