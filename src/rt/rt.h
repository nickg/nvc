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

typedef void (*sig_event_fn_t)(uint64_t now, tree_t, watch_t *, void *user);
typedef void (*timeout_fn_t)(uint64_t now, void *user);
typedef void (*rt_event_fn_t)(void *user);

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
   NET_F_ACTIVE   = (1 << 0),
   NET_F_EVENT    = (1 << 1),
   NET_F_FORCED   = (1 << 2),
   NET_F_OWNS_MEM = (1 << 3),
   NET_F_GLOBAL   = (1 << 4)
} net_flags_t;

typedef enum {
   SCHED_SEQUENTIAL = (1 << 0),
   SCHED_STATIC     = (1 << 1)
} sched_flags_t;

typedef enum {
   RT_START_OF_SIMULATION,
   RT_END_OF_SIMULATION,
   RT_END_OF_PROCESSES,
   RT_LAST_KNOWN_DELTA_CYCLE,
   RT_NEXT_TIME_STEP,

   RT_LAST_EVENT
} rt_event_t;

void rt_start_of_tool(tree_t top, tree_rd_ctx_t ctx);
void rt_end_of_tool(tree_t top);
void rt_run_sim(uint64_t stop_time);
void rt_run_interactive(uint64_t stop_time);
void rt_restart(tree_t top);
void rt_set_timeout_cb(uint64_t when, timeout_fn_t fn, void *user);
watch_t *rt_set_event_cb(tree_t s, sig_event_fn_t fn, void *user,
                         bool postponed);
void rt_set_global_cb(rt_event_t event, rt_event_fn_t fn, void *user);
size_t rt_watch_value(watch_t *w, uint64_t *buf, size_t max, bool last);
size_t rt_watch_string(watch_t *w, const char *map, char *buf, size_t max);
size_t rt_signal_value(tree_t s, uint64_t *buf, size_t max);
size_t rt_signal_string(tree_t s, const char *map, char *buf, size_t max);
bool rt_force_signal(tree_t s, const uint64_t *buf, size_t count,
                     bool propagate);
bool rt_can_create_delta(void);
uint64_t rt_now(unsigned *deltas);
void rt_stop(void);

void jit_init(ident_t top);
void jit_shutdown(void);
void *jit_fun_ptr(const char *name, bool required);
void *jit_var_ptr(const char *name, bool required);
void jit_bind_fn(const char *name, void *ptr);

void shell_run(tree_t top, tree_rd_ctx_t ctx);

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

#ifdef ENABLE_VHPI
void vhpi_load_plugins(tree_t top, const char *plugins);
#else
#define vhpi_load_plugins(top, plugins)
#endif

#endif  // _RT_H
