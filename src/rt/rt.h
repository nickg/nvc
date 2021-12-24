//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "loc.h"

#include <stdint.h>

#define RT_ABI_VERSION 4

typedef struct rt_watch_s  rt_watch_t;
typedef struct rt_signal_s rt_signal_t;

typedef void (*sig_event_fn_t)(uint64_t now, rt_signal_t *signal,
                               rt_watch_t *watch, void *user);
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
   BOUNDS_PARAM_SIZE,
} bounds_kind_t;

typedef enum {
   IMAGE_INTEGER,
   IMAGE_REAL,
   IMAGE_ENUM,
   IMAGE_PHYSICAL,
} image_kind_t;

typedef enum {
   R_MEMO      = (1 << 0),
   R_IDENT     = (1 << 1),
   R_COMPOSITE = (1 << 2),
} res_flags_t;

typedef enum {
   RT_START_OF_SIMULATION,
   RT_END_OF_SIMULATION,
   RT_END_OF_PROCESSES,
   RT_LAST_KNOWN_DELTA_CYCLE,
   RT_NEXT_TIME_STEP,

   RT_LAST_EVENT
} rt_event_t;

typedef enum {
   SEVERITY_NOTE,
   SEVERITY_WARNING,
   SEVERITY_ERROR,
   SEVERITY_FAILURE
} rt_severity_t;

typedef enum {
   FST_OUTPUT_FST,
   FST_OUTPUT_VCD
} fst_output_t;

void rt_start_of_tool(tree_t top, e_node_t e);
void rt_end_of_tool(tree_t top, e_node_t e);
void rt_run_sim(uint64_t stop_time);
void rt_run_interactive(uint64_t stop_time);
void rt_restart(e_node_t top);
void rt_set_timeout_cb(uint64_t when, timeout_fn_t fn, void *user);
rt_watch_t *rt_set_event_cb(rt_signal_t *s, sig_event_fn_t fn, void *user,
                         bool postponed);
void rt_set_global_cb(rt_event_t event, rt_event_fn_t fn, void *user);
size_t rt_watch_value(rt_watch_t *w, uint64_t *buf, size_t max);
size_t rt_watch_string(rt_watch_t *w, const char *map, char *buf, size_t max);
size_t rt_signal_value(rt_signal_t *s, uint64_t *buf, size_t max);
size_t rt_signal_string(rt_signal_t *s, const char *map, char *buf, size_t max);
bool rt_force_signal(rt_signal_t *s, const uint64_t *buf, size_t count,
                     bool propagate);
rt_signal_t *rt_find_signal(e_node_t esignal);
bool rt_can_create_delta(void);
uint64_t rt_now(unsigned *deltas);
void rt_stop(void);
void rt_set_exit_severity(rt_severity_t severity);

void jit_init(e_node_t top);
void jit_shutdown(void);
void *jit_find_symbol(const char *name, bool required);

text_buf_t *pprint(tree_t t, const uint64_t *values, size_t len);

void fst_init(const char *file, tree_t top, fst_output_t output);
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
