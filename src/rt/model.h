//
//  Copyright (C) 2022-2023  Nick Gasson
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

#ifndef _RT_MODEL_H
#define _RT_MODEL_H

#include "prim.h"
#include "rt/rt.h"

rt_model_t *model_new(tree_t top, jit_t *jit);
void model_free(rt_model_t *m);
void model_reset(rt_model_t *m);
void model_run(rt_model_t *m, uint64_t stop_time);
bool model_step(rt_model_t *m);
bool model_can_create_delta(rt_model_t *m);
int64_t model_now(rt_model_t *m, unsigned *deltas);
void model_stop(rt_model_t *m);
void model_interrupt(rt_model_t *m);

void model_set_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                         void *user);
rt_watch_t *model_set_event_cb(rt_model_t *m, rt_signal_t *s, sig_event_fn_t fn,
                               void *user, bool postponed);
void model_set_timeout_cb(rt_model_t *m, uint64_t when, rt_event_fn_t fn,
                          void *user);

void model_clear_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                           void *user);
void model_clear_event_cb(rt_model_t *m, rt_watch_t *w);
bool model_clear_timeout_cb(rt_model_t *m, uint64_t when, rt_event_fn_t fn,
                            void *user);

rt_model_t *get_model(void);
rt_model_t *get_model_or_null(void);
rt_proc_t *get_active_proc(void);
cover_tagging_t *get_coverage(rt_model_t *m);

rt_scope_t *find_scope(rt_model_t *m, tree_t container);
rt_scope_t *child_scope(rt_scope_t *scope, tree_t decl);
rt_signal_t *find_signal(rt_scope_t *scope, tree_t decl);
rt_proc_t *find_proc(rt_scope_t *scope, tree_t proc);

const void *signal_value(rt_signal_t *s);
const void *signal_last_value(rt_signal_t *s);
uint32_t signal_width(rt_signal_t *s);
size_t signal_expand(rt_signal_t *s, uint64_t *buf, size_t max);
void force_signal(rt_signal_t *s, const void *values, size_t count);
void deposit_signal(rt_signal_t *s, const void *values, size_t count);

#endif  // _RT_MODEL_H
