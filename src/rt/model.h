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

#ifndef _RT_MODEL_H
#define _RT_MODEL_H

#include "prim.h"
#include "rt/rt.h"

rt_model_t *model_new(tree_t top, jit_t *jit);
void model_free(rt_model_t *m);
void model_reset(rt_model_t *m);
void model_run(rt_model_t *m, uint64_t stop_time);
void model_finish(rt_model_t *m);
bool model_can_create_delta(rt_model_t *m);
int64_t model_now(rt_model_t *m, unsigned *deltas);
void model_stop(rt_model_t *m);
void model_interrupt(rt_model_t *m);

void model_set_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                         void *user);
rt_watch_t *model_set_event_cb(rt_model_t *m, rt_signal_t *s, sig_event_fn_t fn,
                               void *user, bool postponed);
void model_set_timeout_cb(rt_model_t *m, uint64_t when, timeout_fn_t fn,
                          void *user);

rt_model_t *get_model(void);
rt_model_t *get_model_or_null(void);
rt_proc_t *get_active_proc(void);

rt_scope_t *find_scope(rt_model_t *m, tree_t container);
rt_scope_t *child_scope(rt_scope_t *scope, tree_t decl);
rt_signal_t *find_signal(rt_scope_t *scope, tree_t decl);

const void *signal_value(rt_signal_t *s);
const void *signal_last_value(rt_signal_t *s);
uint32_t signal_size(rt_signal_t *s);
ident_t signal_name(rt_signal_t *s);
size_t signal_expand(rt_signal_t *s, int offset, uint64_t *buf, size_t max);
size_t signal_string(rt_signal_t *s, const char *map, char *buf, size_t max);
bool force_signal(rt_signal_t *s, const uint64_t *buf, size_t count);

#endif  // _RT_MODEL_H
