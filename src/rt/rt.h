//
//  Copyright (C) 2011-2012  Nick Gasson
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

#include <stdint.h>

struct tree;
struct tree_rd_ctx;

typedef void (*sig_event_fn_t)(uint64_t, struct tree *);

void rt_batch_exec(struct tree *e, uint64_t stop_time,
                   struct tree_rd_ctx *ctx);
void rt_slave_exec(struct tree *e, struct tree_rd_ctx *ctx);
void rt_trace_en(bool en);
void rt_set_event_cb(struct tree *s, sig_event_fn_t fn);
size_t rt_signal_value(struct tree *s, uint64_t *buf, size_t max);

void jit_init(ident_t top);
void jit_shutdown(void);
void *jit_fun_ptr(const char *name);
void *jit_var_ptr(const char *name);
void jit_bind_fn(const char *name, void *ptr);

void shell_run(struct tree *e);

const char *pprint(struct tree *t, uint64_t *values, unsigned len);

void vcd_init(const char *file, struct tree *top);
void vcd_restart(void);

#endif  // _RT_H
