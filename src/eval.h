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

#ifndef _EVAL_H
#define _EVAL_H

#include "prim.h"
#include "phase.h"

typedef struct _eval_frame eval_frame_t;

typedef union {
   int64_t integer;
   double  real;
} eval_scalar_t;

typedef enum {
   EVAL_BOUNDS  = (1 << 0),
   EVAL_FCALL   = (1 << 1),
   EVAL_WARN    = (1 << 2),
   EVAL_VERBOSE = (1 << 3),
   EVAL_REPORT  = (1 << 4)
} eval_flags_t;

typedef vcode_unit_t (*lower_fn_t)(ident_t, void *);

eval_t *eval_new(eval_flags_t flags);
void eval_free(eval_t *ex);
eval_frame_t *eval_link(eval_t *ex, ident_t ident);
eval_scalar_t eval_call(eval_t *ex, ident_t func, eval_frame_t *context,
                        const char *fmt, ...);
bool eval_try_call(eval_t *ex, ident_t func, eval_frame_t *context,
                   eval_scalar_t *result, const char *fmt, ...);
tree_t eval_fold(eval_t *ex, tree_t expr, vcode_unit_t thunk);
eval_scalar_t eval_get_frame_var(eval_t *ex, eval_frame_t *frame, unsigned nth);
eval_flags_t eval_get_flags(eval_t *ex);
void eval_set_lower_fn(eval_t *ex, lower_fn_t fn, void *ctx);

#endif  // _EVAL_H
