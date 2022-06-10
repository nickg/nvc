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

typedef enum {
   EVAL_FCALL   = (1 << 1),
   EVAL_WARN    = (1 << 2),
   EVAL_VERBOSE = (1 << 3),
} eval_flags_t;

typedef vcode_unit_t (*lower_fn_t)(ident_t, void *);

eval_t *eval_new(eval_flags_t flags);
void eval_free(eval_t *ex);
tree_t eval_try_fold(eval_t *ex, tree_t expr);
tree_t eval_must_fold(eval_t *ex, tree_t expr);
void eval_set_lower_fn(eval_t *ex, lower_fn_t fn, void *ctx);
bool eval_possible(eval_t *e, tree_t t);

#endif  // _EVAL_H
