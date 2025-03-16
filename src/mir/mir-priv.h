//
//  Copyright (C) 2024-2025  Nick Gasson
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

#ifndef _MIR_PRIV_H
#define _MIR_PRIV_H

#include "prim.h"
#include "mir/mir-node.h"
#include "mir/mir-structs.h"

#ifdef DEBUG
#define MIR_ASSERT(expr, ...) do                                        \
      if (unlikely(!(expr))) {                                          \
         mir_dump(mu);                                                  \
         fatal_trace(__VA_ARGS__);                                      \
      } while (0)
#else
#define MIR_ASSERT(expr, ...)
#endif

typedef enum {
   UNIT_DEFERRED = 1,
   UNIT_GENERATED = 2,
   UNIT_FREED = 3,
} unit_state_t;

const type_data_t *mir_type_data(mir_unit_t *mu, mir_type_t type);
const stamp_data_t *mir_stamp_data(mir_unit_t *mu, mir_stamp_t stamp);
node_data_t *mir_node_data(mir_unit_t *mu, mir_value_t value);
block_data_t *mir_block_data(mir_unit_t *mu, mir_block_t block);
const param_data_t *mir_param_data(mir_unit_t *mu, mir_value_t param);
const var_data_t *mir_var_data(mir_unit_t *mu, mir_value_t value);

const mir_value_t *mir_get_args(mir_unit_t *mu, const node_data_t *nd);
void mir_set_arg(mir_unit_t *mu, node_data_t *n, unsigned nth,
                 mir_value_t value);

bool mir_same_type(mir_unit_t *mu, mir_type_t a, mir_type_t b);
bool mir_is_top(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp);

mir_type_t mir_get_var_pointer(mir_unit_t *mu, mir_type_t type);

mir_stamp_t mir_stamp_elem(mir_unit_t *mu, mir_stamp_t stamp);
mir_stamp_t mir_stamp_add(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right);
mir_stamp_t mir_stamp_sub(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right);
mir_stamp_t mir_stamp_mul(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right);
mir_stamp_t mir_stamp_div(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right);
mir_stamp_t mir_stamp_rem(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right);
mir_stamp_t mir_stamp_cmp(mir_unit_t *mu, mir_cmp_t cmp, mir_stamp_t left,
                          mir_stamp_t right);
mir_stamp_t mir_stamp_union(mir_unit_t *mu, mir_stamp_t left,
                            mir_stamp_t right);

bool mir_stamp_const(mir_unit_t *mu, mir_stamp_t stamp, int64_t *cval);

bool mir_is_terminator(mir_op_t op);

void mir_free_types(type_tab_t *tab);
void *mir_malloc(mir_context_t *mc, size_t fixed, size_t nelems, size_t size);

#endif  // _MIR_PRIV_H
