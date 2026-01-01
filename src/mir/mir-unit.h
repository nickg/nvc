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

#ifndef _MIR_UNIT_H
#define _MIR_UNIT_H

#include "prim.h"

typedef union _mir_block mir_block_t;
typedef union _mir_value mir_value_t;

typedef enum {
   MIR_UNIT_FUNCTION,
   MIR_UNIT_PROCESS,
   MIR_UNIT_INSTANCE,
   MIR_UNIT_PROCEDURE,
   MIR_UNIT_PACKAGE,
   MIR_UNIT_PROTECTED,
   MIR_UNIT_THUNK,
   MIR_UNIT_PROPERTY,
} mir_unit_kind_t;

typedef struct {
   int (*begin_block)(mir_unit_t *, mir_block_t, int, void *);
   int (*value)(mir_unit_t *, mir_value_t, void *);
   void (*end_node)(mir_unit_t *, mir_block_t, mir_value_t, void *);
} mir_annotate_t;

typedef void (*mir_lower_fn_t)(mir_unit_t *, object_t *);

mir_context_t *mir_context_new(void);
void mir_context_free(mir_context_t *mc);

void mir_put_unit(mir_context_t *mc, mir_unit_t *mu);
mir_unit_t *mir_get_unit(mir_context_t *mc, ident_t name);
mir_shape_t *mir_get_shape(mir_context_t *mc, ident_t name);
void mir_defer(mir_context_t *mc, ident_t name, ident_t parent,
               mir_unit_kind_t kind, mir_lower_fn_t fn, object_t *object);

unsigned mir_count_linkage(mir_unit_t *mu);
ident_t mir_get_linkage(mir_unit_t *mu, unsigned nth);

mir_unit_t *mir_import(mir_context_t *mc, vcode_unit_t vu);

mir_unit_t *mir_unit_new(mir_context_t *mc, ident_t name, object_t *obj,
                         mir_unit_kind_t kind, mir_shape_t *parent);
void mir_dump(mir_unit_t *mu);
void mir_annotate(mir_unit_t *mu, const mir_annotate_t *cb, void *ctx);
void mir_unit_free(mir_unit_t *mu);
mir_unit_kind_t mir_get_kind(mir_unit_t *mu);
object_t *mir_get_object(mir_unit_t *mu);
ident_t mir_get_parent(mir_unit_t *mu);
ident_t mir_get_shape_parent(mir_shape_t *shape);
mir_context_t *mir_get_context(mir_unit_t *mu);
int mir_find_slot(mir_shape_t *shape, ident_t name);

mir_value_t mir_search_object(mir_unit_t *mu, const void *obj, int *hops);
void mir_put_object(mir_unit_t *mu, const void *obj, mir_value_t value);

void *mir_get_priv(mir_unit_t *mu, const void *obj);
void mir_put_priv(mir_unit_t *mu, const void *obj, void *value);

void *mir_malloc(mir_unit_t *mu, size_t size);

#ifdef DEBUG
void mir_compare_layout(mir_unit_t *a, mir_unit_t *b);
#endif

typedef enum {
   MIR_PASS_GVN = (1 << 0),
   MIR_PASS_DCE = (1 << 1),
   MIR_PASS_CFG = (1 << 2),
   MIR_PASS_RA  = (1 << 3),
} mir_pass_t;

#define MIR_PASS_O0 (MIR_PASS_CFG | MIR_PASS_RA)
#define MIR_PASS_O1 (MIR_PASS_O0 | MIR_PASS_GVN | MIR_PASS_DCE)
#define MIR_PASS_O2 (MIR_PASS_O1)

void mir_optimise(mir_unit_t *mu, mir_pass_t passes);

#endif  // _MIR_UNIT_H
