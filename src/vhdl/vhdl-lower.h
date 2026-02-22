//
//  Copyright (C) 2025  Nick Gasson
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

#ifndef _VHDL_LOWER_H
#define _VHDL_LOWER_H

#include "prim.h"
#include "mir/mir-node.h"
#include "type.h"

typedef struct {
   type_t      tree;
   mir_type_t  type;
   mir_stamp_t stamp;
   type_kind_t kind;
   size_t      size;
   size_t      stride;
   unsigned    ndims;
   unsigned    udims;
} type_info_t;

typedef struct {
   mir_unit_t *mu;
} vhdl_gen_t;

const type_info_t *type_info(mir_unit_t *mu, type_t type);

void vhdl_lower_predef(mir_unit_t *mu, object_t *obj);
void vhdl_lower_image_helper(mir_unit_t *mu, object_t *obj);

mir_value_t vhdl_lower_array_stride(vhdl_gen_t *g, const type_info_t *ti,
                                    mir_value_t array);
mir_value_t vhdl_lower_total_elements(mir_unit_t *mu, const type_info_t *ti,
                                      mir_value_t array);

mir_value_t vhdl_lower_rvalue(vhdl_gen_t *g, tree_t t);
mir_value_t vhdl_lower_wrap(vhdl_gen_t *g, const type_info_t *ti,
                            mir_value_t data);

#endif  // _VHDL_LOWER_H
