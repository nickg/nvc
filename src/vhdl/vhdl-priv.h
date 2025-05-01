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

#ifndef _VHDL_PRIV_H
#define _VHDL_PRIV_H

#include "prim.h"
#include "mir/mir-node.h"
#include "type.h"

typedef struct {
   mir_type_t  type;
   mir_stamp_t stamp;
   type_kind_t kind;
   size_t      size;
   unsigned    ndims;
   unsigned    udims;
} type_info_t;

const type_info_t *type_info(mir_unit_t *mu, type_t type);

#endif  // _VHDL_PRIV_H
