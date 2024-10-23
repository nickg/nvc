//
//  Copyright (C) 2024  Nick Gasson
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

#ifndef _VPI_MODEL_H
#define _VPI_MODEL_H

#include "prim.h"
#include "jit/jit.h"

typedef unsigned int  PLI_UINT32;
typedef PLI_UINT32   *vpiHandle;

vpi_context_t *vpi_context_new(void);
void vpi_context_initialise(vpi_context_t *c, tree_t top, rt_model_t *model,
                            jit_t *jit, int argc, char **argv);
void vpi_context_free(vpi_context_t *c);

vpiHandle vpi_bind_foreign(ident_t name, vlog_node_t where);
void vpi_call_foreign(vpiHandle handle, jit_scalar_t *args, tlab_t *tlab);

#endif  // _VPI_MODEL_H
