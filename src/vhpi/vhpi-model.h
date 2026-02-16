//
//  Copyright (C) 2014-2025  Nick Gasson
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

#ifndef _VHPI_MODEL_H
#define _VHPI_MODEL_H

#include "prim.h"
#include "jit/jit.h"

#ifdef __MINGW32__
#define PLI_DLLISPEC __declspec(dllexport)
#define PLI_DLLESPEC
#else
#define PLI_DLLISPEC
#define PLI_DLLESPEC
#endif

#include "vhpi/vhpi_ext_nvc.h"
#include "vhpi/vhpi_user.h"

vhpi_context_t *vhpi_context_new(void);
void vhpi_context_initialise(vhpi_context_t *c, tree_t top, rt_model_t *model,
                             jit_t *jit);
void vhpi_context_free(vhpi_context_t *c);
void vhpi_set_plusargs(vhpi_context_t *c, int argc, char **argv);

void vhpi_load_plugins(const char *plugins);
void vhpi_run_callbacks(int32_t reason);

vhpiHandleT vhpi_bind_foreign(const char *obj_lib, const char *model,
                              tree_t where);
void vhpi_call_foreign(vhpiHandleT handle, jit_scalar_t *args, tlab_t *tlab);

#endif  // _VHPI_MODEL_H
