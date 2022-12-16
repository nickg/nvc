//
//  Copyright (C) 2022-2023  Nick Gasson
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

#ifndef _JIT_LLVM_H
#define _JIT_LLVM_H

#include "prim.h"
#include "jit/jit.h"

#if defined LLVM_HAS_LLJIT
void jit_register_llvm_plugin(jit_t *j);
#endif

typedef struct _llvm_obj llvm_obj_t;

typedef enum {
   LLVM_O0,
   LLVM_O1,
   LLVM_O2,
   LLVM_O3
} llvm_opt_level_t;

llvm_obj_t *llvm_obj_new(const char *name);
void llvm_add_abi_version(llvm_obj_t *obj);
void llvm_aot_compile(llvm_obj_t *obj, jit_t *j, jit_handle_t handle);
void llvm_obj_finalise(llvm_obj_t *obj, llvm_opt_level_t level);
void llvm_obj_emit(llvm_obj_t *obj, const char *path);

#endif  // _JIT_LLVM_H
