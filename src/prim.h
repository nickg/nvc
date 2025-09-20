//
//  Copyright (C) 2013-2025  Nick Gasson
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

#ifndef _PRIM_H
#define _PRIM_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct _lib *lib_t;
typedef struct _object object_t;
typedef struct _object_arena object_arena_t;
typedef struct _ident *ident_t;
typedef struct _tree *tree_t;
typedef struct _type *type_t;
typedef struct _vlog_node *vlog_node_t;
typedef struct _sdf_file sdf_file_t;
typedef struct loc loc_t;
typedef struct _fbuf fbuf_t;
typedef struct _hash hash_t;
typedef struct _shash shash_t;
typedef struct _ihash ihash_t;
typedef struct _chash chash_t;
typedef struct _ghash ghash_t;
typedef struct _hset hset_t;
typedef struct _eval_frame eval_frame_t;
typedef struct text_buf text_buf_t;
typedef struct _nametab nametab_t;
typedef struct _diag diag_t;
typedef struct _mspace mspace_t;
typedef struct _tlab tlab_t;
typedef struct _jit jit_t;
typedef struct _sig_shared sig_shared_t;
typedef struct _ffi_uarray ffi_uarray_t;
typedef struct _ffi_closure ffi_closure_t;
typedef struct _wave_dumper wave_dumper_t;
typedef struct _jit_pack jit_pack_t;
typedef struct _lower_unit lower_unit_t;
typedef struct _psl_node *psl_node_t;
typedef struct _tcl_shell tcl_shell_t;
typedef struct _bit_mask bit_mask_t;
typedef struct _printer printer_t;
typedef struct _unit_registry unit_registry_t;
typedef struct _driver_set driver_set_t;
typedef struct _vhpi_context vhpi_context_t;
typedef struct _mem_pool mem_pool_t;
typedef struct _vpi_context vpi_context_t;
typedef struct _mir_context mir_context_t;
typedef struct _mir_unit mir_unit_t;
typedef struct _mir_shape mir_shape_t;
typedef struct _vlog_symtab vlog_symtab_t;

typedef struct _rt_model      rt_model_t;
typedef struct _rt_watch      rt_watch_t;
typedef struct _rt_signal     rt_signal_t;
typedef struct _rt_scope      rt_scope_t;
typedef struct _rt_nexus      rt_nexus_t;
typedef struct _rt_source     rt_source_t;
typedef struct _rt_proc       rt_proc_t;
typedef struct _rt_alias      rt_alias_t;
typedef struct _rt_implicit   rt_implicit_t;
typedef struct _rt_resolution rt_resolution_t;
typedef struct _rt_trigger    rt_trigger_t;
typedef struct _rt_prop       rt_prop_t;
typedef struct _rt_conv_func  rt_conv_func_t;

typedef struct waveform  waveform_t;
typedef struct sens_list sens_list_t;

typedef struct _vcode_unit *vcode_unit_t;

typedef struct _cover_data  cover_data_t;
typedef struct _cover_item  cover_item_t;
typedef struct _cover_scope cover_scope_t;
typedef struct _cover_block cover_block_t;

typedef struct loc_wr_ctx loc_wr_ctx_t;
typedef struct loc_rd_ctx loc_rd_ctx_t;

typedef struct ident_wr_ctx *ident_wr_ctx_t;
typedef struct ident_rd_ctx *ident_rd_ctx_t;

typedef union _number number_t;

#endif  // _PRIM_H
