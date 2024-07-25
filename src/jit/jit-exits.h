//
//  Copyright (C) 2022-2024  Nick Gasson
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

#ifndef _JIT_EXITS_H
#define _JIT_EXITS_H

#include "prim.h"
#include "jit/jit.h"
#include "jit/jit-ffi.h"

#define WEAK __attribute__((weak))

void x_sched_process(int64_t delay);
void x_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count);
sig_shared_t *x_init_signal(int64_t count, uint32_t size, jit_scalar_t value,
                            bool scalar, uint32_t flags, tree_t where,
                            int32_t offset);
void x_sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                      int32_t count, int64_t after, int64_t reject);
void x_transfer_signal(sig_shared_t *target_ss, uint32_t toffset,
                       sig_shared_t *source_ss, uint32_t soffset,
                       int32_t count, int64_t after, int64_t reject);
int32_t x_test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count);
int32_t x_test_net_active(sig_shared_t *ss, uint32_t offset,
                          int32_t count);
void x_sched_event(sig_shared_t *ss, uint32_t offset, int32_t count);
void x_alias_signal(sig_shared_t *ss, tree_t where);
void x_sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                        int64_t after, int64_t reject);
void x_file_open(int8_t *status, void **_fp, const uint8_t *name_bytes,
                 int32_t name_len, int8_t mode);
void x_file_write(void **_fp, void *data, int64_t size, int64_t count);
int64_t x_file_read(void **_fp, void *data, int64_t size, int64_t count);
void x_index_fail(int64_t value, int64_t left, int64_t right, int8_t dir,
                  tree_t where, tree_t hint);
void x_length_fail(int64_t left, int64_t right, int32_t dim, tree_t where);
void x_range_fail(int64_t value, int64_t left, int64_t right, int8_t dir,
                  tree_t where, tree_t hint);
void x_exponent_fail(int64_t value, tree_t where);
void x_overflow(int64_t lhs, int64_t rhs, tree_t where);
void x_null_deref(tree_t where);
void x_div_zero(tree_t where);
void x_assert_fail(const uint8_t *msg, int32_t msg_len, int8_t severity,
                   int64_t hint_left, int64_t hint_right, int8_t hint_valid,
                   object_t *where);
void x_report(const uint8_t *msg, int32_t msg_len, int8_t severity,
              object_t *where);
int64_t x_last_event(sig_shared_t *ss, uint32_t offset, int32_t count);
int64_t x_last_active(sig_shared_t *ss, uint32_t offset, int32_t count);
void x_map_signal(sig_shared_t *src_ss, uint32_t src_offset,
                  sig_shared_t *dst_ss, uint32_t dst_offset, uint32_t count);
void x_map_const(sig_shared_t *ss, uint32_t offset,
                 const uint8_t *values, uint32_t count);
void x_map_implicit(sig_shared_t *src_ss, uint32_t src_offset,
                    sig_shared_t *dst_ss, uint32_t dst_offset,
                    uint32_t count);
void x_push_scope(tree_t where, int32_t size);
void x_pop_scope(void);
bool x_driving(sig_shared_t *ss, uint32_t offset, int32_t count);
void *x_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count);
sig_shared_t *x_implicit_signal(uint32_t count, uint32_t size, tree_t where,
                                implicit_kind_t kind, ffi_closure_t *closure,
                                int64_t delay);
void x_disconnect(sig_shared_t *ss, uint32_t offset, int32_t count,
                  int64_t after, int64_t reject);
void x_force(sig_shared_t *ss, uint32_t offset, int32_t count, void *values);
void x_release(sig_shared_t *ss, uint32_t offset, int32_t count);
void x_deposit_signal(sig_shared_t *ss, uint32_t offset, int32_t count,
                      void *values);
void x_resolve_signal(sig_shared_t *ss, jit_handle_t handle, void *context,
                      int64_t ileft, int32_t nlits, int32_t flags);
void x_elab_order_fail(tree_t where);
void x_unreachable(tree_t where);
void x_cover_setup_toggle_cb(sig_shared_t *ss, int32_t tag);
void x_cover_setup_state_cb(sig_shared_t *ss, int64_t low, int32_t tag);
void x_process_init(jit_handle_t handle, tree_t where);
void x_clear_event(sig_shared_t *ss, uint32_t offset, int32_t count);
void x_enter_state(int32_t state);
void *x_reflect_value(void *context, jit_scalar_t value, tree_t where,
                      const jit_scalar_t *bounds);
void *x_reflect_subtype(void *context, tree_t where,
                        const jit_scalar_t *bounds);
void *x_function_trigger(jit_handle_t handle, unsigned nargs,
                         const jit_scalar_t *args);
void *x_or_trigger(void *left, void *right);
void *x_cmp_trigger(sig_shared_t *ss, uint32_t offset, int64_t right);
void x_add_trigger(void *ptr);
void *x_port_conversion(const ffi_closure_t *driving,
                        const ffi_closure_t *effective);
void x_convert_in(void *ptr, sig_shared_t *ss, uint32_t offset, int32_t count);
void x_convert_out(void *ptr, sig_shared_t *ss, uint32_t offset, int32_t count);

#endif  // _JIT_EXITS_H
