//
//  Copyright (C) 2022  Nick Gasson
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

#define WEAK __attribute__((weak))

void x_sched_process(int64_t delay) WEAK;
void x_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count) WEAK;
sig_shared_t *x_init_signal(uint32_t count, uint32_t size,
                            const uint8_t *values, int32_t flags,
                            tree_t where, int32_t offset) WEAK;
void x_sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                      int32_t count, int64_t after, int64_t reject) WEAK;
int32_t x_test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count) WEAK;
int32_t x_test_net_active(sig_shared_t *ss, uint32_t offset,
                          int32_t count) WEAK;
void x_sched_event(sig_shared_t *ss, uint32_t offset, int32_t count, bool recur,
                   sig_shared_t *wake_ss) WEAK;
int64_t x_now(void) WEAK;
void x_sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                        int64_t after, int64_t reject) WEAK;
void x_file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                 int32_t name_len, int8_t mode, tree_t where);
void x_file_write(void **_fp, uint8_t *data, int32_t len);
void x_file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                 int32_t *out);
void x_file_close(void **_fp);
int8_t x_endfile(void *_f);
void x_file_flush(void *_f);

#endif  // _JIT_EXITS_H
