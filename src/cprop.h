//
//  Copyright (C) 2021  Nick Gasson
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

#ifndef _CPROP_H
#define _CPROP_H

#include "prim.h"
#include "vcode.h"

typedef enum {
   CP_NONE, CP_SIGNAL, CP_CONST, CP_SCALE, CP_OFFSET, CP_FIELD, CP_UNKNOWN
} cprop_tag_t;

typedef struct {
   e_node_t enode;
   unsigned offset;
} cprop_signal_t;

typedef struct {
   vcode_reg_t reg;
   unsigned    stride;
} cprop_offset_t;

typedef struct {
   cprop_tag_t tag;
   vcode_reg_t base;
   union {
      int64_t        cval;
      cprop_signal_t signal;
      vcode_reg_t    scale;
      cprop_offset_t offset;
   };
} cprop_state_t;

typedef enum {
   CPROP_BOUNDS = (1 << 0)
} cprop_flags_t;

typedef void (*cprop_callback_t)(int, cprop_state_t *, void *);
typedef e_node_t (*cprop_find_signal_t)(ident_t, int, void *);

typedef struct cprop_vars_s cprop_vars_t;

typedef struct {
   cprop_flags_t         flags;
   cprop_callback_t      sched_waveform;
   cprop_callback_t      sched_event;
   cprop_callback_t      map_signal;
   cprop_callback_t      init_signal;
   cprop_callback_t      if_generate;
   cprop_callback_t      pcall;
   cprop_callback_t      fcall;
   cprop_callback_t      drive_signal;
   cprop_callback_t      last_value;
   cprop_callback_t      signal_flag;
   cprop_find_signal_t   find_signal;
   cprop_vars_t         *vars;
   unsigned              hop_bias;
   void                 *context;
} cprop_req_t;

void cprop(cprop_req_t *req);
void cprop_dump(int op, cprop_state_t *regs);
void cprop_get_signal(vcode_reg_t target, vcode_reg_t count_reg,
                      cprop_state_t *regs, unsigned *offset, unsigned *stride,
                      unsigned *count, e_node_t *signal);

cprop_vars_t *cprop_vars_new(void);
void cprop_vars_free(cprop_vars_t *vars);
void cprop_vars_enter(cprop_vars_t *vars);
void cprop_vars_leave(cprop_vars_t *vars);

#endif  // _CPROP_H
