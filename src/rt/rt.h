//
//  Copyright (C) 2011-2022  Nick Gasson
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

#ifndef _RT_H
#define _RT_H

#include "ident.h"
#include "prim.h"

#include <stdint.h>

#define RT_ABI_VERSION 5
#define RT_ALIGN_MASK  0x7

#define TIME_HIGH INT64_MAX  // Value of TIME'HIGH

typedef void (*sig_event_fn_t)(uint64_t now, rt_signal_t *signal,
                               rt_watch_t *watch, void *user);
typedef void (*timeout_fn_t)(uint64_t now, void *user);
typedef void (*rt_event_fn_t)(rt_model_t *m, void *user);

typedef enum {
   OPEN_OK      = 0,
   STATUS_ERROR = 1,
   NAME_ERROR   = 2,
   MODE_ERROR   = 3,
} file_open_status_t;

typedef enum {
   R_MEMO      = (1 << 0),
   R_IDENT     = (1 << 1),
   R_COMPOSITE = (1 << 2),
} res_flags_t;

typedef enum {
   NET_F_FORCED       = (1 << 0),
   NET_F_INOUT        = (1 << 1),
   NET_F_LAST_VALUE   = (1 << 2),
   NET_F_R_IDENT      = (1 << 3),
   NET_F_IMPLICIT     = (1 << 4),
   NET_F_REGISTER     = (1 << 5),
   // Unused          = (1 << 6),
   NET_F_EFFECTIVE    = (1 << 7),
} net_flags_t;

typedef enum {
   SIGNAL_BUS,
   SIGNAL_REGISTER
} rt_signal_kind_t;

typedef enum {
   RT_START_OF_SIMULATION,
   RT_END_OF_SIMULATION,
   RT_END_OF_PROCESSES,
   RT_LAST_KNOWN_DELTA_CYCLE,
   RT_NEXT_TIME_STEP,

   RT_LAST_EVENT
} rt_event_t;

void rt_start_of_tool(tree_t top, rt_model_t *m);
void *rt_tlab_alloc(size_t size);

void _std_standard_init(void);
void _std_env_init(void);
void _nvc_sim_pkg_init(void);

#endif  // _RT_H
