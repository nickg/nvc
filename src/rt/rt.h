//
//  Copyright (C) 2011-2024  Nick Gasson
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

#define RT_ABI_VERSION   26
#define RT_ALIGN_MASK    0x7
#define RT_MULTITHREADED 0

#define TIME_HIGH INT64_MAX  // Value of TIME'HIGH

#if RT_MULTITHREADED
#define RT_LOCK(x) SCOPED_LOCK(x)
#define MULTITHREADED_ONLY(x) x
#else
#define RT_LOCK(x)
#define MULTITHREADED_ONLY(x)
#endif

typedef void (*sig_event_fn_t)(uint64_t now, rt_signal_t *signal,
                               rt_watch_t *watch, void *user);
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

#define NET_F_FORCED       (1 << 0)
#define NET_F_INOUT        (1 << 1)
#define NET_F_CACHE_EVENT  (1 << 2)
#define NET_F_R_IDENT      (1 << 3)
#define NET_F_PENDING      (1 << 4)
#define NET_F_DEPOSIT      (1 << 5)
#define NET_F_FAST_DRIVER  (1 << 6)
#define NET_F_EFFECTIVE    (1 << 7)
typedef uint8_t net_flags_t;

#define SIG_F_IMPLICIT     (1 << 8)
#define SIG_F_STD_LOGIC    (1 << 9)
#define SIG_F_CACHE_EVENT  (1 << 10)
#define SIG_F_EVENT_FLAG   (1 << 11)
#define SIG_F_REGISTER     (1 << 12)
typedef uint32_t sig_flags_t;

typedef enum {
   SIGNAL_BUS,
   SIGNAL_REGISTER
} rt_signal_kind_t;

typedef enum {
   RT_END_OF_INITIALISATION,
   RT_START_OF_SIMULATION,
   RT_END_OF_SIMULATION,
   RT_END_OF_PROCESSES,
   RT_LAST_KNOWN_DELTA_CYCLE,
   RT_NEXT_TIME_STEP,
   RT_END_TIME_STEP,
   RT_NEXT_CYCLE,

   RT_LAST_EVENT
} rt_event_t;

typedef uint16_t delta_cycle_t;
#define DELTA_CYCLE_MAX UINT16_MAX

void _std_standard_init(void);
void _std_env_init(void);
void _std_reflection_init(void);
void _file_io_init(void);
void _nvc_sim_pkg_init(void);
void _verilog_init(void);

#endif  // _RT_H
