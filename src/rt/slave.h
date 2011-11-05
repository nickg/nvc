//
//  Copyright (C) 2011  Nick Gasson
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

#ifndef _SLAVE_H
#define _SLAVE_H

// Control channel for slave simulation kernel

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef enum {
   // Messages from master to slave
   SLAVE_QUIT,
   SLAVE_RESTART,
   SLAVE_RUN,
   SLAVE_READ_SIGNAL,

   // Messages from slave to master
   REPLY_READ_SIGNAL
} slave_msg_t;

typedef struct {
   uint64_t time;
} slave_run_msg_t;

typedef struct {
   uint32_t index;
} slave_read_signal_msg_t;

typedef struct {
   uint64_t value;
} reply_read_signal_msg_t;

void slave_post_msg(slave_msg_t msg, const void *args, size_t len);
void slave_get_msg(slave_msg_t *msg, void *buf, size_t *len);
void slave_get_reply(slave_msg_t msg, void *buf, size_t len);
bool slave_msg_ready(void);
bool slave_fork(void);
void slave_kill(int sig);
int slave_wait(void);

#endif
