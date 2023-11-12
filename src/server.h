//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _SERVER_H
#define _SERVER_H

#include "prim.h"
#include "rt/shell.h"

#include <stddef.h>

typedef struct _web_socket web_socket_t;

typedef enum {
   C2S_SHUTDOWN = 0x00,
} c2s_opcode_t;

typedef enum {
   S2C_ADD_WAVE = 0x00,
   S2C_SIGNAL_UPDATE = 0x01,
   S2C_INIT_CMD = 0x02,
   S2C_START_SIM = 0x03,
   S2C_RESTART_SIM = 0x04,
   S2C_QUIT_SIM = 0x05,
   S2C_NEXT_TIME_STEP = 0x06,
   S2C_BACKCHANNEL = 0x07,
} s2c_opcode_t;

typedef struct {
   void (*text_frame)(web_socket_t *, const char *, void *);
   void (*binary_frame)(web_socket_t *, const void *, size_t, void *);
   void (*pong_frame)(web_socket_t *, const void *, size_t, void *);
   void *context;
} ws_handler_t;

web_socket_t *ws_new(int sock, const ws_handler_t *handler, bool mask);
void ws_free(web_socket_t *ws);
void ws_send_binary(web_socket_t *ws, const void *data, size_t size);
void ws_send_text(web_socket_t *ws, const char *text);
void ws_send_ping(web_socket_t *ws, const void *data, size_t size);
void ws_flush(web_socket_t *ws);
void ws_poll(web_socket_t *ws);
bool ws_closing(web_socket_t *ws);

typedef void (*server_ready_fn_t)(void *);

void start_server(jit_factory_t make_jit, tree_t top,
                  server_ready_fn_t cb, void *arg, const char *init_cmd);

#endif   // _SERVER_H
