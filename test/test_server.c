//
//  Copyright (C) 2023-2024  Nick Gasson
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

#include "test_util.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "rt/shell.h"
#include "scan.h"
#include "server.h"
#include "thread.h"

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <jansson.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#endif

static void server_ready_cb(void *arg)
{
   int wfd = (intptr_t)arg;
   static const uint8_t token[1] = { 0x42 };
   if (write(wfd, token, 1) != 1)
      fatal_errno("write pipe");

   close(wfd);
}

static pid_t fork_server(server_kind_t kind, tree_t top, const char *init_cmd)
{
#ifdef __MINGW32__
   ck_abort_msg("not supported on Windows");
#else
   int rfd, wfd;
   open_pipe(&rfd, &wfd);

   pid_t pid = fork();
   if (pid == 0) {
      close(rfd);

      start_server(kind, get_jit(), top, server_ready_cb, (void *)(intptr_t)wfd,
                   init_cmd);

      exit(0);
   }
   else if (pid < 0)
      fatal_errno("fork");
   else {
      close(wfd);

      uint8_t token[1];
      if (read(rfd, token, 1) != 1)
         fatal_errno("read pipe");

      close(rfd);

      ck_assert_int_eq(token[0], 0x42);

      return pid;
   }
#endif
}

static void join_server(pid_t pid)
{
#ifdef __MINGW32__
   ck_abort_msg("not supported on Windows");
#else
   int status;
   if (waitpid(pid, &status, 0) != pid)
      fatal_errno("waitpid");

   ck_assert_int_eq(WEXITSTATUS(status), 0);
#endif
}

static void write_fully(int fd, const void *data, size_t len)
{
   while (len > 0) {
      ssize_t nbytes = write(fd, data, len);
      if (nbytes <= 0)
         fatal_errno("write");

      data += nbytes;
      len -= nbytes;
   }
}

static int open_connection(void)
{
   int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
   if (sock == -1)
      fatal_errno("socket");

   const int port = opt_get_int(OPT_SERVER_PORT);

   struct sockaddr_in addr;
   memset(&addr, '\0', sizeof(addr));
   addr.sin_family = AF_INET;
   addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
   addr.sin_port = htons(port);

   if (connect(sock, &addr, sizeof(addr)) == -1)
      fatal_errno("connect");

   return sock;
}

static void websocket_upgrade(int sock)
{
   static const char req[] =
      "GET / HTTP/1.1\r\n"
      "Host: example.com:80\r\n"
      "User-Agent: curl/7.87.0\r\n"
      "Accept: */*\r\n"
      "Connection: Upgrade\r\n"
      "Upgrade: websocket\r\n"
      "Origin: http://example.com:80\r\n"
      "Sec-WebSocket-Key: 111SGVsbG8sIHdvcmaxkIQ==\r\n"
      "Sec-WebSocket-Version: 13\r\n"
      "\r\n";

   write_fully(sock, req, sizeof(req));

   char resp[256];
   size_t respsz = 0;
   for (;;) {
      const ssize_t nbytes = read(sock, resp + respsz, sizeof(resp) - respsz);
      if (nbytes <= 0)
         fatal_errno("recv");

      respsz += nbytes;

      ck_assert_int_gt(nbytes, 0);
      ck_assert_int_lt(respsz, sizeof(resp));

      resp[respsz - 1] = '\0';

      const char *accept = strstr(resp, "Sec-WebSocket-Accept:");
      if (accept != NULL && strstr(accept, "\r\n"))
         break;
   }

   fail_unless(strstr(resp, "HTTP/1.1 101\r\n"));
   fail_unless(strstr(resp, "Connection: upgrade\r\n"));
}

static void shutdown_server(web_socket_t *ws)
{
   static const uint8_t packet[] = { C2S_SHUTDOWN };
   ws_send_binary(ws, packet, sizeof(packet));
   ws_flush(ws);

   ws_poll(ws);

   ck_assert(ws_closing(ws));
}

static void sanity_text_frame(web_socket_t *ws, const char *text, void *context)
{
   ck_assert_str_eq(text, "3");
}

START_TEST(test_sanity)
{
   pid_t pid = fork_server(SERVER_HTTP, NULL, NULL);
   int sock = open_connection();
   websocket_upgrade(sock);

   ws_handler_t handler = {
      .text_frame = sanity_text_frame
   };
   web_socket_t *ws = ws_new(sock, &handler, true);

   ws_send_text(ws, "expr 1 + 2");
   ws_flush(ws);

   ws_poll(ws);

   static const uint8_t packet[] = { C2S_SHUTDOWN };
   ws_send_binary(ws, packet, sizeof(packet));
   ws_flush(ws);

   ws_poll(ws);

   ck_assert(ws_closing(ws));
   ws_free(ws);

   close(sock);
   join_server(pid);
}
END_TEST

START_TEST(test_second_connection)
{
   pid_t pid = fork_server(SERVER_HTTP, NULL, NULL);
   int sock1 = open_connection();
   websocket_upgrade(sock1);

   ws_handler_t handler = {
      .text_frame = sanity_text_frame
   };
   web_socket_t *ws1 = ws_new(sock1, &handler, true);

   ws_send_text(ws1, "expr 1 + 2");
   ws_flush(ws1);

   ws_poll(ws1);

   int sock2 = open_connection();
   websocket_upgrade(sock2);
   web_socket_t *ws2 = ws_new(sock2, &handler, true);

   ws_send_text(ws2, "expr 1 + 2");
   ws_flush(ws2);

   ws_poll(ws2);
   ws_poll(ws1);

   ck_assert(ws_closing(ws1));
   ws_free(ws1);
   close(sock1);

   shutdown_server(ws2);
   ws_free(ws2);

   close(sock2);
   join_server(pid);
}
END_TEST

START_TEST(test_dirty_close)
{
   pid_t pid = fork_server(SERVER_HTTP, NULL, NULL);
   int sock = open_connection();
   websocket_upgrade(sock);

   ws_handler_t handler = {
      .text_frame = sanity_text_frame
   };
   web_socket_t *ws = ws_new(sock, &handler, true);

   ws_send_text(ws, "expr 1 + 2");
   ws_flush(ws);

   ws_poll(ws);

   ws_free(ws);
   close(sock);

   sock = open_connection();
   websocket_upgrade(sock);
   ws = ws_new(sock, &handler, true);

   ws_send_text(ws, "expr 1 + 2");
   ws_flush(ws);

   ws_poll(ws);

   shutdown_server(ws);
   ws_free(ws);

   close(sock);
   join_server(pid);
}
END_TEST

static void wave_text_frame(web_socket_t *ws, const char *text, void *context)
{
   ck_abort_msg("not expecting a text frame");
}

static void wave_binary_frame(web_socket_t *ws, const void *data, size_t len,
                              void *context)
{
   int *state = context;
   const uint8_t *bytes = data;

   switch ((*state)++) {
   case 0:
      ck_assert_int_eq(len, 18);
      ck_assert_int_eq(bytes[0], S2C_START_SIM);
      ck_assert_int_eq(bytes[1] << 8 | bytes[2], 15);
      ck_assert_mem_eq(bytes + 3, "WORK.WAVE1.elab", 15);
      break;

   case 1:
      ck_assert_int_eq(len, 8);
      ck_assert_int_eq(bytes[0], S2C_INIT_CMD);
      ck_assert_int_eq(bytes[1] << 8 | bytes[2], 5);
      ck_assert_mem_eq(bytes + 3, "hello", 5);
      break;

   case 2:
      ck_assert_int_eq(len, 9);
      ck_assert_int_eq(bytes[0], S2C_ADD_WAVE);
      ck_assert_int_eq(bytes[1] << 8 | bytes[2], 2);
      ck_assert_int_eq(bytes[3], '/');
      ck_assert_int_eq(bytes[4], 'x');
      ck_assert_int_eq(bytes[5] << 8 | bytes[6], 2);
      ck_assert_int_eq(bytes[7], 'b');
      ck_assert_int_eq(bytes[8], '0');
      break;

   case 3:
   case 4:
      ck_assert_int_eq(len, 9);
      ck_assert_int_eq(bytes[0], S2C_NEXT_TIME_STEP);
      break;

   case 5:
      ck_assert_int_eq(len, 9);
      ck_assert_int_eq(bytes[0], S2C_SIGNAL_UPDATE);
      ck_assert_int_eq(bytes[1] << 8 | bytes[2], 2);
      ck_assert_int_eq(bytes[3], '/');
      ck_assert_int_eq(bytes[4], 'x');
      ck_assert_int_eq(bytes[5] << 8 | bytes[6], 2);
      ck_assert_int_eq(bytes[7], 'b');
      ck_assert_int_eq(bytes[8], '1');
      break;

   case 6:
      ck_assert_int_eq(len, 1);
      ck_assert_int_eq(bytes[0], S2C_RESTART_SIM);
      break;

   default:
      ck_abort_msg("unexpected call to binary_frame in state %d", *state - 1);
   }
}

START_TEST(test_wave)
{
   input_from_file(TESTDIR "/shell/wave1.vhd");

   tree_t top = run_elab();

   pid_t pid = fork_server(SERVER_HTTP, top, "hello");
   int sock = open_connection();
   websocket_upgrade(sock);

   int state = 0;
   ws_handler_t handler = {
      .text_frame = wave_text_frame,
      .binary_frame = wave_binary_frame,
      .context = &state
   };
   web_socket_t *ws = ws_new(sock, &handler, true);

   ws_send_text(ws, "add wave /x");
   ws_flush(ws);

   ws_poll(ws);

   ws_send_text(ws, "run 1 ns");
   ws_flush(ws);

   ws_poll(ws);

   ck_assert_int_eq(state, 6);

   ws_send_text(ws, "restart");
   ws_flush(ws);

   ws_poll(ws);

   shutdown_server(ws);
   ws_free(ws);

   ck_assert_int_eq(state, 7);

   close(sock);
   join_server(pid);
}
END_TEST

static void pong_handler(web_socket_t *ws, const void *data, size_t len,
                         void *user)
{
   int *state = user;

   switch ((*state)++) {
   case 0:
      ck_assert_int_eq(len, 5);
      ck_assert_mem_eq(data, "hello", 5);
      break;
   case 1:
      ck_assert_int_eq(len, 2000);
      for (int i = 0; i < 2000; i++)
         ck_assert_int_eq(((const uint8_t *)data)[i], i & 0xff);
      break;
   case 2:
      ck_assert_int_eq(len, 70000);
      break;
   default:
      ck_abort_msg("unexpected call to pong_handler in state %d", *state - 1);
   }
}

START_TEST(test_ping)
{
   pid_t pid = fork_server(SERVER_HTTP, NULL, NULL);
   int sock = open_connection();
   websocket_upgrade(sock);

   int state = 0;
   ws_handler_t handler = {
      .pong_frame = pong_handler,
      .context = &state
   };
   web_socket_t *ws = ws_new(sock, &handler, true);

   ws_send_ping(ws, "hello", 5);
   ws_flush(ws);

   ws_poll(ws);

   ck_assert_int_eq(state, 1);

   unsigned char big[2000];
   for (int i = 0; i < ARRAY_LEN(big); i++)
      big[i] = i & 0xff;

   ws_send_ping(ws, big, ARRAY_LEN(big));
   ws_flush(ws);

   ws_poll(ws);

   ck_assert_int_eq(state, 2);

   unsigned char *huge LOCAL = xcalloc(70000);
   ws_send_ping(ws, huge, 70000);
   ws_flush(ws);

   ws_poll(ws);

   ck_assert_int_eq(state, 3);

   shutdown_server(ws);
   ws_free(ws);

   close(sock);
   join_server(pid);
}
END_TEST

static json_t *read_json(int sock)
{
   size_t bufsz = 0, wptr = 0;
   char *buf LOCAL = NULL;

   do {
      if (wptr == bufsz)
         buf = xrealloc(buf, (bufsz = MAX(bufsz * 2, 1)));

      const ssize_t nbytes = recv(sock, buf + wptr, 1, 0);
      if (nbytes < 0)
         fatal_errno("recv");

      ck_assert_int_eq(nbytes, 1);
   } while (buf[wptr++] != '\0');

   json_error_t error;
   json_t *json = json_loads(buf, 0, &error);
   ck_assert_msg(json != NULL, "failed to parse '%s'", buf);

   return json;
}

static void cxxrtl_greeting(int sock)
{
   {
      json_t *json = json_object();
      json_object_set_new(json, "type", json_string("greeting"));
      json_object_set_new(json, "version", json_integer(0));

      char *str LOCAL = json_dumps(json, JSON_COMPACT);
      write_fully(sock, str, strlen(str) + 1);

      json_decref(json);
   }

   {
      json_t *json = read_json(sock);

      json_t *commands = json_object_get(json, "commands");
      ck_assert_ptr_nonnull(commands);
      ck_assert(json_is_array(commands));
      ck_assert_int_gt(json_array_size(commands), 0);

      json_t *features = json_object_get(json, "features");
      ck_assert_ptr_nonnull(features);
      ck_assert(json_is_object(features));

      json_decref(json);
   }
}

static json_t *cxxrtl_command(int sock, const char *cmd, json_t *req)
{
   json_object_set_new(req, "type", json_string("command"));
   json_object_set_new(req, "command", json_string(cmd));

   char *str LOCAL = json_dumps(req, JSON_COMPACT);
   write_fully(sock, str, strlen(str) + 1);

   json_decref(req);

   json_t *resp = read_json(sock);

   json_t *type = json_object_get(resp, "type");
   ck_assert(json_is_string(type));
   ck_assert_str_eq(json_string_value(type), "response");

   json_t *cmdobj = json_object_get(resp, "command");
   ck_assert(json_is_string(cmdobj));
   ck_assert_str_eq(json_string_value(cmdobj), cmd);

   return resp;
}

static void cxxrtl_quit_simulation(int sock)
{
   json_t *resp = cxxrtl_command(sock, "nvc.quit_simulation", json_object());
   json_decref(resp);
}

START_TEST(test_greeting)
{
   pid_t pid = fork_server(SERVER_CXXRTL, NULL, NULL);
   int sock = open_connection();

   cxxrtl_greeting(sock);

   cxxrtl_quit_simulation(sock);

   close(sock);
   join_server(pid);
}
END_TEST

START_TEST(test_bad_command)
{
   pid_t pid = fork_server(SERVER_CXXRTL, NULL, NULL);
   int sock = open_connection();

   cxxrtl_greeting(sock);

   json_t *req = json_object();
   json_object_set_new(req, "type", json_string("command"));
   json_object_set_new(req, "command", json_string("BAD COMMAND"));

   char *str LOCAL = json_dumps(req, JSON_COMPACT);
   write_fully(sock, str, strlen(str) + 1);

   json_decref(req);

   json_t *resp = read_json(sock);

   json_t *type = json_object_get(resp, "type");
   ck_assert(json_is_string(type));

   json_decref(resp);

   cxxrtl_quit_simulation(sock);

   close(sock);
   join_server(pid);
}
END_TEST

Suite *get_server_tests(void)
{
   Suite *s = suite_create("server");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_sanity);
   tcase_add_test(tc, test_dirty_close);
   tcase_add_test(tc, test_second_connection);
   tcase_add_test(tc, test_wave);
   tcase_add_test(tc, test_ping);
   tcase_add_test(tc, test_greeting);
   tcase_add_test(tc, test_bad_command);
   suite_add_tcase(s, tc);

   return s;
}
