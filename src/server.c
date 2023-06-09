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

#include "util.h"
#include "ident.h"
#include "jit/jit.h"
#include "option.h"
#include "phase.h"
#include "rt/shell.h"
#include "rt/structs.h"
#include "server.h"
#include "sha1.h"
#include "thread.h"

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <microhttpd.h>

#define WS_UPGRADE_VALUE     "websocket"
#define WS_WEBSOCKET_VERSION "13"
#define WS_GUID              "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
#define WS_GUID_LEN          36
#define WS_KEY_LEN           24
#define WS_KEY_GUID_LEN      (WS_KEY_LEN + WS_GUID_LEN)

#define WS_OPCODE_TEXT_FRAME   0x1
#define WS_OPCODE_BINARY_FRAME 0x2
#define WS_OPCODE_CLOSE_FRAME  0x8
#define WS_OPCODE_PING_FRAME   0xa
#define WS_OPCODE_PONG_FRAME   0xb

#define PORT 8888

typedef struct _web_socket {
   int           sock;
   bool          mask;
   bool          closing;
   ws_handler_t  handler;
   size_t        tx_size;
   size_t        tx_wptr;
   size_t        tx_rptr;
   uint8_t      *tx_buf;
   size_t        rx_size;
   size_t        rx_wptr;
   size_t        rx_rptr;
   uint8_t      *rx_buf;
} web_socket_t;

typedef struct _packet_buf {
   char  *buf;
   size_t alloc;
   size_t wptr;
   size_t rptr;
} packet_buf_t;

typedef struct MHD_UpgradeResponseHandle mhd_urh_t;

typedef struct {
   tcl_shell_t  *shell;
   bool          shutdown;
   mhd_urh_t    *urh;
   web_socket_t *websocket;
   MHD_socket    closesock;
   tree_t        top;
   packet_buf_t *packetbuf;
   const char   *init_cmd;
} web_server_t;

////////////////////////////////////////////////////////////////////////////////
// WebSocket wrapper

web_socket_t *ws_new(int sock, const ws_handler_t *handler, bool mask)
{
   web_socket_t *ws = xcalloc(sizeof(web_socket_t));
   ws->sock    = sock;
   ws->mask    = mask;
   ws->handler = *handler;

   return ws;
}

void ws_free(web_socket_t *ws)
{
   free(ws->tx_buf);
   free(ws->rx_buf);
   free(ws);
}

static void ws_queue_buf(web_socket_t *ws, const void *data, size_t size)
{
   if (ws->tx_wptr + size > ws->tx_size) {
      ws->tx_size = MAX(ws->tx_size + size, 1024);
      ws->tx_buf = xrealloc(ws->tx_buf, ws->tx_size);
   }

   memcpy(ws->tx_buf + ws->tx_wptr, data, size);
   ws->tx_wptr += size;
}

static void ws_send(web_socket_t *ws, int opcode, const void *data, size_t size)
{
   const uint8_t size0 = (size < 126 ? size : (size <= UINT16_MAX ? 126 : 127));

   const uint8_t header[2] = {
      0x80 | opcode,
      (ws->mask ? 0x80 : 0x00) | (size0 & 0x7f),
   };
   ws_queue_buf(ws, header, sizeof(header));

   if (size0 == 126) {
      const uint8_t extlength[2] = { PACK_BE16(size) };
      ws_queue_buf(ws, extlength, sizeof(extlength));
   }
   else if (size0 == 127) {
      const uint8_t extlength[8] = { PACK_BE64(size) };
      ws_queue_buf(ws, extlength, sizeof(extlength));
   }

   if (ws->mask) {
      const int key = rand();
      const uint8_t masks[4] = { PACK_BE32(key) };
      ws_queue_buf(ws, masks, sizeof(masks));

      char xord[128];
      for (size_t i = 0; i < size; i += sizeof(xord)) {
         const size_t chunksz = MIN(sizeof(xord), size - i);
         for (int j = 0; j < chunksz; j++)
            xord[j] = ((const uint8_t *)data)[i + j] ^ masks[(i + j) % 4];

         ws_queue_buf(ws, xord, chunksz);
      }
   }
   else if (size > 0)
      ws_queue_buf(ws, data, size);
}

void ws_send_binary(web_socket_t *ws, const void *data, size_t size)
{
   ws_send(ws, WS_OPCODE_BINARY_FRAME, data, size);
}

void ws_send_packet(web_socket_t *ws, packet_buf_t *pb)
{
   ws_send_binary(ws, pb->buf, pb->wptr);
}

void ws_send_text(web_socket_t *ws, const char *text)
{
   ws_send(ws, WS_OPCODE_TEXT_FRAME, text, strlen(text));
}

void ws_send_close(web_socket_t *ws)
{
   ws_send(ws, WS_OPCODE_CLOSE_FRAME, NULL, 0);
}

void ws_send_ping(web_socket_t *ws, const void *data, size_t size)
{
   ws_send(ws, WS_OPCODE_PING_FRAME, data, size);
}

void ws_flush(web_socket_t *ws)
{
   while (ws->tx_wptr != ws->tx_rptr) {
      const size_t chunksz = ws->tx_wptr - ws->tx_rptr;
      const ssize_t nbytes = write(ws->sock, ws->tx_buf + ws->tx_rptr, chunksz);
      if (nbytes == 0)
         break;
      else if (nbytes < 0) {
         ws->closing = true;
         break;
      }

      ws->tx_rptr += nbytes;
   }

   if (ws->tx_wptr == ws->tx_rptr)
      ws->tx_rptr = ws->tx_wptr = 0;
}

void ws_poll(web_socket_t *ws)
{
 read_more:
   if (ws->rx_size - ws->rx_wptr < 1024)
      ws->rx_buf = xrealloc(ws->rx_buf, (ws->rx_size += 1024));

   const ssize_t nbytes = recv(ws->sock, ws->rx_buf + ws->rx_wptr,
                               ws->rx_size - ws->rx_wptr - 1, 0);
   if (nbytes == -1 && errno == EAGAIN)
      return;
   else if (nbytes <= 0) {
      ws->closing = true;
      return;
   }

   ws->rx_wptr += nbytes;
   assert(ws->rx_wptr <= ws->rx_size);
   assert(ws->rx_rptr < ws->rx_wptr);

   do {
      const size_t rbytes = ws->rx_wptr - ws->rx_rptr;

      if (rbytes < 2)
         goto read_more;   // Not enough for WebSocket header

      uint8_t *frame = ws->rx_buf + ws->rx_rptr;

      // Frame format
      //
      //   0    1     2     3     4 5 6 7  8     9 A B C D E F
      //   FIN  RSV1  RSV2  RSV3  Opcode   Mask  Payload length
      //   Extended payload length (optional)
      //   Masking key (optional)
      //   Payload data

      const bool fin = frame[0] & 0x80;
      const int opcode = frame[0] & 0xf;
      const bool mask = frame[1] & 0x80;
      const int size0 = frame[1] & 0x7f;

      size_t headersz = 2 + (mask ? 4 : 0);
      if (size0 == 126)
         headersz += 2;
      else if (size0 == 127)
         headersz += 8;

      if (rbytes < headersz)
         goto read_more;   // Not enough for extended header

      int flength = size0;
      if (size0 == 127)
         flength = UNPACK_BE64(frame + 2);
      else if (size0 == 126)
         flength = UNPACK_BE16(frame + 2);

      if (rbytes < flength + headersz)
         goto read_more;   // Not enough for full frame

      assert(fin);

      if (mask) {
         for (int i = 0; i < flength; i++)
            frame[headersz + i] ^= frame[headersz - 4 + (i % 4)];
      }

      void *payload = frame + headersz;

      switch (opcode) {
      case WS_OPCODE_TEXT_FRAME:
         {
            char *text = payload;
            assert(text + flength < (char *)ws->rx_buf + ws->rx_size);
            text[flength] = '\0';

            if (ws->handler.text_frame != NULL)
               (*ws->handler.text_frame)(ws, text, ws->handler.context);
         }
         break;

      case WS_OPCODE_BINARY_FRAME:
         if (ws->handler.binary_frame != NULL)
            (*ws->handler.binary_frame)(ws, payload, flength,
                                        ws->handler.context);
         break;

      case WS_OPCODE_CLOSE_FRAME:
         ws->closing = true;
         break;

      case WS_OPCODE_PING_FRAME:
         ws_send(ws, WS_OPCODE_PONG_FRAME, payload, flength);
         break;

      case WS_OPCODE_PONG_FRAME:
         if (ws->handler.pong_frame != NULL)
            (*ws->handler.pong_frame)(ws, payload, flength,
                                      ws->handler.context);
         break;

      default:
         debugf("unhandled WebSocket opcode %02x", opcode);
         break;
      }

      ws->rx_rptr += flength + headersz;
   } while (ws->rx_rptr < ws->rx_wptr);

   ws->rx_rptr = ws->rx_wptr = 0;
}

bool ws_closing(web_socket_t *ws)
{
   return ws->closing;
}

////////////////////////////////////////////////////////////////////////////////
// Packet buffers

static packet_buf_t *pb_new(void)
{
   packet_buf_t *pb = xcalloc(sizeof(packet_buf_t));
   pb->alloc = 128;
   pb->buf = xmalloc(pb->alloc);

   return pb;
}

static void pb_free(packet_buf_t *pb)
{
   free(pb->buf);
   free(pb);
}

static void pb_grow(packet_buf_t *pb, size_t need)
{
   if (pb->wptr + need > pb->alloc) {
      pb->alloc = MAX(pb->wptr + need, pb->alloc * 2);
      pb->buf = xrealloc(pb->buf, pb->alloc);
   }
}

static void pb_pack_u8(packet_buf_t *pb, uint8_t value)
{
   pb_grow(pb, 1);
   pb->buf[pb->wptr++] = value;
}

static void pb_pack_u16(packet_buf_t *pb, uint16_t value)
{
   pb_grow(pb, 2);
   pb->buf[pb->wptr++] = value >> 8;
   pb->buf[pb->wptr++] = value & 0xff;
}

static void pb_pack_u32(packet_buf_t *pb, uint32_t value)
{
   pb_grow(pb, 4);
   pb->buf[pb->wptr++] = (value >> 24) & 0xff;
   pb->buf[pb->wptr++] = (value >> 16) & 0xff;
   pb->buf[pb->wptr++] = (value >> 8) & 0xff;
   pb->buf[pb->wptr++] = value & 0xff;
}

static void pb_pack_bytes(packet_buf_t *pb, const void *data, size_t len)
{
   pb_grow(pb, len);
   memcpy(pb->buf + pb->wptr, data, len);
   pb->wptr += len;
}

static void pb_pack_str(packet_buf_t *pb, const char *str)
{
   const size_t len = strlen(str);
   assert(len < UINT16_MAX);

   pb_pack_u16(pb, len);
   pb_pack_bytes(pb, str, len);
}

static void pb_pack_ident(packet_buf_t *pb, ident_t ident)
{
   const size_t len = ident_len(ident);
   assert(len < UINT16_MAX);

   pb_pack_u16(pb, len);
   pb_pack_bytes(pb, istr(ident), len);
}

////////////////////////////////////////////////////////////////////////////////
// Web server

typedef enum {
   LOG_DEBUG,
   LOG_INFO,
   LOG_WARN,
   LOG_ERROR
} log_level_t;

__attribute__((format(printf, 2, 3)))
static void server_log(log_level_t level, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *buf LOCAL = xvasprintf(fmt, ap);
   debugf("%s", buf);

   va_end(ap);
}

static enum MHD_Result send_page(struct MHD_Connection *connection,
                                 int status, const char *page)
{
   struct MHD_Response *response =
      MHD_create_response_from_buffer(strlen(page), (void *)page,
                                      MHD_RESPMEM_PERSISTENT);

   enum MHD_Result ret = MHD_queue_response(connection, status, response);
   MHD_destroy_response(response);

   return ret;
}

#if 0
static enum MHD_Result send_buffer(struct MHD_Connection *connection,
                                   int status, text_buf_t *tb)
{
   const size_t len = tb_len(tb);
   char *buf = tb_claim(tb);

   struct MHD_Response *response =
      MHD_create_response_from_buffer(len, buf, MHD_RESPMEM_MUST_FREE);

   enum MHD_Result ret = MHD_queue_response(connection, status, response);
   MHD_destroy_response(response);

   return ret;
}
#endif

#if 0
static enum MHD_Result send_file(struct MHD_Connection *connection,
                                 const char *file, const char *mime)
{
   int fd = open(file, O_RDONLY);
   if (fd == -1)
      return send_page(connection, MHD_HTTP_NOT_FOUND, "File not found");

   struct stat st;
   if (fstat(fd, &st) != 0) {
      close(fd);
      return send_page(connection, MHD_HTTP_INTERNAL_SERVER_ERROR,
                       "Cannot stat file");
   }

   struct MHD_Response *response = MHD_create_response_from_fd(st.st_size, fd);
   MHD_add_response_header(response, MHD_HTTP_HEADER_CONTENT_TYPE, mime);

   enum MHD_Result ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
   MHD_destroy_response(response);

   return ret;
}
#endif

static bool is_websocket_request(struct MHD_Connection *connection)
{
   const char *upg_header =
      MHD_lookup_connection_value(connection, MHD_HEADER_KIND,
                                  MHD_HTTP_HEADER_UPGRADE);
   const char *con_header =
      MHD_lookup_connection_value(connection, MHD_HEADER_KIND,
                                  MHD_HTTP_HEADER_CONNECTION);

   return (upg_header != NULL && con_header != NULL)
          && (strcmp(upg_header, WS_UPGRADE_VALUE) == 0)
          && (strstr(con_header, "Upgrade") != NULL);
}

static void handle_text_frame(web_socket_t *ws, const char *text, void *context)
{
   web_server_t *server = context;

   const char *result = NULL;
   if (shell_eval(server->shell, text, &result) && *result != '\0')
      ws_send_text(ws, result);
}

static void handle_binary_frame(web_socket_t *ws, const void *data,
                                size_t length, void *context)
{
   web_server_t *server = context;

   if (length == 0) {
      server_log(LOG_WARN, "ignoring zero-length binary frame");
      return;
   }

   const c2s_opcode_t op = *(const uint8_t *)data;
   switch (op) {
   case C2S_SHUTDOWN:
      server->shutdown = true;
      break;
   default:
      server_log(LOG_ERROR, "unhandled client to server opcode %02x", op);
      break;
   }
}

static void kill_connection(web_server_t *server)
{
   if (MHD_upgrade_action(server->urh,
                          MHD_UPGRADE_ACTION_CLOSE) != MHD_YES)
      server_log(LOG_ERROR, "MHD_UPGRADE_ACTION_CLOSE failed on socket %d",
                 server->websocket->sock);

   diag_set_consumer(NULL, NULL);

   ws_free(server->websocket);
   server->websocket = NULL;
   server->urh = NULL;
}

static void tunnel_diag(diag_t *d, void *context)
{
   web_server_t *server = context;

   if (server->websocket != NULL) {
      ws_send_text(server->websocket, diag_get_text(d));
   }
   else
      server_log(LOG_INFO, "%s", diag_get_text(d));
}

static packet_buf_t *fresh_packet_buffer(web_server_t *server)
{
   server->packetbuf->wptr = 0;
   return server->packetbuf;
}

static void add_wave_handler(ident_t path, rt_signal_t *s, void *user)
{
   web_server_t *server = user;

   packet_buf_t *pb = fresh_packet_buffer(server);
   pb_pack_u8(pb, S2C_ADD_WAVE);
   pb_pack_ident(pb, path);
   ws_send_packet(server->websocket, pb);
}

static void signal_update_handler(ident_t path, uint64_t now, rt_signal_t *s,
                                  void *user)
{
   web_server_t *server = user;

   packet_buf_t *pb = fresh_packet_buffer(server);
   pb_pack_u8(pb, S2C_SIGNAL_UPDATE);
   pb_pack_ident(pb, path);
   pb_pack_u32(pb, s->shared.size);
   pb_pack_bytes(pb, s->shared.data, s->shared.size);
   ws_send_packet(server->websocket, pb);
}

static void upgrade_handler(void *cls, struct MHD_Connection *con,
                            void *con_cls, const char *extra_in,
                            size_t extra_in_size, MHD_socket sock,
                            struct MHD_UpgradeResponseHandle *urh)
{
   web_server_t *server = cls;

   if (server->websocket != NULL) {
      ws_send_close(server->websocket);
      ws_flush(server->websocket);
      kill_connection(server);
   }

   const ws_handler_t handler = {
      .text_frame   = handle_text_frame,
      .binary_frame = handle_binary_frame,
      .context      = server
   };

   server->websocket = ws_new(sock, &handler, false);
   server->urh = urh;

   diag_set_consumer(tunnel_diag, server);

   if (server->top != NULL)
      shell_reset(server->shell, server->top);

   if (server->init_cmd != NULL) {
      packet_buf_t *pb = fresh_packet_buffer(server);
      pb_pack_u8(pb, S2C_INIT_CMD);
      pb_pack_str(pb, server->init_cmd);
      ws_send_packet(server->websocket, pb);
   }
}

static void base64_encode(const void *in, size_t len, text_buf_t *tb)
{
   static const char map[] =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   const unsigned char *data = in;

   for (size_t i = 0; i < len; i++) {
      int c0 = (data[i] >> 2) & 0x3F;
      tb_append(tb, map[c0]);
      c0 = (data[i] << 4) & 0x3F;
      if (++i < len)
         c0 |= (data[i] >> 4) & 0x0F;
      tb_append(tb, map[c0]);

      if (i < len) {
         int c1 = (data[i] << 2) & 0x3F;
         if (++i < len)
            c1 |= (data[i] >> 6) & 0x03;
         tb_append(tb, map[c1]);
      }
      else {
         ++i;
         tb_append(tb, '=');
      }

      if (i < len) {
         int c2 = data[i] & 0x3F;
         tb_append(tb, map[c2]);
      }
      else
         tb_append(tb, '=');
   }
}

static bool get_websocket_accept_value(const char *key, text_buf_t *tb)
{
   if (key == NULL || strlen(key) != WS_KEY_LEN)
      return false;

   char *str LOCAL = xmalloc(WS_KEY_LEN + WS_GUID_LEN + 1);
   strncpy(str, key, (WS_KEY_LEN + 1));
   strncpy(str + WS_KEY_LEN, WS_GUID, WS_GUID_LEN + 1);

   SHA1_CTX ctx;
   SHA1Init(&ctx);
   SHA1Update(&ctx, (unsigned char *)str, WS_KEY_GUID_LEN);

   unsigned char hash[SHA1_LEN];
   SHA1Final(hash, &ctx);

   base64_encode(hash, SHA1_LEN, tb);
   return true;
}

static enum MHD_Result websocket_upgrade(web_server_t *server,
                                         struct MHD_Connection *connection,
                                         const char *method,
                                         const char *version)
{
   if (strcmp(method, MHD_HTTP_METHOD_GET) != 0
       || strcmp(version, MHD_HTTP_VERSION_1_1) != 0)
      return send_page(connection, MHD_HTTP_BAD_REQUEST, "Bad request");

   const char *ws_version_header =
      MHD_lookup_connection_value(connection, MHD_HEADER_KIND,
                                  MHD_HTTP_HEADER_SEC_WEBSOCKET_VERSION);

   if (ws_version_header == NULL
       || strcmp(ws_version_header, WS_WEBSOCKET_VERSION) != 0) {
      static const char page[] = "Upgrade required";
      struct MHD_Response *response =
         MHD_create_response_from_buffer(sizeof(page), (void *)page,
                                         MHD_RESPMEM_PERSISTENT);
      MHD_add_response_header(response, MHD_HTTP_HEADER_SEC_WEBSOCKET_VERSION,
                              WS_WEBSOCKET_VERSION);

      enum MHD_Result ret =
         MHD_queue_response(connection, MHD_HTTP_UPGRADE_REQUIRED, response);
      MHD_destroy_response(response);

      return ret;
   }

   const char *ws_key_header = NULL;
   size_t key_size = 0;
   enum MHD_Result ret =
      MHD_lookup_connection_value_n(connection, MHD_HEADER_KIND,
                                    MHD_HTTP_HEADER_SEC_WEBSOCKET_KEY,
                                    strlen(MHD_HTTP_HEADER_SEC_WEBSOCKET_KEY),
                                    &ws_key_header, &key_size);
   if (ret == MHD_NO || key_size != WS_KEY_LEN)
      return send_page(connection, MHD_HTTP_BAD_REQUEST, "Bad request");

   LOCAL_TEXT_BUF ws_ac_value = tb_new();
   if (!get_websocket_accept_value(ws_key_header, ws_ac_value))
      return MHD_NO;

   struct MHD_Response *response =
      MHD_create_response_for_upgrade(upgrade_handler, server);
   MHD_add_response_header(response, MHD_HTTP_HEADER_UPGRADE, WS_UPGRADE_VALUE);
   MHD_add_response_header(response, MHD_HTTP_HEADER_SEC_WEBSOCKET_ACCEPT,
                           tb_get(ws_ac_value));

   ret = MHD_queue_response(connection, MHD_HTTP_SWITCHING_PROTOCOLS, response);
   MHD_destroy_response(response);

   return ret;
}

static enum MHD_Result handle_connection(void *cls,
                                         struct MHD_Connection *connection,
                                         const char *url,
                                         const char *method,
                                         const char *version,
                                         const char *upload_data,
                                         size_t *upload_data_size,
                                         void **con_cls)
{
   server_log(LOG_INFO, "%s %s", method, url);

   if (is_websocket_request(connection))
      return websocket_upgrade(cls, connection, method, version);

   return send_page(connection, MHD_HTTP_NOT_FOUND, "Not found");
}

static void tunnel_output(const char *buf, size_t nchars, void *user)
{
   web_server_t *server = user;
   ws_send(server->websocket, WS_OPCODE_TEXT_FRAME, buf, nchars);
}

void start_server(jit_factory_t make_jit, tree_t top,
                  server_ready_fn_t cb, void *arg, const char *init_cmd)
{
   web_server_t *server = xcalloc(sizeof(web_server_t));
   server->shell     = shell_new(make_jit);
   server->closesock = MHD_INVALID_SOCKET;
   server->top       = top;
   server->packetbuf = pb_new();
   server->init_cmd  = init_cmd;

   shell_handler_t handler = {
      .add_wave = add_wave_handler,
      .signal_update = signal_update_handler,
      .stderr_write = tunnel_output,
      .stdout_write = tunnel_output,
      .context = server
   };
   shell_set_handler(server->shell, &handler);

   const int flags = MHD_ALLOW_UPGRADE | MHD_USE_ERROR_LOG;
   struct MHD_Daemon *daemon = MHD_start_daemon(flags, PORT, NULL, NULL,
                                                handle_connection, server,
                                                MHD_OPTION_END);
   if (daemon == NULL)
      fatal("failed to start microhttpd daemon");

   server_log(LOG_INFO, "listening on localhost:%d", PORT);

   if (cb != NULL)
      (*cb)(arg);

   for (;;) {
      MHD_socket max_fd = -1;
      fd_set rfd, wfd, efd;
      FD_ZERO(&rfd);
      FD_ZERO(&wfd);
      FD_ZERO(&efd);

      if (MHD_get_fdset(daemon, &rfd, &wfd, &efd, &max_fd) != MHD_YES)
         fatal("MHD_get_fdset failed");

      if (server->websocket != NULL) {
         FD_SET(server->websocket->sock, &rfd);

         if (server->websocket->tx_wptr != server->websocket->tx_rptr)
            FD_SET(server->websocket->sock, &wfd);

         max_fd = MAX(max_fd, server->websocket->sock);
      }

      if (max_fd == -1)
         break;

      unsigned long long timeout;
      if (MHD_get_timeout(daemon, &timeout) != MHD_YES)
         timeout = 1000;

      struct timeval tv = {
         .tv_sec = timeout / 1000,
         .tv_usec = (timeout % 1000) * 1000
      };

      if (select(max_fd + 1, &rfd, &wfd, &efd, &tv) == -1)
         fatal_errno("select");

      if (server->websocket != NULL) {
         if (FD_ISSET(server->websocket->sock, &rfd))
            ws_poll(server->websocket);

         if (FD_ISSET(server->websocket->sock, &wfd))
            ws_flush(server->websocket);

         if (server->websocket->closing)
            kill_connection(server);
      }

      if (MHD_run_from_select(daemon, &rfd, &wfd, &efd) != MHD_YES)
         fatal("MHD_run_from_select failed");

      if (server->shutdown && server->closesock == MHD_INVALID_SOCKET) {
         server->closesock = MHD_quiesce_daemon(daemon);

         if (server->websocket != NULL)
            ws_send_close(server->websocket);
      }
   }

   server_log(LOG_INFO, "stopping server");
   MHD_stop_daemon(daemon);

   assert(server->closesock != MHD_INVALID_SOCKET);
   close(server->closesock);

   pb_free(server->packetbuf);
   free(server);
}
