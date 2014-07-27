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

#include "slave.h"
#include "util.h"
#include "rt.h"
#include "tree.h"
#include "common.h"

#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/uio.h>

static bool  am_master = false;
static pid_t slave_pid;
static int   slave_fd = -1;

static tree_t top_level;
static tree_rd_ctx_t tree_rd_ctx;

void slave_post_msg(slave_msg_t msg, const void *args, size_t len)
{
   struct iovec iov[] = {
      { .iov_base = &msg,        .iov_len = sizeof(msg) },
      { .iov_base = (void*)args, .iov_len = len }
   };

   ssize_t rc = writev(slave_fd, iov, 2);
   if (rc < 0)
      fatal_errno("writev");
}

void slave_get_msg(slave_msg_t *msg, void *buf, size_t *len)
{
   ssize_t nr = read(slave_fd, msg, sizeof(slave_msg_t));
   if (nr < 0)
      fatal_errno("read");
   else if (nr == 0)
      fatal("slave connection terminated\n");

   size_t body_len = 0;
   switch (*msg) {
   case SLAVE_QUIT:
   case SLAVE_RESTART:
   case SLAVE_NOW:
   case EVENT_STOP:
      break;
   case SLAVE_RUN:
      body_len = sizeof(slave_run_msg_t);
      break;
   case SLAVE_READ_SIGNAL:
      body_len = sizeof(slave_read_signal_msg_t);
      break;
   case SLAVE_WATCH:
      body_len = sizeof(slave_watch_msg_t);
      break;
   case SLAVE_UNWATCH:
      body_len = sizeof(slave_unwatch_msg_t);
      break;
   case REPLY_READ_SIGNAL:
      body_len = *len;
      break;
   case REPLY_NOW:
      body_len = sizeof(reply_now_msg_t);
      break;
   case EVENT_WATCH:
      body_len = sizeof(event_watch_msg_t);
      break;
   default:
      fatal("invalid slave message %u\n", *msg);
   }

   if (body_len > 0) {
      assert(body_len <= *len);
      nr = read(slave_fd, buf, body_len);
      if (nr < 0)
         fatal_errno("read");
   }
}

void slave_get_reply(slave_msg_t msg, void *buf, size_t len)
{
   slave_msg_t actual;
   slave_get_msg(&actual, buf, &len);
   assert(msg == actual);
}

bool slave_msg_ready(void)
{
   struct pollfd pfd = {
      .fd     = slave_fd,
      .events = POLLIN
   };

   int rc = poll(&pfd, 1, 0);
   if (rc < 0)
      fatal_errno("poll");
   else
      return rc == 1;
}

bool slave_fork(void)
{
   int socks[2];
   if (socketpair(PF_LOCAL, SOCK_STREAM, 0, socks) < 0)
      fatal_errno("socketpair");

   slave_pid = fork();
   if (slave_pid < 0)
      fatal_errno("fork");
   else if (slave_pid == 0) {
      // Slave
      close(socks[0]);
      slave_fd = socks[1];
      return (am_master = false);
   }
   else {
      // Master
      close(socks[1]);
      slave_fd = socks[0];
      return (am_master = true);
   }
}

void slave_kill(int sig)
{
   assert(am_master);
}

int slave_wait(void)
{
   assert(am_master);

   int status;
   if (waitpid(slave_pid, &status, 0) < 0)
      fatal("waitpid");

   return WEXITSTATUS(status);
}

static void slave_read_signal(slave_read_signal_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   const size_t rsz =
      sizeof(reply_read_signal_msg_t) + (msg->len * sizeof(uint64_t));
   reply_read_signal_msg_t *reply = xmalloc(rsz);

   const size_t size = rt_signal_value(t, reply->values, msg->len);
   msg->len = MIN(size, msg->len);

   slave_post_msg(REPLY_READ_SIGNAL, reply, rsz);

   free(reply);
}

static void slave_now(void)
{
   reply_now_msg_t reply = {
      .now = rt_now(NULL)
   };
   fmt_time_r(reply.text, sizeof(reply.text), reply.now);
   slave_post_msg(REPLY_NOW, &reply, sizeof(reply));
}

static void slave_watch_cb(uint64_t now, tree_t decl, watch_t *w, void *user)
{
   uint64_t value[1];
   rt_watch_value(w, value, 1, false);

   uint64_t last[1];
   rt_watch_value(w, last, 1, true);

   event_watch_msg_t event = {
      .index = tree_index(decl),
      .now   = now,
      .value = value[0],
      .last  = last[0]
   };
   fmt_time_r(event.now_text, sizeof(event.now_text), now);
   slave_post_msg(EVENT_WATCH, &event, sizeof(event));
}

static void slave_watch(slave_watch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, slave_watch_cb, NULL, true);
}

static void slave_unwatch(slave_unwatch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, NULL, NULL, true);
}

bool slave_poll(void)
{
   char buf[256];
   slave_msg_t msg;
   size_t len = sizeof(buf);
   slave_get_msg(&msg, buf, &len);

   switch (msg) {
   case SLAVE_QUIT:
      return false;

   case SLAVE_RESTART:
      rt_restart(top_level);
      break;

   case SLAVE_RUN:
      rt_slave_run(((slave_run_msg_t *)buf)->time);
      break;

   case SLAVE_READ_SIGNAL:
      slave_read_signal((slave_read_signal_msg_t *)buf);
      break;

   case SLAVE_NOW:
      slave_now();
      break;

   case SLAVE_WATCH:
      slave_watch((slave_watch_msg_t *)buf);
      break;

   case SLAVE_UNWATCH:
      slave_unwatch((slave_unwatch_msg_t *)buf);
      break;

   default:
      assert(false);
   }

   return true;
}

void slave_init(tree_t top, tree_rd_ctx_t ctx)
{
   top_level = top;
   tree_rd_ctx = ctx;
}
