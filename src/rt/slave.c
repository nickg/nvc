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
