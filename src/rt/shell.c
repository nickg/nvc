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

#include "util.h"
#include "slave.h"
#include "rt.h"
#include "tree.h"

static void shell_cmd_quit(void)
{
   slave_post_msg(SLAVE_QUIT, NULL, 0);
}

static void shell_cmd_restart(void)
{
   slave_post_msg(SLAVE_RESTART, NULL, 0);
}

static void shell_cmd_run(uint64_t time)
{
   slave_run_msg_t msg = {
      .time = time
   };
   slave_post_msg(SLAVE_RUN, &msg, sizeof(msg));
}

void shell_run(tree_t e)
{
   shell_cmd_restart();
   shell_cmd_run(0);
   shell_cmd_quit();
   slave_wait();
}
