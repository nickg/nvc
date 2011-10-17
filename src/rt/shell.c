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

#include <assert.h>
#include <tcl.h>

static int shell_cmd_restart(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   slave_post_msg(SLAVE_RESTART, NULL, 0);
   return TCL_OK;
}

static int shell_cmd_run(ClientData cd, Tcl_Interp *interp,
                         int objc, Tcl_Obj *const objv[])
{
   slave_run_msg_t msg = {
      .time = 0
   };
   slave_post_msg(SLAVE_RUN, &msg, sizeof(msg));
   return TCL_OK;
}

static int shell_cmd_quit(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   slave_post_msg(SLAVE_QUIT, NULL, 0);
   bool *have_quit = (bool*)cd;
   *have_quit = true;
   return TCL_OK;
}

void shell_run(tree_t e)
{
   Tcl_Interp *interp = Tcl_CreateInterp();

   bool have_quit = false;

   Tcl_CreateObjCommand(interp, "quit", shell_cmd_quit, &have_quit, NULL);
   Tcl_CreateObjCommand(interp, "run", shell_cmd_run, NULL, NULL);
   Tcl_CreateObjCommand(interp, "restart", shell_cmd_restart, NULL, NULL);

   slave_post_msg(SLAVE_RESTART, NULL, 0);

   char line[256];
   while (!have_quit && fgets(line, sizeof(line), stdin)) {
      switch (Tcl_Eval(interp, line)) {
      case TCL_OK:
         break;
      case TCL_ERROR:
         fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
         break;
      default:
         assert(false);
      }
   }

   if (!have_quit)
      slave_post_msg(SLAVE_QUIT, NULL, 0);

   Tcl_DeleteInterp(interp);
   slave_wait();
}
