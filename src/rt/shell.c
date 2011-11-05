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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <tcl/tcl.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#endif

#ifdef HAVE_READLINE_HISTORY
#include <readline/history.h>
#endif

static const char *shell_fmt_signal_value(tree_t t, uint64_t value)
{
   static char buf[256];

   type_t type = tree_type(t);
   switch (type_kind(type)) {
   case T_INTEGER:
      snprintf(buf, sizeof(buf), "%"PRIi64, value);
      break;

   case T_ENUM:
      assert(value < type_enum_literals(type));
      snprintf(buf, sizeof(buf), "%s",
               istr(tree_ident(type_enum_literal(type, value))));
      break;

   default:
      snprintf(buf, sizeof(buf), "%"PRIx64, value);
   }

   return buf;
}

static int shell_cmd_restart(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   slave_post_msg(SLAVE_RESTART, NULL, 0);
   return TCL_OK;
}

static int shell_cmd_run(ClientData cd, Tcl_Interp *interp,
                         int objc, Tcl_Obj *const objv[])
{
   uint64_t time = UINT64_MAX;
   if (objc == 3) {
      Tcl_WideInt base;
      int error = Tcl_GetWideIntFromObj(interp, objv[1], &base);
      if (error != TCL_OK || base <= 0) {
         fprintf(stderr, "invalid time\n");
         return TCL_ERROR;
      }
      const char *unit = Tcl_GetString(objv[2]);

      uint64_t mult;
      if      (strcmp(unit, "fs") == 0) mult = 1;
      else if (strcmp(unit, "ps") == 0) mult = 1000;
      else if (strcmp(unit, "ns") == 0) mult = 1000000;
      else if (strcmp(unit, "us") == 0) mult = 1000000000;
      else if (strcmp(unit, "ms") == 0) mult = 1000000000000;
      else {
         fprintf(stderr, "invalid time unit %s", unit);
         return TCL_ERROR;
      }

      time = base * mult;
   }
   else if (objc != 1) {
      fprintf(stderr, "usage: run [time units]\n");
      return TCL_ERROR;
   }

   slave_run_msg_t msg = { .time = time };
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

static int shell_cmd_show(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tree_t top = cd;

   if (objc == 1) {
      fprintf(stderr, "try 'show help' for usage\n");
      return TCL_ERROR;
   }

   const char *what = Tcl_GetString(objv[1]);
   if (strcmp(what, "help") == 0) {
      printf("Usage: show [something]\n"
             "  signals - list all signals in design with current value\n"
             "  process - list all processes in design\n");
   }
   else if (strcmp(what, "signals") == 0) {
      for (unsigned i = 0; i < tree_decls(top); i++) {
         tree_t d = tree_decl(top, i);

         slave_read_signal_msg_t msg = { .index = tree_index(d) };
         slave_post_msg(SLAVE_READ_SIGNAL, &msg, sizeof(msg));

         reply_read_signal_msg_t reply;
         slave_get_reply(REPLY_READ_SIGNAL, &reply, sizeof(reply));

         printf("%-30s%-25s%s\n",
                istr(tree_ident(d)),
                type_pp(tree_type(d)),
                shell_fmt_signal_value(d, reply.value));
      }
   }
   else if (strcmp(what, "process") == 0) {
      for (unsigned i = 0; i < tree_stmts(top); i++) {
         tree_t p = tree_stmt(top, i);
         printf("%s\n", istr(tree_ident(p)));
      }
   }
   else {
      fprintf(stderr, "cannot show '%s' - try 'show help' for usage\n", what);
      return TCL_ERROR;
   }

   return TCL_OK;
}

static char *shell_get_line(void)
{
   if (isatty(fileno(stdin))) {
#ifdef HAVE_LIBREADLINE
      char *buf = readline("% ");
      add_history(buf);
      return buf;
#else   // HAVE_LIBREADLINE
      printf("%% ");
      fflush(stdout);
#endif  // HAVE_LIBREADLINE
   }

   size_t buflen = 256;
   char *buf = xmalloc(buflen);

   size_t off = 0;
   for (;;) {
      if (off == buflen) {
         buflen *= 2;
         buf = xrealloc(buf, buflen);
      }

      int ch = fgetc(stdin);
      switch (ch) {
      case EOF:
         buf[off] = '\0';
         return (off > 0 ? buf : NULL);

      case '\n':
         buf[off] = '\0';
         return buf;

      default:
         buf[off++] = ch;
      }
   }
}

static void shell_exit_handler(ClientData cd)
{
   bool *have_quit = cd;

   if (!*have_quit)
      slave_post_msg(SLAVE_QUIT, NULL, 0);

   slave_wait();
}

void shell_run(tree_t e)
{
   Tcl_Interp *interp = Tcl_CreateInterp();

   bool have_quit = false;

   Tcl_CreateExitHandler(shell_exit_handler, &have_quit);

   Tcl_CreateObjCommand(interp, "quit", shell_cmd_quit, &have_quit, NULL);
   Tcl_CreateObjCommand(interp, "run", shell_cmd_run, NULL, NULL);
   Tcl_CreateObjCommand(interp, "restart", shell_cmd_restart, NULL, NULL);
   Tcl_CreateObjCommand(interp, "show", shell_cmd_show, e, NULL);

   slave_post_msg(SLAVE_RESTART, NULL, 0);

   char *line;
   while (!have_quit && (line = shell_get_line())) {
      switch (Tcl_Eval(interp, line)) {
      case TCL_OK:
         break;
      case TCL_ERROR:
         fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
         break;
      default:
         assert(false);
      }

      free(line);
   }

   Tcl_Exit(EXIT_SUCCESS);
}
