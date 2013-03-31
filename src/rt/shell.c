//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include "common.h"
#include "hash.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <inttypes.h>

#if defined HAVE_TCL_TCL_H
#include <tcl/tcl.h>
#elif defined HAVE_TCL_H
#include <tcl.h>
#endif

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#endif

#ifdef HAVE_READLINE_HISTORY
#include <readline/history.h>
#endif

typedef struct {
   const char     *name;
   Tcl_ObjCmdProc *fn;
   ClientData      cd;
   const char     *help;
} shell_cmd_t;

#define CMD(name, cd, help) \
   { #name, shell_cmd_##name, cd, help }

__attribute__((format(printf, 2, 3)))
static int tcl_error(Tcl_Interp *interp, const char *fmt, ...)
{
   va_list ap;
   char buf[1024];
   va_start(ap, fmt);
   vsnprintf(buf, sizeof(buf), fmt, ap);
   va_end(ap);

   Tcl_SetObjResult(interp, Tcl_NewStringObj(buf, -1));
   return TCL_ERROR;
}

static void event_watch(event_watch_msg_t *event, tree_rd_ctx_t ctx)
{
   tree_t decl = tree_read_recall(ctx, event->index);

   printf("%s: update %s ", event->now_text, istr(tree_ident(decl)));
   printf("%s -> ", pprint(decl, &(event->last), 1));
   printf("%s\n", pprint(decl, &(event->value), 1));
}

static bool show_help(int objc, Tcl_Obj *const objv[], const char *help)
{
   for (int i = 1; i < objc; i++) {
      const char *what = Tcl_GetString(objv[1]);
      if (strcmp(what, "-help") == 0) {
         printf("%s", help);
         return true;
      }
   }

   return false;
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

   tree_rd_ctx_t tree_rd_ctx = cd;

   slave_msg_t event;
   do {
      union {
         event_watch_msg_t watch;
      } payload;
      size_t sz = sizeof(payload);
      slave_get_msg(&event, &payload, &sz);

      switch (event) {
      case EVENT_STOP:
         break;
      case EVENT_WATCH:
         event_watch(&payload.watch, tree_rd_ctx);
         break;
      default:
         fatal("unhandled slave event %d", event);
      }
   } while (event != EVENT_STOP);

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

static int shell_cmd_signals(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   const char *help =
      "signals - Find signal objects in the design\n"
      "\n"
      "Usage: signals [GLOB]\n"
      "\n"
      "Returns a list of signal names. Pass to `show' to see current values\n"
      "and types. GLOB is either the fully qualified name of a signal; a\n"
      "pattern containing a wildcard character '*'; or an unqualified name\n"
      "without the hierarchy separator ':' which matches that name at any\n"
      "level.\n"
      "\n"
      "Examples:\n"
      "  signals            List of all signal objects\n"
      "  signals {*foo*}    Find signals containing `foo'\n"
      "  signals {*:clk}    Find signals named `clk' at any level\n"
      "  signals {clk}      Equivalent to the above\n"
      "  show [signals]     Display current value of all signals\n";

   if (show_help(objc, objv, help))
      return TCL_OK;

   tree_t top = cd;
   const int ndecls = tree_decls(top);

   Tcl_Obj *result = Tcl_NewListObj(0, NULL);

   if (objc == 1) {
      for (int j = 0; j < ndecls; j++) {
         tree_t d = tree_decl(top, j);
         if (tree_kind(d) != T_SIGNAL_DECL)
            continue;

         Tcl_Obj *obj = Tcl_NewStringObj(istr(tree_ident(d)), -1);
         Tcl_ListObjAppendElement(interp, result, obj);
      }
   }
   else {
      for (int i = 1; i < objc; i++) {
         const char *glob = Tcl_GetString(objv[i]);
         char *expand = NULL;
         if ((*glob != ':') && (*glob != '*')) {
            expand = xmalloc(strlen(glob) + 3);
            strcpy(expand, "*:");
            strcpy(expand + 2, glob);

            glob = expand;
         }

         for (int j = 0; j < ndecls; j++) {
            tree_t d = tree_decl(top, j);
            if (tree_kind(d) != T_SIGNAL_DECL)
               continue;

            ident_t name = tree_ident(d);
            if (ident_glob(name, glob, -1)) {
               Tcl_Obj *obj = Tcl_NewStringObj(istr(name), -1);
               Tcl_ListObjAppendElement(interp, result, obj);
            }
         }

         if (expand != NULL)
            free(expand);
      }
   }

   Tcl_SetObjResult(interp, result);
   return TCL_OK;
}

static int shell_cmd_show(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   const char *help =
      "show - Display simulation objects\n"
      "\n"
      "Usage: show LIST...\n"
      "\n"
      "Prints a representation of each simulation object in LIST. Typically\n"
      "this will be a list of signal names and the output will show their\n"
      "current value.\n"
      "\n"
      "Examples:\n"
      "  show {:top:foo}      Print value of signal :top_foo\n"
      "  show [signals]       Print value of all signals\n";

   if (show_help(objc, objv, help))
      return TCL_OK;

   if (objc == 1) {
      warnf("nothing to show (try -help for usage)");
      return TCL_OK;
   }

   hash_t *decl_hash = (hash_t *)cd;

   for (int i = 1; i < objc; i++) {
      int length;
      if (Tcl_ListObjLength(interp, objv[i], &length) != TCL_OK)
         return TCL_ERROR;

      for (int j = 0; j < length; j++) {
         Tcl_Obj *obj;
         if (Tcl_ListObjIndex(interp, objv[i], j, &obj) != TCL_OK)
            return TCL_ERROR;

         const char *str = Tcl_GetString(obj);

         tree_t t = hash_get(decl_hash, ident_new(str));
         if (t == NULL)
            return tcl_error(interp, "object not found: %s", str);

         tree_kind_t kind = tree_kind(t);
         switch (kind) {
         case T_SIGNAL_DECL:
            {
               size_t len = 1;
               type_t type = tree_type(t);
               while (type_is_array(type)) {
                  int64_t low = 0, high = 0;
                  range_bounds(type_dim(type, 0), &low, &high);
                  len *= (high - low + 1);

                  type = type_elem(type);
               }

               slave_read_signal_msg_t msg = {
                  .index = tree_index(t),
                  .len   = len
               };
               slave_post_msg(SLAVE_READ_SIGNAL, &msg, sizeof(msg));

               const size_t rsz =
                  sizeof(reply_read_signal_msg_t)
                  + (msg.len * sizeof(uint64_t));
               reply_read_signal_msg_t *reply = xmalloc(rsz);
               slave_get_reply(REPLY_READ_SIGNAL, reply, rsz);

               const char *type_str = type_pp(type);
               const char *short_name = strrchr(type_str, '.');

               printf("%-30s%-20s%s\n",
                      str,
                      (short_name != NULL ? short_name + 1 : type_str),
                      pprint(t, reply->values, msg.len));

               free(reply);
            }
            break;

         default:
            return tcl_error(interp, "cannot show tree kind %s",
                             tree_kind_str(kind));
         }
      }
   }

   return TCL_OK;
}

static int shell_cmd_now(ClientData cd, Tcl_Interp *interp,
                         int objc, Tcl_Obj *const objv[])
{
   const char *help =
      "now - Display current simulation time\n"
      "\n"
      "Usage: now [-q]\n"
      "\n"
      "Prints the current simulation time to the standard output unless -q\n"
      "is specified. Returns the time as a string.\n"
      "\n"
      "Examples:\n"
      "  now               Print current time\n"
      "  set n [now -q]    Store current time in a variable\n";

   if (show_help(objc, objv, help))
      return TCL_OK;

   bool quiet = false;

   for (int i = 1; i < objc; i++) {
      const char *what = Tcl_GetString(objv[i]);
      if (strcmp(what, "-q") == 0)
         quiet = true;
      else
         return tcl_error(interp, "invalid argument '%s' "
                          "(try -help for usage)", what);
   }

   slave_post_msg(SLAVE_NOW, NULL, 0);

   reply_now_msg_t reply;
   slave_get_reply(REPLY_NOW, &reply, sizeof(reply));

   if (!quiet)
      printf("%s\n", reply.text);

   Tcl_SetObjResult(interp, Tcl_NewIntObj(reply.now));
   return TCL_OK;
}

static int shell_cmd_watch(ClientData cd, Tcl_Interp *interp,
                           int objc, Tcl_Obj *const objv[])
{
   const char *help =
      "watch - Trace changes to a signal\n"
      "\n"
      "Usage: watch SIGNALS...\n"
      "\n"
      "Prints a message every time an update occurs to a signal listed."
      "\n"
      "Examples:\n"
      "  watch [signals {clk}]  Trace updates to all signals named clk\n";

   if (show_help(objc, objv, help))
      return TCL_OK;

   if (objc == 1) {
      warnf("nothing to watch (try -help for usage)");
      return TCL_OK;
   }

   hash_t *decl_hash = (hash_t *)cd;

   for (int i = 1; i < objc; i++) {
      int length;
      if (Tcl_ListObjLength(interp, objv[i], &length) != TCL_OK)
         return TCL_ERROR;

      for (int j = 0; j < length; j++) {
         Tcl_Obj *obj;
         if (Tcl_ListObjIndex(interp, objv[i], j, &obj) != TCL_OK)
            return TCL_ERROR;

         const char *str = Tcl_GetString(obj);

         tree_t t = hash_get(decl_hash, ident_new(str));
         if (t == NULL)
            return tcl_error(interp, "object not found: %s", str);

         if (t == NULL)
            return tcl_error(interp, "object not found: %s", str);
         else if (tree_kind(t) != T_SIGNAL_DECL)
            return tcl_error(interp, "not a signal: %s", str);
         else if (type_is_array(tree_type(t)))
            return tcl_error(interp, "only scalar signals may be watched");
         // TODO: make this work for arrays

         slave_watch_msg_t msg = {
            .index = tree_index(t)
         };
         slave_post_msg(SLAVE_WATCH, &msg, sizeof(msg));
      }
   }

   return TCL_OK;
}

static int shell_cmd_unwatch(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   const char *help =
      "unwatch - Stop tracing signals\n"
      "\n"
      "Usage: unwatch SIGNALS...\n"
      "\n"
      "Clears any watch callback on SIGNALS. Note this will also stop any\n"
      "VCD or other waveform capture for these signals.\n"
      "\n"
      "Examples:\n"
      "  watch [signals {clk}]  Stop tracing updates to clk\n";

   if (show_help(objc, objv, help))
      return TCL_OK;

   if (objc == 1) {
      warnf("nothing to unwatch (try -help for usage)");
      return TCL_OK;
   }

   hash_t *decl_hash = (hash_t *)cd;

   // TODO: refactor this code to avoid duplication with "watch" and "show"
   for (int i = 1; i < objc; i++) {
      int length;
      if (Tcl_ListObjLength(interp, objv[i], &length) != TCL_OK)
         return TCL_ERROR;

      for (int j = 0; j < length; j++) {
         Tcl_Obj *obj;
         if (Tcl_ListObjIndex(interp, objv[i], j, &obj) != TCL_OK)
            return TCL_ERROR;

         const char *str = Tcl_GetString(obj);

         tree_t t = hash_get(decl_hash, ident_new(str));
         if (t == NULL)
            return tcl_error(interp, "object not found: %s", str);
         else if (tree_kind(t) != T_SIGNAL_DECL)
            return tcl_error(interp, "not a signal: %s", str);

         slave_unwatch_msg_t msg = {
            .index = tree_index(t)
         };
         slave_post_msg(SLAVE_UNWATCH, &msg, sizeof(msg));
      }
   }

   return TCL_OK;
}

static int shell_cmd_help(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   printf("NVC commands:\n");

   for (shell_cmd_t *c = cd; c->name != NULL; c++)
      printf("  %-16s%s\n", c->name, c->help);

   printf(
      "\n"
      "Use -help on each command for detailed usage.\n"
      "Standard TCL commands are also accepted.\n");

   return TCL_OK;
}

static int shell_cmd_copyright(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   extern char *copy_string;
   printf("%s\n", copy_string);
   return TCL_OK;
}

static char *shell_get_line(void)
{
   if (isatty(fileno(stdin))) {
#ifdef HAVE_LIBREADLINE
      char *buf = readline("% ");
      if ((buf != NULL) && (*buf != '\0'))
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

   printf("\nBye.\n");
}

static void show_banner(void)
{
   extern const char *version_string;
   printf("%s\n", version_string);
   printf("Type \"help\" or \"copyright\" for more information.\n");
}

static int compare_shell_cmd(const void *a, const void *b)
{
   return strcmp(((shell_cmd_t *)a)->name, ((shell_cmd_t *)b)->name);
}

void shell_run(tree_t e, struct tree_rd_ctx *ctx)
{
   const int ndecls = tree_decls(e);
   hash_t *decl_hash = hash_new(ndecls * 2, true);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(e, i);
      hash_put(decl_hash, tree_ident(d), d);
   }

   Tcl_Interp *interp = Tcl_CreateInterp();

   bool have_quit = false;

   Tcl_CreateExitHandler(shell_exit_handler, &have_quit);

   shell_cmd_t shell_cmds[] = {
      CMD(quit,      &have_quit, "Exit simulation"),
      CMD(run,       ctx,        "Start or resume simulation"),
      CMD(restart,   NULL,       "Restart simulation"),
      CMD(show,      decl_hash,  "Display simulation objects"),
      CMD(help,      shell_cmds, "Display this message"),
      CMD(copyright, NULL,       "Display copyright information"),
      CMD(signals,   e,          "Find signal objects in the design"),
      CMD(now,       NULL,       "Display current simulation time"),
      CMD(watch,     decl_hash,  "Trace changes to a signal"),
      CMD(unwatch,   decl_hash,  "Stop tracing signals"),

      { NULL, NULL, NULL, NULL}
   };

   qsort(shell_cmds, ARRAY_LEN(shell_cmds) - 1, sizeof(shell_cmd_t),
         compare_shell_cmd);

   for (shell_cmd_t *c = shell_cmds; c->name != NULL; c++)
      Tcl_CreateObjCommand(interp, c->name, c->fn, c->cd, NULL);

   show_banner();

   slave_post_msg(SLAVE_RESTART, NULL, 0);

   char *line;
   while (!have_quit && (line = shell_get_line())) {
      switch (Tcl_Eval(interp, line)) {
      case TCL_OK:
         break;
      case TCL_ERROR:
         errorf("%s", Tcl_GetStringResult(interp));
         break;
      default:
         assert(false);
      }

      free(line);
   }

   Tcl_Exit(EXIT_SUCCESS);
}
