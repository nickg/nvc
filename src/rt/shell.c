//
//  Copyright (C) 2011-2023  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "eval.h"
#include "hash.h"
#include "lib.h"
#include "phase.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "scan.h"
#include "shell.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#undef DLLEXPORT
#include <tcl.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#endif

#ifdef HAVE_READLINE_HISTORY
#include <readline/history.h>
#endif

#define TIME_BUFSZ 32

typedef struct {
   const char     *name;
   Tcl_ObjCmdProc *fn;
   const char     *help;
} shell_cmd_t;

typedef struct {
   rt_signal_t *signal;
   ident_t      name;
   ident_t      path;
} shell_signal_t;

typedef struct _tcl_shell {
   char           *prompt;
   Tcl_Interp     *interp;
   shell_cmd_t    *cmds;
   size_t          ncmds;
   size_t          cmdalloc;
   rt_model_t     *model;
   tree_t          top;
   rt_scope_t     *root;
   shell_signal_t *signals;
   unsigned        nsignals;
   eval_t         *eval;
   jit_t          *jit;
   int64_t         now_var;
   unsigned        deltas_var;
} tcl_shell_t;

__attribute__((format(printf, 2, 3)))
static int tcl_error(tcl_shell_t *sh, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf LOCAL = color_vasprintf(fmt, ap);
   va_end(ap);

   Tcl_SetObjResult(sh->interp, Tcl_NewStringObj(buf, -1));
   return TCL_ERROR;
}

static bool shell_has_model(tcl_shell_t *sh)
{
   if (sh->model == NULL) {
      tcl_error(sh, "no simulation loaded, try the $bold$elaborate$$ "
                "command first");
      return false;
   }

   return true;
}

static void shell_update_now(tcl_shell_t *sh)
{
   sh->now_var = model_now(sh->model, &sh->deltas_var);

   Tcl_UpdateLinkedVar(sh->interp, "now");
   Tcl_UpdateLinkedVar(sh->interp, "deltas");
}

static int count_signals(rt_scope_t *scope)
{
   int total = list_size(scope->signals) + list_size(scope->aliases);

   for (rt_scope_t *child = scope->child; child; child = child->chain)
      total += count_signals(child);

   return total;
}

static void recurse_signals(rt_scope_t *scope, text_buf_t *path,
                            shell_signal_t **wptr)
{
   const int base = tb_len(path);

   list_foreach(rt_signal_t *, s, scope->signals) {
      shell_signal_t *ss = (*wptr)++;
      ss->signal = s;
      ss->name = ident_downcase(tree_ident(s->where));

      tb_istr(path, ss->name);
      ss->path = ident_new(tb_get(path));
      tb_trim(path, base);
   }

   list_foreach(rt_alias_t *, a, scope->aliases) {
      shell_signal_t *ss = (*wptr)++;
      ss->signal = a->signal;
      ss->name = ident_downcase(tree_ident(a->where));

      tb_istr(path, ss->name);
      ss->path = ident_new(tb_get(path));
      tb_trim(path, base);
   }

   for (rt_scope_t *child = scope->child; child; child = child->chain) {
      ident_t name = ident_downcase(tree_ident(child->where));

      tb_istr(path, name);
      tb_append(path, '/');
      recurse_signals(child, path, wptr);
      tb_trim(path, base);
   }
}

static void shell_cmd_add(tcl_shell_t *sh, const char *name, Tcl_ObjCmdProc fn,
                          const char *help)
{
   shell_cmd_t cmd = { name, fn, help };

   if (sh->cmdalloc == sh->ncmds) {
      sh->cmdalloc = MAX(sh->cmdalloc * 2, 16);
      sh->cmds = xrealloc_array(sh->cmds, sh->cmdalloc, sizeof(shell_cmd_t));
   }

   sh->cmds[sh->ncmds++] = cmd;

   Tcl_CreateObjCommand(sh->interp, name, fn, sh, NULL);
}

static const char restart_help[] =
   "Restart the simulation";

static int shell_cmd_restart(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   return tcl_error(cd, "not implemented");
}

static const char run_help[] =
   "Start or resume the simulation";

static int shell_cmd_run(ClientData cd, Tcl_Interp *interp,
                         int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;
   static bool sim_running = false;

   if (!shell_has_model(sh))
      return TCL_ERROR;
   else if (sim_running)
      return tcl_error(sh, "simulation already running");

   uint64_t stop_time = UINT64_MAX;
   if (objc == 3) {
      Tcl_WideInt base;
      int error = Tcl_GetWideIntFromObj(interp, objv[1], &base);
      if (error != TCL_OK || base <= 0)
         return tcl_error(sh, "invalid time");

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

      stop_time = model_now(sh->model, NULL) + (base * mult);
   }
   else if (objc != 1)
      return tcl_error(sh, "usage: $bold$run [time units]$$");


   sim_running = true;
   model_run(sh->model, stop_time);
   sim_running = false;

   shell_update_now(sh);

   return TCL_OK;
}

static const char exit_help[] =
   "Exit the simulation";

static int shell_cmd_exit(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   fclose(stdin);
   return TCL_OK;
}

static const char find_help[] =
   "Find signals and other objects in the design\n"
   "\n"
   "Syntax:\n"
   "  find signals <name>\n"
   "\n"
   "Examples:\n"
   "  find signals /*\tList all signals in the design\n"
   "  find signals /uut/x*\tAll signals in instance UUT that start with X\n";

static int shell_cmd_find(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (objc != 3 || strcmp(Tcl_GetString(objv[1]), "signals") != 0)
      goto usage;
   else if (!shell_has_model(sh))
      return TCL_ERROR;

   const char *glob = Tcl_GetString(objv[2]);
   Tcl_Obj *result = Tcl_NewListObj(0, NULL);

   for (int i = 0; i < sh->nsignals; i++) {
      if (!ident_glob(sh->signals[i].path, glob, -1))
         continue;

      Tcl_Obj *obj = Tcl_NewStringObj(istr(sh->signals[i].path), -1);
      Tcl_ListObjAppendElement(interp, result, obj);
   }

   Tcl_SetObjResult(interp, result);
   return TCL_OK;

 usage:
   return tcl_error(sh, "syntax error, enter $bold$help find$$ for usage");
}

static const char analyse_help[] =
   "Analyse a VHDL source file\n"
   "\n"
   "Syntax:\n"
   "  analyse [options] <file> [<file> ...]\n"
   "\n"
   "Note \"vcom\" is an alias of this command.\n"
   "\n"
   "Options:\n"
   "  -verbose\tPrint name of each analysed design unit.\n"
   "\n"
   "Examples:\n"
   "  analyse file.vhd\n"
   "  vcom file1.vhd file2.vhd\n";

static int shell_cmd_analyse(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   bool verbose = false;
   int pos = 1;
   for (const char *opt; (opt = Tcl_GetString(objv[pos]))[0] == '-'; pos++) {
      if (strcmp(opt, "-verbose") == 0)
         verbose = true;
      else
         goto usage;
   }

   if (pos == objc)
      goto usage;

   reset_error_count();

   for (; pos < objc; pos++) {
      const char *fname = Tcl_GetString(objv[pos]);

      struct stat st;
      if (access(fname, R_OK) != 0 || stat(fname, &st) != 0)
         return tcl_error(sh, "cannot open %s: %s", fname, strerror(errno));
      else if (S_ISDIR(st.st_mode))
         return tcl_error(sh, "%s is a directory", fname);
      else if (!S_ISREG(st.st_mode))
         return tcl_error(sh, "%s is not a regular file", fname);

      input_from_file(fname);

      switch (source_kind()) {
      case SOURCE_VERILOG:
         analyse_verilog(verbose);
         break;

      case SOURCE_VHDL:
         analyse_vhdl(sh->eval, verbose);
         break;
      }
   }

   return error_count() > 0 ? TCL_ERROR : TCL_OK;

 usage:
   return tcl_error(sh, "syntax error, enter $bold$help %s$$ for usage",
                    Tcl_GetString(objv[0]));
}

static const char elaborate_help[] =
   "Elaborate a design hierarchy\n"
   "\n"
   "Syntax:\n"
   "  elaborate [options] <toplevel>\n"
   "\n"
   "Note \"vsim\" is an alias of this command.\n"
   "\n"
   "Options:\n"
   "\n"
   "Examples:\n"
   "  elaborate toplevel\n"
   "  vsim toplevel\n";

static int shell_cmd_elaborate(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;
   LOCAL_TEXT_BUF tb = tb_new();

   int pos = 1;
   for (const char *opt; (opt = Tcl_GetString(objv[pos]))[0] == '-'; pos++)
      goto usage;

   if (pos + 1 != objc)
      goto usage;

   if (sh->model != NULL) {
      model_free(sh->model);
      sh->model = NULL;
   }

   lib_t work = lib_work();

   tb_istr(tb, lib_name(work));
   tb_append(tb, '.');
   tb_cat(tb, Tcl_GetString(objv[pos]));
   tb_upcase(tb);

   tree_t unit = lib_get(lib_work(), ident_new(tb_get(tb)));
   if (unit == NULL)
      return tcl_error(sh, "cannot find unit %s in library %s",
                       Tcl_GetString(objv[pos]), istr(lib_name(work)));

   reset_error_count();

   tree_t top = elab(unit, NULL);
   if (top == NULL)
      return TCL_ERROR;

   sh->top = top;
   sh->model = model_new(top, sh->jit);
   model_reset(sh->model);

   if ((sh->root = find_scope(sh->model, tree_stmt(top, 0))) == NULL)
      fatal_trace("cannot find root scope");

   sh->nsignals = count_signals(sh->root);
   sh->signals = xcalloc_array(sh->nsignals, sizeof(shell_signal_t));

   text_buf_t *path = tb_new();
   shell_signal_t *wptr = sh->signals;
   tb_cat(path, "/");
   recurse_signals(sh->root, path, &wptr);
   assert(wptr == sh->signals + sh->nsignals);
   tb_free(path);

   shell_update_now(sh);

   return TCL_OK;

 usage:
   return tcl_error(sh, "syntax error, enter $bold$help %s$$ for usage",
                    Tcl_GetString(objv[0]));
}

static const char help_help[] =
   "Display list of commands or detailed help\n"
   "\n"
   "Use $bold$help <command>$$ to display detailed usage of a particular\n"
   "command.\n";

static int shell_cmd_help(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (objc == 2) {
      const char *which = Tcl_GetString(objv[1]);
      for (int i = 0; i < sh->ncmds; i++) {
         if (strcmp(sh->cmds[i].name, which) == 0) {
            fputs(sh->cmds[i].help, stdout);
            return TCL_OK;
         }
      }

      return tcl_error(sh, "invalid command '%s'", which);
   }
   else if (objc != 1)
      return tcl_error(sh, "syntax error, try $bold$help$$");

   printf("List of supported commands:\n");

   for (shell_cmd_t *c = sh->cmds; c < sh->cmds + sh->ncmds; c++) {
      const int linelen = strchrnul(c->help, '\n') - c->help;
      color_printf("  $bold$%-16s$$", c->name);
      fwrite(c->help, linelen, 1, stdout);
      printf("\n");
   }

   printf("\n");
   wrapped_printf("Use $bold$help <command>$$ for detailed usage of a "
                  "particular command. Standard TCL commands are also "
                  "accepted.\n");

   return TCL_OK;
}

static const char copyright_help[] = "Display copyright information";

static int shell_cmd_copyright(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   extern char copy_string[];
   printf("%s\n", copy_string);
   return TCL_OK;
}

static char *shell_get_line(tcl_shell_t *sh)
{
   if (isatty(fileno(stdin))) {
#ifdef HAVE_LIBREADLINE
      char *buf = readline(sh->prompt);
      if ((buf != NULL) && (*buf != '\0'))
         add_history(buf);
      return buf;
#endif  // HAVE_LIBREADLINE
   }

   fputs(sh->prompt, stdout);
   fflush(stdout);

   LOCAL_TEXT_BUF tb = tb_new();

   size_t off = 0;
   for (;;) {
      int ch = fgetc(stdin);
      fputc(ch, stdout);
      switch (ch) {
      case EOF:
         return (off > 0) ? tb_claim(tb) : NULL;
      case '\n':
         return tb_claim(tb);
      default:
         tb_append(tb, ch);
      }
   }
}

static void show_banner(void)
{
   extern const char version_string[];
   printf("\n");
   print_centred(version_string);
   wrapped_printf(
      "\n\nThis program comes with ABSOLUTELY NO WARRANTY. This is free "
      "software, and you are welcome to redistribute it under certain "
      "conditions; type $bold$copyright$$ for details.\n\n"
      "Type $bold$help$$ for a list of supported commands.\n\n",
      version_string);
}

static int compare_shell_cmd(const void *a, const void *b)
{
   return strcmp(((shell_cmd_t *)a)->name, ((shell_cmd_t *)b)->name);
}

tcl_shell_t *shell_new(jit_t *jit)
{
   tcl_shell_t *sh = xcalloc(sizeof(tcl_shell_t));
   sh->prompt = color_asprintf("$+cyan$%%$$ ");
   sh->interp = Tcl_CreateInterp();
   sh->eval   = eval_new();
   sh->jit    = jit;

   Tcl_LinkVar(sh->interp, "now", (char *)&sh->now_var,
               TCL_LINK_WIDE_INT | TCL_LINK_READ_ONLY);
   Tcl_LinkVar(sh->interp, "deltas", (char *)&sh->deltas_var,
               TCL_LINK_UINT | TCL_LINK_READ_ONLY);

   atexit(Tcl_Finalize);

   shell_cmd_add(sh, "help", shell_cmd_help, help_help);
   shell_cmd_add(sh, "exit", shell_cmd_exit, exit_help);
   shell_cmd_add(sh, "copyright", shell_cmd_copyright, copyright_help);
   shell_cmd_add(sh, "find", shell_cmd_find, find_help);
   shell_cmd_add(sh, "run", shell_cmd_run, run_help);
   shell_cmd_add(sh, "restart", shell_cmd_restart, restart_help);
   shell_cmd_add(sh, "analyse", shell_cmd_analyse, analyse_help);
   shell_cmd_add(sh, "vcom", shell_cmd_analyse, analyse_help);
   shell_cmd_add(sh, "elaborate", shell_cmd_elaborate, elaborate_help);
   shell_cmd_add(sh, "vsim", shell_cmd_elaborate, elaborate_help);

   qsort(sh->cmds, sh->ncmds, sizeof(shell_cmd_t), compare_shell_cmd);

   return sh;
}

void shell_free(tcl_shell_t *sh)
{
   if (sh->model != NULL)
      model_free(sh->model);

   eval_free(sh->eval);
   Tcl_DeleteInterp(sh->interp);
   free(sh);
}

bool shell_eval(tcl_shell_t *sh, const char *script, const char **result)
{
   const int code = Tcl_Eval(sh->interp, script);
   const char *str = Tcl_GetStringResult(sh->interp);

   switch (code) {
   case TCL_OK:
      if (result != NULL)
         *result = str;
      return true;
   case TCL_ERROR:
      if (str != NULL && *str != '\0')
         errorf("%s", str);
      return false;
   default:
      warnf("Tcl_Eval returned unknown code %d", code);
      return false;
   }
}

void shell_interact(tcl_shell_t *sh)
{
   show_banner();

   char *line;
   while ((line = shell_get_line(sh))) {
      const char *result = NULL;
      if (shell_eval(sh, line, &result) && *result != '\0')
         color_printf("$+black$%s$$\n", result);

      free(line);
   }
}

bool shell_do(tcl_shell_t *sh, const char *file)
{
   const int code = Tcl_EvalFile(sh->interp, file);

   switch (code) {
   case TCL_OK:
      return true;
   case TCL_ERROR:
      {
         const char *str = Tcl_GetStringResult(sh->interp);
         if (str != NULL && *str != '\0')
            errorf("%s", str);
         return false;
      }
   default:
      warnf("Tcl_Eval returned unknown code %d", code);
      return false;
   }
}
