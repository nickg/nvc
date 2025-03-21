//
//  Copyright (C) 2011-2025  Nick Gasson
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
#include "hash.h"
#include "printer.h"
#include "rt/assert.h"
#include "rt/model.h"
#include "rt/structs.h"
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

#include <readline/readline.h>
#include <readline/history.h>

#undef DLLEXPORT
#include <tcl.h>

#define TIME_BUFSZ 32

#if TCL_MAJOR_VERSION < 9
typedef int Tcl_Size;
#endif

typedef struct {
   const char     *name;
   Tcl_ObjCmdProc *fn;
   const char     *help;
} shell_cmd_t;

typedef enum {
   SHELL_SIGNAL,
   SHELL_REGION,
} object_kind_t;

typedef struct {
   object_kind_t kind;
   ident_t       name;
   ident_t       path;
} shell_object_t;

typedef struct {
   shell_object_t  obj;
   rt_signal_t    *signal;
   print_func_t   *printer;
   rt_watch_t     *watch;
   tcl_shell_t    *owner;
} shell_signal_t;

typedef struct {
   shell_object_t  obj;
   rt_scope_t     *scope;
} shell_region_t;

typedef char *(*get_line_fn_t)(tcl_shell_t *);

typedef struct _tcl_shell {
   char            *prompt;
   Tcl_Interp      *interp;
   shell_cmd_t     *cmds;
   size_t           ncmds;
   size_t           cmdalloc;
   rt_model_t      *model;
   tree_t           top;
   rt_scope_t      *root;
   shell_signal_t  *signals;
   unsigned         nsignals;
   shell_region_t  *regions;
   unsigned         nregions;
   hash_t          *namemap;
   jit_t           *jit;
   int64_t          now_var;
   unsigned         deltas_var;
   printer_t       *printer;
   get_line_fn_t    getline;
   shell_handler_t  handler;
   bool             quit;
   char            *datadir;
} tcl_shell_t;

static __thread tcl_shell_t *rl_shell = NULL;

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

static void tcl_dict_put(tcl_shell_t *sh, Tcl_Obj *dict, const char *key,
                         Tcl_Obj *value)
{
   Tcl_DictObjPut(sh->interp, dict, Tcl_NewStringObj(key, -1), value);
}

static Tcl_Obj *tcl_ident_string(ident_t ident)
{
   return Tcl_NewStringObj(istr(ident), ident_len(ident));
}

static int syntax_error(tcl_shell_t *sh, Tcl_Obj *const objv[])
{
   return tcl_error(sh, "syntax error, enter $bold$help %s$$ for usage",
                    Tcl_GetString(objv[0]));
}

__attribute__((format(printf, 2, 3)))
static void shell_printf(tcl_shell_t *sh, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   if (sh->handler.stdout_write != NULL) {
      char *buf LOCAL = color_vasprintf(fmt, ap);
      (*sh->handler.stdout_write)(buf, strlen(buf), sh->handler.context);
   }
   else
      wrapped_vprintf(fmt, ap);

   va_end(ap);
}

static bool shell_has_model(tcl_shell_t *sh)
{
   if (sh->model == NULL) {
      tcl_error(sh, "no simulation loaded, elaborate first with the "
                "$bold$-e$$ command or pass the top-level design unit "
                "as a command line argument");
      return false;
   }

   return true;
}

static void shell_next_time_step(rt_model_t *m, void *user)
{
   tcl_shell_t *sh = user;
   assert(sh->handler.next_time_step != NULL);

   uint64_t now = model_now(m, NULL);
   (*sh->handler.next_time_step)(now, sh->handler.context);

   model_set_global_cb(sh->model, RT_NEXT_TIME_STEP, shell_next_time_step, sh);
}

static void shell_create_model(tcl_shell_t *sh)
{
   assert(sh->model == NULL);

   sh->model = model_new(sh->jit, NULL);
   create_scope(sh->model, sh->top, NULL);

   if (sh->handler.next_time_step != NULL)
      model_set_global_cb(sh->model, RT_NEXT_TIME_STEP,
                          shell_next_time_step, sh);

   model_reset(sh->model);

   if ((sh->root = find_scope(sh->model, tree_stmt(sh->top, 0))) == NULL)
      fatal_trace("cannot find root scope");
}

static void shell_update_now(tcl_shell_t *sh)
{
   sh->now_var = model_now(sh->model, &sh->deltas_var);

   Tcl_UpdateLinkedVar(sh->interp, "now");
   Tcl_UpdateLinkedVar(sh->interp, "deltas");
}

static bool shell_get_printer(tcl_shell_t *sh, shell_signal_t *ss)
{
   if (ss->printer == NULL)
      ss->printer = printer_for(sh->printer, tree_type(ss->signal->where));

   if (ss->printer == NULL) {
      tcl_error(sh, "cannot display type %s",
                type_pp(tree_type(ss->signal->where)));
      return false;
   }

   return true;
}

static void shell_add_cmd(tcl_shell_t *sh, const char *name, Tcl_ObjCmdProc fn,
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

static void shell_event_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                           void *user)
{
   shell_signal_t *ss = user;
   shell_handler_t *h = &(ss->owner->handler);

   if (h->signal_update != NULL) {
      const char *enc = print_signal(ss->printer, ss->signal, PRINT_F_ENCODE);
      (*h->signal_update)(ss->obj.path, now, s, enc, h->context);
   }
}

static void watch_signal(shell_signal_t *ss)
{
   ss->watch = watch_new(ss->owner->model, shell_event_cb, ss,
                         WATCH_POSTPONED, 1);
   model_set_event_cb(ss->owner->model, ss->signal, ss->watch);
}

static void recreate_objects(tcl_shell_t *sh, rt_scope_t *scope,
                             shell_signal_t **sptr, shell_region_t **rptr)
{
   shell_region_t *r = (*rptr)++;
   assert(r->obj.name == ident_downcase(tree_ident(scope->where)));
   r->scope = scope;

   for (int i = 0; i < scope->signals.count; i++) {
      shell_signal_t *ss = (*sptr)++;
      ss->signal = scope->signals.items[i];
      assert(ss->obj.name == ident_downcase(tree_ident(ss->signal->where)));

      if (ss->watch != NULL)
         watch_signal(ss);
   }

   for (int i = 0; i < scope->aliases.count; i++) {
      rt_alias_t *a = scope->aliases.items[i];

      shell_signal_t *ss = (*sptr)++;
      assert(ss->obj.name == ident_downcase(tree_ident(a->where)));
      ss->signal = a->signal;

      if (ss->watch != NULL)
         watch_signal(ss);
   }

   for (int i = 0; i < scope->children.count; i++)
      recreate_objects(sh, scope->children.items[i], sptr, rptr);
}

const char *next_option(int *pos, int objc, Tcl_Obj *const objv[])
{
   if (*pos >= objc)
      return NULL;

   const char *opt = Tcl_GetString(objv[*pos]);
   if (opt[0] != '-')
      return NULL;

   (*pos)++;
   return opt;
}

static shell_object_t *get_object(tcl_shell_t *sh, const char *name)
{
   shell_object_t *obj = hash_get(sh->namemap, ident_new(name));
   if (obj == NULL)
      tcl_error(sh, "cannot find name '%s'", name);

   return obj;
}

static shell_signal_t *get_signal(tcl_shell_t *sh, const char *name)
{
   shell_object_t *obj = get_object(sh, name);
   if (obj == NULL)
      return NULL;
   else if (obj->kind != SHELL_SIGNAL) {
      tcl_error(sh, "'%s' is not a signal", name);
      return NULL;
   }

   return container_of(obj, shell_signal_t, obj);
}

static const char restart_help[] =
   "Restart the simulation";

static int shell_cmd_restart(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (!shell_has_model(sh))
      return TCL_ERROR;

   model_free(sh->model);
   sh->model = NULL;

   jit_reset(sh->jit);

   clear_vhdl_assert();
   for (vhdl_severity_t s = SEVERITY_NOTE; s <= SEVERITY_FAILURE; s++)
      set_vhdl_assert_enable(s, true);

   shell_create_model(sh);

   shell_signal_t *wptr = sh->signals;
   shell_region_t *rptr = sh->regions;
   recreate_objects(sh, sh->root, &wptr, &rptr);
   assert(wptr == sh->signals + sh->nsignals);
   assert(rptr == sh->regions + sh->nregions);

   shell_update_now(sh);

   if (sh->handler.restart_sim != NULL)
      (*sh->handler.restart_sim)(sh->handler.context);

   return TCL_OK;
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
      else
         return tcl_error(sh, "invalid time unit %s", unit);

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

static const char find_help[] =
   "Find signals and other objects in the design\n"
   "\n"
   "Syntax:\n"
   "  find signals [options] <pattern>\n"
   "  find regions [options] <pattern>\n"
   "\n"
   "Options:\n"
   "  -r, -recursive\tInclude subregions in wildcard search.\n"
   "\n"
   "Examples:\n"
   "  find signals -r /*\tList all signals in the design\n"
   "  find signals /uut/x*\tAll signals in instance UUT that start with X\n"
   "  find regions -r *\tList all regions in the design\n";

static int shell_cmd_find(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   enum { SIGNALS, REGIONS } what;
   if (!shell_has_model(sh))
      return TCL_ERROR;
   else if (objc < 3)
      goto usage;
   else if (strcmp(Tcl_GetString(objv[1]), "signals") == 0)
      what = SIGNALS;
   else if (strcmp(Tcl_GetString(objv[1]), "regions") == 0)
      what = REGIONS;
   else
      goto usage;

   int pos = 2;
   for (const char *opt; (opt = next_option(&pos, objc, objv)); ) {
      if (strcmp(opt, "-recursive") == 0 || strcmp(opt, "-r") == 0) {
         // Always recursive for now...
      }
      else
         goto usage;
   }

   const char *glob = Tcl_GetString(objv[pos]);
   Tcl_Obj *result = Tcl_NewListObj(0, NULL);

   switch (what) {
   case SIGNALS:
      {
         for (int i = 0; i < sh->nsignals; i++) {
            if (!ident_glob(sh->signals[i].obj.path, glob, -1))
               continue;

            Tcl_Obj *obj = tcl_ident_string(sh->signals[i].obj.path);
            Tcl_ListObjAppendElement(interp, result, obj);
         }
      }
      break;

   case REGIONS:
      {
         for (int i = 0; i < sh->nregions; i++) {
            if (!ident_glob(sh->regions[i].obj.path, glob, -1))
               continue;

            Tcl_Obj *obj = tcl_ident_string(sh->regions[i].obj.path);
            Tcl_ListObjAppendElement(interp, result, obj);
         }
         break;
      }
      break;
   }

   Tcl_SetObjResult(interp, result);
   return TCL_OK;

 usage:
   return syntax_error(sh, objv);
}

static const char elaborate_help[] =
   "Obsolete command which does nothing.\n";

static int shell_cmd_elaborate(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;
   return tcl_error(sh, "the elaborate/vsim command has been removed, "
                    "elaborate the design first with \"nvc -e top --do ...\" "
                    "or \"nvc -e top -i\"");
}

static const char examine_help[] =
   "Display current value of one of more signals\n"
   "\n"
   "Syntax:\n"
   "  examine [options] <name>...\n"
   "\n"
   "Note \"exa\" is an alias of this command.\n"
   "\n"
   "Options:\n"
   "  -radix <type>\tFormat as hexadecimal, decimal, or binary.\n"
   "  -<radix>\tAlias of \"-radix <radix>\".\n"
   "\n"
   "Examples:\n"
   "  examine /uut/foo\n"
   "  exa -hex sig\n";

static bool parse_radix(const char *str, print_flags_t *flags)
{
   if (strcmp(str, "binary") == 0 || strcmp(str, "bin") == 0
       || strcmp(str, "b") == 0) {
      *flags &= ~PRINT_F_RADIX;
      *flags |= PRINT_F_BIN;
      return true;
   }
   else if (strcmp(str, "-hexadecimal") == 0 || strcmp(str, "hex") == 0
            || strcmp(str, "h") == 0) {
      *flags &= ~PRINT_F_RADIX;
      *flags |= PRINT_F_HEX;
      return true;
   }
   else
      return false;
}

static int shell_cmd_examine(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (!shell_has_model(sh))
      return TCL_ERROR;

   print_flags_t flags = 0;
   int pos = 1;
   for (const char *opt; (opt = next_option(&pos, objc, objv)); ) {
      if (parse_radix(opt + 1, &flags))
         continue;
      else if (strcmp(opt, "-radix") == 0 && pos + 1 < objc) {
         const char *arg = Tcl_GetString(objv[pos++]);
         if (!parse_radix(arg, &flags))
            goto usage;
      }
      else
         goto usage;
   }

   if (pos == objc)
      goto usage;

   const int count = objc - pos;
   Tcl_Obj *single[1], **result = single;

   if (count > 1)
      result = xmalloc_array(count, sizeof(Tcl_Obj *));

   for (int i = 0; pos < objc; pos++, i++) {
      const char *name = Tcl_GetString(objv[pos]);
      shell_signal_t *ss = get_signal(sh, name);
      if (ss == NULL)
         return TCL_ERROR;

      if (!shell_get_printer(sh, ss))
         return TCL_ERROR;

      const char *str = print_signal(ss->printer, ss->signal, flags);
      result[i] = Tcl_NewStringObj(str, -1);
   }

   if (count > 1) {
      Tcl_Obj *list = Tcl_NewListObj(count, result);
      Tcl_SetObjResult(interp, list);
      free(result);
   }
   else
      Tcl_SetObjResult(interp, result[0]);

   return TCL_OK;

 usage:
   return syntax_error(sh, objv);
}

static const char describe_help[] =
   "Return information about an object or region\n"
   "\n"
   "Syntax:\n"
   "  describe <name>...\n"
   "\n"
   "Examples:\n"
   "  describe /uut/foo\n"
   "  describe /uut\n";

static int shell_cmd_describe(ClientData cd, Tcl_Interp *interp,
                              int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (!shell_has_model(sh))
      return TCL_ERROR;

   int pos = 1;
   for (const char *opt; (opt = next_option(&pos, objc, objv)); ) {
      goto usage;
   }

   if (pos == objc)
      goto usage;

   const int count = objc - pos;
   Tcl_Obj *single[1], **result = single;

   if (count > 1)
      result = xmalloc_array(count, sizeof(Tcl_Obj *));

   for (int i = 0; pos < objc; pos++, i++) {
      const char *name = Tcl_GetString(objv[pos]);
      shell_object_t *obj = get_object(sh, name);
      if (obj == NULL)
         return TCL_ERROR;

      Tcl_Obj *d = result[i] = Tcl_NewDictObj();

      tcl_dict_put(sh, d, "name", tcl_ident_string(obj->name));
      tcl_dict_put(sh, d, "path", tcl_ident_string(obj->path));

      switch (obj->kind) {
      case SHELL_SIGNAL:
         {
            tcl_dict_put(sh, d, "kind", Tcl_NewStringObj("signal", -1));

            shell_signal_t *ss = container_of(obj, shell_signal_t, obj);
            type_t type = tree_type(ss->signal->where);
            tcl_dict_put(sh, d, "type", Tcl_NewStringObj(type_pp(type), -1));
         }
         break;
      case SHELL_REGION:
         tcl_dict_put(sh, d, "kind", Tcl_NewStringObj("region", -1));
         break;
      }
   }

   if (count > 1) {
      Tcl_Obj *list = Tcl_NewListObj(count, result);
      Tcl_SetObjResult(interp, list);
      free(result);
   }
   else
      Tcl_SetObjResult(interp, result[0]);

   return TCL_OK;

 usage:
   return syntax_error(sh, objv);
}

static const char force_help[] =
   "Force the value of a signal\n"
   "\n"
   "Syntax:\n"
   "  force [<signal> <value>]\n"
   "\n"
   "Value can be either an enumeration literal ('1', true), an integer "
   "(42, 0), or a bit string literal (\"10111\") and must be appropriate "
   "for the signal type. Without arguments lists all currently forced "
   "signals.\n"
   "\n"
   "Examples:\n"
   "  force /uut/foo '1'\n"
   "  force /bitvec \"10011\"\n";

static int shell_cmd_force(ClientData cd, Tcl_Interp *interp,
                           int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (!shell_has_model(sh))
      return TCL_ERROR;
   else if (objc != 3 && objc != 1)
      return syntax_error(sh, objv);

   if (objc == 1) {
      for (int i = 0; i < sh->nsignals; i++) {
         shell_signal_t *ss = &(sh->signals[i]);
         if (!(ss->signal->nexus.flags & NET_F_FORCED))
            continue;

         if (!shell_get_printer(sh, ss))
            return TCL_ERROR;

         const size_t nbytes = ss->signal->shared.size;
         uint8_t *value LOCAL = xmalloc(nbytes);
         get_forcing_value(ss->signal, value);

         shell_printf(sh, "force %s %s\n", istr(ss->obj.path),
                      print_raw(ss->printer, value, nbytes, 0));
      }

      return TCL_OK;
   }

   const char *signame = Tcl_GetString(objv[1]);
   const char *valstr = Tcl_GetString(objv[2]);

   shell_signal_t *ss = get_signal(sh, signame);
   if (ss == NULL)
      return TCL_ERROR;

   type_t type = tree_type(ss->signal->where);

   parsed_value_t value;
   if (!parse_value(type, valstr, &value))
      return tcl_error(sh, "value '%s' is not valid for type %s",
                       valstr, type_pp(type));

   if (type_is_scalar(type))
      force_signal(sh->model, ss->signal, &value.integer, 0, 1);
   else if (type_is_character_array(type)) {
      const int width = signal_width(ss->signal);
      if (value.enums->count != width) {
         tcl_error(sh, "expected %d elements for signal %s but have %d", width,
                   signame, value.enums->count);
         free(value.enums);
         return TCL_ERROR;
      }

      force_signal(sh->model, ss->signal, value.enums->values, 0, width);
      free(value.enums);
   }
   else
      return tcl_error(sh, "cannot force signals of type %s", type_pp(type));

   return TCL_OK;
}

static const char noforce_help[] =
   "Stop forcing the value of signals\n"
   "\n"
   "Syntax:\n"
   "  noforce <signal>...\n"
   "  noforce *\n"
   "\n"
   "The second form stops forcing all currently forced signals.\n"
   "\n"
   "Examples:\n"
   "  noforce /uut/foo /baz\n";

static int shell_cmd_noforce(ClientData cd, Tcl_Interp *interp,
                             int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   if (!shell_has_model(sh))
      return TCL_ERROR;
   else if (objc == 1)
      return syntax_error(sh, objv);

   for (int i = 1; i < objc; i++) {
      const char *signame = Tcl_GetString(objv[i]);
      if (strcmp(signame, "*") == 0) {
         for (int i = 0; i < sh->nsignals; i++) {
            shell_signal_t *ss = &(sh->signals[i]);
            if (ss->signal->nexus.flags & NET_F_FORCED)
               release_signal(sh->model, ss->signal, 0,
                              signal_width(ss->signal));
         }
      }
      else {
         shell_signal_t *ss = get_signal(sh, signame);
         if (ss == NULL)
            return TCL_ERROR;

         if (!(ss->signal->nexus.flags & NET_F_FORCED))
            return tcl_error(sh, "signal %s is not forced", signame);

         release_signal(sh->model, ss->signal, 0, signal_width(ss->signal));
      }
   }

   return TCL_OK;
}

static const char add_help[] =
   "Add signals and other objects to the display\n"
   "\n"
   "Syntax:\n"
   "  add wave [options] <name>...\n"
   "\n"
   "Options:\n"
   "  -r, -recursive\tInclude subregions in wildcard search.\n"
   "\n"
   "Examples:\n"
   "  add wave /*\tAdd all signals to waveform\n";

static int shell_cmd_add(ClientData cd, Tcl_Interp *interp,
                         int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;
   char **globs LOCAL = NULL;

   if (objc < 3 || strcmp(Tcl_GetString(objv[1]), "wave") != 0)
      goto usage;
   else if (!shell_has_model(sh))
      return TCL_ERROR;

   int pos = 2;
   for (const char *opt; (opt = next_option(&pos, objc, objv)); ) {
      if (strcmp(opt, "-recursive") == 0 || strcmp(opt, "-r") == 0) {
         // Always recursive for now...
      }
      else
         goto usage;
   }

   const int nglobs = objc - pos;
   globs = xmalloc_array(nglobs, sizeof(char *));
   for (int i = 0; i < nglobs; i++)
      globs[i] = Tcl_GetString(objv[pos++]);

   for (int i = 0; i < sh->nsignals; i++) {
      shell_signal_t *ss = &(sh->signals[i]);

      bool match = false;
      for (int j = 0; j < nglobs; j++)
         match |= ident_glob(ss->obj.path, globs[j], -1);

      if (!match || !shell_get_printer(sh, ss))
         continue;

      if (sh->handler.add_wave != NULL) {
         const char *enc =
            print_signal(ss->printer, ss->signal, PRINT_F_ENCODE);
         (*sh->handler.add_wave)(ss->obj.path, enc, sh->handler.context);
      }

      if (ss->watch == NULL)
         watch_signal(ss);
   }

   return TCL_OK;

 usage:
   return syntax_error(sh, objv);
}

static const char quit_help[] =
   "Obsolete command which does nothing.\n";

static int shell_cmd_quit(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;
   return tcl_error(sh, "the quit command has been removed");
}

static const char exit_help[] =
   "Exit the simulator and return a status code\n"
   "\n"
   "Syntax:\n"
   "  exit [-code <integer>]\n"
   "\n"
   "Options:\n"
   "  -code <integer>\tStatus code to return to shell.\n";

static int shell_cmd_exit(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   tcl_shell_t *sh = cd;

   int pos = 1, status = EXIT_SUCCESS;
   for (const char *opt; (opt = next_option(&pos, objc, objv)); ) {
      if (strcmp(opt, "-code") == 0 && pos < objc)
         status = atoi(Tcl_GetString(objv[pos++]));
      else
         goto usage;
   }

   if (pos != objc)
      goto usage;

   if (sh->handler.exit != NULL)
      (*sh->handler.exit)(status, sh->handler.context);

   Tcl_Exit(status);

 usage:
   return syntax_error(sh, objv);
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
            shell_printf(sh, "%s", sh->cmds[i].help);
            return TCL_OK;
         }
      }

      return tcl_error(sh, "invalid command '%s'", which);
   }
   else if (objc != 1)
      return tcl_error(sh, "syntax error, try $bold$help$$");

   shell_printf(sh, "List of supported commands:\n");

   for (shell_cmd_t *c = sh->cmds; c < sh->cmds + sh->ncmds; c++) {
      const int linelen = strchrnul(c->help, '\n') - c->help;
      shell_printf(sh, "  $bold$%-16s$$%.*s\n", c->name, linelen, c->help);
   }

   shell_printf(sh, "\n");
   shell_printf(sh, "Use $bold$help <command>$$ for detailed usage "
                "of a particular command. Standard TCL commands are "
                "also accepted.\n");

   return TCL_OK;
}

static const char copyright_help[] = "Display copyright information";

static int shell_cmd_copyright(ClientData cd, Tcl_Interp *interp,
                               int objc, Tcl_Obj *const objv[])
{
   Tcl_Channel channel = Tcl_GetStdChannel(TCL_STDOUT);

   extern char copy_string[];
   Tcl_WriteChars(channel, copy_string, -1);
   Tcl_WriteChars(channel, "\n", 1);
   Tcl_Flush(channel);

   return TCL_OK;
}

static const char echo_help[] = "Display value of arguments";

static int shell_cmd_echo(ClientData cd, Tcl_Interp *interp,
                          int objc, Tcl_Obj *const objv[])
{
   Tcl_Channel channel = Tcl_GetStdChannel(TCL_STDOUT);

   for (int i = 1; i < objc; i++) {
      if (i > 1) Tcl_WriteChars(channel, " ", 1);
      Tcl_WriteObj(channel, objv[i]);
   }

   Tcl_WriteChars(channel, "\n", 1);
   Tcl_Flush(channel);

   return TCL_OK;
}

static char *shell_list_generator(const char *script, const char *text,
                                  int state, int prefix)
{
   static Tcl_Obj *list = NULL;
   static int index, len;
   static Tcl_Size max;

   if (!state) {
      if (Tcl_Eval(rl_shell->interp, script) != TCL_OK)
         return NULL;

      list = Tcl_GetObjResult(rl_shell->interp);

      if (Tcl_ListObjLength(rl_shell->interp, list, &max) != TCL_OK)
         return NULL;

      index = 0;
      len = strlen(text);
   }

   while (index < max) {
      Tcl_Obj *obj;
      if (Tcl_ListObjIndex(rl_shell->interp, list, index++, &obj) != TCL_OK)
         return NULL;

      const char *str = Tcl_GetString(obj);
      if (strncmp(str, text + prefix, len - prefix) == 0) {
         if (prefix == 0)
            return xstrdup(str);
         else {
            assert(len >= prefix);
            const size_t complen = strlen(str);
            char *buf = xmalloc(prefix + complen + 1);
            memcpy(buf, text, prefix);
            memcpy(buf + prefix, str, complen + 1);
            return buf;
         }
      }
   }

   return NULL;
}

static char *shell_command_generator(const char *text, int state)
{
   return shell_list_generator("info commands", text, state, 0);
}

static char *shell_variable_generator(const char *text, int state)
{
   return shell_list_generator("info vars", text, state, 1);
}

static char **shell_tab_completion(const char *text, int start, int end)
{
   rl_attempted_completion_over = 0;

   if (text[0] == '$')
      return rl_completion_matches(text, shell_variable_generator);

   // Determine if we are completing a TCL command or not
   int pos = start - 1;
   for (; pos >= 0 && isspace_iso88591(rl_line_buffer[pos]); pos--);

   if (pos == -1 || rl_line_buffer[pos] == '[')
      return rl_completion_matches(text, shell_command_generator);

   return NULL;
}

static char *shell_completing_get_line(tcl_shell_t *sh)
{
   rl_attempted_completion_function = shell_tab_completion;
   rl_completer_quote_characters = "\"'";
   rl_completer_word_break_characters = " \t\r\n[]{}";
   rl_special_prefixes = "$";
   rl_shell = sh;

   char *buf = readline(sh->prompt);
   if ((buf != NULL) && (*buf != '\0'))
      add_history(buf);

   rl_shell = NULL;
   return buf;
}


static char *shell_raw_get_line(tcl_shell_t *sh)
{
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

void shell_print_banner(tcl_shell_t *sh)
{
   extern const char version_string[];
   shell_printf(sh, "\n");

   if (sh->handler.stdout_write == NULL)
      print_centred(version_string);
   else
      shell_printf(sh, "\t%s", version_string);

   static const char blurb[] =
      "\n\nThis program comes with ABSOLUTELY NO WARRANTY. This is free "
      "software, and you are welcome to redistribute it under certain "
      "conditions; type $bold$copyright$$ for details.\n\n"
      "Type $bold$help$$ for a list of supported commands.\n\n";

   shell_printf(sh, blurb);
}

static int compare_shell_cmd(const void *a, const void *b)
{
   return strcmp(((shell_cmd_t *)a)->name, ((shell_cmd_t *)b)->name);
}

tcl_shell_t *shell_new(jit_t *jit)
{
   tcl_shell_t *sh = xcalloc(sizeof(tcl_shell_t));
#ifdef RL_VERSION_MAJOR
   sh->prompt   = color_asprintf("\001$+cyan$\002%%\001$$\002 ");
#else
   sh->prompt   = color_asprintf("$+cyan$%%$$ ");
#endif
   sh->interp   = Tcl_CreateInterp();
   sh->jit      = jit;
   sh->printer  = printer_new();

   if (isatty(fileno(stdin)))
      sh->getline = shell_completing_get_line;
   else
      sh->getline = shell_raw_get_line;

   if (Tcl_Init(sh->interp) != 0)
      fatal("%s", Tcl_GetStringResult(sh->interp));

   Tcl_LinkVar(sh->interp, "now", (char *)&sh->now_var,
               TCL_LINK_WIDE_INT | TCL_LINK_READ_ONLY);
   Tcl_LinkVar(sh->interp, "deltas", (char *)&sh->deltas_var,
               TCL_LINK_UINT | TCL_LINK_READ_ONLY);

   {
      LOCAL_TEXT_BUF tb = tb_new();
      get_data_dir(tb);
      sh->datadir = tb_claim(tb);
   }
   Tcl_LinkVar(sh->interp, "nvc_dataDir", (char *)&sh->datadir,
               TCL_LINK_READ_ONLY | TCL_LINK_STRING);

   atexit(Tcl_Finalize);

   Tcl_DeleteCommand(sh->interp, "exit");

   shell_add_cmd(sh, "help", shell_cmd_help, help_help);
   shell_add_cmd(sh, "exit", shell_cmd_exit, exit_help);
   shell_add_cmd(sh, "copyright", shell_cmd_copyright, copyright_help);
   shell_add_cmd(sh, "find", shell_cmd_find, find_help);
   shell_add_cmd(sh, "run", shell_cmd_run, run_help);
   shell_add_cmd(sh, "restart", shell_cmd_restart, restart_help);
   shell_add_cmd(sh, "elaborate", shell_cmd_elaborate, elaborate_help);
   shell_add_cmd(sh, "vsim", shell_cmd_elaborate, elaborate_help);
   shell_add_cmd(sh, "examine", shell_cmd_examine, examine_help);
   shell_add_cmd(sh, "exa", shell_cmd_examine, examine_help);
   shell_add_cmd(sh, "add", shell_cmd_add, add_help);
   shell_add_cmd(sh, "quit", shell_cmd_quit, quit_help);
   shell_add_cmd(sh, "force", shell_cmd_force, force_help);
   shell_add_cmd(sh, "noforce", shell_cmd_noforce, noforce_help);
   shell_add_cmd(sh, "echo", shell_cmd_echo, echo_help);
   shell_add_cmd(sh, "describe", shell_cmd_describe, describe_help);

   qsort(sh->cmds, sh->ncmds, sizeof(shell_cmd_t), compare_shell_cmd);

   return sh;
}

void shell_free(tcl_shell_t *sh)
{
   if (sh->model != NULL) {
      model_free(sh->model);
      hash_free(sh->namemap);
      free(sh->signals);
      free(sh->regions);
   }

   printer_free(sh->printer);
   Tcl_DeleteInterp(sh->interp);

   free(sh->datadir);
   free(sh->prompt);
   free(sh->cmds);
   free(sh);
}

bool shell_eval(tcl_shell_t *sh, const char *script, const char **result)
{
   const int code = Tcl_Eval(sh->interp, script);

   switch (code) {
   case TCL_OK:
      if (result != NULL)
         *result = Tcl_GetStringResult(sh->interp);
      return true;
   case TCL_ERROR:
      {
         const char *info = Tcl_GetVar(sh->interp, "::errorInfo", 0);
         if (info != NULL && *info != '\n')
            errorf("%s", info);

         *result = Tcl_GetStringResult(sh->interp);
         if (info == NULL && *result != NULL && **result != '\0')
            errorf("%s", *result);
      }
      return false;
   default:
      warnf("Tcl_Eval returned unknown code %d", code);
      return false;
   }
}

static void count_objects(rt_scope_t *scope, unsigned *nsignals,
                          unsigned *nregions)
{
   *nsignals += scope->signals.count + scope->aliases.count;
   *nregions += 1;

   for (int i = 0; i < scope->children.count;i ++)
      count_objects(scope->children.items[i], nsignals, nregions);
}

static void recurse_objects(tcl_shell_t *sh, rt_scope_t *scope,
                            text_buf_t *path, shell_signal_t **sptr,
                            shell_region_t **rptr)
{
   const int base = tb_len(path);

   shell_region_t *r = (*rptr)++;
   r->scope = scope;
   r->obj.kind = SHELL_REGION;
   r->obj.name = ident_downcase(tree_ident(scope->where));
   r->obj.path = ident_new(tb_get(path));

   hash_put(sh->namemap, r->obj.path, &(r->obj));

   for (int i = 0; i < scope->signals.count; i++) {
      shell_signal_t *ss = (*sptr)++;
      ss->signal = scope->signals.items[i];
      ss->obj.kind = SHELL_SIGNAL;
      ss->obj.name = ident_downcase(tree_ident(ss->signal->where));
      ss->owner = sh;

      tb_istr(path, ss->obj.name);
      ss->obj.path = ident_new(tb_get(path));
      tb_trim(path, base);

      hash_put(sh->namemap, ss->obj.path, &(ss->obj));
   }

   for (int i = 0; i < scope->aliases.count; i++) {
      rt_alias_t *a = scope->aliases.items[i];

      shell_signal_t *ss = (*sptr)++;
      ss->signal = a->signal;
      ss->obj.kind = SHELL_SIGNAL;
      ss->obj.name = ident_downcase(tree_ident(a->where));
      ss->owner = sh;

      tb_istr(path, ss->obj.name);
      ss->obj.path = ident_new(tb_get(path));
      tb_trim(path, base);

      hash_put(sh->namemap, ss->obj.path, &(ss->obj));
   }

   for (int i = 0; i < scope->children.count; i++) {
      rt_scope_t *child = scope->children.items[i];
      ident_t name = ident_downcase(tree_ident(child->where));
      tb_istr(path, name);
      tb_append(path, '/');
      recurse_objects(sh, child, path, sptr, rptr);
      tb_trim(path, base);
   }
}

void shell_reset(tcl_shell_t *sh, tree_t top)
{
   jit_reset(sh->jit);

   sh->top = top;

   shell_create_model(sh);

   sh->nsignals = sh->nregions = 0;
   count_objects(sh->root, &sh->nsignals, &sh->nregions);

   sh->signals = xcalloc_array(sh->nsignals, sizeof(shell_signal_t));
   sh->regions = xcalloc_array(sh->nregions, sizeof(shell_region_t));
   sh->namemap = hash_new(1 + sh->nsignals * 2);

   text_buf_t *path = tb_new();
   shell_signal_t *sptr = sh->signals;
   shell_region_t *rptr = sh->regions;
   tb_cat(path, "/");
   recurse_objects(sh, sh->root, path, &sptr, &rptr);
   assert(sptr == sh->signals + sh->nsignals);
   assert(rptr == sh->regions + sh->nregions);
   tb_free(path);

   shell_update_now(sh);

   if (sh->handler.start_sim != NULL)
      (*sh->handler.start_sim)(tree_ident(top), sh->handler.context);
}

void shell_interact(tcl_shell_t *sh)
{
   shell_print_banner(sh);

   char *line;
   while (!sh->quit && (line = (*sh->getline)(sh))) {
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
         const char *info = Tcl_GetVar(sh->interp, "::errorInfo", 0);
         if (info != NULL && *info != '\n')
            errorf("%s", info);
         else {
            const char *str = Tcl_GetStringResult(sh->interp);
            if (str != NULL && *str != '\0')
               errorf("%s", str);
         }

         return false;
      }
   default:
      warnf("Tcl_Eval returned unknown code %d", code);
      return false;
   }
}

static int shell_redirect_close(ClientData cd, Tcl_Interp *interp)
{
   return EINVAL;
}

static int shell_redirect_close2(void *instanceData, Tcl_Interp *interp,
                                 int flags)
{
   return EINVAL;
}

static void shell_redirect_watch(ClientData cd, int mask)
{
}

static int shell_redirect_output(ClientData cd, const char *buf, int nchars,
                                 int *error)
{
   tcl_shell_t *sh = untag_pointer(cd, tcl_shell_t);
   switch (pointer_tag(cd)) {
   case 0:
      (*sh->handler.stdout_write)(buf, nchars, sh->handler.context);
      break;
   case 1:
      (*sh->handler.stderr_write)(buf, nchars, sh->handler.context);
      break;
   case 2:
      (*sh->handler.backchannel_write)(buf, nchars, sh->handler.context);
      break;
   default:
      fatal_trace("invalid channel number %"PRIiPTR, pointer_tag(cd));
   }

   return nchars;
}

static const Tcl_ChannelType redirect_funcs = {
   .typeName = "redirect",
   .version = TCL_CHANNEL_VERSION_5,
   .closeProc = shell_redirect_close,
#if TCL_MAJOR_VERSION >= 9
   .close2Proc = shell_redirect_close2,
#endif
   .watchProc = shell_redirect_watch,
   .outputProc = shell_redirect_output,
};

void shell_set_handler(tcl_shell_t *sh, const shell_handler_t *h)
{
   sh->handler = *h;

   if (h->stdout_write != NULL) {
      Tcl_Channel chan = Tcl_CreateChannel(&redirect_funcs, "redirect0",
                                           tag_pointer(sh, 0), TCL_WRITABLE);
      Tcl_SetChannelOption(NULL, chan, "-translation", "lf");
      Tcl_SetChannelOption(NULL, chan, "-buffering", "line");
      Tcl_SetChannelOption(NULL, chan, "-encoding", "utf-8");

      Tcl_RegisterChannel(sh->interp, chan);
      Tcl_SetStdChannel(chan, TCL_STDOUT);
   }

   if (h->stderr_write != NULL) {
      Tcl_Channel chan = Tcl_CreateChannel(&redirect_funcs, "redirect1",
                                           tag_pointer(sh, 1), TCL_WRITABLE);
      Tcl_SetChannelOption(NULL, chan, "-translation", "lf");
      Tcl_SetChannelOption(NULL, chan, "-buffering", "none");
      Tcl_SetChannelOption(NULL, chan, "-encoding", "utf-8");

      Tcl_RegisterChannel(sh->interp, chan);
      Tcl_SetStdChannel(chan, TCL_STDERR);
   }

   if (h->backchannel_write != NULL) {
      Tcl_Channel chan = Tcl_CreateChannel(&redirect_funcs, "backchannel",
                                           tag_pointer(sh, 2), TCL_WRITABLE);
      Tcl_SetChannelOption(NULL, chan, "-translation", "lf");
      Tcl_SetChannelOption(NULL, chan, "-buffering", "full");
      Tcl_SetChannelOption(NULL, chan, "-encoding", "utf-8");

      Tcl_RegisterChannel(sh->interp, chan);
   }
}
