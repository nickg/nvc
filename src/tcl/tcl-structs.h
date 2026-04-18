//
//  Copyright (C) 2026  Nick Gasson
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

#ifndef _TCL_STRUCTS_H
#define _TCL_STRUCTS_H

#include "rt/printer.h"
#include "tcl/tcl-priv.h"
#include "tcl/tcl-shell.h"

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

#endif  // _TCL_STRUCTS_H
