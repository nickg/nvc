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

#include "rt.h"
#include "tree.h"
#include "lib.h"
#include "util.h"

#include <assert.h>

typedef void (*simple_proc_fn_t)(void);

struct rt_proc {
   tree_t           source;
   simple_proc_fn_t simple_proc_fn;
};

static struct rt_proc *procs = NULL;
static size_t         n_procs = 0;

static void _sched_process(void)
{
   printf("_sched_process!!\n");
}

static void rt_setup(tree_t top)
{
   jit_bind_fn("_sched_process", _sched_process);

   n_procs = tree_stmts(top);
   procs   = xmalloc(sizeof(struct rt_proc) * n_procs);

   for (unsigned i = 0; i < tree_stmts(top); i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source         = p;
      procs[i].simple_proc_fn = jit_fun_ptr(istr(tree_ident(p)));

      printf("%s fun at %p\n", istr(tree_ident(p)), procs[i].simple_proc_fn);
   }
}

static void rt_initial(void)
{
   // Initialisation is described in LRM 93 section 12.6.4

   for (size_t i = 0; i < n_procs; i++) {
      printf("run process %s\n", istr(tree_ident(procs[i].source)));

      (*procs[i].simple_proc_fn)();
   }
}

void rt_exec(ident_t top)
{
   ident_t ename = ident_prefix(top, ident_new("elab"));
   tree_t e = lib_get(lib_work(), ename);
   if (e == NULL)
      fatal("%s not elaborated", istr(top));
   else if (tree_kind(e) != T_ELAB)
      fatal("%s not suitable top level", istr(top));

   jit_init(ename);

   rt_setup(e);
   rt_initial();

   jit_shutdown();
}
