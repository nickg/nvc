//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "jit/jit-ffi.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "phase.h"
#include "thread.h"
#include "type.h"
#include "vlog/vlog-node.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <ctype.h>

typedef A(ident_t) unit_list_t;

static void cgen_find_dependencies(mir_context_t *mc, unit_registry_t *ur,
                                   unit_list_t *units, hset_t *seen,
                                   ident_t name)
{
   mir_unit_t *mu = mir_get_unit(mc, name);
   if (mu == NULL) {
      (void)unit_registry_get(ur, name);
      mu = mir_get_unit(mc, name);
   }

   if (mu == NULL)
      fatal_trace("missing vcode for %s", istr(name));

   const int nlink = mir_count_linkage(mu);
   for (int i = 0; i < nlink; i++) {
      ident_t link = mir_get_linkage(mu, i);
      if (hset_contains(seen, link))
         continue;
      else if (ident_char(link, 0) == '$')
         continue;   // TODO: handle VPI differently
      else {
         APUSH(*units, link);
         hset_insert(seen, link);
      }
   }
}

static void cgen_walk_hier(unit_list_t *units, hset_t *seen, tree_t block)
{
   assert(tree_kind(block) == T_BLOCK);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   ident_t unit_name = tree_ident(hier), prefix = tree_ident2(hier);
   APUSH(*units, unit_name);
   assert(!hset_contains(seen, unit_name));
   hset_insert(seen, unit_name);

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         cgen_walk_hier(units, seen, s);
         break;
      case T_PROCESS:
      case T_PSL_DIRECT:
      case T_VERILOG:
         {
            ident_t sym = ident_prefix(prefix, tree_ident(s), '.');
            if (!hset_contains(seen, sym)) {
               APUSH(*units, sym);
               hset_insert(seen, sym);
            }
         }
         break;
      default:
         break;
      }
   }
}

static void cgen_jit_pack(ident_t name, jit_t *jit, unit_list_t *units)
{
   const char *fname LOCAL = xasprintf("_%s.pack", istr(name));

   FILE *f = lib_fopen(lib_work(), fname, "wb");
   if (f == NULL)
      fatal_errno("fopen: %s", fname);

   jit_write_pack(jit, units->items, units->count, f);
   fclose(f);

   progress("writing JIT pack");
}

void cgen(tree_t top, unit_registry_t *ur, mir_context_t *mc, jit_t *jit)
{
   assert(tree_kind(top) == T_ELAB);

   hset_t *seen = hset_new(16);
   unit_list_t units = AINIT;

   cgen_walk_hier(&units, seen, tree_stmt(top, 0));

   for (int i = 0; i < units.count; i++)
      cgen_find_dependencies(mc, ur, &units, seen, units.items[i]);

   hset_free(seen);
   seen = NULL;

   cgen_jit_pack(tree_ident(top), jit, &units);
   ACLEAR(units);
}
