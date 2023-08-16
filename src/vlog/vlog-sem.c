//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "diag.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <stdlib.h>

typedef struct _vlog_scope vlog_scope_t;

struct _vlog_scope {
   vlog_scope_t *parent;
   hash_t       *symbols;
};

static vlog_scope_t *top_scope = NULL;

static bool is_net(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      if (vlog_has_ref(v)) {
         vlog_node_t decl = vlog_ref(v);
         switch (vlog_kind(decl)) {
         case V_NET_DECL: return true;
         case V_PORT_DECL: return vlog_subkind(decl) != V_PORT_OUTPUT_REG;
         default: return false;
         }
      }
      else
         return false;

   default:
      return false;
   }
}

static void name_for_diag(diag_t *d, vlog_node_t v, const char *alt)
{
   switch (vlog_kind(v)) {
   case V_REF:
      diag_printf(d, "'%s'", istr(vlog_ident(v)));
      break;
   default:
      diag_printf(d, "%s", alt);
      break;
   }
}

static void push_scope(void)
{
   vlog_scope_t *s = xcalloc(sizeof(vlog_scope_t));
   s->symbols = hash_new(128);
   s->parent  = top_scope;

   top_scope = s;
}

static void pop_scope(void)
{
   assert(top_scope != NULL);

   vlog_scope_t *tmp = top_scope;
   top_scope = top_scope->parent;

   hash_free(tmp->symbols);
   free(tmp);
}

static void vlog_insert_decl(vlog_node_t v)
{
   ident_t id = vlog_ident(v);

   vlog_node_t exist = hash_get(top_scope->symbols, id);
   if (exist != NULL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "duplicate declaration of %s", istr(id));
      diag_hint(d, vlog_loc(exist), "%s was previously declared here",
                istr(id));
      diag_hint(d, vlog_loc(v), "duplicate declaration");
      diag_emit(d);
      return;
   }

   hash_put(top_scope->symbols, id, v);
}

static void vlog_check_ref(vlog_node_t ref)
{
   ident_t id = vlog_ident(ref);

   vlog_node_t decl = hash_get(top_scope->symbols, id);
   if (decl == NULL) {
      error_at(vlog_loc(ref), "no visible declaration for %s", istr(id));
      return;
   }

   vlog_set_ref(ref, decl);
}

static void vlog_check_number(vlog_node_t num)
{

}

static void vlog_check_nbassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check(target);

   vlog_node_t value = vlog_value(stmt);
   vlog_check(value);

   if (is_net(target)) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(target));
      name_for_diag(d, target, "target");
      diag_printf(d, " cannot be assigned in a procedural block");
      diag_emit(d);
   }
}

static void vlog_check_assign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check(target);

   vlog_node_t value = vlog_value(stmt);
   vlog_check(value);

   if (!is_net(target)) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(target));
      name_for_diag(d, target, "target");
      diag_printf(d, " cannot be driven by continuous assignment");
      diag_emit(d);
   }
}

static void vlog_check_timing(vlog_node_t timing)
{
   vlog_check(vlog_value(timing));

   const int nstmts = vlog_stmts(timing);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(timing, i));
}

static void vlog_check_event(vlog_node_t event)
{
   vlog_check(vlog_value(event));
}

static void vlog_check_always(vlog_node_t always)
{
   const int nstmts = vlog_stmts(always);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(always, i));
}

static void vlog_check_initial(vlog_node_t initial)
{
   const int nstmts = vlog_stmts(initial);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(initial, i));
}

static void vlog_check_seq_block(vlog_node_t block)
{
   const int nstmts = vlog_stmts(block);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(block, i));
}

static void vlog_check_systask(vlog_node_t call)
{
   const well_known_t name = is_well_known(vlog_ident(call));

   v_systask_kind_t kind;
   switch (name) {
   case W_DOLLAR_DISPLAY: kind = V_SYS_DISPLAY; break;
   case W_DOLLAR_FINISH:  kind = V_SYS_FINISH; break;
   case W_DOLLAR_WRITE:   kind = V_SYS_WRITE; break;
   default:
      error_at(vlog_loc(call), "system task %s not recognised",
               istr(vlog_ident(call)));
      return;
   }

   vlog_set_subkind(call, kind);
}

static void vlog_check_port_decl(vlog_node_t port)
{
   vlog_insert_decl(port);
}

static void vlog_check_net_decl(vlog_node_t net)
{
   vlog_insert_decl(net);
}

static void vlog_check_module(vlog_node_t module)
{
   assert(top_scope == NULL);
   push_scope();

   const int ndecls = vlog_decls(module);
   for (int i = 0; i < ndecls; i++)
      vlog_check(vlog_decl(module, i));

   const int nports = vlog_ports(module);
   for (int i = 0; i < nports; i++)
      vlog_check(vlog_port(module, i));

   const int nstmts = vlog_stmts(module);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(module, i));

   pop_scope();
}

void vlog_check(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_check_module(v);
      break;
   case V_ALWAYS:
      vlog_check_always(v);
      break;
   case V_INITIAL:
      vlog_check_initial(v);
      break;
   case V_TIMING:
      vlog_check_timing(v);
      break;
   case V_EVENT:
      vlog_check_event(v);
      break;
   case V_NBASSIGN:
      vlog_check_nbassign(v);
      break;
   case V_ASSIGN:
      vlog_check_assign(v);
      break;
   case V_REF:
      vlog_check_ref(v);
      break;
   case V_PORT_DECL:
      vlog_check_port_decl(v);
      break;
   case V_NET_DECL:
      vlog_check_net_decl(v);
      break;
   case V_SEQ_BLOCK:
      vlog_check_seq_block(v);
      break;
   case V_SYSTASK:
      vlog_check_systask(v);
      break;
   case V_NUMBER:
      vlog_check_number(v);
      break;
   default:
      fatal_trace("cannot check verilog node %s", vlog_kind_str(vlog_kind(v)));
   }
}
