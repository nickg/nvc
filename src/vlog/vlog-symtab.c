//
//  Copyright (C) 2025  Nick Gasson
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
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-symtab.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <stdlib.h>

#define POISON_NODE (vlog_node_t)-1

typedef struct _vlog_scope vlog_scope_t;
typedef A(vlog_node_t) node_list_t;

typedef struct _vlog_symtab {
   vlog_scope_t    *top;
   hash_t          *scopes;
   mem_pool_t      *pool;
   vlog_net_kind_t  implicit;
} vlog_symtab_t;

typedef struct {
   ident_t     name;
   vlog_node_t node;
} vlog_symbol_t;

typedef struct _vlog_scope {
   vlog_scope_t  *parent;
   vlog_node_t    container;
   size_t         max_symbols;
   size_t         num_symbols;
   vlog_symbol_t *symbols;
   node_list_t    deferred;
   bool           suppress;
} vlog_scope_t;

vlog_symtab_t *vlog_symtab_new(void)
{
   vlog_symtab_t *st = xcalloc(sizeof(vlog_symtab_t));
   st->pool     = pool_new();
   st->scopes   = hash_new(16);
   st->implicit = V_NET_NONE;

   return st;
}

void vlog_symtab_free(vlog_symtab_t *st)
{
   assert(st->top == NULL);
   pool_free(st->pool);
   hash_free(st->scopes);
   free(st);
}

static vlog_symbol_t *fresh_symbol_for(vlog_symtab_t *st, ident_t name)
{
   vlog_scope_t *s = st->top;
   assert(s != NULL);
   assert(is_power_of_2(s->max_symbols));

   if (unlikely(s->num_symbols + 1 > s->max_symbols / 2)) {
      const int old_max = s->max_symbols;
      s->max_symbols = MAX(s->max_symbols * 2, 8);

      const vlog_symbol_t *old_symbols = s->symbols;
      size_t size = s->max_symbols * sizeof(vlog_symbol_t);
      s->symbols = pool_calloc(st->pool, size);

      for (int i = 0; i < old_max; i++) {
         if (old_symbols[i].name != NULL) {
            int slot = ident_hash(old_symbols[i].name) & (s->max_symbols - 1);
            for (; ; slot = (slot + 1) & (s->max_symbols - 1)) {
               if (s->symbols[slot].name == NULL) {
                  s->symbols[slot] = old_symbols[i];
                  break;
               }
               else
                  assert(s->symbols[slot].name != old_symbols[i].name);
            }
         }
      }
   }

   for (int slot = ident_hash(name) & (s->max_symbols - 1);;
        slot = (slot + 1) & (s->max_symbols - 1)) {
      if (s->symbols[slot].name == name)
         return &(s->symbols[slot]);
      else if (s->symbols[slot].name == NULL) {
         s->symbols[slot].name = name;
         s->num_symbols++;
         return &(s->symbols[slot]);
      }
   }
}

static const vlog_symbol_t *symbol_for(vlog_symtab_t *st, ident_t name)
{
   for (vlog_scope_t *s = st->top; s != NULL; s = s->parent) {
      assert(is_power_of_2(s->max_symbols));

      if (s->num_symbols > 0) {
         for (int slot = ident_hash(name) & (s->max_symbols - 1);
              s->symbols[slot].name != NULL;
              slot = (slot + 1) & (s->max_symbols - 1)) {
            if (s->symbols[slot].name == name)
               return &(s->symbols[slot]);
         }
      }
   }

   return NULL;
}

void vlog_symtab_push(vlog_symtab_t *st, vlog_node_t v)
{
   vlog_scope_t *s = pool_calloc(st->pool, sizeof(vlog_scope_t));
   s->parent    = st->top;
   s->container = v;

   st->top = s;
}

void vlog_symtab_pop(vlog_symtab_t *st)
{
   assert(st->top != NULL);

   unsigned wptr = 0;
   for (int i = 0; i < st->top->deferred.count; i++) {
      vlog_node_t v = st->top->deferred.items[i];
      vlog_node_t d = vlog_symtab_query(st, vlog_ident(v));
      if (d != NULL)
         vlog_set_ref(v, d);
      else
         st->top->deferred.items[wptr++] = v;
   }
   ATRIM(st->top->deferred, wptr);

   for (int i = 0; i < st->top->deferred.count; i++) {
      vlog_node_t v = st->top->deferred.items[i];
      if (st->top->parent == NULL || is_top_level(st->top->container)) {
         ident_t name = vlog_ident(v);
         error_at(vlog_loc(v), "no visible declaration for '%pi'", name);
         vlog_symtab_poison(st, name);
      }
      else
         APUSH(st->top->parent->deferred, v);
   }
   ACLEAR(st->top->deferred);

   st->top = st->top->parent;
}

void vlog_symtab_set_implicit(vlog_symtab_t *st, vlog_net_kind_t kind)
{
   st->implicit = kind;
}

static void lookup_member(vlog_symtab_t *st, vlog_node_t v)
{
   vlog_node_t prefix = vlog_value(v), type = vlog_get_type(prefix);
   if (type == NULL) {  // Was earlier error
      assert(error_count() > 0);
      return;
   }

   const vlog_kind_t kind = vlog_kind(type);
   if (kind != V_STRUCT_DECL && kind != V_CLASS_DECL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(prefix));
      if (vlog_kind(prefix) == V_REF)
         diag_printf(d, "'%pi'", vlog_ident(prefix));
      else
         diag_printf(d, "prefix");
      diag_printf(d, " is not a struct or class");
      diag_emit(d);
      return;
   }

   ident_t id = vlog_ident(v);

   const int ndecls = vlog_decls(type);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(type, i);
      if (vlog_ident(d) == id) {
         vlog_set_ref(v, d);
         return;
      }
   }

   diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
   if (kind == V_STRUCT_DECL)
      diag_printf(d, "struct ");
   else
      diag_printf(d, "class '%pi' ", vlog_ident(type));
   diag_printf(d, "has no field named '%pi'", id);
   diag_hint(d, vlog_loc(type), "struct declared here");
   diag_emit(d);
}

void vlog_symtab_lookup(vlog_symtab_t *st, vlog_node_t v)
{
   if (vlog_has_ref(v))
      return;

   assert(!loc_invalid_p(vlog_loc(v)));

   switch (vlog_kind(v)) {
   case V_MEMBER_REF:
      lookup_member(st, v);
      return;
   default:
      break;
   }

   ident_t name = vlog_ident(v);
   const vlog_symbol_t *sym = symbol_for(st, name);
   if (sym == NULL && vlog_kind(v) == V_REF && st->implicit != V_NET_NONE) {
      // See 1800-2017 section 6.10 "Implicit declarations"
      vlog_node_t decl = vlog_new(V_NET_DECL);
      vlog_set_ident(decl, name);
      vlog_set_loc(decl, vlog_loc(v));
      vlog_set_subkind(decl, V_NET_WIRE);

      vlog_node_t dt = vlog_new(V_DATA_TYPE);
      vlog_set_subkind(dt, DT_LOGIC);

      vlog_set_type(decl, dt);

      vlog_set_ref(v, decl);

      vlog_add_decl(st->top->container, decl);

      fresh_symbol_for(st, name)->node = decl;
   }
   else if (sym == NULL) {
      switch (vlog_kind(v)) {
      case V_USER_FCALL:
      case V_USER_TCALL:
      case V_HIER_REF:
         // May be declared later
         APUSH(st->top->deferred, v);
         break;
      default:
         error_at(vlog_loc(v), "no visible declaration for '%pi'", name);
         vlog_symtab_poison(st, name);
      }
   }
   else if (sym->node != POISON_NODE)
      vlog_set_ref(v, sym->node);
}

void vlog_symtab_put(vlog_symtab_t *st, vlog_node_t v)
{
   ident_t name = vlog_ident(v);
   vlog_symbol_t *sym = fresh_symbol_for(st, name);

   assert(!loc_invalid_p(vlog_loc(v)));

   if (sym->node == POISON_NODE)
      return;

   if (sym->node != NULL) {
      switch (vlog_kind(sym->node)) {
      case V_VAR_DECL:
      case V_NET_DECL:
         switch (vlog_kind(v)) {
         case V_PORT_DECL:
         case V_TF_PORT_DECL:
            if (!vlog_has_ref(v)) {
               vlog_set_ref(v, sym->node);
               sym->node = v;
               return;
            }
            break;
         default:
            break;
         }
         break;
      case V_PORT_DECL:
      case V_TF_PORT_DECL:
         if (!vlog_has_ref(sym->node)) {
            switch (vlog_kind(v)) {
            case V_VAR_DECL:
            case V_NET_DECL:
               vlog_set_ref(sym->node, v);
               return;
            default:
               break;
            }
         }
      default:
         break;
      }

      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "duplicate declaration of '%pi'", name);
      diag_hint(d, vlog_loc(sym->node), "'%pi' was previously declared here",
                name);
      diag_hint(d, vlog_loc(v), "duplicate declaration");
      diag_suppress(d, name == well_known(W_ERROR) || st->top->suppress);
      diag_emit(d);
   }

   sym->node = v;
}

void vlog_symtab_poison(vlog_symtab_t *st, ident_t name)
{
   vlog_symbol_t *sym = fresh_symbol_for(st, name);
   sym->node = POISON_NODE;
}

vlog_node_t vlog_symtab_query(vlog_symtab_t *st, ident_t name)
{
   const vlog_symbol_t *sym = symbol_for(st, name);
   if (sym == NULL || sym->node == POISON_NODE)
      return NULL;

   return sym->node;
}

void vlog_symtab_suppress(vlog_symtab_t *st)
{
   st->top->suppress = true;
}

vlog_node_t vlog_symtab_subr(vlog_symtab_t *st)
{
   for (vlog_scope_t *s = st->top; s != NULL; s = s->parent) {
      if (s->container != NULL) {
         switch (vlog_kind(s->container)) {
         case V_FUNC_DECL:
         case V_TASK_DECL:
            return s->container;
         default:
            break;
         }
      }
   }

   return NULL;
}
