//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "mir/mir-node.h"
#include "mir/mir-priv.h"
#include "mir/mir-structs.h"
#include "mir/mir-unit.h"
#include "object.h"
#include "option.h"
#include "thread.h"

#include <assert.h>
#include <stdlib.h>

#define TYPE_TAB_SIZE 256

mir_context_t *mir_context_new(void)
{
   mir_context_t *mc = xcalloc(sizeof(mir_context_t));
   mc->map  = chash_new(32);
   mc->pool = pool_new();

   type_tab_t *tab =
      xcalloc_flex(sizeof(type_tab_t), TYPE_TAB_SIZE, sizeof(type_data_t));
   tab->max_types = TYPE_TAB_SIZE;
   tab->hashtab = xcalloc_array(TYPE_TAB_SIZE, sizeof(uint32_t));

   store_release(&mc->resizing, tab);
   store_release(&mc->typetab, tab);

   return mc;
}

static void mir_unit_free_memory(mir_unit_t *mu)
{
#ifdef DEBUG
   if (mu->comments != NULL)
      tb_free(mu->comments);
#endif

   for (int i = 0; i < mu->blocks.count; i++)
      free(mu->blocks.items[i].nodes);

   ACLEAR(mu->blocks);
   ACLEAR(mu->params);
   ACLEAR(mu->vars);
   ACLEAR(mu->stamps);
   ACLEAR(mu->linkage);
   ACLEAR(mu->extvars);

   pool_free(mu->pool);

   hash_free(mu->objmap);
   hash_free(mu->privmap);
   free(mu->vregs);
   free(mu->nodes);
   free(mu->argspill);
   free(mu);
}

static void mir_free_unit_cb(const void *key, void *value)
{
   switch (pointer_tag(value)) {
   case UNIT_GENERATED:
      mir_unit_free_memory(untag_pointer(value, mir_unit_t));
      break;
   case UNIT_FREED:
      {
         mir_shape_t *s = untag_pointer(value, mir_shape_t);
         hash_free(s->objmap);
      }
      break;
   }
}

void mir_context_free(mir_context_t *mc)
{
   chash_iter(mc->map, mir_free_unit_cb);
   mir_free_types(mc->typetab);
   pool_free(mc->pool);
   chash_free(mc->map);
   free(mc);
}

mir_unit_t *mir_unit_new(mir_context_t *mc, ident_t name, object_t *object,
                         mir_unit_kind_t kind, mir_shape_t *parent)
{
   mir_unit_t *mu = xcalloc(sizeof(mir_unit_t));
   mu->context = mc;
   mu->name    = name;
   mu->object  = object;
   mu->kind    = kind;
   mu->parent  = parent;
   mu->result  = MIR_NULL_TYPE;

   mir_block_t entry = mir_add_block(mu);
   mir_set_cursor(mu, entry, MIR_APPEND);

   if (object != NULL)
      mir_set_loc(mu, &(object->loc));

   return mu;
}

mir_unit_kind_t mir_get_kind(mir_unit_t *mu)
{
   return mu->kind;
}

object_t *mir_get_object(mir_unit_t *mu)
{
   return mu->object;
}

ident_t mir_get_parent(mir_unit_t *mu)
{
   if (mu->parent != NULL)
      return mu->parent->name;
   else
      return NULL;
}

mir_context_t *mir_get_context(mir_unit_t *mu)
{
   return mu->context;
}

int mir_find_slot(mir_shape_t *shape, ident_t name)
{
   for (int i = 0; i < shape->num_slots; i++) {
      if (shape->slots[i].name == name)
         return i;
   }

   return -1;
}

ident_t mir_get_shape_parent(mir_shape_t *shape)
{
   return shape->parent ? shape->parent->name : NULL;
}

mir_value_t mir_search_object(mir_unit_t *mu, const void *obj, int *hops)
{
   *hops = 0;

   if (mu->objmap != NULL) {
      void *ptr = hash_get(mu->objmap, obj);
      if (ptr != NULL)
         return (mir_value_t){ .bits = (uintptr_t)ptr };
   }

   for (mir_shape_t *s = mu->parent; s != NULL; s = s->parent) {
      (*hops)++;
      if (s->objmap != NULL) {
         void *ptr = hash_get(s->objmap, obj);
         if (ptr != NULL) {
            mir_value_t value = { .bits = (uintptr_t)ptr };
            assert(value.tag == MIR_TAG_VAR);
            return value;
         }
      }
   }

   return MIR_NULL_VALUE;
}

void mir_put_object(mir_unit_t *mu, const void *obj, mir_value_t value)
{
   if (mu->objmap == NULL)
      mu->objmap = hash_new(16);

   hash_put(mu->objmap, obj, (void *)(uintptr_t)value.bits);
}

void *mir_get_priv(mir_unit_t *mu, const void *obj)
{
   if (mu->privmap == NULL)
      return NULL;
   else
      return hash_get(mu->privmap, obj);
}

void mir_put_priv(mir_unit_t *mu, const void *obj, void *value)
{
   if (mu->privmap == NULL)
      mu->privmap = hash_new(16);

   hash_put(mu->privmap, obj, value);
}

void *mir_malloc(mir_unit_t *mu, size_t size)
{
   if (mu->pool == NULL)
      mu->pool = pool_new();

   return pool_malloc(mu->pool, size);
}

void *mir_global_malloc(mir_context_t *mc, size_t fixed, size_t nelems,
                        size_t size)
{
   SCOPED_LOCK(mc->pool_mtx);
   return pool_malloc_flex(mc->pool, fixed, nelems, size);
}

static mir_shape_t *mir_build_shape(mir_unit_t *mu)
{
   mir_shape_t *sh = mir_global_malloc(mu->context, sizeof(mir_shape_t),
                                       mu->vars.count, sizeof(shape_slot_t));
   sh->name      = mu->name;
   sh->kind      = mu->kind;
   sh->type      = mir_self_type(mu);
   sh->num_slots = mu->vars.count;
   sh->parent    = mu->parent;
   sh->objmap    = mu->objmap;

   mu->objmap = NULL;   // Takes ownership

   for (int i = 0; i < mu->vars.count; i++) {
      const var_data_t *vd = &(mu->vars.items[i]);
      sh->slots[i].name    = vd->name;
      sh->slots[i].type    = vd->type;
      sh->slots[i].pointer = vd->pointer;
   }

   return sh;
}

void mir_unit_free(mir_unit_t *mu)
{
   if (mu->name != NULL) {
      void *ptr = chash_get(mu->context->map, mu->name);
      switch (pointer_tag(ptr)) {
      case UNIT_GENERATED:
         {
            mir_shape_t *sh = mu->shape ?: mir_build_shape(mu);
            chash_put(mu->context->map, mu->name, tag_pointer(sh, UNIT_FREED));
         }
         break;
      case UNIT_FREED:
      case UNIT_DEFERRED:
         should_not_reach_here();
      default:
         break;
      }
   }

   mir_unit_free_memory(mu);
}

void mir_put_unit(mir_context_t *mc, mir_unit_t *mu)
{
   assert(mu->context == mc);

#ifdef DEBUG
   void *ptr = chash_get(mc->map, mu->name);
   if (ptr != NULL)
      fatal_trace("%s already registered", istr(mu->name));
#endif

   if (opt_get_verbose(OPT_LOWER_VERBOSE, istr(mu->name))) {
      mir_set_cursor(mu, MIR_NULL_BLOCK, MIR_APPEND);
      mir_dump(mu);
   }

   chash_put(mc->map, mu->name, tag_pointer(mu, UNIT_GENERATED));
}

static mir_unit_t *mir_lazy_build(mir_context_t *mc, deferred_unit_t *du)
{
   mir_shape_t *parent = NULL;
   if (du->parent != NULL && (parent = mir_get_shape(mc, du->parent)) == NULL)
      fatal_trace("cannot get parent %s of unit %s", istr(du->parent),
                  istr(du->name));

   mir_unit_t *mu = mir_unit_new(mc, du->name, du->object, du->kind, parent);
   (*du->fn)(mu, du->object);

   if (opt_get_verbose(OPT_LOWER_VERBOSE, istr(du->name))) {
      mir_set_cursor(mu, MIR_NULL_BLOCK, MIR_APPEND);
      mir_dump(mu);
   }

   chash_put(mc->map, du->name, tag_pointer(mu, UNIT_GENERATED));
   return mu;
}

mir_unit_t *mir_get_unit(mir_context_t *mc, ident_t name)
{
   void *ptr = chash_get(mc->map, name);
   if (ptr == NULL)
      return NULL;

   switch (pointer_tag(ptr)) {
   case UNIT_DEFERRED:
      {
         deferred_unit_t *du = untag_pointer(ptr, deferred_unit_t);
         return mir_lazy_build(mc, du);
      }
   case UNIT_GENERATED:
      return untag_pointer(ptr, mir_unit_t);
   case UNIT_FREED:
      fatal_trace("unit %s has already been freed", istr(name));
   default:
      should_not_reach_here();
   }
}

mir_shape_t *mir_get_shape(mir_context_t *mc, ident_t name)
{
   void *ptr = chash_get(mc->map, name);
   if (ptr == NULL)
      return NULL;

   switch (pointer_tag(ptr)) {
   case UNIT_DEFERRED:
      {
         deferred_unit_t *du = untag_pointer(ptr, deferred_unit_t);
         mir_unit_t *mu = mir_lazy_build(mc, du);
         return (mu->shape = mir_build_shape(mu));
      }
   case UNIT_GENERATED:
      {
         mir_unit_t *mu = untag_pointer(ptr, mir_unit_t);
         if (mu->shape != NULL)
            return mu->shape;

         return (mu->shape = mir_build_shape(mu));
      }
   case UNIT_FREED:
      return untag_pointer(ptr, mir_shape_t);
   default:
      should_not_reach_here();
   }
}

void mir_defer(mir_context_t *mc, ident_t name, ident_t parent,
               mir_unit_kind_t kind, mir_lower_fn_t fn, object_t *object)
{
#ifdef DEBUG
   void *ptr = chash_get(mc->map, name);
   if (ptr != NULL) {
      switch (pointer_tag(ptr)) {
      case UNIT_DEFERRED:
         {
            deferred_unit_t *du = untag_pointer(ptr, deferred_unit_t);
            assert(du->fn == fn);
            assert(du->kind == kind);
            assert(du->object == object);
         }
         return;
      case UNIT_GENERATED:
         {
            mir_unit_t *mu = untag_pointer(ptr, mir_unit_t);
            assert(mu->kind == kind);
            assert(mu->object == object);
         }
         return;
      case UNIT_FREED:
         {
            mir_shape_t *s = untag_pointer(ptr, mir_shape_t);
            assert(s->kind == kind);
         }
         return;
      default:
         should_not_reach_here();
      }
   }
#endif

   deferred_unit_t *du = mir_global_malloc(mc, sizeof(deferred_unit_t), 0, 0);
   du->name   = name;
   du->fn     = fn;
   du->parent = parent;
   du->kind   = kind;
   du->object = object;

   chash_put(mc->map, name, tag_pointer(du, UNIT_DEFERRED));
}

unsigned mir_count_linkage(mir_unit_t *mu)
{
   return mu->linkage.count;
}

ident_t mir_get_linkage(mir_unit_t *mu, unsigned nth)
{
   return AGET(mu->linkage, nth);
}

#ifdef DEBUG
void mir_compare_layout(mir_unit_t *a, mir_unit_t *b)
{
   if (a->vars.count != b->vars.count)
      goto differ;

   for (int i = 0; i < a->vars.count; i++) {
      if (!mir_equals(a->vars.items[i].type, b->vars.items[i].type))
         goto differ;
   }

   return;

 differ:
   mir_dump(a);
   mir_dump(b);
   fatal_trace("%s and %s have differing layout", istr(a->name), istr(b->name));
}
#endif
