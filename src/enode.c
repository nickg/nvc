//
//  Copyright (C) 2021  Nick Gasson
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

#include "enode.h"
#include "object.h"

#include <string.h>
#include <inttypes.h>
#include <stdlib.h>

static const imask_t has_map[E_LAST_NODE_KIND] = {
   // E_ROOT
   (I_IDENT | I_SCOPES | I_NEXUS | I_DEPS),

   // E_SCOPE
   (I_PARENT | I_SIGNALS | I_SCOPES | I_PROCS | I_IDENT2 | I_PATH | I_VCODE),

   // E_SIGNAL
   (I_IDENT | I_NEXUS | I_IVAL | I_IDENT2 | I_PATH | I_FLAGS | I_TYPE
    | I_PARENT),

   // E_PROCESS
   (I_IDENT | I_IDENT2 | I_PATH | I_NEXUS | I_VCODE | I_PARENT | I_FLAGS
    | I_TRIGGERS),

   // E_NEXUS
   (I_IDENT | I_IVAL | I_SIGNALS | I_POS | I_SIZE | I_SOURCES | I_OUTPUTS
    | I_TRIGGERS),

   // E_PORT
   (I_IDENT | I_NEXUS | I_FLAGS),

   // E_IMPLICIT
   (I_IDENT | I_NEXUS | I_IVAL | I_IDENT2 | I_PATH | I_FLAGS | I_TYPE
    | I_PARENT | I_TRIGGERS | I_VCODE),
};

static const char *kind_text_map[E_LAST_NODE_KIND] = {
   "E_ROOT", "E_SCOPE", "E_SIGNAL", "E_PROCESS", "E_NEXUS", "E_PORT",
   "E_IMPLICIT",
};

static const change_allowed_t change_allowed[] = {
   { -1, -1 }
};

object_class_t e_node_object = {
   .name           = "e-node",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_E_NODE,
   .last_kind      = E_LAST_NODE_KIND,
   .gc_roots       = { E_ROOT },
   .gc_num_roots   = 1
};

extern object_arena_t *global_arena;

struct _e_node {
   object_t object;
};

struct _type {
   object_t object;
};

static inline e_node_t e_array_nth(item_t *item, unsigned n)
{
   return container_of(AGET(item->obj_array, n), struct _e_node, object);
}

static inline void e_array_add(item_t *item, e_node_t e)
{
   APUSH(item->obj_array, &(e->object));
}

static inline void e_array_insert(item_t *item, e_node_t after, e_node_t new)
{
   unsigned opos = 0;
   for (; opos < item->obj_array.count; opos++) {
      if (item->obj_array.items[opos] == &(after->object))
         break;
   }
   assert(opos != item->obj_array.count);

   if (opos + 1 == item->obj_array.count)
      APUSH(item->obj_array, &(new->object));
   else {
      ARESIZE(item->obj_array, item->obj_array.count + 1);
      memmove(item->obj_array.items + opos + 1,
              item->obj_array.items + opos,
              (item->obj_array.count - 1 - opos) * sizeof(object_t*));
      item->obj_array.items[opos + 1] = &(new->object);
   }
}

e_node_t e_new(e_kind_t kind)
{
   return (e_node_t)object_new(global_arena, &e_node_object, kind);
}

e_kind_t e_kind(e_node_t e)
{
   return e->object.kind;
}

const char *e_kind_str(e_kind_t t)
{
   return kind_text_map[t];
}

ident_t e_ident(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

void e_set_ident(e_node_t e, ident_t i)
{
   lookup_item(&e_node_object, e, I_IDENT)->ident = i;
}

const loc_t *e_loc(e_node_t e)
{
   assert(e != NULL);
   return &(e->object.loc);
}

void e_set_loc(e_node_t e, const loc_t *loc)
{
   assert(e != NULL);
   assert(loc != NULL);
   e->object.loc = *loc;
}

ident_t e_instance(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_IDENT2);
   assert(item->ident != NULL);
   return item->ident;
}

void e_set_instance(e_node_t e, ident_t i)
{
   lookup_item(&e_node_object, e, I_IDENT2)->ident = i;
}

ident_t e_path(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_PATH);
   assert(item->ident != NULL);
   return item->ident;
}

void e_set_path(e_node_t e, ident_t i)
{
   lookup_item(&e_node_object, e, I_PATH)->ident = i;
}

unsigned e_deps(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_DEPS)->ident_array.count;
}

ident_t e_dep(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_DEPS);
   return AGET(item->ident_array, n);
}

void e_add_dep(e_node_t e, ident_t i)
{
   item_t *item = lookup_item(&e_node_object, e, I_DEPS);
   APUSH(item->ident_array, i);
}

unsigned e_pos(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_POS);
   assert(item->ival != NEXUS_POS_INVALID);
   return item->ival;
}

void e_set_pos(e_node_t e, unsigned pos)
{
   lookup_item(&e_node_object, e, I_POS)->ival = pos;
}

bool e_has_pos(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_POS)->ival != NEXUS_POS_INVALID;
}

e_flags_t e_flags(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_FLAGS)->ival;
}

void e_set_flag(e_node_t e, e_flags_t mask)
{
   lookup_item(&e_node_object, e, I_FLAGS)->ival |= mask;
}

unsigned e_scopes(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_SCOPES)->obj_array.count;
}

e_node_t e_scope(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_SCOPES);
   return e_array_nth(item, n);
}

void e_add_scope(e_node_t e, e_node_t s)
{
   assert(s->object.kind == E_SCOPE);
   item_t *item = lookup_item(&e_node_object, e, I_SCOPES);
   e_array_add(item, s);
   object_write_barrier(&(e->object), &(s->object));
}

unsigned e_procs(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_PROCS)->obj_array.count;
}

e_node_t e_proc(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_PROCS);
   return e_array_nth(item, n);
}

void e_add_proc(e_node_t e, e_node_t p)
{
   assert(p->object.kind == E_PROCESS);
   item_t *item = lookup_item(&e_node_object, e, I_PROCS);
   e_array_add(item, p);
   object_write_barrier(&(e->object), &(p->object));
}

unsigned e_sources(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_SOURCES)->obj_array.count;
}

e_node_t e_source(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_SOURCES);
   return e_array_nth(item, n);
}

void e_add_source(e_node_t e, e_node_t s)
{
   assert(s->object.kind == E_PROCESS || s->object.kind == E_PORT);
   item_t *item = lookup_item(&e_node_object, e, I_SOURCES);
   e_array_add(item, s);
   object_write_barrier(&(e->object), &(s->object));
}

unsigned e_outputs(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_OUTPUTS)->obj_array.count;
}

e_node_t e_output(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_OUTPUTS);
   return e_array_nth(item, n);
}

void e_add_output(e_node_t e, e_node_t s)
{
   assert(s->object.kind == E_PORT);
   item_t *item = lookup_item(&e_node_object, e, I_OUTPUTS);
   e_array_add(item, s);
   object_write_barrier(&(e->object), &(s->object));
}

e_node_t e_parent(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_PARENT);
   assert(item->object != NULL);
   return container_of(item->object, struct _e_node, object);
}

void e_set_parent(e_node_t e, e_node_t p)
{
   lookup_item(&e_node_object, e, I_PARENT)->object = &(p->object);
   object_write_barrier(&(e->object), &(p->object));
}

bool e_has_parent(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_PARENT)->object != NULL;
}

unsigned e_signals(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_SIGNALS)->obj_array.count;
}

e_node_t e_signal(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_SIGNALS);
   return e_array_nth(item, n);
}

void e_add_signal(e_node_t e, e_node_t s)
{
   item_t *item = lookup_item(&e_node_object, e, I_SIGNALS);
   e_array_add(item, s);
   object_write_barrier(&(e->object), &(s->object));
}

unsigned e_triggers(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_TRIGGERS)->obj_array.count;
}

e_node_t e_trigger(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_TRIGGERS);
   return e_array_nth(item, n);
}

void e_add_trigger(e_node_t e, e_node_t t)
{
   item_t *item = lookup_item(&e_node_object, e, I_TRIGGERS);
   e_array_add(item, t);
   object_write_barrier(&(e->object), &(t->object));
}

unsigned e_nexuses(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_NEXUS)->obj_array.count;
}

e_node_t e_nexus(e_node_t e, unsigned n)
{
   item_t *item = lookup_item(&e_node_object, e, I_NEXUS);
   return e_array_nth(item, n);
}

void e_add_nexus(e_node_t e, e_node_t n)
{
   assert(n->object.kind == E_NEXUS);
   item_t *item = lookup_item(&e_node_object, e, I_NEXUS);
   e_array_add(item, n);
   object_write_barrier(&(e->object), &(n->object));
}

void e_change_nexus(e_node_t e, unsigned o, e_node_t n)
{
   assert(n->object.kind == E_NEXUS);
   item_t *item = lookup_item(&e_node_object, e, I_NEXUS);
   assert(o < item->obj_array.count);
   item->obj_array.items[o] = &(n->object);
   object_write_barrier(&(e->object), &(n->object));
}

void e_insert_nexus(e_node_t e, e_node_t after, e_node_t new)
{
   assert(after->object.kind == E_NEXUS);
   assert(new->object.kind == E_NEXUS);

   e_array_insert(lookup_item(&e_node_object, e, I_NEXUS), after, new);
   object_write_barrier(&(e->object), &(new->object));
}

unsigned e_width(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_IVAL)->ival;
}

void e_set_width(e_node_t e, unsigned w)
{
   assert(w < INT32_MAX);
   lookup_item(&e_node_object, e, I_IVAL)->ival = w;
}

unsigned e_size(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_SIZE)->ival;
}

void e_set_size(e_node_t e, unsigned s)
{
   lookup_item(&e_node_object, e, I_SIZE)->ival = s;
}

bool e_has_vcode(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_VCODE)->ident != NULL;
}

ident_t e_vcode(e_node_t e)
{
   item_t *item = lookup_item(&e_node_object, e, I_VCODE);
   assert(item->ident != NULL);
   return item->ident;
}

void e_set_vcode(e_node_t e, ident_t vunit)
{
   lookup_item(&e_node_object, e, I_VCODE)->ident = vunit;
}

type_t e_type(e_node_t e)
{
   return lookup_item(&e_node_object, e, I_TYPE)->type;
}

void e_set_type(e_node_t e, type_t type)
{
   assert(type != NULL);
   lookup_item(&e_node_object, e, I_TYPE)->type = type;
   object_write_barrier(&(e->object), &(type->object));
}

static void e_clone_port(e_node_t p, e_node_t from, e_node_t to)
{
   e_node_t p2 = e_new(E_PORT);
   e_set_loc(p2, e_loc(p));
   e_set_ident(p2, e_ident(p));
   e_set_flag(p2, e_flags(p));
   e_add_nexus(p2, from);
   e_add_nexus(p2, to);

   e_add_source(to, p2);
   e_add_output(from, p2);
}

e_node_t e_split_nexus(e_node_t root, e_node_t orig, unsigned width)
{
   assert(root->object.kind == E_ROOT);
   assert(orig->object.kind == E_NEXUS);
   assert(width > 0);

   const unsigned owidth = e_width(orig);
   assert(width < owidth);

   e_node_t new = e_new(E_NEXUS);
   e_set_pos(new, NEXUS_POS_INVALID);
   e_set_ident(new, e_ident(orig));
   e_set_width(new, owidth - width);
   e_set_size(new, e_size(orig));

   e_array_insert(lookup_item(&e_node_object, root, I_NEXUS), orig, new);

   const int nsignals = e_signals(orig);
   for (int i = 0; i < nsignals; i++) {
      e_node_t s = e_signal(orig, i);
      e_add_signal(new, s);
      e_array_insert(lookup_item(&e_node_object, s, I_NEXUS), orig, new);
   }

   const int ntriggers = e_triggers(orig);
   for (int i = 0; i < ntriggers; i++) {
      e_node_t t = e_trigger(orig, i);
      e_add_trigger(new, t);
      e_array_insert(lookup_item(&e_node_object, t, I_TRIGGERS), orig, new);
   }

   e_set_width(orig, width);

   const int nsources = e_sources(orig);
   for (int i = 0; i < nsources; i++) {
      e_node_t p = e_source(orig, i);
      switch (e_kind(p)) {
      case E_PROCESS:
         e_add_source(new, p);
         e_add_nexus(p, new);
         break;
      case E_PORT:
         {
            assert(e_nexus(p, 1) == orig);
            e_node_t from = e_nexus(p, 0);
            if (e_flags(p) & E_F_CONV_FUNC)
               e_clone_port(p, from, new);
            else if (e_width(from) > width) {
               e_node_t from2 = e_split_nexus(root, from, width);
               e_clone_port(p, from2, new);
            }
            else
               assert(e_width(from) == width);  // Cycle breaking
         }
         break;
      default:
         fatal_trace("unexpected source kind %s", e_kind_str(e_kind(p)));
      }
   }

   const int noutputs = e_outputs(orig);
   for (int i = 0; i < noutputs; i++) {
      e_node_t o = e_output(orig, i);
      assert(e_kind(o) == E_PORT);
      assert(e_nexus(o, 0) == orig);

      e_node_t to = e_nexus(o, 1);
      if (e_flags(o) & E_F_CONV_FUNC)
         e_clone_port(o, new, to);
      else if (e_width(to) > width) {
         e_node_t to2 = e_split_nexus(root, to, width);
         e_clone_port(o, new, to2);
      }
      else
         assert(e_width(to) == width);  // Cycle breaking
   }

   return new;
}

void e_collapse_port(e_node_t root, unsigned pos, e_node_t old, e_node_t port)
{
   assert(root->object.kind == E_ROOT);
   assert(old->object.kind == E_NEXUS);
   assert(port->object.kind == E_PORT);
   assert(e_nexus(port, 1) == old);

   e_node_t new = e_nexus(port, 0);

   const int nsignals = e_signals(old);
   for (int i = 0; i < nsignals; i++) {
      e_node_t s = e_signal(old, i);
      e_add_signal(new, s);

      obj_array_t *nexus_array =
         &(lookup_item(&e_node_object, s, I_NEXUS)->obj_array);
      for (unsigned i = 0; i < nexus_array->count; i++) {
         if (nexus_array->items[i] == &(old->object))
            nexus_array->items[i] = &(new->object);
      }
   }

   const int ntriggers = e_triggers(old);
   for (int i = 0; i < ntriggers; i++) {
      e_node_t t = e_trigger(old, i);
      e_add_trigger(new, t);

      obj_array_t *nexus_array =
         &(lookup_item(&e_node_object, t, I_TRIGGERS)->obj_array);
      for (unsigned i = 0; i < nexus_array->count; i++) {
         if (nexus_array->items[i] == &(old->object))
            nexus_array->items[i] = &(new->object);
      }
   }

   obj_array_t *new_outs =
      &(lookup_item(&e_node_object, new, I_OUTPUTS)->obj_array);

   unsigned wptr = 0;
   for (unsigned i = 0; i < new_outs->count; i++) {
      if (new_outs->items[i] != &(port->object))
         new_outs->items[wptr++] = new_outs->items[i];
   }
   ATRIM(*new_outs, wptr);

   obj_array_t *old_outs =
      &(lookup_item(&e_node_object, old, I_OUTPUTS)->obj_array);

   for (unsigned i = 0; i < old_outs->count; i++) {
      e_node_t p = container_of(old_outs->items[i], struct _e_node, object);
      obj_array_t *nexus_array =
         &(lookup_item(&e_node_object, p, I_NEXUS)->obj_array);
      assert(nexus_array->items[0] == &(old->object));
      nexus_array->items[0] = &(new->object);

      APUSH(*new_outs, old_outs->items[i]);
   }

   lookup_item(&e_node_object, root, I_NEXUS)->obj_array.items[pos] = NULL;
}

void e_clean_nexus_array(e_node_t root)
{
   assert(root->object.kind == E_ROOT);
   obj_array_t *array =
      &(lookup_item(&e_node_object, root, I_NEXUS)->obj_array);

   unsigned wptr = 0;
   for (unsigned i = 0; i < array->count; i++) {
      object_t *o = array->items[i];
      if (o == NULL) continue;

      e_node_t n = container_of(o, struct _e_node, object);
      item_t *pos = lookup_item(&e_node_object, n, I_POS);
      if (pos->ival == NEXUS_POS_INVALID) {
         pos->ival = wptr;
         array->items[wptr++] = o;
      }
   }

   ATRIM(*array, wptr);
}

static void e_dump_indent(int indent)
{
  for (int i = 0; i < indent; i++)
      putc(' ', stdout);
}

static void _e_dump(e_node_t e, int indent)
{
   e_dump_indent(indent);

   const e_kind_t kind = e_kind(e);
   switch (kind) {
   case E_ROOT:
      {
         color_printf("$cyan$root$$ %s\n", istr(e_ident(e)));

         color_printf("  $cyan$dependencies$$\n");
         const int ndeps = e_deps(e);
         for (int i = 0; i < ndeps; i++)
            printf("    %s\n", istr(e_dep(e, i)));

         const int nnexus = e_nexuses(e);
         for (int i = 0; i < nnexus; i++)
            _e_dump(e_nexus(e, i), indent + 2);

         const int nscopes = e_scopes(e);
         for (int i = 0; i < nscopes; i++)
            _e_dump(e_scope(e, i), indent + 2);
      }
      break;

   case E_SCOPE:
      {
         color_printf("$cyan$scope$$ %s\n", istr(e_instance(e)));

         const int nsignals = e_signals(e);
         for (int i = 0; i < nsignals; i++)
            _e_dump(e_signal(e, i), indent + 2);

         const int nscopes = e_scopes(e);
         for (int i = 0; i < nscopes; i++)
            _e_dump(e_scope(e, i), indent + 2);

         const int nprocs = e_procs(e);
         for (int i = 0; i < nprocs; i++)
            _e_dump(e_proc(e, i), indent + 2);
      }
      break;

   case E_PROCESS:
      {
         color_printf("$cyan$process$$ %s", istr(e_ident(e)));
         if (e_flags(e) & E_F_POSTPONED)
            color_printf(" $cyan$postponed$$");
         printf("\n");

         e_dump_indent(indent + 2);
         color_printf("$cyan$vcode$$ %s\n", istr(e_vcode(e)));

         const int nnexus = e_nexuses(e);
         for (int i = 0; i < nnexus; i++) {
            e_node_t n = e_nexus(e, i);
            e_dump_indent(indent + 2);
            color_printf("$cyan$drives$$ %s", istr(e_ident(n)));
            if (e_has_pos(n))
               color_printf(" $cyan$id$$ %u", e_pos(n));
            color_printf(" $cyan$width$$ %u\n", e_width(n));
         }

         const int ntriggers = e_triggers(e);
         for (int i = 0; i < ntriggers; i++) {
            e_node_t t = e_trigger(e, i);
            e_dump_indent(indent + 2);
            color_printf("$cyan$triggered by$$ %s\n", istr(e_ident(t)));
         }
      }
      break;

   case E_IMPLICIT:
   case E_SIGNAL:
      {
         color_printf("$cyan$%ssignal$$ %s $cyan$width$$ %u",
                      kind == E_IMPLICIT ? "implicit " : "",
                      istr(e_ident(e)), e_width(e));

         const e_flags_t flags = e_flags(e);
         if (flags & E_F_LAST_VALUE)
            color_printf(" $cyan$last-value$$");
         if (!(flags & E_F_CONTIGUOUS))
            color_printf(" $cyan$non-contiguous$$");

         const int nnexus = e_nexuses(e);
         fputc(nnexus == 1 && flags == 0 ? ' ' : '\n', stdout);
         for (int i = 0; i < nnexus; i++) {
            e_node_t n = e_nexus(e, i);
            if (nnexus != 1 || flags != 0) e_dump_indent(indent + 2);
            color_printf("$cyan$nexus$$ %s", istr(e_ident(n)));
            if (nnexus != 1 || flags != 0 || e_width(n) != e_width(e)) {
               if (e_kind(n) == E_NEXUS && e_has_pos(n))
                  color_printf(" $cyan$id$$ %u", e_pos(n));
               color_printf(" $cyan$width$$ %u $cyan$size$$ %u",
                            e_width(n), e_size(n));
            }
            printf("\n");
         }

         if (kind == E_IMPLICIT) {
            e_dump_indent(indent + 2);
            color_printf("$cyan$vcode$$ %s\n", istr(e_vcode(e)));

            const int ntriggers = e_triggers(e);
            for (int i = 0; i < ntriggers; i++) {
               e_node_t t = e_trigger(e, i);
               e_dump_indent(indent + 2);
               color_printf("$cyan$triggered by$$ %s\n", istr(e_ident(t)));
            }
         }
      }
      break;

   case E_NEXUS:
      {
         color_printf("$cyan$nexus$$ %s", istr(e_ident(e)));
         if (e_has_pos(e))
            color_printf(" $cyan$id$$ %u", e_pos(e));
         color_printf(" $cyan$width$$ %u $cyan$size$$ %u\n",
                      e_width(e), e_size(e));

         const int nsignals = e_signals(e);
         for (int i = 0; i < nsignals; i++)
            color_printf("    $cyan$signal$$ %s\n",
                         istr(e_path(e_signal(e, i))));

         const int nsources = e_sources(e);
         for (int i = 0; i < nsources; i++) {
            e_node_t s = e_source(e, i);
            switch (e_kind(s)) {
            case E_PORT:
               color_printf("    $cyan$port$$ %s", istr(e_ident(s)));
               if (e_flags(s) & E_F_CONV_FUNC)
                  color_printf(" $cyan$conv-func$$");
               printf("\n");
               break;
            case E_PROCESS:
               color_printf("    $cyan$driver$$ %s\n", istr(e_path(s)));
               break;
            default:
               fatal_trace("unexpected source kind %s", e_kind_str(e_kind(s)));
            }
         }

         const int noutputs = e_outputs(e);
         for (int i = 0; i < noutputs; i++) {
            e_node_t s = e_output(e, i);
            assert(e_kind(s) == E_PORT);

            e_node_t o = e_nexus(s, 1);
            color_printf("    $cyan$sources$$ %s\n", istr(e_ident(o)));
         }

         const int ntriggers = e_triggers(e);
         for (int i = 0; i < ntriggers; i++) {
            e_node_t t = e_trigger(e, i);
            color_printf("    $cyan$triggers$$ %s\n", istr(e_path(t)));
         }
      }
      break;

   default:
      fatal_trace("cannot dump e-node kind %s", e_kind_str(e_kind(e)));
   }
}

void e_dump(e_node_t e)
{
   _e_dump(e, 0);
   printf("\n");
}
