//
//  Copyright (C) 2013-2016  Nick Gasson
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
#include "tree.h"
#include "phase.h"
#include "common.h"
#include "rt/netdb.h"

#include <assert.h>
#include <stdlib.h>

typedef struct {
   group_t   *groups;
   groupid_t  next_gid;
} group_nets_ctx_t;

static void group_name(tree_t t, group_nets_ctx_t *ctx);

static groupid_t group_alloc(group_nets_ctx_t *ctx,
                             netid_t first, unsigned length)
{
   group_t *g = xmalloc(sizeof(group_t));
   g->next   = ctx->groups;
   g->gid    = ctx->next_gid++;
   g->first  = first;
   g->length = length;

   ctx->groups = g;

   return g->gid;
}

static void group_unlink(group_nets_ctx_t *ctx, group_t *where, group_t *prev)
{
   if (prev == NULL)
      ctx->groups = where->next;
   else
      prev->next = where->next;
}

static groupid_t group_add(group_nets_ctx_t *ctx, netid_t first, int length)
{
   assert(length > 0);

   group_t *it, *last;
   for (it = ctx->groups, last = NULL; it != NULL; last = it, it = it->next) {

      if ((it->first == first) && (it->length == length)) {
         // Exactly matches
         return it->gid;
      }
      else if (it->first >= first + length) {
         // Disjoint, to the right
      }
      else if (first >= it->first + it->length) {
         // Disjoint, to the left
      }
      else if ((first == it->first) && (length > it->length)) {
         // Overlaps on left
         group_add(ctx, first + it->length, length - it->length);
         return GROUPID_INVALID;
      }
      else if ((first > it->first)
               && (first + length == it->first + it->length)) {
         // Overlaps on right
         group_unlink(ctx, it, last);
         group_add(ctx, it->first, first - it->first);
         free(it);
         break;
      }
      else if ((first > it->first)
               && (first + length < it->first + it->length)) {
         // Overlaps completely
         group_unlink(ctx, it, last);
         group_add(ctx, it->first, first - it->first);
         group_add(ctx, first + length,
                   it->first + it->length - first - length);
         free(it);
         break;
      }
      else if ((first < it->first)
               && (first + length > it->first + it->length)) {
         // Contains in middle
         group_add(ctx, first, it->first - first);
         group_add(ctx, it->first + it->length,
                   first + length - it->first - it->length);
         return GROUPID_INVALID;
      }
      else if ((first == it->first)
               && (first + length < it->first + it->length)) {
         // Contains on left
         group_unlink(ctx, it, last);
         group_add(ctx, first + length, it->length - length);
         free(it);
         break;
      }
      else if ((first < it->first)
               && (first + length == it->first + it->length)) {
         // Contains on right
         group_add(ctx, first, it->first - first);
         return GROUPID_INVALID;
      }
      else if ((first < it->first) && (first + length > it->first)) {
         // Split left
         group_unlink(ctx, it, last);
         group_add(ctx, first, it->first - first);
         group_add(ctx, it->first, first + length - it->first);
         group_add(ctx, first + length,
                   it->first + it->length - first - length);
         free(it);
         return GROUPID_INVALID;
      }
      else if ((first > it->first) && (it->first + it->length > first)) {
         // Split right
         group_unlink(ctx, it, last);
         group_add(ctx, it->first, first - it->first);
         group_add(ctx, first, it->first + it->length - first);
         group_add(ctx, it->first + it->length,
                   first + length - it->first - it->length);
         free(it);
         return GROUPID_INVALID;
      }
      else
         fatal("unhandled case in group_add: first=%d length=%d "
               "it->first=%d it->length=%d", first, length,
               it->first, it->length);
   }

   return group_alloc(ctx, first, length);
}

static bool group_contains_record(type_t type)
{
   if (type_is_record(type))
      return true;
   else if (type_is_array(type))
      return group_contains_record(type_elem(type));
   else
      return false;
}

static int group_net_to_field(type_t type, netid_t nid)
{
   int count = 0;
   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      netid_t first = 0;
      for (int i = 0; i < nfields; i++) {
         tree_t field = type_field(type, i);
         type_t ftype = tree_type(field);
         const netid_t next = first + type_width(tree_type(field));
         if (nid >= first && nid < next) {
            if (type_is_record(ftype))
               return count + group_net_to_field(ftype, nid - first);
            else
               return count;
         }
         first = next;
         count += type_width(ftype);
      }
      fatal_trace("group_net_to_field failed to find field for nid=%d type=%s",
                  nid, type_pp(type));
   }
   else if (type_is_array(type)) {
      type_t elem = type_elem(type);
      return group_net_to_field(elem, nid % type_width(elem));
   }
   else
      fatal_trace("unexpected type %s %s in group_net_to_field",
                  type_pp(type), type_kind_str(type_kind(type)));
}

static void group_decl(tree_t decl, group_nets_ctx_t *ctx, int start, int n)
{
   netid_t first = NETID_INVALID;
   unsigned len = 0;
   type_t type = tree_type(decl);
   const int nnets = tree_nets(decl);
   const bool record = group_contains_record(type);
   int ffield = -1;
   assert((n == -1) | (start + n <= nnets));
   for (int i = start; i < (n == -1 ? nnets : start + n); i++) {
      netid_t nid = tree_net(decl, i);
      if (first == NETID_INVALID) {
         first  = nid;
         len    = 1;
         ffield = record ? group_net_to_field(type, i) : -1;
      }
      else if (nid == first + len
               && (!record || group_net_to_field(type, i) == ffield))
         ++len;
      else {
         group_add(ctx, first, len);
         first  = nid;
         len    = 1;
         ffield = record ? group_net_to_field(type, i) : -1;
      }
   }

   if (first != NETID_INVALID)
      group_add(ctx, first, len);
   else {
      // Array signal with null range
      tree_add_attr_int(decl, null_range_i, 1);
   }
}

static void group_ref(tree_t target, group_nets_ctx_t *ctx, int start, int n)
{
   assert(tree_kind(target) == T_REF);

   tree_t decl = tree_ref(target);
   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:
      group_decl(decl, ctx, start, n);
      break;
   case T_ALIAS:
      group_name(tree_value(decl), ctx);
      break;
   default:
      break;
   }
}

static void ungroup_ref(tree_t target, group_nets_ctx_t *ctx)
{
   tree_t decl = tree_ref(target);
   if (tree_kind(decl) == T_SIGNAL_DECL) {
      const int nnets = tree_nets(decl);
      for (int i = 0; i < nnets; i++)
         group_add(ctx, tree_net(decl, i), 1);
   }
}

static bool group_calc_offset(tree_t t, int *offset, tree_t *ref)
{
   switch (tree_kind(t)) {
   case T_REF:
      *offset = 0;
      *ref    = t;
      return true;

   case T_ARRAY_REF:
      {
         tree_t value = tree_value(t);
         int offset0;
         if (!group_calc_offset(value, &offset0, ref))
            return false;

         type_t type = tree_type(value);
         if (type_is_unconstrained(type))
            return false;

         *offset = 0;
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t index = tree_value(tree_param(t, i));

            if (tree_kind(index) != T_LITERAL)
               return false;

            const int stride = type_width(type_elem(type));

            if (i > 0) {
               range_t type_r = type_dim(type, i);
               int64_t low, high;
               range_bounds(type_r, &low, &high);

               *offset *= high - low + 1;
            }

            *offset += stride * rebase_index(type, i, assume_int(index));
         }

         return true;
      }

   case T_RECORD_REF:
      {
         tree_t value = tree_value(t);
         int offset0;
         if (!group_calc_offset(value, &offset0, ref))
            return false;

         type_t rec = tree_type(value);
         *offset = offset0 + record_field_to_net(rec, tree_ident(t));
         return true;
      }

   case T_AGGREGATE:
   case T_LITERAL:
      // This can appear due to assignments to open ports with a
      // default value
      *offset = 0;
      *ref    = NULL;
      return false;

   default:
      fatal_at(tree_loc(t), "tree kind %s not yet supported for offset "
               "calculation", tree_kind_str(tree_kind(t)));
   }
}

static void group_array_ref(tree_t target, group_nets_ctx_t *ctx)
{
   tree_t value = tree_value(target);

   type_t type = tree_type(value);
   if (type_is_unconstrained(type))
      return;    // Only in procedure

   const int width  = type_width(type);
   const int stride = type_width(type_elem(type));

   tree_t ref = NULL;
   int offset;
   if (group_calc_offset(target, &offset, &ref))
      group_ref(ref, ctx, offset, stride);
   else if (group_calc_offset(value, &offset, &ref)) {
      for (int i = 0; i < width; i += stride)
         group_ref(ref, ctx, offset + i, stride);
   }
   else if (ref != NULL)
      ungroup_ref(ref, ctx);
}

static void group_array_slice(tree_t target, group_nets_ctx_t *ctx)
{
   tree_t value = tree_value(target);
   type_t type  = tree_type(value);

   if (type_is_unconstrained(type))
      return;    // Only in procedure

   range_t slice = tree_range(target);

   const bool folded =
      (tree_kind(slice.left) == T_LITERAL)
      && (tree_kind(slice.right) == T_LITERAL);

   tree_t ref = NULL;
   int offset;
   if (group_calc_offset(value, &offset, &ref) && folded) {
      int64_t low, high;
      range_bounds(slice, &low, &high);

      const int64_t low0 = rebase_index(type, 0, assume_int(slice.left));
      const int stride   = type_width(type_elem(type));
      const int length   = MAX(high - low + 1, 0);

      group_ref(ref, ctx, offset + (low0 * stride), length * stride);
   }
   else if (ref != NULL)
      ungroup_ref(ref, ctx);
}

static void group_record_ref(tree_t target, group_nets_ctx_t *ctx)
{
   tree_t ref = NULL;
   int offset;
   if (group_calc_offset(target, &offset, &ref)) {
      tree_t field = find_record_field(target);
      group_ref(ref, ctx, offset, type_width(tree_type(field)));
   }
   else if (ref != NULL)
      ungroup_ref(ref, ctx);
}

static void group_name(tree_t t, group_nets_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      group_ref(t, ctx, 0, -1);
      break;

   case T_ARRAY_REF:
      group_array_ref(t, ctx);
      break;

   case T_ARRAY_SLICE:
      group_array_slice(t, ctx);
      break;

   case T_RECORD_REF:
      group_record_ref(t, ctx);
      break;

   case T_LITERAL:
      // Constant folding can cause this to appear
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++)
            group_name(tree_value(tree_assoc(t, i)), ctx);
      }
      break;

   default:
      assert(false);
   }
}

static void ungroup_proc_params(tree_t t, group_nets_ctx_t *ctx)
{
   // Ungroup any signal that is passed to a procedure as in general we
   // cannot guarantee anything about the procedure's behaviour

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t value = tree_value(tree_param(t, i));

      while (tree_kind(value) != T_REF) {
         switch (tree_kind(value)) {
         case T_ARRAY_REF:
         case T_ARRAY_SLICE:
            value = tree_value(value);
            break;

         default:
            return;
         }
      }

      tree_t decl = tree_ref(value);

      if (tree_kind(decl) != T_SIGNAL_DECL)
         return;

      const int nnets = tree_nets(decl);
      for (int i = 0; i < nnets; i++)
         group_add(ctx, tree_net(decl, i), 1);
   }
}

static void group_nets_visit_fn(tree_t t, void *_ctx)
{
   group_nets_ctx_t *ctx = _ctx;

   switch (tree_kind(t)) {
   case T_SIGNAL_ASSIGN:
      group_name(tree_target(t), ctx);
      break;

   case T_WAIT:
      {
         const int ntriggers = tree_triggers(t);
         for (int i = 0; i < ntriggers; i++)
            group_name(tree_trigger(t, i), ctx);
      }
      break;

   case T_PCALL:
      ungroup_proc_params(t, ctx);
      break;

   case T_SIGNAL_DECL:
      // Ensure that no group is larger than a signal declaration
      group_decl(t, ctx, 0, -1);
      break;

   default:
      break;
   }
}

static void group_write_netdb(tree_t top, group_nets_ctx_t *ctx)
{
   char *name = xasprintf("_%s.netdb", istr(tree_ident(top)));

   fbuf_t *f = lib_fbuf_open(lib_work(), name, FBUF_OUT);
   if (f == NULL)
      fatal("failed to create net database file %s", name);

   free(name);

   for (group_t *it = ctx->groups; it != NULL; it = it->next) {
      write_u32(it->gid, f);
      write_u32(it->first, f);
      write_u32(it->length, f);
   }
   write_u32(GROUPID_INVALID, f);

   fbuf_close(f);
}

void group_nets(tree_t top)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   group_write_netdb(top, &ctx);

   if (getenv("NVC_GROUP_STATS") != NULL) {
      const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
      int ngroups = 0;
      for (group_t *it = ctx.groups; it != NULL; it = it->next)
         ngroups++;

      notef("nets:groups ratio %.3f", (float)nnets / (float)ngroups);
   }

   while (ctx.groups != NULL) {
      group_t *tmp = ctx.groups->next;
      free(ctx.groups);
      ctx.groups = tmp;
   }
}
