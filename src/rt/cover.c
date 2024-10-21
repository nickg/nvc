//
//  Copyright (C) 2013-2023  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-data.h"
#include "hash.h"
#include "lib.h"
#include "option.h"
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/structs.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

enum std_ulogic {
   _U  = 0x0,
   _X  = 0x1,
   _0  = 0x2,
   _1  = 0x3,
   _Z  = 0x4,
   _W  = 0x5,
   _L  = 0x6,
   _H  = 0x7,
   _DC = 0x8
};

//#define COVER_DEBUG_CALLBACK

///////////////////////////////////////////////////////////////////////////////
// Toggle coverage
///////////////////////////////////////////////////////////////////////////////

static inline void increment_counter(int32_t *ptr)
{
   *ptr = saturate_add(*ptr, 1);
}

static inline void cover_toggle_check_0_1(uint8_t old, uint8_t new,
                                          int32_t *toggle_01, int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);
}

static inline void cover_toggle_check_0_1_u(uint8_t old, uint8_t new,
                                            int32_t *toggle_01, int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);

   else if (old == _U && new == _1)
      increment_counter(toggle_01);
   else if (old == _U && new == _0)
      increment_counter(toggle_10);
}

static inline void cover_toggle_check_0_1_z(uint8_t old, uint8_t new,
                                            int32_t *toggle_01, int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);

   else if (old == _0 && new == _Z)
      increment_counter(toggle_01);
   else if (old == _Z && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _Z)
      increment_counter(toggle_10);
   else if (old == _Z && new == _0)
      increment_counter(toggle_10);
}

static inline void cover_toggle_check_0_1_u_z(uint8_t old, uint8_t new,
                                              int32_t *toggle_01, int32_t *toggle_10)
{

   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);

   else if (old == _U && new == _1)
      increment_counter(toggle_01);
   else if (old == _U && new == _0)
      increment_counter(toggle_10);

   else if (old == _0 && new == _Z)
      increment_counter(toggle_01);
   else if (old == _Z && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _Z)
      increment_counter(toggle_10);
   else if (old == _Z && new == _0)
      increment_counter(toggle_10);
}

#ifdef COVER_DEBUG_CALLBACK
#define COVER_TGL_CB_MSG(signal)                                              \
   do {                                                                       \
      printf("Time: %lu Callback on signal: %s\n",                            \
              now, istr(tree_ident(signal->where)));                          \
   } while (0);

#define COVER_TGL_SIGNAL_DETAILS(signal, size)                                \
   do {                                                                       \
      printf("New signal value:\n");                                          \
      for (int i = 0; i < size; i++)                                          \
         printf("0x%x ", ((uint8_t*)signal_value(signal))[i]);                \
      printf("\n");                                                           \
      printf("Old signal value:\n");                                          \
      for (int i = 0; i < size; i++)                                          \
         printf("0x%x ", ((const uint8_t *)signal_last_value(signal))[i]);    \
      printf("\n\n");                                                         \
   } while (0);

#else
#define COVER_TGL_CB_MSG(signal)
#define COVER_TGL_SIGNAL_DETAILS(signal, size)
#endif

// TODO: Could multi-bit signals be optimized with vector instructions ?
#define DEFINE_COVER_TOGGLE_CB(name, check_fnc)                               \
   static void name(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)  \
   {                                                                          \
      uint32_t s_size = s->shared.size;                                       \
      rt_model_t *m = get_model();                                            \
      const int32_t tag = (uintptr_t)user;                                    \
      int32_t *toggle_01 = get_cover_counter(m, tag);                         \
      int32_t *toggle_10 = toggle_01 + 1;                                     \
      COVER_TGL_CB_MSG(s)                                                     \
      for (int i = 0; i < s_size; i++) {                                      \
         uint8_t new = ((uint8_t*)signal_value(s))[i];                        \
         uint8_t old = ((uint8_t*)signal_last_value(s))[i];                   \
         check_fnc(old, new, toggle_01, toggle_10);                           \
         toggle_01 += 2;                                                      \
         toggle_10 += 2;                                                      \
      }                                                                       \
      COVER_TGL_SIGNAL_DETAILS(s, s_size)                                     \
   }                                                                          \

DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1,     cover_toggle_check_0_1)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_u,   cover_toggle_check_0_1_u)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_z,   cover_toggle_check_0_1_z)
DEFINE_COVER_TOGGLE_CB(cover_toggle_cb_0_1_u_z, cover_toggle_check_0_1_u_z)

static bool is_constant_input(rt_signal_t *s)
{
   tree_t decl = s->where;
   tree_kind_t kind = tree_kind(decl);

   if (kind == T_FIELD_DECL) {
      rt_scope_t *sc = s->parent;
      while (sc->parent->kind == SCOPE_SIGNAL) {
         sc = sc->parent;
      }
      decl = sc->where;
   }

   if (tree_kind(decl) != T_PORT_DECL)
      return false;
   else if (tree_subkind(decl) != PORT_IN)
      return false;
   else {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++, n = n->chain) {
         if (n->n_sources > 0)
            return false;
      }
      return true;
   }
}

void x_cover_setup_toggle_cb(sig_shared_t *ss, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();
   cover_mask_t op_mask = get_coverage(m)->mask;

   if (is_constant_input(s)) {
      int32_t *toggle_01 = get_cover_counter(m, tag);
      int32_t *toggle_10 = toggle_01 + 1;

      // Each std_logic bit encoded as single byte. There are two run-time
      // counters for each std_logic bit
      for (int i = 0; i < s->shared.size; i++) {
         // Remember constant driver in run-time data.
         // Unreachable mask not available at run-time.
         *toggle_01 = COV_FLAG_UNREACHABLE;
         *toggle_10 = COV_FLAG_UNREACHABLE;
         toggle_01 += 2;
         toggle_10 += 2;
      }
      return;
   }

   sig_event_fn_t fn = &cover_toggle_cb_0_1;

   if ((op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED) &&
       (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z))
      fn = &cover_toggle_cb_0_1_u_z;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED)
      fn = &cover_toggle_cb_0_1_u;

   else if (op_mask & COVER_MASK_TOGGLE_COUNT_FROM_TO_Z)
      fn = &cover_toggle_cb_0_1_z;

   model_set_event_cb(m, s, fn, (void *)(uintptr_t)tag, false);
}

///////////////////////////////////////////////////////////////////////////////
// FSM state coverage
///////////////////////////////////////////////////////////////////////////////

#define READ_STATE(type) offset = *((type *)signal_value(s));

static void cover_state_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)
{
   // I-th enum literal is encoded in i-th tag from first tag, that corresponds
   // to enum value.
   int size = signal_size(s);
   int32_t offset = 0;
   FOR_ALL_SIZES(size, READ_STATE);

   rt_model_t *m = get_model();
   int32_t *mask = get_cover_counter(m, ((uintptr_t)user) + offset);

   increment_counter(mask);
}

void x_cover_setup_state_cb(sig_shared_t *ss, int64_t low, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();

   int32_t *mask = get_cover_counter(m, tag);

   // TYPE'left is a default value of enum type that does not
   // cause an event. First tag needs to be flagged as covered manually.
   *mask = 1;

   model_set_event_cb(m, s, cover_state_cb, (void *)(uintptr_t)(tag - low), false);
}

///////////////////////////////////////////////////////////////////////////////
// Run-time API
///////////////////////////////////////////////////////////////////////////////

static cover_scope_t *find_cover_scope(cover_data_t *data, rt_model_t *m,
                                       rt_scope_t *inst)
{
   if (inst->parent == NULL)
      return data->root_scope;

   cover_scope_t *parent = find_cover_scope(data, m, inst->parent);
   if (parent == NULL)
      return NULL;

   ident_t suffix = ident_rfrom(inst->name, '.');

   for (int i = 0; i < parent->children.count; i++) {
      if (parent->children.items[i]->name == suffix)
         return parent->children.items[i];
   }

   tree_t hier = tree_decl(inst->parent->where, 0);
   assert(tree_kind(hier) == T_HIER);

   if (tree_kind(tree_ref(hier)) == T_COMPONENT)
      return parent;   // Skip over implicit block for components

   return NULL;
}

static void sanitise_name(text_buf_t *tb, const char *bytes, size_t len)
{
   for (size_t i = 0; i < len; i++) {
      const char ch = bytes[i];
      if (isalnum_iso88591(ch) || ch == '_')
         tb_append(tb, ch);
      else
         tb_printf(tb, "&#%d;", ch);
   }
}

DLLEXPORT
void _nvc_create_cover_scope(jit_scalar_t *args)
{
   cover_scope_t **ptr = args[2].pointer;
   const char *name_bytes = args[3].pointer;
   size_t name_len = ffi_array_length(args[5].integer);

   *ptr = NULL;

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage scope name cannot be empty");

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *data = get_coverage(m);
   if (data == NULL || !cover_enabled(data, COVER_MASK_FUNCTIONAL))
      return;

   rt_scope_t *inst = get_active_scope(m);
   assert(inst->kind == SCOPE_INSTANCE);

   cover_scope_t *parent = find_cover_scope(data, m, inst);
   if (parent == NULL)
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   sanitise_name(tb, name_bytes, name_len);

   const size_t pfxlen = tb_len(tb);
   for (int i = 0, dup = 0; i < parent->children.count; i++) {
      if (icmp(parent->children.items[i]->name, tb_get(tb))) {
         tb_trim(tb, pfxlen);
         tb_printf(tb, "#%d", ++dup);
      }
   }

   ident_t name = ident_new(tb_get(tb));
   *ptr = cover_create_scope(data, parent, inst->where, name);
   (*ptr)->block_name = name;
}

DLLEXPORT
void _nvc_set_cover_scope_name(jit_scalar_t *args)
{
   cover_scope_t *s = *(cover_scope_t **)args[2].pointer;
   const char *name_bytes = args[3].pointer;
   size_t name_len = ffi_array_length(args[5].integer);

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage scope name cannot be empty");

   // Rename the scope
   LOCAL_TEXT_BUF tb = tb_new();
   sanitise_name(tb, name_bytes, name_len);
   ident_t name_id = ident_new(tb_get(tb));

   if (s->items.count > 0)
      jit_msg(NULL, DIAG_FATAL, "cannot change name of cover scope after "
              "items are created");

   s->name = name_id;
   s->block_name = name_id;

   ident_t prefix = ident_runtil(s->hier,'.');
   s->hier = ident_prefix(prefix, name_id, '.');
}

DLLEXPORT
void _nvc_add_cover_item(jit_scalar_t *args)
{
   cover_scope_t *s = *(cover_scope_t **)args[2].pointer;
   int32_t *index_ptr = args[3].pointer;
   const char *name_bytes = args[4].pointer;
   size_t name_len = ffi_array_length(args[6].integer);

   *index_ptr = -1;

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage item name cannot be empty");

   if (s == NULL || !s->emit)
      return;

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *data = get_coverage(m);
   if (data == NULL || !cover_enabled(data, COVER_MASK_FUNCTIONAL))
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, s->hier);
   tb_append(tb, '.');
   sanitise_name(tb, name_bytes, name_len);

   const size_t pfxlen = tb_len(tb);
   for (int i = 0, dup = 0; i < s->items.count; i++) {
      if (icmp(s->items.items[i].hier, tb_get(tb))) {
         tb_trim(tb, pfxlen);
         tb_printf(tb, "#%d", ++dup);
      }
   }

   cover_item_t *item = cover_add_items_for(data, s, NULL, COV_ITEM_FUNCTIONAL);
   assert(item != NULL);   // Preconditions checked above

   item->hier = ident_new(tb_get(tb));
   item->loc = s->loc;   // XXX: keeps report from crashing but location
                         //      does not make sense here

   // Name remembered at the time of cover point creation in its scope
   item->func_name = s->block_name;

   item->source = COV_SRC_USER_COVER;
   item->atleast = args[7].integer;

   item->flags = COV_FLAG_USER_DEFINED;
   if (args[8].integer > 0)
      item->flags |= (COV_FLAG_EXCLUDED | COV_FLAG_EXCLUDED_USER);

   item->n_ranges = ffi_array_length(args[11].integer);
   item->ranges = xcalloc_array(item->n_ranges, sizeof(cover_range_t));

   int32_t *ptr = (int32_t *)args[9].pointer;

   for (int i = 0; i < item->n_ranges; i++) {
      item->ranges[i].min = *ptr++;
      item->ranges[i].max = *ptr++;
   }

   *index_ptr = item - s->items.items;

   cover_item_t *first = AREF(s->items, 0);
   first->consecutive = s->items.count;
}

DLLEXPORT
void _nvc_increment_cover_item(jit_scalar_t *args)
{
   cover_scope_t *s = *(cover_scope_t **)args[2].pointer;
   int32_t index = args[3].integer;

   if (s == NULL || !s->emit)
      return;

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *data = get_coverage(m);
   if (data == NULL || !cover_enabled(data, COVER_MASK_FUNCTIONAL))
      return;

   if (index < 0 || index >= s->items.count)
      jit_msg(NULL, DIAG_FATAL, "cover item index %d out of range", index);

   int32_t *counter = get_cover_counter(m, s->items.items[index].tag);
   increment_counter(counter);
}
