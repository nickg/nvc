//
//  Copyright (C) 2013-2026  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-priv.h"
#include "ident.h"
#include "jit/jit-exits.h"
#include "rt/model.h"
#include "rt/structs.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef struct {
   int32_t   *counters;
   int32_t    offset;
   uint32_t   count;
} rt_toggle_data_t;

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

///////////////////////////////////////////////////////////////////////////////
// Toggle coverage
///////////////////////////////////////////////////////////////////////////////

typedef void (*toggle_check_fn_t)(uint8_t, uint8_t, int32_t *, int32_t *);

__attribute__((always_inline))
static inline void increment_counter(int32_t *ptr)
{
   *ptr = saturate_add(*ptr, 1);
}

__attribute__((always_inline))
static inline void cover_toggle_check_0_1(uint8_t old, uint8_t new,
                                          int32_t *toggle_01,
                                          int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);
}

__attribute__((always_inline))
static inline void cover_toggle_check_0_1_u(uint8_t old, uint8_t new,
                                            int32_t *toggle_01,
                                            int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);

   else if ((old == _U || old == _X) && new == _1)
      increment_counter(toggle_01);
   else if ((old == _U || old == _X) && new == _0)
      increment_counter(toggle_10);
}

__attribute__((always_inline))
static inline void cover_toggle_check_0_1_z(uint8_t old, uint8_t new,
                                            int32_t *toggle_01,
                                            int32_t *toggle_10)
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

__attribute__((always_inline))
static inline void cover_toggle_check_0_1_u_z(uint8_t old, uint8_t new,
                                              int32_t *toggle_01,
                                              int32_t *toggle_10)
{
   if (old == _0 && new == _1)
      increment_counter(toggle_01);
   else if (old == _1 && new == _0)
      increment_counter(toggle_10);

   else if ((old == _U || old == _X) && new == _1)
      increment_counter(toggle_01);
   else if ((old == _U || old == _X) && new == _0)
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

__attribute__((always_inline))
static inline void cover_toggle_generic(rt_signal_t *s, rt_toggle_data_t *td,
                                        toggle_check_fn_t fn)
{
   assert(s->nexus.size == 1);

   const void *cur = signal_value(s) + td->offset;
   const void *last = signal_last_value(s) + td->offset;

   // Callback is optimized for performance
   // Check only group of 8 bytes that do have change of signal value
   // Optimize for assumption that most bits don't change in large signals
   uint32_t batches = ((td->count - 1) / sizeof(uint64_t)) + 1;
   for (int i = 0; i < batches; i++) {
      int batch_size = sizeof(uint64_t);
      if (i < batches - 1) {
         const void *batch_new = cur + i * sizeof(uint64_t);
         const void *batch_old = last + i * sizeof(uint64_t);
         if (memcmp(batch_new, batch_old, sizeof(uint64_t)) == 0)
            continue;
      }
      else
         batch_size = ((td->count - 1) % sizeof(uint64_t)) + 1;

      int32_t low = i * sizeof(uint64_t);
      int32_t high = low + batch_size;
      int32_t *toggle_01 = td->counters + low * 2;
      int32_t *toggle_10 = toggle_01 + 1;
      for (int j = low; j < high; j++) {
         uint8_t new = ((const uint8_t *)cur)[j];
         uint8_t old = ((const uint8_t *)last)[j];
         if (new != old)
            (*fn)(old, new, toggle_01, toggle_10);
         toggle_01 += 2;
         toggle_10 += 2;
      }
   }
}

static void cover_toggle_cb_0_1(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                                void *user)
{
   cover_toggle_generic(s, user, cover_toggle_check_0_1);
}

static void cover_toggle_cb_0_1_u(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                                  void *user)
{
   cover_toggle_generic(s, user, cover_toggle_check_0_1_u);
}

static void cover_toggle_cb_0_1_z(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                                  void *user)
{
   cover_toggle_generic(s, user, cover_toggle_check_0_1_z);
}

static void cover_toggle_cb_0_1_u_z(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                                    void *user)
{
   cover_toggle_generic(s, user, cover_toggle_check_0_1_u_z);
}

static bool is_constant_input(rt_signal_t *s)
{
   tree_t decl = s->where;
   tree_kind_t kind = tree_kind(decl);

   if (kind == T_FIELD_DECL) {
      rt_scope_t *sc = s->parent;
      while (is_signal_scope(sc))
         sc = sc->parent;
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

void x_cover_setup_toggle_cb(sig_shared_t *ss, uint32_t offset, int32_t count,
                             int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();

   cover_data_t *data = get_coverage(m);
   if (data == NULL)
      return;

   int32_t *counters = cover_get_counters(data, get_active_scope(m)->name);
   if (counters == NULL)
      return;

   cover_mask_t op_mask = cover_get_u32(data, COVER_NULL_OBJ, COV_ATTR_MASK, 0);

   if (is_constant_input(s)) {
      int32_t *toggle_01 = counters + tag;
      int32_t *toggle_10 = toggle_01 + 1;

      // Each std_logic bit encoded as single byte. There are two run-time
      // counters for each std_logic bit
      for (int i = 0; i < count; i++) {
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

   rt_toggle_data_t *td = xcalloc(sizeof(rt_toggle_data_t));
   td->counters = counters + tag;
   td->offset   = offset;
   td->count    = count;

   rt_watch_t *w = watch_new(m, fn, td, WATCH_EVENT, 1);
   model_set_event_cb(m, s, offset, count, w);
}

///////////////////////////////////////////////////////////////////////////////
// FSM state coverage
///////////////////////////////////////////////////////////////////////////////

#define READ_STATE(type) offset = *((type *)signal_value(s));

static void cover_state_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                           void *user)
{
   // I-th enum literal is encoded in i-th tag from first tag, that corresponds
   // to enum value.
   int size = signal_size(s);
   int32_t offset = 0;
   FOR_ALL_SIZES(size, READ_STATE);

   int32_t *counters = user;
   increment_counter(counters + offset);
}

void x_cover_setup_state_cb(sig_shared_t *ss, int64_t low, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();

   cover_data_t *data = get_coverage(m);
   if (data == NULL)
      return;

   int32_t *counters = cover_get_counters(data, get_active_scope(m)->name);
   if (counters == NULL)
      return;

   // TYPE'left is a default value of enum type that does not
   // cause an event. First tag needs to be flagged as covered manually.
   *(counters + tag) = 1;

   rt_watch_t *w = watch_new(m, cover_state_cb, counters + tag - low,
                             WATCH_EVENT, 1);
   model_set_event_cb(m, s, 0, signal_width(s), w);
}

///////////////////////////////////////////////////////////////////////////////
// Run-time API
///////////////////////////////////////////////////////////////////////////////

typedef struct {
   ident_t      name;
   cover_obj_t  scope;
   int32_t     *counters;
} user_scope_t;

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
   user_scope_t **ptr = args[2].pointer;
   const char *name_bytes = args[3].pointer;
   size_t name_len = ffi_array_length(args[5].integer);

   *ptr = NULL;

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage scope name cannot be empty");

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *db = get_coverage(m);
   if (db == NULL || !cover_enabled(db, COVER_MASK_FUNCTIONAL))
      return;

   rt_scope_t *inst = get_active_scope(m);
   assert(inst->kind == SCOPE_INSTANCE);

   cover_obj_t parent = cover_get_scope(db, inst->name);
   if (cover_is_null(parent))
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   sanitise_name(tb, name_bytes, name_len);

   const size_t pfxlen = tb_len(tb);
   const int nchildren = cover_count(db, parent, COV_REL_CHILDREN);
   for (int i = 0, dup = 0; i < nchildren; i++) {
      cover_obj_t child = cover_at(db, parent, COV_REL_CHILDREN, i);
      ident_t name = cover_get_ident(db, child, COV_ATTR_NAME);

      if (icmp(name, tb_get(tb))) {
         tb_trim(tb, pfxlen);
         tb_printf(tb, "#%d", ++dup);
      }
   }

   ident_t suffix = ident_new(tb_get(tb));
   ident_t name = ident_prefix(inst->name, suffix, '.');

   user_scope_t *us = jit_mspace_alloc(sizeof(user_scope_t));
   us->counters = NULL;
   us->name     = name;
   us->scope    = cover_scope_new(db, parent, CSCOPE_USER, suffix,
                                  *tree_loc(inst->where));

   *ptr = us;
}

DLLEXPORT
void _nvc_set_cover_scope_name(jit_scalar_t *args)
{
   user_scope_t *us = *(user_scope_t **)args[2].pointer;
   const char *name_bytes = args[3].pointer;
   size_t name_len = ffi_array_length(args[5].integer);

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage scope name cannot be empty");

   if (us == NULL)
      return;

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *db = get_coverage(m);

   LOCAL_TEXT_BUF tb = tb_new();
   sanitise_name(tb, name_bytes, name_len);

   ident_t name_id = ident_new(tb_get(tb));

   if (cover_count(db, us->scope, COV_REL_ITEMS) > 0)
      jit_msg(NULL, DIAG_FATAL, "cannot change name of cover scope after "
              "items are created");

   cover_put_ident(db, us->scope, COV_ATTR_NAME, name_id);

   ident_t hier = cover_get_ident(db, us->scope, COV_ATTR_HIER);
   ident_t prefix = ident_runtil(hier, '.');
   cover_put_ident(db, us->scope, COV_ATTR_HIER,
                   ident_prefix(prefix, name_id, '.'));
}

DLLEXPORT
void _nvc_add_cover_item(jit_scalar_t *args)
{
   user_scope_t *us = *(user_scope_t **)args[2].pointer;
   int32_t *index_ptr = args[3].pointer;
   const char *name_bytes = args[4].pointer;
   size_t name_len = ffi_array_length(args[6].integer);

   *index_ptr = -1;

   if (name_len == 0)
      jit_msg(NULL, DIAG_FATAL, "coverage item name cannot be empty");

   if (us == NULL)
      return;

   assert(us->counters == NULL);   // TODO: add test

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *db = get_coverage(m);
   if (db == NULL || !cover_enabled(db, COVER_MASK_FUNCTIONAL))
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, cover_get_ident(db, us->scope, COV_ATTR_HIER));
   tb_append(tb, '.');
   sanitise_name(tb, name_bytes, name_len);

   const size_t pfxlen = tb_len(tb);
   const int nitems = cover_count(db, us->scope, COV_REL_ITEMS);
   for (int i = 0, dup = 0; i < nitems; i++) {
      cover_obj_t item = cover_at(db, us->scope, COV_REL_ITEMS, i);
      cover_obj_t bin0 = cover_at(db, item, COV_REL_BINS, 0);

      ident_t hier = cover_get_ident(db, bin0, COV_ATTR_HIER);
      if (icmp(hier, tb_get(tb))) {
         tb_trim(tb, pfxlen);
         tb_printf(tb, "#%d", ++dup);
      }
   }

   // XXX: keeps report from crashing but location does not make sense here
   loc_t scope_loc = cover_get_loc(db, us->scope, COV_ATTR_LOC);

   cover_obj_t item = cover_item_new(db, us->scope, COV_ITEM_FUNCTIONAL,
                                     scope_loc, 1);
   assert(!cover_is_null(item));   // Preconditions checked above

   cover_obj_t bin0 = cover_at(db, item, COV_REL_BINS, 0);
   cover_put_ident(db, bin0, COV_ATTR_HIER, ident_new(tb_get(tb)));

   // Name remembered at the time of cover point creation in its scope
   ident_t scope_name = cover_get_ident(db, us->scope, COV_ATTR_NAME);
   cover_put_ident(db, item, COV_ATTR_FUNC_NAME, scope_name);

   cover_put_u32(db, item, COV_ATTR_SOURCE, COV_SRC_USER_COVER);

   const unsigned atleast = args[7].integer;
   cover_put_u32(db, item, COV_ATTR_ATLEAST, atleast);

   cover_flags_t flags = COV_FLAG_USER_DEFINED;
   if (atleast == 0)
      flags |= (COV_FLAG_EXCLUDED | COV_FLAG_EXCLUDED_USER);

   cover_put_flags(db, bin0, flags);

   const int n_ranges = ffi_array_length(args[10].integer);
   cover_add_ranges(db, bin0, n_ranges);

   int64_t *ptr = (int64_t *)args[8].pointer;

   for (int i = 0; i < n_ranges; i++) {
      cover_obj_t r = cover_at(db, bin0, COV_REL_RANGES, i);
      cover_put_i64(db, r, COV_ATTR_MIN, *ptr++);
      cover_put_i64(db, r, COV_ATTR_MAX, *ptr++);
   }

   *index_ptr = nitems;
}

DLLEXPORT
void _nvc_increment_cover_item(jit_scalar_t *args)
{
   user_scope_t *us = *(user_scope_t **)args[2].pointer;
   int32_t index = args[3].integer;

   if (us == NULL)
      return;

   rt_model_t *m = get_model_or_null();
   if (m == NULL)
      return;

   cover_data_t *db = get_coverage(m);
   if (db == NULL || !cover_enabled(db, COVER_MASK_FUNCTIONAL))
      return;

   if (index < 0)
      jit_msg(NULL, DIAG_FATAL, "cover item handle not initialised");

   cover_obj_t item = cover_at(db, us->scope, COV_REL_ITEMS, index);
   if (cover_is_null(item))
      jit_msg(NULL, DIAG_FATAL, "cover item index %d out of range", index);

   if (us->counters == NULL)
      us->counters = cover_get_counters(db, us->name);

   assert(cover_count(db, item, COV_REL_BINS) == 1);

   cover_obj_t bin0 = cover_at(db, item, COV_REL_BINS, 0);
   cover_merge_bin(db, bin0, 1);
}
