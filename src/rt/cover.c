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
// Runtime handling
///////////////////////////////////////////////////////////////////////////////

static inline void cover_toggle_check_0_1(uint8_t old, uint8_t new,
                                          int32_t *toggle_mask)
{
   if (old == _0 && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _1 && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_u(uint8_t old, uint8_t new,
                                        int32_t *toggle_mask)
{
   if (old == _U && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _U && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_z(uint8_t old, uint8_t new,
                                        int32_t *toggle_mask)
{
   if (old == _0 && new == _Z)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;
   if (old == _Z && new == _1)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_1;

   if (old == _1 && new == _Z)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
   if (old == _Z && new == _0)
      *toggle_mask |= COV_FLAG_TOGGLE_TO_0;
}

static inline void cover_toggle_check_0_1_u(uint8_t old, uint8_t new,
                                            int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_u(old, new, toggle_mask);
}

static inline void cover_toggle_check_0_1_z(uint8_t old, uint8_t new,
                                            int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_z(old, new, toggle_mask);
}

static inline void cover_toggle_check_0_1_u_z(uint8_t old, uint8_t new,
                                              int32_t *toggle_mask)
{
   cover_toggle_check_0_1(old, new, toggle_mask);
   cover_toggle_check_u(old, new, toggle_mask);
   cover_toggle_check_z(old, new, toggle_mask);
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

#define DEFINE_COVER_TOGGLE_CB(name, check_fnc)                               \
   static void name(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)  \
   {                                                                          \
      uint32_t s_size = s->shared.size;                                       \
      rt_model_t *m = get_model();                                            \
      const int32_t tag = (uintptr_t)user;                                    \
      int32_t *toggle_mask = get_cover_counter(m, tag);                       \
      COVER_TGL_CB_MSG(s)                                                     \
      for (int i = 0; i < s_size; i++) {                                      \
         uint8_t new = ((uint8_t*)signal_value(s))[i];                        \
         uint8_t old = ((uint8_t*)signal_last_value(s))[i];                   \
         check_fnc(old, new, toggle_mask);                                    \
         toggle_mask++;                                                       \
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
      int32_t *toggle_mask = get_cover_counter(m, tag);

      for (int i = 0; i < s->shared.size; i++) {
         // Remember constant driver in run-time data. Unreachable mask not
         // available at run-time.
         (*toggle_mask++) |= COV_FLAG_CONST_DRIVEN;
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

#define READ_STATE(type) offset = *((type *)signal_value(s));

static void cover_state_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w, void *user)
{
   // I-th enum literal is encoded in i-th tag from first tag, that corresponds to enum value.
   int size = signal_size(s);
   int32_t offset = 0;
   FOR_ALL_SIZES(size, READ_STATE);

   rt_model_t *m = get_model();
   int32_t *mask = get_cover_counter(m, ((uintptr_t)user) + offset);

   *mask |= COV_FLAG_STATE;
}

void x_cover_setup_state_cb(sig_shared_t *ss, int64_t low, int32_t tag)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();

   int32_t *mask = get_cover_counter(m, tag);

   // TYPE'left is a default value of enum type that does not
   // cause an event. First tag needs to be flagged as covered manually.
   *mask |= COV_FLAG_STATE;

   model_set_event_cb(m, s, cover_state_cb, (void *)(uintptr_t)(tag - low), false);
}
