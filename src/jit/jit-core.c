//
//  Copyright (C) 2022  Nick Gasson
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
#include "lib.h"
#include "jit/jit-priv.h"
#include "opt.h"
#include "rt/mspace.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

typedef A(jit_func_t *) func_array_t;

typedef struct _jit {
   func_array_t    funcs;
   hash_t         *index;
   mspace_t       *mspace;
   jit_lower_fn_t  lower_fn;
   void           *lower_ctx;
   hash_t         *layouts;
   bool            silent;
   unsigned        backedge;
} jit_t;

static void jit_oom_cb(mspace_t *m, size_t size)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "out of memory attempting to allocate %zu byte object", size);

   const int heapsize = opt_get_int(OPT_HEAP_SIZE);
   diag_hint(d, NULL, "the current heap size is %u bytes which you can "
             "increase with the $bold$-H$$ option, for example $bold$-H %um$$",
             heapsize, MAX(1, (heapsize * 2) / 1024 / 1024));

   diag_emit(d);
}

jit_t *jit_new(void)
{
   jit_t *j = xcalloc(sizeof(jit_t));
   j->index = hash_new(256);
   j->mspace = mspace_new(opt_get_int(OPT_HEAP_SIZE));

   mspace_set_oom_handler(j->mspace, jit_oom_cb);

   return j;
}

static void jit_free_func(jit_func_t *f)
{
   mptr_free(f->jit->mspace, &(f->privdata));
   free(f->irbuf);
   free(f->varoff);
   free(f->cpool);
   free(f);
}

void jit_free(jit_t *j)
{
   for (int i = 0; i < j->funcs.count; i++)
      jit_free_func(j->funcs.items[i]);
   ACLEAR(j->funcs);

   if (j->layouts != NULL) {
      hash_iter_t it = HASH_BEGIN;
      const void *key;
      void *value;
      while (hash_iter(j->layouts, &it, &key, &value))
         free(value);
      hash_free(j->layouts);
   }

   mspace_destroy(j->mspace);
   hash_free(j->index);
   free(j);
}

mspace_t *jit_get_mspace(jit_t *j)
{
   return j->mspace;
}

jit_handle_t jit_lazy_compile(jit_t *j, ident_t name)
{
   jit_func_t *f = hash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   vcode_unit_t vu = vcode_find_unit(name);

   if (vu == NULL) {
      if (opt_get_verbose(OPT_JIT_VERBOSE, NULL))
         debugf("loading vcode for %s", istr(name));

      tree_t unit = lib_get_qualified(name);
      if (unit != NULL && tree_kind(unit) == T_PACKAGE)
         (void)body_of(unit);  // Make sure body is loaded

      vu = vcode_find_unit(name);
   }

   ident_t alias = NULL;
   if (vu == NULL && j->lower_fn != NULL) {
      vcode_state_t state;
      vcode_state_save(&state);

      if ((vu = (*j->lower_fn)(name, j->lower_ctx))) {
         vcode_select_unit(vu);
         alias = vcode_unit_name();   // May have $thunk appended
      }

      vcode_state_restore(&state);
   }

   if (vu == NULL)
      return JIT_HANDLE_INVALID;

   assert(hash_get(j->index, vu) == NULL);

   f = xcalloc(sizeof(jit_func_t));

   f->name   = alias ?: name;
   f->unit   = vu;
   f->jit    = j;
   f->handle = j->funcs.count;

   hash_put(j->index, vu, f);
   hash_put(j->index, name, f);

   if (alias != NULL && alias != name)
      hash_put(j->index, alias, f);

   APUSH(j->funcs, f);
   return f->handle;
}

jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle)
{
   return AGET(j->funcs, handle);
}

jit_handle_t jit_compile(jit_t *j, ident_t name)
{
   jit_handle_t handle = jit_lazy_compile(j, name);

   jit_func_t *f = jit_get_func(j, handle);
   if (f->irbuf == NULL)
      jit_irgen(f);

   return handle;
}

void *jit_link(jit_t *j, jit_handle_t handle)
{
   if (handle == JIT_HANDLE_INVALID)
      return NULL;

   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata != MPTR_INVALID)
      return mptr_get(j->mspace, f->privdata);

   f->privdata = mptr_new(j->mspace, "privdata");

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(f->unit);
   const vunit_kind_t kind = vcode_unit_kind();
   if (kind != VCODE_UNIT_PACKAGE && kind != VCODE_UNIT_INSTANCE)
      fatal_trace("cannot link unit %s", istr(f->name));

   const loc_t *loc = vcode_unit_loc();

   jit_scalar_t args[JIT_MAX_ARGS] = { { .integer = 0 } };
   if (!jit_interp(f, args, 1, 0, NULL)) {
      error_at(loc, "failed to initialise %s", istr(f->name));
      args[0].pointer = NULL;
   }
   else if (args[0].pointer == NULL)
      fatal_trace("link %s returned NULL", istr(f->name));

   vcode_state_restore(&state);

   // Initialisation should save the context pointer
   assert(args[0].pointer == mptr_get(j->mspace, f->privdata));

   return args[0].pointer;
}

void *jit_get_privdata(jit_t *j, jit_func_t *f)
{
   if (f->privdata == MPTR_INVALID)
      f->privdata = mptr_new(j->mspace, "privdata");

   return mptr_get(j->mspace, f->privdata);
}

void jit_put_privdata(jit_t *j, jit_func_t *f, void *ptr)
{
   assert(f->privdata != MPTR_INVALID);
   mptr_put(j->mspace, f->privdata, ptr);
}

void *jit_get_frame_var(jit_t *j, jit_handle_t handle, uint32_t var)
{
   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata == MPTR_INVALID)
      fatal_trace("%s not linked", istr(f->name));

   assert(var < f->nvars);
   return (char *)mptr_get(j->mspace, f->privdata) + f->varoff[var];
}

static bool jit_try_vcall(jit_t *j, ident_t func, bool pcall, void *state,
                          void *context, jit_scalar_t *result,
                          const char *fmt, va_list ap)
{
   jit_scalar_t args[JIT_MAX_ARGS];

   int nargs = 0;
   if (pcall) args[nargs++].pointer = state;
   args[nargs++].pointer = context;

   for (; *fmt; fmt++) {
      if (nargs == JIT_MAX_ARGS)
         fatal_trace("too many arguments to function %s", istr(func));

      switch (*fmt) {
      case 'I':
         args[nargs++].integer = va_arg(ap, int64_t);
         break;
      case 'i':
         args[nargs++].integer = va_arg(ap, int32_t);
         break;
      case 'R':
         args[nargs++].real = va_arg(ap, double);
         break;
      case 'u':
         args[nargs++].pointer = va_arg(ap, void *);
         args[nargs++].integer = va_arg(ap, int32_t);  // Left
         args[nargs++].integer = va_arg(ap, int32_t);  // Length
         break;
      case 'p':
         args[nargs++].integer = (intptr_t)va_arg(ap, void *);
         break;
      default:
         fatal_trace("invalid character '%c' in jit_call format", *fmt);
      }
   }

   jit_handle_t handle = jit_compile(j, func);
   bool ok = jit_interp(jit_get_func(j, handle), args, nargs, 0, NULL);

   *result = args[0];
   return ok;
}

bool jit_try_call(jit_t *j, ident_t func, void *context, jit_scalar_t *result,
                  const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   bool ok = jit_try_vcall(j, func, false, NULL, context, result, fmt, ap);
   va_end(ap);
   return ok;
}

bool jit_try_pcall(jit_t *j, ident_t func, void *state, void *context,
                   const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   jit_scalar_t result;
   bool ok = jit_try_vcall(j, func, true, state, context, &result, fmt, ap);
   va_end(ap);
   return ok;
}

jit_scalar_t jit_call(jit_t *j, ident_t func, void *context,
                      const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   jit_scalar_t result;
   if (!jit_try_vcall(j, func, false, NULL, context, &result, fmt, ap))
      fatal_trace("call to %s failed", istr(func));

   va_end(ap);
   return result;
}

jit_scalar_t jit_pcall(jit_t *j, ident_t func, void *state, void *context,
                       const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   jit_scalar_t result;
   if (!jit_try_vcall(j, func, true, state, context, &result, fmt, ap))
      fatal_trace("call to %s failed", istr(func));

   va_end(ap);
   return result;
}

bool jit_call_thunk(jit_t *j, vcode_unit_t unit, jit_scalar_t *result)
{
   vcode_select_unit(unit);
   assert(vcode_unit_kind() == VCODE_UNIT_THUNK);

   jit_func_t *f = xcalloc(sizeof(jit_func_t));
   f->unit   = unit;
   f->jit    = j;
   f->handle = JIT_HANDLE_INVALID;

   jit_irgen(f);

   jit_scalar_t args[JIT_MAX_ARGS];
   bool ok = jit_interp(f, args, 0, j->backedge, NULL);

   jit_free_func(f);

   *result = args[0];
   return ok;
}

void jit_set_lower_fn(jit_t *j, jit_lower_fn_t fn, void *ctx)
{
   j->lower_fn = fn;
   j->lower_ctx = ctx;
}

const jit_layout_t *jit_layout(jit_t *j, type_t type)
{
   if (j->layouts == NULL)
      j->layouts = hash_new(256);

   jit_layout_t *l = hash_get(j->layouts, type);
   if (l != NULL)
      return l;

   if (type_is_integer(type) || type_is_physical(type) || type_is_enum(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      tree_t r = type_dim(type, 0);
      int64_t low, high;
      if (!folded_bounds(r, &low, &high))
         fatal_trace("type %s has unknown bounds", type_pp(type));

      const int bits = bits_for_range(low, high);
      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = ALIGN_UP(bits, 8) / 8;
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_real(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = sizeof(double);
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_array(type)) {
      const int ndims = dimension_of(type);

      if (type_is_unconstrained(type)) {
         l = xcalloc_flex(sizeof(jit_layout_t), 2, sizeof(layout_part_t));
         l->nparts = 2;
         l->size   = sizeof(void *) + ndims * 2 * sizeof(int32_t);
         l->align  = sizeof(void *);

         l->parts[0].offset = 0;
         l->parts[0].size   = sizeof(void *);
         l->parts[0].repeat = 1;
         l->parts[0].align  = l->parts[0].size;

         l->parts[1].offset = sizeof(void *);
         l->parts[1].size   = sizeof(int32_t);
         l->parts[1].repeat = ndims * 2;
         l->parts[1].align  = l->parts[1].size;
      }
      else {
         int length = 1;
         for (int i = 0; i < ndims; i++) {
            tree_t r = range_of(type, i);

            int64_t dlen;
            if (!folded_length(r, &dlen))
               fatal_at(tree_loc(r), "dimension %d of type %s is not static",
                        i, type_pp(type));

            length *= dlen;
         }

         const jit_layout_t *el = jit_layout(j, type_elem(type));
         assert(!type_is_array(type_elem(type)));

         l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
         l->nparts = 1;
         l->size   = length * el->size;
         l->align  = el->align;

         l->parts[0].offset = 0;
         l->parts[0].size   = el->size;
         l->parts[0].repeat = length;
         l->parts[0].align  = el->align;
      }
   }
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);

      l = xcalloc_flex(sizeof(jit_layout_t), nfields, sizeof(layout_part_t));
      l->nparts = nfields;

      int offset = 0;
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         const jit_layout_t *fl = jit_layout(j, ftype);

         offset = ALIGN_UP(offset, fl->align);

         l->parts[i].offset = offset;
         l->parts[i].size   = fl->size;
         l->parts[i].repeat = 1;
         l->parts[i].align  = fl->align;

         offset += fl->size;
      }

      l->size  = offset;
      l->align = sizeof(void *);  // Matches irgen_align_of
   }
   else
      fatal_trace("cannot get layout for %s", type_pp(type));

   hash_put(j->layouts, type, l);
   return l;
}

void jit_limit_backedges(jit_t *j, int limit)
{
   j->backedge = limit;
}

void jit_set_silent(jit_t *j, bool silent)
{
   j->silent = silent;
}

bool jit_show_errors(jit_t *j)
{
   return !j->silent;
}
