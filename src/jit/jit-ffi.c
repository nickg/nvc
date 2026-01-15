//
//  Copyright (C) 2021-2024  Nick Gasson
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
#include "jit/jit-ffi.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "option.h"
#include "thread.h"
#include "type.h"
#include "vhpi/vhpi-model.h"
#include "vlog/vlog-node.h"
#include "vpi/vpi-model.h"

#include <assert.h>
#include <ctype.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

typedef enum {
   GHDL_ARG_DROP,
   GHDL_ARG_PASS,
   GHDL_ARG_LENGTH,
} ghdl_arg_t;

typedef struct {
   ffi_cif    cif;
   void      *ptr;
   unsigned   nvhdl;
   unsigned   nforeign;
   ghdl_arg_t args[0];
} ghdl_ffi_t;

typedef struct _jit_dll {
   jit_dll_t *next;
   char      *path;
#ifdef __MINGW32__
   HMODULE    handle;
#else
   void      *handle;
#endif
} jit_dll_t;

static jit_dll_t *dlls;

ffi_uarray_t ffi_wrap(void *ptr, int64_t left, int64_t right)
{
   // Assumes ascending range
   const int64_t length = right < left ? 0 : right - left + 1;

   ffi_uarray_t u = {
      .ptr = ptr,
      .dims = { [0] = { .left = left, .length = length } }
   };
   return u;
}

void ffi_return_string(const char *str, jit_scalar_t *args, tlab_t *tlab)
{
   const size_t len = strlen(str);

   void *mem = tlab_alloc(tlab, len);
   memcpy(mem, str, len);

   args[0].pointer = mem;
   args[1].integer = 1;
   args[2].integer = len;
}

bool ffi_is_integral(ffi_type_t type)
{
   return type == FFI_INT8 || type == FFI_INT16 || type == FFI_INT32
      || type == FFI_INT64 || type == FFI_UINT8 || type == FFI_UINT16
      || type == FFI_UINT32;
}

static jit_dll_t *ffi_load_exe(void)
{
   if (dlls != NULL)
      return dlls;  // First entry is always the executable

#ifdef __MINGW32__
   HMODULE libc_handle = GetModuleHandle("ucrtbase.dll");
   if (libc_handle == NULL && !(libc_handle = GetModuleHandle("msvcrt.dll")))
      fatal("failed to get handle to ucrtbase.dll or msvcrt.dll");

   char *libc_path = xmalloc(MAX_PATH);
   if (!GetModuleFileName(libc_handle, libc_path, MAX_PATH))
      fatal("failed to get C runtime library path: %s", last_os_error());

   DEBUG_ONLY(debugf("C runtime library: %s", libc_path));

   jit_dll_t *libc = xcalloc(sizeof(jit_dll_t));
   libc->next   = dlls;
   libc->handle = libc_handle;
   libc->path   = libc_path;

   dlls = libc;

   HMODULE exe_handle = GetModuleHandle(NULL);
   if (exe_handle == NULL)
      fatal("failed to get handle to main process");
#else
   void *exe_handle = dlopen(NULL, RTLD_NOW);
   if (exe_handle == NULL)
      fatal("%s", dlerror());
#endif

   LOCAL_TEXT_BUF tb = tb_new();
   get_exe_path(tb);

   jit_dll_t *dll = xcalloc(sizeof(jit_dll_t));
   dll->next   = dlls;
   dll->handle = exe_handle;
   dll->path   = tb_claim(tb);

   return (dlls = dll);
}

jit_dll_t *ffi_load_dll(const char *path)
{
   if (path == NULL)
      return ffi_load_exe();

   char *abs = realpath(path, NULL);
   if (abs == NULL)
      fatal_errno("%s", path);

   for (jit_dll_t *it = dlls; it; it = it->next) {
      if (strcmp(it->path, abs) == 0) {
         free(abs);
         return it;
      }
   }

   if (opt_get_verbose(OPT_JIT_VERBOSE, NULL))
      debugf("loading shared library %s", path);

#ifdef __MINGW32__
   HMODULE handle = LoadLibrary(abs);
   if (handle == NULL)
      fatal_errno("failed to load %s", abs);
#else
   void *handle = dlopen(abs, RTLD_LAZY);
   if (handle == NULL)
      fatal("%s", dlerror());
#endif

   if (dlls == NULL)
      ffi_load_exe();  // First time initialisation

   jit_dll_t *dll = xcalloc(sizeof(jit_dll_t));
   dll->handle = handle;
   dll->path   = abs;

   jit_dll_t **where;
   for (where = &dlls; *where != NULL; where = &((*where)->next));

   return (*where = dll);
}

void ffi_unload_dll(jit_dll_t *dll)
{
#ifdef __MINGW32__
   FreeLibrary(dll->handle);
#else
   dlclose(dll->handle);
#endif

   free(dll->path);

   jit_dll_t **prev;
   for (prev = &dlls; *prev != dll; prev = &((*prev)->next))
      ;

   *prev = dll->next;
   free(dll);
}

void *ffi_find_symbol(jit_dll_t *dll, const char *name)
{
   if (name == NULL)
      return NULL;
   else if (dll == NULL) {
      for (jit_dll_t *it = dlls; it; it = it->next) {
         void *p = ffi_find_symbol(it, name);
         if (p != NULL)
            return p;
      }

      return NULL;
   }
   else {
#ifdef __MINGW32__
      return (void *)(uintptr_t)GetProcAddress(dll->handle, name);
#else
      return dlsym(dll->handle, name);
#endif
   }
}

ffi_spec_t ffi_spec_new(const ffi_type_t *types, size_t count)
{
   assert(count > 0);

#ifdef DEBUG
   for (int i = 0; i < count; i++)
      assert(isalnum(types[i]));
#endif

   ffi_spec_t spec = {};
   if (count < 7) {
      memcpy(spec.embed, types, count);
      spec.count = count;
   }
   else
      spec.ext = types;

   return spec;
}

static void jit_internal_entry(jit_func_t *f, jit_anchor_t *caller,
                               jit_scalar_t *args, tlab_t *tlab)
{
   jit_thread_local_t *thread = jit_attach_thread(caller);

   ffi_internal_t fn = *jit_get_privdata_ptr(f->jit, f);
   if (unlikely(fn == NULL)) {
      // JIT has been reset, need to elaborate again
      f->entry = jit_interp;
      jit_interp(f, caller, args, tlab);
   }
   else
      (*fn)(args, tlab);

   thread->anchor = NULL;
}

static void jit_vhpi_entry(jit_func_t *f, jit_anchor_t *caller,
                           jit_scalar_t *args, tlab_t *tlab)
{
   jit_thread_local_t *thread = jit_attach_thread(caller);

   vhpiHandleT handle = *jit_get_privdata_ptr(f->jit, f);
   if (unlikely(handle == NULL)) {
      // JIT has been reset, need to elaborate again
      f->entry = jit_interp;
      jit_interp(f, caller, args, tlab);
   }
   else
      vhpi_call_foreign(handle, args, tlab);

   thread->anchor = NULL;
}

static void jit_ghdl_entry(jit_func_t *f, jit_anchor_t *caller,
                           jit_scalar_t *args, tlab_t *tlab)
{
   jit_thread_local_t *thread = jit_attach_thread(caller);

   ghdl_ffi_t *gffi = *jit_get_privdata_ptr(f->jit, f);
   if (unlikely(gffi == NULL)) {
      // JIT has been reset, need to elaborate again
      f->entry = jit_interp;
      jit_interp(f, caller, args, tlab);
   }
   else {
      void *aptrs[gffi->nforeign];
      int opos = 0;
      for (int ipos = 0; ipos < gffi->nvhdl; ipos++) {
         switch (gffi->args[ipos]) {
         case GHDL_ARG_DROP:
            break;
         case GHDL_ARG_PASS:
            aptrs[opos++] = &(args[ipos].integer);
            break;
         case GHDL_ARG_LENGTH:
            args[ipos].integer = ffi_array_length(args[ipos].integer);
            aptrs[opos++] = &(args[ipos].integer);
            break;
         }
      }

      intmax_t result = 0;
      ffi_call(&(gffi->cif), gffi->ptr, &result, aptrs);

      args[0].integer = result;
   }

   thread->anchor = NULL;
}

static ffi_type *ghdl_ffi_result_type(type_t type)
{
   type_t base = type_base_recur(type);
   switch (type_kind(base)) {
   case T_INTEGER:
   case T_ENUM:
   case T_PHYSICAL:
      {
         switch (type_byte_width(base)) {
         case 8: return &ffi_type_sint64;
         case 4: return &ffi_type_sint32;
         case 2: return &ffi_type_sint16;
         case 1: return &ffi_type_sint8;
         }
      }
      break;
   case T_REAL:
      return &ffi_type_double;
   default:
      break;
   }

   jit_msg(NULL, DIAG_FATAL, "cannot return type %s using GHDL "
           "VHPIDIRECT calling convention", type_pp(type));
   return &ffi_type_void;
}

static void ghdl_ffi_add_arg(ghdl_ffi_t *gffi, ffi_type **types, tree_t p)
{
   type_t type = tree_type(p), base = type_base_recur(type);
   switch (type_kind(base)) {
   case T_INTEGER:
   case T_ENUM:
   case T_PHYSICAL:
      {
         gffi->args[gffi->nvhdl++] = GHDL_ARG_PASS;

         if (tree_subkind(p) == PORT_IN) {
            switch (type_byte_width(base)) {
            case 8: types[gffi->nforeign++] = &ffi_type_sint64; break;
            case 4: types[gffi->nforeign++] = &ffi_type_sint32; break;
            case 2: types[gffi->nforeign++] = &ffi_type_sint16; break;
            case 1: types[gffi->nforeign++] = &ffi_type_sint8; break;
            }
         }
         else
            types[gffi->nforeign++] = &ffi_type_pointer;

         return;
      }
   case T_REAL:
      gffi->args[gffi->nvhdl++] = GHDL_ARG_PASS;
      types[gffi->nforeign++] = &ffi_type_double;
      return;
   case T_ACCESS:  // TOOD: deprecate this?
   case T_RECORD:
      gffi->args[gffi->nvhdl++] = GHDL_ARG_PASS;
      types[gffi->nforeign++] = &ffi_type_pointer;
      return;
   case T_ARRAY:
      if (dimension_of(type) > 1)
         break;
      else if (type_is_unconstrained(type)) {
         gffi->args[gffi->nvhdl++] = GHDL_ARG_PASS;    // Data pointer
         types[gffi->nforeign++] = &ffi_type_pointer;

         gffi->args[gffi->nvhdl++] = GHDL_ARG_DROP;    // Left index

         gffi->args[gffi->nvhdl++] = GHDL_ARG_LENGTH;  // Array length
         types[gffi->nforeign++] = &ffi_type_sint64;

         return;
      }
      else {
         gffi->args[gffi->nvhdl++] = GHDL_ARG_PASS;
         types[gffi->nforeign++] = &ffi_type_pointer;
         return;
      }
   default:
      break;
   }

   jit_msg(NULL, DIAG_FATAL, "cannot pass type %s using GHDL "
           "VHPIDIRECT calling convention", type_pp(type));
}

static void *ffi_prepare_ghdl(tree_t decl, const char *symbol)
{
   assert(tree_kind(decl) == T_ATTR_SPEC);

   tree_t sub = tree_ref(decl);
   const int nports = tree_ports(sub);

   const size_t ghdl_ffi_sz =
      sizeof(ghdl_ffi_t) + (2 + nports*3) * sizeof(ghdl_arg_t);
   ghdl_ffi_t *gffi =
      jit_mspace_alloc(ghdl_ffi_sz + nports * 2 * sizeof(ffi_type *));
   ffi_type **types = (void *)gffi + ghdl_ffi_sz;

   ffi_type *ret;
   type_t type = tree_type(sub);
   if (type_has_result(type))
      ret = ghdl_ffi_result_type(type_result(type));
   else
      ret = &ffi_type_void;

   if ((gffi->ptr = ffi_find_symbol(NULL, symbol)) == NULL)
      jit_msg(NULL, DIAG_FATAL, "foreign function %s not found", symbol);

   gffi->nvhdl = gffi->nforeign = 0;

   gffi->args[gffi->nvhdl++] = GHDL_ARG_DROP;   // Drop context argument

   if (!type_has_result(type))
      gffi->args[gffi->nvhdl++] = GHDL_ARG_DROP;   // Drop state argument

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(sub, i);
      if (tree_class(p) == C_SIGNAL)
         jit_msg(tree_loc(p), DIAG_FATAL, "SIGNAL parameters are not "
                 "supported for VHPIDIRECT subprograms using the GHDL "
                 "calling convention");

      ghdl_ffi_add_arg(gffi, types, p);
   }

   assert(gffi->nvhdl <= 2 + nports * 3);
   assert(gffi->nforeign <= nports * 2);

   if (ffi_prep_cif(&(gffi->cif), FFI_DEFAULT_ABI, gffi->nforeign,
                    ret, types) != FFI_OK)
      fatal("ffi_prep_cif failed for %s", type_pp(type));

   return gffi;
}

void jit_bind_foreign(jit_func_t *f, const uint8_t *spec, size_t length,
                      tree_t where)
{
   char *tmp LOCAL = null_terminate(spec, length), *p = strtok(tmp, " ");
   if (strcmp(p, "VHPIDIRECT") == 0 || strcmp(p, "GHDL") == 0) {
      p = strtok(NULL, " ");
      if (p != NULL) {
         // The object library specifier is silently ignored
         char *p2 = strtok(NULL, " ");
         if (p2 != NULL) p = p2;
      }

      *jit_get_privdata_ptr(f->jit, f) = ffi_prepare_ghdl(where, p);

      assert(f->entry != jit_ghdl_entry);   // Should only be called once
      f->entry = jit_ghdl_entry;
   }
   else if (strcmp(p, "VHPI") == 0) {
      const char *obj_lib = strtok(NULL, " ");
      if (obj_lib == NULL)
         jit_msg(NULL, DIAG_FATAL, "missing object library name");

      const char *model = strtok(NULL, " ");
      if (model == NULL)
         jit_msg(NULL, DIAG_FATAL, "missing model name");

      vhpiHandleT handle = vhpi_bind_foreign(obj_lib, model, where);
      if (handle == NULL)
         jit_msg(NULL, DIAG_FATAL, "foreign subprogram %s/%s not registered",
                 obj_lib, model);

      *jit_get_privdata_ptr(f->jit, f) = handle;

      assert(f->entry != jit_vhpi_entry);   // Should only be called once
      f->entry = jit_vhpi_entry;
   }
   else if (strcmp(p, "INTERNAL") == 0) {
      p = strtok(NULL, " ");

      jit_dll_t *exe = ffi_load_exe();
      ffi_internal_t fn = ffi_find_symbol(exe, p);
      if (fn == NULL)
         jit_msg(NULL, DIAG_FATAL, "missing internal symbol %s", p);

      *jit_get_privdata_ptr(f->jit, f) = fn;

      assert(f->entry != jit_internal_entry);   // Should only be called once
      f->entry = jit_internal_entry;
   }
   else {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_printf(d, "failed to parse FOREIGN attribute");
      diag_hint(d, NULL, "expecting specification to start with "
                "VHPIDIRECT or GHDL");
      diag_emit(d);

      jit_abort_with_status(1);
   }
}

void jit_do_syscall(vlog_node_t where, jit_anchor_t *caller, jit_scalar_t *args,
                    tlab_t *tlab)
{
   ident_t name = vlog_ident(where);
   vpiHandle handle = vpi_bind_foreign(name, where);
   if (handle == NULL)
      jit_msg(NULL, DIAG_FATAL, "system task %s not registered", istr(name));

   vpi_call_foreign(handle, args, tlab);
}
