//
//  Copyright (C) 2021-2022  Nick Gasson
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

#if defined __MINGW32__ || defined __CYGWIN__
#define WINVER 0x0A00
#define _WIN32_WINNT 0x0A00
#include <windows.h>
#include <DbgHelp.h>
#endif

#include "debug.h"
#include "array.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#if !defined __MINGW32__ && !defined __CYGWIN__
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <dlfcn.h>
#include <errno.h>
#include <unwind.h>
#endif  // !__MINGW32__ && !__CYGWIN__

#if defined HAVE_LIBDW
#include <elfutils/libdw.h>
#include <elfutils/libdwfl.h>
#include <dwarf.h>
#elif defined HAVE_LIBDWARF
#if defined HAVE_LIBDWARF_LIBDWARF_H
#include <libdwarf/libdwarf.h>
#include <libdwarf/dwarf.h>
#else  // HAVE_LIBDWARF_LIBDWARF_H
#include <libdwarf.h>
#include <dwarf.h>
#endif  // HAVE_LIBDWARF_LIBDWARF_H
#include <libelf.h>
#endif

#define MAX_TRACE_DEPTH   25

struct debug_info {
   A(debug_frame_t*) frames;
   unsigned          skip;
};

////////////////////////////////////////////////////////////////////////////////
// Utilities

typedef struct _di_lru_cache di_lru_cache_t;

struct _di_lru_cache {
   di_lru_cache_t *next;
   di_lru_cache_t *prev;
   debug_frame_t   frame;
};

#define DI_LRU_SIZE 256
STATIC_ASSERT(DI_LRU_SIZE > MAX_TRACE_DEPTH);

static di_lru_cache_t *lru_cache = NULL;

static void di_lru_reuse_frame(debug_frame_t *frame, uintptr_t pc)
{
   free((char *)frame->module);
   free((char *)frame->symbol);
   free((char *)frame->srcfile);

   for (debug_inline_t *it = frame->inlined; it != NULL; ) {
      debug_inline_t *next = it->next;
      free((char *)it->symbol);
      free((char *)it->srcfile);
      free(it);
      it = next;
   }

   memset(frame, '\0', sizeof(debug_frame_t));

   frame->pc = pc;
}

static bool di_lru_get(uintptr_t pc, debug_frame_t **pframe)
{
   unsigned size;
   di_lru_cache_t **it;
   for (it = &lru_cache, size = 0;
        *it != NULL && (*it)->frame.pc != pc;
        it = &((*it)->next), size++)
      ;

   if (*it != NULL) {
      di_lru_cache_t *tmp = *it;
      if ((*it)->next) (*it)->next->prev = (*it)->prev;
      *it = (*it)->next;
      tmp->next = lru_cache;
      tmp->prev = NULL;
      lru_cache->prev = tmp;
      lru_cache = tmp;
      *pframe = &(tmp->frame);
      return true;
   }
   else if (size < DI_LRU_SIZE) {
      di_lru_cache_t *new = xcalloc(sizeof(di_lru_cache_t));
      new->prev = NULL;
      new->next = lru_cache;
      if (lru_cache) lru_cache->prev = new;
      lru_cache = new;
      di_lru_reuse_frame(&(new->frame), pc);
      *pframe = &(new->frame);
      return false;
   }
   else {
      di_lru_cache_t *lru = container_of(it, di_lru_cache_t, next);
      lru->prev->next = NULL;
      lru->next = lru_cache;
      lru_cache->prev = lru;
      lru_cache = lru;
      di_lru_reuse_frame(&(lru->frame), pc);
      *pframe = &(lru->frame);
      return false;
   }
}

#if !defined HAVE_LIBDW && !defined HAVE_LIBDWARF
static void guess_vhdl_symbol(debug_frame_t *frame)
{
   // Use some dodgy heuristics to determine if this a VHDL symbol when
   // DWARF information is not available

   const char *slash = strrchr(frame->module, DIR_SEP[0]);
   char *file LOCAL = xstrdup(slash ? slash + 1 : frame->module);
   if (file[0] != '_')
      return;

#if !defined __MINGW32__ && !defined __CYGWIN__
   char *last_dot = strrchr(file, '.');
   if (last_dot == NULL || strcmp(last_dot + 1, DLL_EXT) != 0)
      return;

   *last_dot = '\0';
#endif

   if (frame->symbol == NULL)
      return;

   char *dup LOCAL = xstrdup(frame->symbol);

   char *dot1 = strchr(dup, '.');
   if (dot1 == NULL)
      return;

   char *dot2 = strchr(dot1 + 1, '.');
   if (dot2 == NULL)
      return;

   *dot2 = '\0';

   const size_t duplen = strlen(dup);
   if (strncmp(file + 1, dup, duplen) == 0 && file[duplen + 1] == '.') {
      frame->kind = FRAME_VHDL;
      frame->vhdl_unit = ident_new(file + 1);
   }
   else {
      frame->kind = FRAME_VHDL;
      frame->vhdl_unit = ident_new(dup);
   }
}
#endif  // !HAVE_LIBDW && !HAVE_LIBDWARF

////////////////////////////////////////////////////////////////////////////////
// Libdw backend

#if defined HAVE_LIBDW

static void libdw_fill_inlining(uintptr_t biased_ip, Dwarf_Die *fundie,
                                debug_frame_t *frame)
{
   Dwarf_Die child;
   if (dwarf_child(fundie, &child) == 0) {
      Dwarf_Die *iter = &child;
      do {
         if (dwarf_tag(iter) != DW_TAG_inlined_subroutine)
            continue;
         else if (!dwarf_haspc(iter, biased_ip))
            continue;

         debug_inline_t *inl = xcalloc(sizeof(debug_inline_t));
         inl->symbol = xstrdup(dwarf_diename(iter));

         Dwarf_Attribute attr;
         if (dwarf_attr(iter, DW_AT_abstract_origin, &attr) == NULL)
            continue;

         Dwarf_Die origin;
         if (dwarf_formref_die(&attr, &origin) == NULL)
            continue;

         const char *srcfile = dwarf_decl_file(iter);
         if (srcfile != NULL)
            inl->srcfile = xstrdup(srcfile);

         if (frame->kind == FRAME_VHDL) {
            Dwarf_Die *scopes;
            int n = dwarf_getscopes_die(&origin, &scopes);
            for (int i = 0; i < n; i++) {
               if (dwarf_tag(&(scopes[i])) == DW_TAG_module)
                  inl->vhdl_unit = ident_new(dwarf_diename(&(scopes[i])));
            }
         }

         Dwarf_Word call_lineno = 0;
         if (dwarf_attr(iter, DW_AT_call_line, &attr))
            dwarf_formudata(&attr, &call_lineno);

         Dwarf_Word call_colno = 0;
         if (dwarf_attr(iter, DW_AT_call_column, &attr))
            dwarf_formudata(&attr, &call_colno);

         if (frame->inlined) {
            inl->lineno = frame->inlined->lineno;
            inl->colno  = frame->inlined->colno;

            frame->inlined->lineno = call_lineno;
            frame->inlined->colno  = call_colno;
         }
         else {
            inl->lineno = frame->lineno;
            inl->colno  = frame->colno;

            frame->lineno = call_lineno;
            frame->colno  = call_colno;
         }

         inl->next = frame->inlined;
         frame->inlined = inl;

         libdw_fill_inlining(biased_ip, iter, frame);
      } while (dwarf_siblingof(iter, iter) == 0);
   }
}

static void libdw_fill_frame(uintptr_t ip, debug_frame_t *frame)
{
   static Dwfl *handle = NULL;
   static Dwfl_Module *home = NULL;

   if (handle == NULL) {
      static Dwfl_Callbacks callbacks = {
         .find_elf = dwfl_linux_proc_find_elf,
         .find_debuginfo = dwfl_standard_find_debuginfo,
         .debuginfo_path = NULL
      };

      if ((handle = dwfl_begin(&callbacks)) == NULL)
         fatal("failed to initialise dwfl");

      dwfl_report_begin(handle);
      if (dwfl_linux_proc_report(handle, getpid()) < 0)
         fatal("dwfl_linux_proc_report failed");
      dwfl_report_end(handle, NULL, NULL);

      home = dwfl_addrmodule(handle, (uintptr_t)libdw_fill_frame);
   }

   Dwfl_Module *mod = dwfl_addrmodule(handle, ip);
   frame->kind = (mod == home) ? FRAME_PROG : FRAME_LIB;
   frame->pc   = ip;

   const char *module_name = dwfl_module_info(mod, 0, 0, 0, 0, 0, 0, 0);
   const char *sym_name = dwfl_module_addrname(mod, ip);

   Dwarf_Addr mod_bias = 0;
   Dwarf_Die *cudie = dwfl_module_addrdie(mod, ip, &mod_bias);

   if (cudie == NULL) {
      // Clang does not emit aranges so search each CU
      cudie = dwfl_module_nextcu(mod, NULL, &mod_bias);
   }

   Dwarf_Die *fundie = NULL, *module = NULL, child, child1;
   do {
      if (dwarf_child(cudie, &child) != 0)
         continue;

      Dwarf_Die *iter = &child;
      do {
         switch (dwarf_tag(iter)) {
         case DW_TAG_inlined_subroutine:
         case DW_TAG_subprogram:
            if (dwarf_haspc(iter, ip - mod_bias)) {
               fundie = iter;
               goto found_die_with_ip;
            }
            break;
         case DW_TAG_module:
            if (dwarf_child(&child, &child1) == 0) {
               Dwarf_Die *iter1 = &child1;
               do {
                  switch (dwarf_tag(iter1)) {
                  case DW_TAG_inlined_subroutine:
                  case DW_TAG_subprogram:
                     if (dwarf_haspc(iter1, ip - mod_bias)) {
                        module = iter;
                        fundie = iter1;
                        goto found_die_with_ip;
                     }
                  }
               } while (dwarf_siblingof(iter1, iter1) == 0);
            }
            break;
         }
      } while (dwarf_siblingof(iter, iter) == 0);
   } while ((cudie = dwfl_module_nextcu(mod, cudie, &mod_bias)));

 found_die_with_ip:
   if (cudie != NULL) {
      Dwarf_Line *srcloc = dwarf_getsrc_die(cudie, ip - mod_bias);
      const char *srcfile = dwarf_linesrc(srcloc, 0, 0);

      dwarf_lineno(srcloc, (int *)&(frame->lineno));
      dwarf_linecol(srcloc, (int *)&(frame->colno));

      if (module != NULL) {
         // VHDL compilation units are wrapped in a DWARF module which
         // gives the unit name
         frame->kind = FRAME_VHDL;
         frame->vhdl_unit = ident_new(dwarf_diename(module));
      }

      if (srcfile != NULL)
         frame->srcfile = xstrdup(srcfile);

      if (fundie != NULL)
         libdw_fill_inlining(ip - mod_bias, fundie, frame);
   }

   if (sym_name != NULL)
      frame->symbol = xstrdup(sym_name);
   if (module_name != NULL)
      frame->module = xstrdup(module_name);
}

static _Unwind_Reason_Code libdw_frame_iter(struct _Unwind_Context* ctx,
                                            void *param)
{
   debug_info_t *di = param;

   if (di->skip > 0) {
      di->skip--;
      return _URC_NO_REASON;
   }

   int ip_before_instruction = 0;
   uintptr_t ip = _Unwind_GetIPInfo(ctx, &ip_before_instruction);

   if (ip == 0)
      return _URC_NO_REASON;
   else if (!ip_before_instruction)
      ip -= 1;

   debug_frame_t *frame;
   if (!di_lru_get(ip, &frame))
      libdw_fill_frame(ip, frame);

   APUSH(di->frames, frame);

   if (frame->symbol != NULL && strcmp(frame->symbol, "main") == 0)
      return _URC_NORMAL_STOP;
   else
      return _URC_NO_REASON;
}

__attribute__((always_inline))
static inline void debug_walk_frames(debug_info_t *di)
{
   di->skip = 1;
   _Unwind_Backtrace(libdw_frame_iter, di);
}

////////////////////////////////////////////////////////////////////////////////
// Libdwarf backend

#elif defined HAVE_LIBDWARF

typedef struct {
   Dwarf_Debug   debug;
   Dwarf_Arange *aranges;
   Dwarf_Signed  arange_count;
   int           fd;
} libdwarf_handle_t;

static bool libdwarf_die_has_pc(Dwarf_Die die, Dwarf_Addr pc)
{
   Dwarf_Addr low_pc = 0, high_pc = 0;

   if (dwarf_lowpc(die, &low_pc, NULL) != DW_DLV_OK)
      return false;

   Dwarf_Half form;
   enum Dwarf_Form_Class class;
   if (dwarf_highpc_b(die, &high_pc, &form, &class, NULL) == DW_DLV_OK) {
      if (class == DW_FORM_CLASS_CONSTANT)
         high_pc += low_pc;   // DWARF4
      return pc >= low_pc && pc < high_pc;
   }

   return false;
}

static libdwarf_handle_t *libdwarf_handle_for_file(const char *fname)
{
   static shash_t *hash = NULL;

   if (hash == NULL)
      hash = shash_new(64);

   libdwarf_handle_t *handle = shash_get(hash, fname);

   if (handle == (void *)-1)
      return NULL;
   else if (handle == NULL) {
      if (elf_version(EV_CURRENT) == EV_NONE)
         fatal("ELF library too old");

      int fd;
      if (strchr(fname, DIR_SEP[0]))
         fd = open(fname, O_RDONLY);
      else {
         char LOCAL *full = search_path(fname);
         fd = open(full, O_RDONLY);
      }

      if (fd == -1) {
         warnf("open: %s: %s", fname, strerror(errno));
         shash_put(hash, fname, (void *)-1);
         return NULL;
      }

      Dwarf_Debug debug;
      Dwarf_Error err;
      if (dwarf_init(fd, DW_DLC_READ, NULL, NULL, &debug, &err) != DW_DLV_OK) {
         warnf("dwarf_init: %s: %s", fname, dwarf_errmsg(err));
         shash_put(hash, fname, (void *)-1);
         return NULL;
      }

      handle = xcalloc(sizeof(libdwarf_handle_t));
      handle->fd    = fd;
      handle->debug = debug;

      shash_put(hash, fname, handle);
   }

   return handle;
}

static void libdwarf_get_symbol(libdwarf_handle_t *handle, Dwarf_Die die,
                                Dwarf_Unsigned rel_addr, debug_frame_t *frame)
{
   Dwarf_Die child, prev = NULL;
   if (dwarf_child(die, &child, NULL) != DW_DLV_OK)
      return;

   Dwarf_Half tag;
   if (dwarf_tag(child, &tag, NULL) == DW_DLV_OK && tag == DW_TAG_module) {
      char *name;
      if (dwarf_diename(child, &name, NULL) == DW_DLV_OK) {
         frame->vhdl_unit = ident_new(name);
         dwarf_dealloc(handle->debug, name, DW_DLA_STRING);
      }

      prev = child;
      if (dwarf_child(prev, &child, NULL) != DW_DLV_OK)
         goto out_dealloc;
   }

   do {
      if (prev != NULL)
         dwarf_dealloc(handle->debug, prev, DW_DLA_DIE);
      prev = child;

      if (dwarf_tag(child, &tag, NULL) != DW_DLV_OK)
         continue;
      else if (tag != DW_TAG_subprogram && tag != DW_TAG_inlined_subroutine)
         continue;
      else if (!libdwarf_die_has_pc(child, rel_addr))
         continue;

      char *name;
      if (dwarf_diename(child, &name, NULL) == DW_DLV_OK) {
         frame->symbol = xstrdup(name);
         dwarf_dealloc(handle->debug, name, DW_DLA_STRING);
      }

      Dwarf_Unsigned srclang;
      if (dwarf_srclang(die, &srclang, NULL) == DW_DLV_OK) {
         if (srclang == DW_LANG_Ada83) {
            frame->kind = FRAME_VHDL;
         }
      }

      break;
   } while (dwarf_siblingof(handle->debug, child, &child, NULL) == DW_DLV_OK);

 out_dealloc:
   if (prev != NULL)
      dwarf_dealloc(handle->debug, prev, DW_DLA_DIE);
}

static void libdwarf_get_srcline(libdwarf_handle_t *handle, Dwarf_Die die,
                                 Dwarf_Unsigned rel_addr, debug_frame_t *frame)
{
   Dwarf_Line*  linebuf = NULL;
   Dwarf_Signed linecount = 0;
   Dwarf_Signed idx = 0;
   if (dwarf_srclines(die, &linebuf, &linecount, NULL) == DW_DLV_OK) {
      Dwarf_Unsigned pladdr = 0;
      for (Dwarf_Signed i = 0; i < linecount; i++) {
         Dwarf_Unsigned laddr;
         if (dwarf_lineaddr(linebuf[i], &laddr, NULL) != DW_DLV_OK)
            break;
         else if (rel_addr == laddr) {
            idx = i;
            break;
         }
         else if (rel_addr < laddr && rel_addr > pladdr) {
            idx = i - 1;
            break;
         }
         pladdr = laddr;
      }

      if (idx >= 0) {
         Dwarf_Unsigned lineno;
         if (dwarf_lineno(linebuf[idx], &lineno, NULL) == DW_DLV_OK)
            frame->lineno = lineno;

         char *srcfile;
         if (dwarf_linesrc(linebuf[idx], &srcfile, NULL) == DW_DLV_OK) {
            frame->srcfile = xstrdup(srcfile);
            dwarf_dealloc(handle->debug, srcfile, DW_DLA_STRING);
         }
      }

      dwarf_srclines_dealloc(handle->debug, linebuf, linecount);
   }
}

static bool libdwarf_scan_aranges(libdwarf_handle_t *handle,
                                  Dwarf_Unsigned rel_addr,
                                  debug_frame_t *frame)
{
   Dwarf_Off cu_header_offset = 0;
   bool      found = false;

   if (handle->aranges == NULL) {
      if (dwarf_get_aranges(handle->debug, &(handle->aranges),
                            &(handle->arange_count), NULL) != DW_DLV_OK)
         return false;
   }

   Dwarf_Arange arange;
   if (dwarf_get_arange(handle->aranges, handle->arange_count, rel_addr,
                        &arange, NULL) != DW_DLV_OK)
      return false;

   if (dwarf_get_cu_die_offset(arange, &cu_header_offset, NULL) != DW_DLV_OK)
      return false;

   Dwarf_Die die = NULL;
   if (dwarf_offdie(handle->debug, cu_header_offset, &die, NULL) != DW_DLV_OK)
      return false;

   Dwarf_Half tag = 0;
   if (dwarf_tag(die, &tag, NULL) != DW_DLV_OK)
      goto free_die;
   else if (tag != DW_TAG_compile_unit)
      goto free_die;

   if (libdwarf_die_has_pc(die, rel_addr)) {
      libdwarf_get_srcline(handle, die, rel_addr, frame);
      libdwarf_get_symbol(handle, die, rel_addr, frame);
      found = true;
   }

 free_die:
   dwarf_dealloc(handle->debug, die, DW_DLA_DIE);

   return found;
}

static bool libdwarf_scan_cus(libdwarf_handle_t *handle,
                              Dwarf_Unsigned rel_addr,
                              debug_frame_t *frame)
{
   Dwarf_Unsigned next_cu_offset;
   bool found = false;
   while (dwarf_next_cu_header(handle->debug, NULL, NULL, NULL, NULL,
                               &next_cu_offset, NULL) == DW_DLV_OK) {

      if (found)
         continue;   // Read all the way to the end to reset the iterator

      Dwarf_Die die = NULL;
      if (dwarf_siblingof(handle->debug, 0, &die, NULL) != DW_DLV_OK)
         continue;

      Dwarf_Half tag = 0;
      if (dwarf_tag(die, &tag, NULL) != DW_DLV_OK)
         goto free_die;
      else if (tag != DW_TAG_compile_unit)
         goto free_die;

      if (libdwarf_die_has_pc(die, rel_addr)) {
         libdwarf_get_srcline(handle, die, rel_addr, frame);
         libdwarf_get_symbol(handle, die, rel_addr, frame);
         found = true;
      }

   free_die:
      dwarf_dealloc(handle->debug, die, DW_DLA_DIE);
      die = NULL;
   }

   return found;
}

static void libdwarf_fill_frame(uintptr_t ip, debug_frame_t *frame)
{
   Dl_info dli;
   if (!dladdr((void *)ip, &dli))
      return;

   static void *home_fbase = NULL;
   if (home_fbase == NULL) {
      Dl_info dli_home;
      extern int main(int, char **);
      if (dladdr(main, &dli_home))
         home_fbase = dli_home.dli_fbase;
   }

   frame->kind   = dli.dli_fbase == home_fbase ? FRAME_PROG : FRAME_LIB;
   frame->module = xstrdup(dli.dli_fname);
   frame->disp   = ip - (uintptr_t)dli.dli_saddr;

#ifdef __FreeBSD__
   // FreeBSD has non-standard libdwarf
   Dwarf_Addr rel_addr;
   if (frame->kind == FRAME_PROG)
      rel_addr = ip;
   else
      rel_addr = ip - (uintptr_t)dli.dli_fbase;
#else
   Dwarf_Addr rel_addr = ip - (uintptr_t)dli.dli_fbase;
#endif

   libdwarf_handle_t *handle = libdwarf_handle_for_file(dli.dli_fname);
   if (handle == NULL)
      return;

   if (!libdwarf_scan_aranges(handle, rel_addr, frame)) {
      // Clang does emit aranges so we have to search each compilation unit
      libdwarf_scan_cus(handle, rel_addr, frame);
   }

   if (frame->symbol == NULL && dli.dli_sname != NULL) {
      // Fallback: just use the nearest global symbol
      frame->symbol = xstrdup(dli.dli_sname);
   }
}

static _Unwind_Reason_Code libdwarf_frame_iter(struct _Unwind_Context* ctx,
                                               void *param)
{
   debug_info_t *di = param;

   if (di->skip > 0) {
      di->skip--;
      return _URC_NO_REASON;
   }

   int ip_before_instruction = 0;
   uintptr_t ip = _Unwind_GetIPInfo(ctx, &ip_before_instruction);

   if (ip == 0)
      return _URC_NO_REASON;
   else if (!ip_before_instruction)
      ip -= 1;

   debug_frame_t *frame;
   if (!di_lru_get(ip, &frame))
      libdwarf_fill_frame(ip, frame);

   APUSH(di->frames, frame);

   if (frame->symbol != NULL && strcmp(frame->symbol, "main") == 0)
      return _URC_NORMAL_STOP;
   else
      return _URC_NO_REASON;
}

__attribute__((always_inline))
static inline void debug_walk_frames(debug_info_t *di)
{
   di->skip = 1;
   _Unwind_Backtrace(libdwarf_frame_iter, di);
}

////////////////////////////////////////////////////////////////////////////////
// Windows backend

#elif defined __MINGW32__ || defined __CYGWIN__

__attribute__((noinline))
static void debug_walk_frames(debug_info_t *di)
{
   CONTEXT context;
   RtlCaptureContext(&context);

#ifdef __WIN64
   STACKFRAME64 stk;
   memset(&stk, 0, sizeof(stk));

   stk.AddrPC.Offset    = context.Rip;
   stk.AddrPC.Mode      = AddrModeFlat;
   stk.AddrStack.Offset = context.Rsp;
   stk.AddrStack.Mode   = AddrModeFlat;
   stk.AddrFrame.Offset = context.Rbp;
   stk.AddrFrame.Mode   = AddrModeFlat;

   HANDLE hProcess = GetCurrentProcess();

   static bool done_sym_init = false;
   if (!done_sym_init) {
     SymInitialize(hProcess, NULL, TRUE);
     done_sym_init = true;
   }

   int skip = 2;
   for (ULONG n = 0; n < MAX_TRACE_DEPTH; n++) {
      if (!StackWalk64(IMAGE_FILE_MACHINE_AMD64,
                       hProcess,
                       GetCurrentThread(),
                       &stk,
                       &context,
                       NULL,
                       SymFunctionTableAccess,
                       SymGetModuleBase,
                       NULL))
         break;

      if (skip-- > 0)
         continue;

      debug_frame_t *frame;
      if (!di_lru_get((uintptr_t)stk.AddrPC.Offset, &frame)) {
         frame->kind = FRAME_PROG;

         char buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)];
         PSYMBOL_INFO psym = (PSYMBOL_INFO)buffer;
         memset(buffer, '\0', sizeof(SYMBOL_INFO));
         psym->SizeOfStruct = sizeof(SYMBOL_INFO);
         psym->MaxNameLen = MAX_SYM_NAME;

         DWORD64 disp;
         if (SymFromAddr(hProcess, stk.AddrPC.Offset, &disp, psym)) {
            text_buf_t *tb = unsafe_symbol(psym->Name);
            frame->symbol = tb_claim(tb);
            frame->disp   = disp;
         }

         IMAGEHLP_MODULE module;
         memset(&module, '\0', sizeof(module));
         module.SizeOfStruct = sizeof(module);
         if (SymGetModuleInfo(hProcess, stk.AddrPC.Offset, &module)) {
            frame->module = xstrdup(module.ModuleName);

            if (lib_at(module.ImageName) != NULL)
               guess_vhdl_symbol(frame);
         }
      }

      APUSH(di->frames, frame);
   }
#endif  // __WIN64
}

////////////////////////////////////////////////////////////////////////////////
// Unwind backend

#else

static void unwind_fill_frame(uintptr_t ip, debug_frame_t *frame)
{
   Dl_info dli;
   if (!dladdr((void *)ip, &dli))
      return;

   static void *home_fbase = NULL;
   if (home_fbase == NULL) {
      Dl_info dli_home;
      extern int main(int, char **);
      if (dladdr(main, &dli_home))
         home_fbase = dli_home.dli_fbase;
   }

   frame->kind   = dli.dli_fbase == home_fbase ? FRAME_PROG : FRAME_LIB;
   frame->module = xstrdup(dli.dli_fname);
   frame->disp   = ip - (uintptr_t)dli.dli_saddr;

   if (dli.dli_sname) {
      frame->symbol = xstrdup(dli.dli_sname);

      if (lib_at(frame->module) != NULL)
         guess_vhdl_symbol(frame);
   }
}

static _Unwind_Reason_Code unwind_frame_iter(struct _Unwind_Context* ctx,
                                             void *param)
{
   debug_info_t *di = param;

   if (di->skip > 0) {
      di->skip--;
      return _URC_NO_REASON;
   }

   int ip_before_instruction = 0;
   uintptr_t ip = _Unwind_GetIPInfo(ctx, &ip_before_instruction);

   if (ip == 0)
      return _URC_NO_REASON;
   else if (!ip_before_instruction)
      ip -= 1;

   debug_frame_t *frame;
   if (!di_lru_get(ip, &frame))
      unwind_fill_frame(ip, frame);

   APUSH(di->frames, frame);

   if (frame->symbol != NULL && strcmp(frame->symbol, "main") == 0)
      return _URC_NORMAL_STOP;
   else
      return _URC_NO_REASON;
}

__attribute__((always_inline))
static inline void debug_walk_frames(debug_info_t *di)
{
   di->skip = 1;
   _Unwind_Backtrace(unwind_frame_iter, di);
}

#endif

////////////////////////////////////////////////////////////////////////////////
// Public interface

__attribute__((noinline))
debug_info_t *debug_capture(void)
{
   debug_info_t *di = xcalloc(sizeof(debug_info_t));
   debug_walk_frames(di);
   return di;
}

void debug_free(debug_info_t *di)
{
   ACLEAR(di->frames);
   free(di);
}

unsigned debug_count_frames(debug_info_t *di)
{
   return di->frames.count;
}

const debug_frame_t *debug_get_frame(debug_info_t *di, unsigned n)
{
   return AGET(di->frames, n);
}
