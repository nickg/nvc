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

#if defined(__MINGW32__)
#define WINVER 0x0A00
#define _WIN32_WINNT 0x0A00
#include <windows.h>
#include <DbgHelp.h>
#endif

#include "debug.h"
#include "array.h"
#include "hash.h"
#include "ident.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#ifndef __MINGW32__
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <dlfcn.h>
#include <errno.h>
#endif  // !__MINGW32__

#if defined HAVE_LIBDW
#include <elfutils/libdw.h>
#include <elfutils/libdwfl.h>
#include <dwarf.h>
#include <unwind.h>
#elif defined HAVE_LIBDWARF
#include <libdwarf/libdwarf.h>
#include <libdwarf/dwarf.h>
#include <libelf.h>
#include <unwind.h>
#elif defined HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#define MAX_TRACE_DEPTH   25

struct debug_info {
   A(debug_frame_t) frames;
   unsigned         skip;
};

////////////////////////////////////////////////////////////////////////////////
// Libdw backend

#if defined HAVE_LIBDW

static bool die_has_pc(Dwarf_Die* die, Dwarf_Addr pc)
{
   Dwarf_Addr low, high;

   if (dwarf_hasattr(die, DW_AT_low_pc) && dwarf_hasattr(die, DW_AT_high_pc)) {
      if (dwarf_lowpc(die, &low) != 0)
         return false;
      if (dwarf_highpc(die, &high) != 0) {
         Dwarf_Attribute attr_mem;
         Dwarf_Attribute* attr = dwarf_attr(die, DW_AT_high_pc, &attr_mem);
         Dwarf_Word value;
         if (dwarf_formudata(attr, &value) != 0)
            return false;
         high = low + value;
      }
      return pc >= low && pc < high;
   }

   Dwarf_Addr base;
   ptrdiff_t offset = 0;
   while ((offset = dwarf_ranges(die, offset, &base, &low, &high)) > 0) {
      if (pc >= low && pc < high)
         return true;
   }

   return false;
}

static _Unwind_Reason_Code libdw_frame_iter(struct _Unwind_Context* ctx,
                                            void *param)
{
   static Dwfl *handle = NULL;
   static Dwfl_Module *home = NULL;

   debug_info_t *di = param;

   if (handle == NULL) {
      static Dwfl_Callbacks callbacks = {
         .find_elf = dwfl_linux_proc_find_elf,
         .find_debuginfo = dwfl_standard_find_debuginfo,
         .debuginfo_path = NULL
      };

      if ((handle = dwfl_begin(&callbacks)) == NULL) {
         warnf("failed to initialise dwfl");
         return _URC_NORMAL_STOP;
      }

      dwfl_report_begin(handle);
      if (dwfl_linux_proc_report(handle, getpid()) < 0) {
         warnf("dwfl_linux_proc_report failed");
         return _URC_NORMAL_STOP;
      }
      dwfl_report_end(handle, NULL, NULL);

      home = dwfl_addrmodule(handle, (uintptr_t)libdw_frame_iter);
   }

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

   debug_frame_t frame = {
      .pc = ip
   };

   Dwfl_Module *mod = dwfl_addrmodule(handle, ip);

   const char *module_name = dwfl_module_info(mod, 0, 0, 0, 0, 0, 0, 0);
   const char *sym_name = dwfl_module_addrname(mod, ip);

   Dwarf_Addr mod_bias = 0;
   Dwarf_Die *die = dwfl_module_addrdie(mod, ip, &mod_bias);

   if (die == NULL) {
      // Clang does not emit aranges so search each CU
      while ((die = dwfl_module_nextcu(mod, die, &mod_bias))) {
         Dwarf_Die child;
         if (dwarf_child(die, &child) != 0)
            continue;

         Dwarf_Die* iter = &child;
         do {
            switch (dwarf_tag(iter)) {
            case DW_TAG_subprogram:
            case DW_TAG_inlined_subroutine:
               if (die_has_pc(iter, ip))
                  goto found_die_with_ip;
            }
         } while (dwarf_siblingof(iter, iter) == 0);
      }
   found_die_with_ip:
      ;
   }

   Dwarf_Line* srcloc = dwarf_getsrc_die(die, ip - mod_bias);
   const char* srcfile = dwarf_linesrc(srcloc, 0, 0);

   int line = 0, col = 0;
   dwarf_lineno(srcloc, &line);
   dwarf_linecol(srcloc, &col);

   if (srcfile != NULL)
      frame.srcfile = xstrdup(srcfile);
   if (sym_name != NULL)
      frame.symbol = xstrdup(sym_name);
   if (module_name != NULL)
      frame.module = xstrdup(module_name);

   frame.lineno = line;

   if (mod == home)
      frame.kind = FRAME_PROG;
   else
      frame.kind = FRAME_LIB;

   APUSH(di->frames, frame);

   if (sym_name != NULL && strcmp(sym_name, "main") == 0)
      return _URC_NORMAL_STOP;
   else
      return _URC_NO_REASON;
}

__attribute__((noinline))
static void debug_walk_frames(debug_info_t *di)
{
   di->skip = 2;
   _Unwind_Backtrace(libdw_frame_iter, di);
}

////////////////////////////////////////////////////////////////////////////////
// Libdwarf backend

#elif defined HAVE_LIBDWARF

static bool libdwarf_die_has_pc(Dwarf_Die die, Dwarf_Addr pc)
{
   Dwarf_Addr low_pc = 0, high_pc = 0;
   Dwarf_Error error;

   if (dwarf_lowpc(die, &low_pc, &error) == DW_DLV_OK
       && dwarf_highpc(die, &high_pc, &error) == DW_DLV_OK) {

      return pc >= low_pc && pc < high_pc;
   }

   return false;
}

static Dwarf_Debug libdwarf_handle_for_file(const char *fname)
{
   static hash_t *hash = NULL;

   if (hash == NULL)
      hash = hash_new(64, true);

   ident_t key = ident_new(fname);
   Dwarf_Debug handle = hash_get(hash, key);

   if (handle == NULL) {
      if (elf_version(EV_CURRENT) == EV_NONE)
         fatal("ELF library too old");

      int fd = open(fname, O_RDONLY);
      if (fd == -1) {
         warnf("%s: %s", fname, strerror(errno));
         return NULL;
      }

      Dwarf_Error err;
      if (dwarf_init(fd, DW_DLC_READ, NULL, NULL, &handle, &err) != DW_DLV_OK) {
         warnf("dwarf_init: %s: %s", fname, dwarf_errmsg(err));
         return NULL;
      }

      hash_put(hash, key, handle);
   }

   return handle;
}

static void libdwarf_get_symbol(Dwarf_Debug handle, Dwarf_Die die,
                                Dwarf_Unsigned rel_addr, debug_frame_t *frame)
{
   Dwarf_Error error;
   Dwarf_Die child;
   if (dwarf_child(die, &child, &error) == DW_DLV_OK) {
      do {
         Dwarf_Half tag;
         if (dwarf_tag(child, &tag, &error) != DW_DLV_OK)
            continue;
         else if (tag != DW_TAG_subprogram && tag != DW_TAG_inlined_subroutine)
            continue;
         else if (!libdwarf_die_has_pc(child, rel_addr))
            continue;

         char *name;
         if (dwarf_diename(child, &name, &error) == DW_DLV_OK)
            frame->symbol = xstrdup(name);

      } while (dwarf_siblingof(handle, child, &child, &error) == DW_DLV_OK);
   }
}

static void libdwarf_get_srcline(Dwarf_Debug handle, Dwarf_Die die,
                                 Dwarf_Unsigned rel_addr, debug_frame_t *frame)
{
   Dwarf_Error  error;
   Dwarf_Line*  linebuf = NULL;
   Dwarf_Signed linecount = 0;
   Dwarf_Signed idx = 0;
   if (dwarf_srclines(die, &linebuf, &linecount, &error) == DW_DLV_OK) {
      Dwarf_Unsigned pladdr = 0;
      for (Dwarf_Signed i = 0; i < linecount; i++) {
         Dwarf_Unsigned laddr;
         if (dwarf_lineaddr(linebuf[i], &laddr, &error) != DW_DLV_OK)
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
         if (dwarf_lineno(linebuf[idx], &lineno, &error) == DW_DLV_OK)
            frame->lineno = lineno;

         char *srcfile;
         if (dwarf_linesrc(linebuf[idx], &srcfile, &error) == DW_DLV_OK)
            frame->srcfile = xstrdup(srcfile);
      }

      dwarf_srclines_dealloc(handle, linebuf, linecount);
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

   debug_frame_t frame = {
      .pc = ip
   };

   Dl_info dli;
   if (!dladdr((void *)ip, &dli)) {
      warnf("dladdr: %p: %s", (void *)ip, dlerror());
      return _URC_NO_REASON;
   }

   static void *home_fbase = NULL;
   if (home_fbase == NULL) {
      Dl_info dli_home;
      extern int main(int, char **);
      if (dladdr(main, &dli_home))
         home_fbase = dli_home.dli_fbase;
   }

   frame.kind   = dli.dli_fbase == home_fbase ? FRAME_PROG : FRAME_LIB;
   frame.module = xstrdup(dli.dli_fname);
   frame.disp   = ip - (uintptr_t)dli.dli_saddr;

   Dwarf_Addr rel_addr = ip - (uintptr_t)dli.dli_fbase;

   Dwarf_Debug handle = libdwarf_handle_for_file(dli.dli_fname);
   if (handle == NULL)
      return _URC_NO_REASON;

   Dwarf_Error error = DW_DLE_NE;
   Dwarf_Die die = NULL;
   bool found = false;

   // We could check dwarf_get_aranges here but Clang doesn't emit it

   Dwarf_Unsigned next_cu_header_offset;
   Dwarf_Half tag = 0;

   while (dwarf_next_cu_header_d(handle, 1, NULL, NULL, NULL, NULL, NULL,
                                 NULL, NULL, NULL, &next_cu_header_offset,
                                 NULL, &error) == DW_DLV_OK) {

      if (!found && dwarf_siblingof(handle, 0, &die, &error) == DW_DLV_OK
          && dwarf_tag(die, &tag, &error) == DW_DLV_OK
          && tag == DW_TAG_compile_unit
          && libdwarf_die_has_pc(die, rel_addr)) {

         found = true;

         libdwarf_get_srcline(handle, die, rel_addr, &frame);
         libdwarf_get_symbol(handle, die, rel_addr, &frame);
      }

      if (die) {
         dwarf_dealloc(handle, die, DW_DLA_DIE);
         die = NULL;
      }
   }

   if (frame.symbol == NULL)
      frame.symbol = xstrdup(dli.dli_sname);

   APUSH(di->frames, frame);

   if (strcmp(frame.symbol, "main") == 0)
      return _URC_NORMAL_STOP;
   else
      return _URC_NO_REASON;
}

__attribute__((noinline))
static void debug_walk_frames(debug_info_t *di)
{
   di->skip = 2;
   _Unwind_Backtrace(libdwarf_frame_iter, di);
}

////////////////////////////////////////////////////////////////////////////////
// Windows backend

#elif defined __MINGW32__

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

   SymInitialize(hProcess, NULL, TRUE);

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

      debug_frame_t frame = {
         .kind = FRAME_PROG,
         .pc   = (uintptr_t)stk.AddrPC.Offset
      };

      char buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)];
      PSYMBOL_INFO psym = (PSYMBOL_INFO)buffer;
      psym->SizeOfStruct = sizeof(SYMBOL_INFO);
      psym->MaxNameLen = MAX_SYM_NAME;

      DWORD64 disp;
      if (SymFromAddr(hProcess, stk.AddrPC.Offset, &disp, psym)) {
         frame.symbol = xstrdup(psym->Name);
         frame.disp   = disp;
      }

      APUSH(di->frames, frame);
   }

   SymCleanup(hProcess);
#endif  // __WIN64
}

////////////////////////////////////////////////////////////////////////////////
// Execinfo backend

#elif defined HAVE_EXECINFO_H

__attribute__((noinline))
static void debug_walk_frames(debug_info_t *di)
{
   void *trace[MAX_TRACE_DEPTH];
   char **messages = NULL;
   int trace_size = 0;

   trace_size = backtrace(trace, MAX_TRACE_DEPTH);
   messages = backtrace_symbols(trace, trace_size);

   for (int i = 2; i < trace_size; i++) {
      debug_frame_t frame = {
         .kind   = FRAME_PROG,
         .pc     = (uintptr_t)trace[i],
         .symbol = xstrdup(messages[i])
      };

      APUSH(di->frames, frame);
   }

   free(messages);
}

#else
#error No suitable frame walking backend
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
   for (unsigned i = 0; i < di->frames.count; i++) {
      debug_frame_t *f = AREF(di->frames, i);
      free((char *)f->module);
      free((char *)f->symbol);
      free((char *)f->srcfile);
   }

   ACLEAR(di->frames);
   free(di);
}

unsigned debug_count_frames(debug_info_t *di)
{
   return di->frames.count;
}

const debug_frame_t *debug_get_frame(debug_info_t *di, unsigned n)
{
   return AREF(di->frames, n);
}
